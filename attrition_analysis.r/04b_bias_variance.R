# =============================================================================
# Phase 4b: Bias–Variance Diagnosis
# Project: Predicting Employee Attrition Using AI
# Inputs:  train_data.rds, val_data.rds (scaled)
#          train_data_unscaled.rds, val_data_unscaled.rds
#          model_logistic.rds, model_glmnet.rds, model_tree.rds
# Outputs: outputs/plots/12_bias_variance_f1.png
#          outputs/plots/13_bias_variance_auc.png
#          outputs/plots/14_roc_curves.png
# =============================================================================

library(dplyr)
library(glmnet)
library(rpart)
library(ggplot2)

# =============================================================================
# Helpers (identical to 04_model_training.R)
# =============================================================================

evaluate <- function(probs, labels, threshold = 0.5) {
  preds     <- ifelse(probs >= threshold, 1, 0)
  tp        <- sum(preds == 1 & labels == 1)
  fp        <- sum(preds == 1 & labels == 0)
  tn        <- sum(preds == 0 & labels == 0)
  fn        <- sum(preds == 0 & labels == 1)
  accuracy  <- (tp + tn) / length(labels)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall    <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1        <- if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0
  auc       <- roc_auc(probs, labels)
  list(accuracy = accuracy, precision = precision,
       recall = recall, f1 = f1, auc = auc,
       tp = tp, fp = fp, tn = tn, fn = fn)
}

roc_auc <- function(probs, labels) {
  ord   <- order(probs, decreasing = TRUE)
  labs  <- labels[ord]
  n_pos <- sum(labels == 1)
  n_neg <- sum(labels == 0)
  if (n_pos == 0 || n_neg == 0) return(NA)
  tpr <- c(0, cumsum(labs == 1) / n_pos, 1)
  fpr <- c(0, cumsum(labs == 0) / n_neg, 1)
  sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1)) / 2)
}

roc_curve_df <- function(probs, labels, model_name) {
  ord   <- order(probs, decreasing = TRUE)
  labs  <- labels[ord]
  n_pos <- sum(labels == 1)
  n_neg <- sum(labels == 0)
  tpr <- c(0, cumsum(labs == 1) / n_pos, 1)
  fpr <- c(0, cumsum(labs == 0) / n_neg, 1)
  data.frame(fpr = fpr, tpr = tpr, model = model_name)
}

diagnose <- function(train_f1, val_f1, gap) {
  if (gap > 0.10 && train_f1 >= 0.65) {
    "HIGH VARIANCE -- overfitting"
  } else if (train_f1 < 0.65 && gap < 0.05) {
    "HIGH BIAS -- underfitting"
  } else if (gap < 0.05 && train_f1 >= 0.65) {
    "GOOD FIT"
  } else {
    "MODERATE VARIANCE"
  }
}

# =============================================================================
# STEP 1: Load data and models
# =============================================================================

cat("==============================================\n")
cat("PHASE 4b: BIAS-VARIANCE DIAGNOSIS\n")
cat("==============================================\n\n")

cat("--- Loading data ---\n")
train_s <- readRDS("train_data.rds")
val_s   <- readRDS("val_data.rds")
train_u <- readRDS("train_data_unscaled.rds")
val_u   <- readRDS("val_data_unscaled.rds")

cat(sprintf("Train (scaled):   %d rows x %d cols\n", nrow(train_s), ncol(train_s)))
cat(sprintf("Val   (scaled):   %d rows x %d cols\n", nrow(val_s),   ncol(val_s)))

cat("\n--- Loading models ---\n")
model_logistic  <- readRDS("model_logistic.rds")
model_glmnet_obj <- readRDS("model_glmnet.rds")   # list: model, alpha, lambda
model_tree      <- readRDS("model_tree.rds")

glmnet_fit    <- model_glmnet_obj$model
glmnet_alpha  <- model_glmnet_obj$alpha
glmnet_lambda <- model_glmnet_obj$lambda

cat(sprintf("glmnet: alpha=%.1f, lambda=%.5f\n", glmnet_alpha, glmnet_lambda))
cat("All models loaded.\n\n")

# Feature matrices for glmnet
X_train <- as.matrix(train_s %>% select(-Attrition))
y_train <- train_s$Attrition
X_val   <- as.matrix(val_s   %>% select(-Attrition))
y_val   <- val_s$Attrition

# =============================================================================
# STEP 2: Get probabilities on train AND val for all models
# =============================================================================

# --- Logistic Regression ---
prob_lr_train <- predict(model_logistic, newdata = train_s, type = "response")
prob_lr_val   <- predict(model_logistic, newdata = val_s,   type = "response")

# --- glmnet ---
prob_gn_train <- predict(glmnet_fit, newx = X_train, s = glmnet_lambda, type = "response")[, 1]
prob_gn_val   <- predict(glmnet_fit, newx = X_val,   s = glmnet_lambda, type = "response")[, 1]

# --- Decision Tree ---
prob_dt_train <- predict(model_tree, newdata = train_u, type = "prob")[, "1"]
prob_dt_val   <- predict(model_tree, newdata = val_u,   type = "prob")[, "1"]

y_train_u <- train_u$Attrition
y_val_u   <- val_u$Attrition

# =============================================================================
# STEP 3: Evaluate metrics
# =============================================================================

m_lr_tr <- evaluate(prob_lr_train, y_train)
m_lr_vl <- evaluate(prob_lr_val,   y_val)

m_gn_tr <- evaluate(prob_gn_train, y_train)
m_gn_vl <- evaluate(prob_gn_val,   y_val)

m_dt_tr <- evaluate(prob_dt_train, y_train_u)
m_dt_vl <- evaluate(prob_dt_val,   y_val_u)

# =============================================================================
# STEP 4: Compute gaps and diagnoses
# =============================================================================

results <- list(
  list(name  = "Logistic Regression",
       tr_f1 = m_lr_tr$f1, vl_f1 = m_lr_vl$f1,
       tr_ac = m_lr_tr$accuracy, vl_ac = m_lr_vl$accuracy,
       tr_au = m_lr_tr$auc, vl_au = m_lr_vl$auc),
  list(name  = "glmnet Ridge",
       tr_f1 = m_gn_tr$f1, vl_f1 = m_gn_vl$f1,
       tr_ac = m_gn_tr$accuracy, vl_ac = m_gn_vl$accuracy,
       tr_au = m_gn_tr$auc, vl_au = m_gn_vl$auc),
  list(name  = "Decision Tree",
       tr_f1 = m_dt_tr$f1, vl_f1 = m_dt_vl$f1,
       tr_ac = m_dt_tr$accuracy, vl_ac = m_dt_vl$accuracy,
       tr_au = m_dt_tr$auc, vl_au = m_dt_vl$auc)
)

for (r in results) {
  r$gap_f1  <- r$tr_f1 - r$vl_f1
  r$gap_auc <- r$tr_au - r$vl_au
  r$diag    <- diagnose(r$tr_f1, r$vl_f1, r$gap_f1)
}

# =============================================================================
# STEP 5: Print table
# =============================================================================

cat("==============================================\n")
cat("BIAS-VARIANCE DIAGNOSIS TABLE\n")
cat("==============================================\n\n")

hdr <- sprintf("%-22s | %-9s | %-7s | %-16s | %-10s | %-8s | %s",
               "Model", "Train F1", "Val F1", "Gap (Variance)",
               "Train AUC", "Val AUC", "Diagnosis")
sep <- paste(rep("-", nchar(hdr) + 2), collapse = "")
cat(sep, "\n")
cat(hdr, "\n")
cat(sep, "\n")

for (r in results) {
  gap_f1 <- r$tr_f1 - r$vl_f1
  gap_auc <- r$tr_au - r$vl_au
  diag   <- diagnose(r$tr_f1, r$vl_f1, gap_f1)
  cat(sprintf("%-22s | %-9.3f | %-7.3f | %-16.3f | %-10.3f | %-8.3f | %s\n",
              r$name, r$tr_f1, r$vl_f1, gap_f1, r$tr_au, r$vl_au, diag))
}
cat(sep, "\n\n")

# =============================================================================
# STEP 6: Training error rates
# =============================================================================

cat("--- Training Error Rates (1 - Train Accuracy) ---\n")
for (r in results) {
  err <- 1 - r$tr_ac
  cat(sprintf("  %-22s : %.4f  (Train Acc: %.4f)\n", r$name, err, r$tr_ac))
}
cat("\n")

# =============================================================================
# STEP 7: Plots
# =============================================================================

dir.create("../outputs/plots", recursive = TRUE, showWarnings = FALSE)

# ---- Plot 12: F1 Bar chart — Train vs Val ----
f1_df <- data.frame(
  Model  = rep(c("Logistic\nRegression", "glmnet\nRidge", "Decision\nTree"), each = 2),
  Split  = rep(c("Train", "Val"), 3),
  F1     = c(m_lr_tr$f1, m_lr_vl$f1,
             m_gn_tr$f1, m_gn_vl$f1,
             m_dt_tr$f1, m_dt_vl$f1)
)
f1_df$Model <- factor(f1_df$Model,
                      levels = c("Logistic\nRegression", "glmnet\nRidge", "Decision\nTree"))
f1_df$Split <- factor(f1_df$Split, levels = c("Train", "Val"))

p12 <- ggplot(f1_df, aes(x = Model, y = F1, fill = Split)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  geom_text(aes(label = sprintf("%.3f", F1)),
            position = position_dodge(width = 0.6),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("Train" = "#2196F3", "Val" = "#FF5722")) +
  scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.1)) +
  labs(title    = "Bias-Variance: F1 Score — Train vs Validation",
       subtitle = "Large gap = high variance (overfitting); Low train F1 = high bias",
       x = NULL, y = "F1 Score", fill = "Split") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        legend.position = "top")

ggsave("../outputs/plots/12_bias_variance_f1.png", p12,
       width = 8, height = 5, dpi = 150)
cat("Saved: outputs/plots/12_bias_variance_f1.png\n")

# ---- Plot 13: AUC Bar chart — Train vs Val ----
auc_df <- data.frame(
  Model = rep(c("Logistic\nRegression", "glmnet\nRidge", "Decision\nTree"), each = 2),
  Split = rep(c("Train", "Val"), 3),
  AUC   = c(m_lr_tr$auc, m_lr_vl$auc,
            m_gn_tr$auc, m_gn_vl$auc,
            m_dt_tr$auc, m_dt_vl$auc)
)
auc_df$Model <- factor(auc_df$Model,
                       levels = c("Logistic\nRegression", "glmnet\nRidge", "Decision\nTree"))
auc_df$Split <- factor(auc_df$Split, levels = c("Train", "Val"))

p13 <- ggplot(auc_df, aes(x = Model, y = AUC, fill = Split)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  geom_text(aes(label = sprintf("%.3f", AUC)),
            position = position_dodge(width = 0.6),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("Train" = "#2196F3", "Val" = "#FF5722")) +
  scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.1)) +
  labs(title    = "Bias-Variance: ROC-AUC — Train vs Validation",
       subtitle = "Large gap = high variance (overfitting); Low train AUC = high bias",
       x = NULL, y = "ROC-AUC", fill = "Split") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        legend.position = "top")

ggsave("../outputs/plots/13_bias_variance_auc.png", p13,
       width = 8, height = 5, dpi = 150)
cat("Saved: outputs/plots/13_bias_variance_auc.png\n")

# ---- Plot 14: ROC Curves — Val set, all 3 models ----
auc_lr <- round(m_lr_vl$auc, 3)
auc_gn <- round(m_gn_vl$auc, 3)
auc_dt <- round(m_dt_vl$auc, 3)

roc_df <- rbind(
  roc_curve_df(prob_lr_val, y_val,   sprintf("Logistic Regression (AUC=%.3f)", auc_lr)),
  roc_curve_df(prob_gn_val, y_val,   sprintf("glmnet Ridge (AUC=%.3f)",        auc_gn)),
  roc_curve_df(prob_dt_val, y_val_u, sprintf("Decision Tree (AUC=%.3f)",       auc_dt))
)

p14 <- ggplot(roc_df, aes(x = fpr, y = tpr, color = model)) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey60", linewidth = 0.7) +
  scale_color_manual(values = c("#1565C0", "#E65100", "#2E7D32")) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  labs(title    = "ROC Curves — Validation Set",
       subtitle = "Higher and to the left = better discrimination",
       x = "False Positive Rate", y = "True Positive Rate",
       color = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = c(0.65, 0.25),
        legend.background = element_rect(fill = "white", color = "grey80"))

ggsave("../outputs/plots/14_roc_curves.png", p14,
       width = 7, height = 6, dpi = 150)
cat("Saved: outputs/plots/14_roc_curves.png\n\n")

cat("==============================================\n")
cat("Phase 4b complete.\n")
cat("==============================================\n")
