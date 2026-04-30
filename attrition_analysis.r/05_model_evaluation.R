# =============================================================================
# Phase 5: Model Evaluation
# Compare all 6 models using optimal thresholds on the validation set
# =============================================================================

library(dplyr)
library(glmnet)
library(randomForest)
library(xgboost)
library(e1071)
library(ggplot2)

setwd("C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r")

cat("==============================================\n")
cat("PHASE 5: MODEL EVALUATION\n")
cat("==============================================\n\n")

# =============================================================================
# Helpers
# =============================================================================

roc_auc <- function(probs, labels) {
  ord   <- order(probs, decreasing = TRUE)
  lbl   <- labels[ord]
  n_pos <- sum(labels == 1)
  n_neg <- sum(labels == 0)
  tp    <- cumsum(lbl == 1)
  fp    <- cumsum(lbl == 0)
  tpr   <- tp / n_pos
  fpr   <- fp / n_neg
  sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1)) / 2)
}

evaluate <- function(probs, labels, threshold) {
  preds <- ifelse(probs >= threshold, 1, 0)
  tp  <- sum(preds == 1 & labels == 1)
  fp  <- sum(preds == 1 & labels == 0)
  fn  <- sum(preds == 0 & labels == 1)
  tn  <- sum(preds == 0 & labels == 0)
  acc  <- (tp + tn) / length(labels)
  prec <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  rec  <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1   <- if ((prec + rec) > 0) 2 * prec * rec / (prec + rec) else 0
  list(acc = acc, prec = prec, rec = rec, f1 = f1, tp = tp, fp = fp, fn = fn, tn = tn)
}

# =============================================================================
# Load data
# =============================================================================

val_s <- readRDS("val_data.rds")
val_u <- readRDS("val_data_unscaled.rds")
y_val <- val_s$Attrition

cat(sprintf("Val set: %d rows | %d leavers (%.1f%%) | %d stayers\n\n",
            length(y_val), sum(y_val), 100 * mean(y_val), sum(y_val == 0)))

# =============================================================================
# Optimal thresholds (from threshold sweep — Phase 4c / threshold_tables.R)
# =============================================================================

opt_t <- list(
  "Logistic Regression" = 0.25,
  "glmnet Ridge"        = 0.50,
  "Decision Tree"       = 0.11,
  "Random Forest"       = 0.24,
  "XGBoost"             = 0.39,
  "SVM"                 = 0.31
)

# =============================================================================
# Generate probabilities
# =============================================================================

cat("--- Loading models and generating probabilities ---\n")

# Logistic Regression (scaled data)
lr      <- readRDS("model_logistic.rds")
prob_lr <- predict(lr, newdata = val_s, type = "response")
cat("  Logistic Regression ... OK\n")

# glmnet Ridge (scaled data, matrix form)
gn_obj  <- readRDS("model_glmnet.rds")
X_val_s <- as.matrix(val_s %>% select(-Attrition))
prob_gn <- predict(gn_obj$model, newx = X_val_s,
                   s = gn_obj$lambda, type = "response")[, 1]
cat("  glmnet Ridge        ... OK\n")

# Decision Tree (unscaled data)
tree    <- readRDS("model_tree.rds")
prob_dt <- predict(tree, newdata = val_u, type = "prob")[, "1"]
cat("  Decision Tree       ... OK\n")

# Random Forest (unscaled, make.names, features only)
rf_obj <- readRDS("model_rf.rds")
val_rf <- val_u
names(val_rf) <- make.names(names(val_rf), unique = TRUE)
prob_rf <- predict(rf_obj$model,
                   newdata = val_rf %>% select(-Attrition),
                   type = "prob")[, "1"]
cat("  Random Forest       ... OK\n")

# XGBoost (unscaled, make.names, DMatrix)
xgb_obj <- readRDS("model_xgb.rds")
val_xgb <- val_u
names(val_xgb) <- make.names(names(val_xgb), unique = TRUE)
X_xgb   <- as.matrix(val_xgb %>% select(-Attrition))
dval    <- xgb.DMatrix(data = X_xgb)
prob_xgb <- predict(xgb_obj$model, dval)
cat("  XGBoost             ... OK\n")

# SVM (scaled, make.names, probability = TRUE)
svm_obj <- readRDS("model_svm.rds")
val_svm <- val_s
names(val_svm) <- make.names(names(val_svm), unique = TRUE)
pred_svm <- predict(svm_obj$model,
                    newdata = val_svm %>% select(-Attrition),
                    probability = TRUE)
prob_svm <- attr(pred_svm, "probabilities")[, "1"]
cat("  SVM                 ... OK\n\n")

probs_all <- list(
  "Logistic Regression" = prob_lr,
  "glmnet Ridge"        = prob_gn,
  "Decision Tree"       = prob_dt,
  "Random Forest"       = prob_rf,
  "XGBoost"             = prob_xgb,
  "SVM"                 = prob_svm
)

# =============================================================================
# Evaluate all models at optimal thresholds
# =============================================================================

results <- data.frame(
  Model     = character(),
  Threshold = numeric(),
  Accuracy  = numeric(),
  Precision = numeric(),
  Recall    = numeric(),
  F1        = numeric(),
  AUC       = numeric(),
  stringsAsFactors = FALSE
)

conf_mats <- list()

for (name in names(probs_all)) {
  t     <- opt_t[[name]]
  probs <- probs_all[[name]]
  m     <- evaluate(probs, y_val, t)
  auc   <- roc_auc(probs, y_val)
  results <- rbind(results, data.frame(
    Model     = name,
    Threshold = t,
    Accuracy  = round(m$acc,  3),
    Precision = round(m$prec, 3),
    Recall    = round(m$rec,  3),
    F1        = round(m$f1,   3),
    AUC       = round(auc,    3),
    stringsAsFactors = FALSE
  ))
  conf_mats[[name]] <- m
}

# Sort by F1 descending
results <- results[order(-results$F1), ]
rownames(results) <- NULL

# =============================================================================
# Print comparison table
# =============================================================================

cat("==============================================================================\n")
cat("  MODEL COMPARISON — OPTIMAL THRESHOLDS (Validation Set)\n")
cat("==============================================================================\n")
cat(sprintf("  %-22s  %-6s  %-8s  %-8s  %-8s  %-8s  %-6s\n",
            "Model", "Thresh", "Acc", "Prec", "Recall", "F1", "AUC"))
cat(rep("-", 78), "\n", sep = "")

for (i in seq_len(nrow(results))) {
  r    <- results[i, ]
  rank <- if (i == 1) " <-- best F1" else ""
  cat(sprintf("  %-22s  %-6.2f  %-8.3f  %-8.3f  %-8.3f  %-8.3f  %-6.3f%s\n",
              r$Model, r$Threshold, r$Accuracy,
              r$Precision, r$Recall, r$F1, r$AUC, rank))
}

cat(rep("-", 78), "\n", sep = "")
cat(sprintf("\n  Best F1     : %-22s  F1=%.3f  (t=%.2f)\n",
            results$Model[1], results$F1[1], results$Threshold[1]))
cat(sprintf("  Best AUC    : %-22s  AUC=%.3f\n",
            results$Model[which.max(results$AUC)], max(results$AUC)))
cat(sprintf("  Best Recall : %-22s  Recall=%.3f  (maximises attrition catch)\n",
            results$Model[which.max(results$Recall)], max(results$Recall)))
cat(sprintf("  Best Prec   : %-22s  Prec=%.3f   (minimises false alarms)\n",
            results$Model[which.max(results$Precision)], max(results$Precision)))
cat("\n")

# =============================================================================
# Confusion matrices
# =============================================================================

cat("==============================================================================\n")
cat("  CONFUSION MATRICES (Optimal Threshold)\n")
cat("==============================================================================\n\n")

for (name in results$Model) {
  m <- conf_mats[[name]]
  t <- opt_t[[name]]
  cat(sprintf("  %s  (t=%.2f)\n", name, t))
  cat(sprintf("                   Pred: Stay   Pred: Leave\n"))
  cat(sprintf("    Actual: Stay    %5d         %5d\n", m$tn, m$fp))
  cat(sprintf("    Actual: Leave   %5d         %5d\n", m$fn, m$tp))
  cat(sprintf("    Miss rate (FN): %.1f%%  |  False alarm rate (FP): %.1f%%\n",
              100 * m$fn / sum(y_val == 1),
              100 * m$fp / sum(y_val == 0)))
  cat("\n")
}

# =============================================================================
# Save results
# =============================================================================

saveRDS(list(
  results        = results,
  probabilities  = probs_all,
  opt_thresholds = opt_t,
  y_val          = y_val,
  conf_mats      = conf_mats
), "model_evaluation.rds")
cat("Saved: model_evaluation.rds\n\n")

# =============================================================================
# Plots
# =============================================================================

dir.create("../outputs/plots", recursive = TRUE, showWarnings = FALSE)

# ── Plot 27: F1 bar chart (sorted) ──────────────────────────────────────────
res_plot <- results
res_plot$Model <- factor(res_plot$Model,
                         levels = res_plot$Model[order(res_plot$F1)])  # asc so top = best in coord_flip

p27 <- ggplot(res_plot, aes(x = Model, y = F1,
                             fill = ifelse(F1 == max(F1), "best", "other"))) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", F1), y = F1 + 0.01),
            hjust = 0, size = 3.6) +
  scale_fill_manual(values = c("best" = "#2a9d8f", "other" = "#74b9d8"),
                    guide = "none") +
  coord_flip() +
  ylim(0, 0.82) +
  labs(
    title    = "Model Comparison — F1 Score at Optimal Threshold",
    subtitle = "Validation set | Ranked by F1",
    x = NULL, y = "F1 Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

ggsave("../outputs/plots/27_model_comparison_f1.png",
       plot = p27, width = 8, height = 5, dpi = 150)
cat("Saved: outputs/plots/27_model_comparison_f1.png\n")

# ── Plot 28: Multi-metric grouped bar chart ──────────────────────────────────
ml <- do.call(rbind, lapply(c("Precision", "Recall", "F1", "AUC"), function(met) {
  data.frame(Model = res_plot$Model, Metric = met,
             Value = res_plot[[met]], stringsAsFactors = FALSE)
}))
ml$Metric <- factor(ml$Metric, levels = c("AUC", "Precision", "Recall", "F1"))

p28 <- ggplot(ml, aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Value)),
            position = position_dodge(width = 0.75),
            vjust = -0.4, size = 2.6) +
  scale_fill_manual(values = c(
    "F1"        = "#2a9d8f",
    "AUC"       = "#e9c46a",
    "Precision" = "#e76f51",
    "Recall"    = "#264653"
  )) +
  ylim(0, 1.08) +
  labs(
    title    = "Model Comparison — All Metrics at Optimal Threshold",
    subtitle = "Validation set | Lower threshold = higher recall",
    x = NULL, y = "Score", fill = "Metric"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave("../outputs/plots/28_model_comparison_all_metrics.png",
       plot = p28, width = 11, height = 6, dpi = 150)
cat("Saved: outputs/plots/28_model_comparison_all_metrics.png\n")

cat("\n==============================================\n")
cat("Phase 5 (Model Evaluation) complete.\n")
cat("==============================================\n")
