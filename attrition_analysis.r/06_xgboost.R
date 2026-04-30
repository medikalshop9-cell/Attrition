# =============================================================================
# Phase 6: XGBoost
# Project: Predicting Employee Attrition Using AI
# Inputs:  train_data_unscaled.rds, val_data_unscaled.rds
# Outputs: model_xgb.rds
#          outputs/plots/21_xgb_feature_importance.png
#          outputs/plots/22_xgb_threshold.png
#          outputs/plots/23_xgb_bias_variance.png
# =============================================================================

library(dplyr)
library(xgboost)
library(ggplot2)

# =============================================================================
# Helpers
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
  list(accuracy = accuracy, precision = precision, recall = recall,
       f1 = f1, tp = tp, fp = fp, tn = tn, fn = fn)
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

diagnose <- function(train_f1, val_f1) {
  gap <- train_f1 - val_f1
  if (gap > 0.10 && train_f1 >= 0.65) "HIGH VARIANCE -- overfitting"
  else if (train_f1 < 0.65 && gap < 0.05) "HIGH BIAS -- underfitting"
  else if (gap < 0.05 && train_f1 >= 0.65) "GOOD FIT"
  else "MODERATE VARIANCE"
}

# =============================================================================
# STEP 1: Load data
# =============================================================================

cat("==============================================\n")
cat("PHASE 6: XGBOOST\n")
cat("==============================================\n\n")

train_u <- readRDS("train_data_unscaled.rds")
val_u   <- readRDS("val_data_unscaled.rds")

# Clean column names — remove chars that may break matrix conversion
names(train_u) <- make.names(names(train_u), unique = TRUE)
names(val_u)   <- make.names(names(val_u),   unique = TRUE)

y_train <- train_u$Attrition
y_val   <- val_u$Attrition

# Build feature matrices
X_train <- as.matrix(train_u %>% select(-Attrition))
X_val   <- as.matrix(val_u   %>% select(-Attrition))

# DMatrix format for xgboost
# scale_pos_weight = majority / minority = 1233 / 237 ≈ 5.2
spw <- 1233 / 237

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dval   <- xgb.DMatrix(data = X_val,   label = y_val)

cat(sprintf("Train: %d rows x %d features\n", nrow(X_train), ncol(X_train)))
cat(sprintf("Val:   %d rows x %d features\n", nrow(X_val),   ncol(X_val)))
cat(sprintf("scale_pos_weight = %.2f\n\n", spw))

# =============================================================================
# STEP 2: Grid search — tune max_depth and eta on val set F1
# =============================================================================

cat("--- Tuning max_depth x eta on val set F1 ---\n")
set.seed(42)

max_depths <- c(3, 4, 5)
etas       <- c(0.01, 0.03, 0.05)

best_xgb    <- NULL
best_f1     <- -Inf
best_depth  <- NA
best_eta    <- NA
best_nrounds <- NA

for (depth in max_depths) {
  for (eta_val in etas) {

    params <- list(
      objective         = "binary:logistic",
      max_depth         = depth,
      eta               = eta_val,
      scale_pos_weight  = spw,
      eval_metric       = "logloss",
      nthread           = 1,
      seed              = 42
    )

    # Train with early stopping watching val logloss
    fit <- xgb.train(
      params  = params,
      data    = dtrain,
      nrounds = 500,
      watchlist = list(val = dval),
      early_stopping_rounds = 30,
      verbose = 0
    )

    probs_val <- predict(fit, dval)
    met       <- evaluate(probs_val, y_val)
    auc_val   <- roc_auc(probs_val, y_val)

    cat(sprintf("  depth=%d eta=%.2f | nrounds=%3d | Val F1: %.4f | Val AUC: %.4f\n",
                depth, eta_val, fit$best_iteration, met$f1, auc_val))

    if (met$f1 > best_f1) {
      best_f1      <- met$f1
      best_xgb     <- fit
      best_depth   <- depth
      best_eta     <- eta_val
      best_nrounds <- fit$best_iteration
    }
  }
}

cat(sprintf("\nBest config: max_depth=%d, eta=%.2f, nrounds=%d  (Val F1: %.4f)\n\n",
            best_depth, best_eta, best_nrounds, best_f1))

# =============================================================================
# STEP 3: Evaluate on BOTH train and val at threshold = 0.50
# =============================================================================

prob_xgb_train <- predict(best_xgb, dtrain)
prob_xgb_val   <- predict(best_xgb, dval)

m_train <- evaluate(prob_xgb_train, y_train)
m_val   <- evaluate(prob_xgb_val,   y_val)
auc_tr  <- roc_auc(prob_xgb_train, y_train)
auc_vl  <- roc_auc(prob_xgb_val,   y_val)

gap_f1  <- m_train$f1 - m_val$f1
diag    <- diagnose(m_train$f1, m_val$f1)

cat("==============================================\n")
cat("BIAS-VARIANCE DIAGNOSIS (threshold = 0.50)\n")
cat("==============================================\n")
cat(sprintf("  Best max_depth  : %d\n", best_depth))
cat(sprintf("  Best eta        : %.2f\n", best_eta))
cat(sprintf("  Best nrounds    : %d\n", best_nrounds))
cat(sprintf("  Train F1        : %.4f\n", m_train$f1))
cat(sprintf("  Val F1          : %.4f\n", m_val$f1))
cat(sprintf("  Gap (Variance)  : %.4f\n", gap_f1))
cat(sprintf("  Train AUC       : %.4f\n", auc_tr))
cat(sprintf("  Val AUC         : %.4f\n", auc_vl))
cat(sprintf("  Diagnosis       : %s\n", diag))
cat(sprintf("  Training Error  : %.4f  (Train Acc: %.4f)\n\n",
            1 - m_train$accuracy, m_train$accuracy))

# =============================================================================
# STEP 4: Threshold sweep 0.01 → 0.99
# =============================================================================

thresholds <- seq(0.01, 0.99, by = 0.01)
sweep_df <- do.call(rbind, lapply(thresholds, function(t) {
  m <- evaluate(prob_xgb_val, y_val, threshold = t)
  data.frame(threshold = t, precision = m$precision,
             recall = m$recall, f1 = m$f1)
}))

opt     <- sweep_df[which.max(sweep_df$f1), ]
m_val50 <- evaluate(prob_xgb_val, y_val, threshold = 0.50)

cat("--- Val Metrics at threshold = 0.50 ---\n")
cat(sprintf("  Precision: %.3f | Recall: %.3f | F1: %.3f\n\n",
            m_val50$precision, m_val50$recall, m_val50$f1))

cat("--- Optimal Threshold (max F1) ---\n")
cat(sprintf("  Threshold : %.2f\n", opt$threshold))
cat(sprintf("  Precision : %.3f\n", opt$precision))
cat(sprintf("  Recall    : %.3f\n", opt$recall))
cat(sprintf("  F1        : %.3f\n\n", opt$f1))

# =============================================================================
# STEP 5: Feature importance top 10 by gain
# =============================================================================

imp_mat <- xgb.importance(model = best_xgb)
cat("--- Top 10 Features by Gain ---\n")
for (i in seq_len(min(10, nrow(imp_mat)))) {
  cat(sprintf("  %2d. %-35s Gain: %.4f\n", i,
              imp_mat$Feature[i], imp_mat$Gain[i]))
}
cat("\n")

# =============================================================================
# STEP 6: Save model
# =============================================================================

saveRDS(list(
  model         = best_xgb,
  best_depth    = best_depth,
  best_eta      = best_eta,
  best_nrounds  = best_nrounds,
  train_f1      = m_train$f1,
  val_f1        = m_val$f1,
  val_precision = m_val$precision,
  val_recall    = m_val$recall,
  train_auc     = auc_tr,
  val_auc       = auc_vl,
  train_err     = 1 - m_train$accuracy,
  val_err       = 1 - m_val$accuracy
), "model_xgb.rds")
cat("Saved: model_xgb.rds\n\n")

# =============================================================================
# STEP 7: Plots
# =============================================================================

dir.create("../outputs/plots", recursive = TRUE, showWarnings = FALSE)

# ── Plot 21: Feature Importance (top 20 by gain) ────────────────────────────
top20_imp <- head(imp_mat, 20)
top20_imp$Feature <- factor(top20_imp$Feature, levels = rev(top20_imp$Feature))

p21 <- ggplot(top20_imp, aes(x = Feature, y = Gain)) +
  geom_col(fill = "#0D47A1", alpha = 0.8) +
  geom_text(aes(label = round(Gain, 4)), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "XGBoost — Top 20 Features by Gain",
    subtitle = sprintf("max_depth=%d, eta=%.2f, nrounds=%d",
                       best_depth, best_eta, best_nrounds),
    x        = NULL,
    y        = "Gain"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

ggsave("../outputs/plots/21_xgb_feature_importance.png", p21,
       width = 9, height = 7, dpi = 150)
cat("Saved: outputs/plots/21_xgb_feature_importance.png\n")

# ── Plot 22: Precision / Recall / F1 vs Threshold ───────────────────────────
thr_long <- rbind(
  data.frame(threshold = sweep_df$threshold, value = sweep_df$precision, metric = "Precision"),
  data.frame(threshold = sweep_df$threshold, value = sweep_df$recall,    metric = "Recall"),
  data.frame(threshold = sweep_df$threshold, value = sweep_df$f1,        metric = "F1")
)
thr_long$metric <- factor(thr_long$metric, levels = c("Precision", "Recall", "F1"))

p22 <- ggplot(thr_long, aes(x = threshold, y = value, color = metric)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 0.5, linetype = "dashed",
             color = "grey40", linewidth = 0.7) +
  annotate("text", x = 0.5, y = 0.02, label = "0.5 (default)",
           color = "grey40", angle = 90, vjust = -0.4, hjust = 0, size = 3) +
  geom_vline(xintercept = opt$threshold, linetype = "dotdash",
             color = "#2E7D32", linewidth = 0.8) +
  annotate("text", x = opt$threshold, y = 0.96,
           label = sprintf("Opt F1\n%.2f", opt$threshold),
           color = "#2E7D32", size = 2.8, hjust = -0.1) +
  scale_color_manual(values = c("Precision" = "#1565C0",
                                "Recall"    = "#C62828",
                                "F1"        = "#2E7D32")) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05)) +
  labs(
    title    = "XGBoost — Precision, Recall and F1 vs Threshold",
    subtitle = sprintf("Optimal F1 threshold: %.2f  |  Val F1: %.3f",
                       opt$threshold, opt$f1),
    x = "Threshold", y = "Score", color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top",
        panel.grid.minor = element_blank())

ggsave("../outputs/plots/22_xgb_threshold.png", p22,
       width = 8, height = 5, dpi = 150)
cat("Saved: outputs/plots/22_xgb_threshold.png\n")

# ── Plot 23: Bias-Variance (same style as 04d) ───────────────────────────────
xgb_train_err <- 1 - m_train$f1
xgb_val_err   <- 1 - m_val$f1

bv_models <- data.frame(
  name      = c("glmnet Ridge", "Logistic\nRegression", "Decision Tree",
                "Random Forest", "XGBoost"),
  x_pos     = c(2.0,            3.0,                    8.5,   5.5,  4.5),
  train_err = c(0.459,          0.412,                  0.248, 0.000, xgb_train_err),
  val_err   = c(0.362,          0.389,                  0.575, 1 - 0.1667, xgb_val_err),
  col       = c("#00897B", "#AD1457", "#4527A0", "#1B5E20", "#0D47A1"),
  stringsAsFactors = FALSE
)

x_seq2      <- seq(0.3, 9.5, length.out = 500)
irr2        <- 0.12
bias2_y2    <- 0.55 * exp(-0.38 * x_seq2) + 0.025
variance_y2 <- 0.009 * exp(0.45 * x_seq2)
total_y2    <- bias2_y2 + variance_y2 + irr2
opt_idx2    <- which.min(total_y2)
opt_x2      <- x_seq2[opt_idx2]
opt_y2      <- total_y2[opt_idx2]

curves2 <- rbind(
  data.frame(x = x_seq2, y = bias2_y2,    curve = "Bias\u00B2"),
  data.frame(x = x_seq2, y = variance_y2, curve = "Variance"),
  data.frame(x = x_seq2, y = total_y2,    curve = "Total Error")
)
curves2$curve <- factor(curves2$curve,
                        levels = c("Bias\u00B2", "Variance", "Total Error"))
y_max2 <- 0.73

lbl2 <- data.frame(
  x     = c(1.20, 3.80, 7.40, 5.50, 4.50),
  y     = c(0.56, 0.52, 0.66, 0.35, 0.60),
  label = bv_models$name,
  col   = bv_models$col
)
tip_y2 <- pmax(bv_models$train_err, bv_models$val_err) + 0.025

p23 <- ggplot() +
  annotate("rect", xmin = 0.3, xmax = 4.3, ymin = 0, ymax = y_max2,
           fill = "#FFF3E0", alpha = 0.45) +
  annotate("rect", xmin = 5.8, xmax = 9.5, ymin = 0, ymax = y_max2,
           fill = "#E3F2FD", alpha = 0.45) +
  annotate("text", x = 2.0, y = y_max2 - 0.025,
           label = "HIGH BIAS\nZONE", color = "#BF360C",
           size = 3.2, fontface = "italic", alpha = 0.75, vjust = 1) +
  annotate("text", x = 7.8, y = y_max2 - 0.025,
           label = "HIGH VARIANCE\nZONE", color = "#0D47A1",
           size = 3.2, fontface = "italic", alpha = 0.75, vjust = 1) +
  geom_hline(yintercept = irr2, linetype = "dashed",
             color = "grey45", linewidth = 0.75) +
  annotate("text", x = 9.3, y = irr2 + 0.027,
           label = "Irreducible Error", color = "grey45",
           size = 3.0, fontface = "italic", hjust = 1) +
  geom_line(data = curves2, aes(x = x, y = y, color = curve), linewidth = 1.1) +
  geom_vline(xintercept = opt_x2, linetype = "dotted",
             color = "#388E3C", linewidth = 0.7) +
  annotate("point", x = opt_x2, y = opt_y2,
           shape = 23, size = 4, color = "#388E3C", fill = "#C8E6C9") +
  {
    layers <- list()
    for (i in seq_len(nrow(bv_models))) {
      lo <- min(bv_models$train_err[i], bv_models$val_err[i])
      hi <- max(bv_models$train_err[i], bv_models$val_err[i])
      layers <- c(layers, list(
        annotate("segment",
                 x = bv_models$x_pos[i], xend = bv_models$x_pos[i],
                 y = lo, yend = hi,
                 color = bv_models$col[i], linewidth = 2.0, alpha = 0.65),
        annotate("point",
                 x = bv_models$x_pos[i], y = bv_models$train_err[i],
                 shape = 17, size = 4, color = bv_models$col[i]),
        annotate("point",
                 x = bv_models$x_pos[i], y = bv_models$val_err[i],
                 shape = 16, size = 4, color = bv_models$col[i]),
        annotate("text",
                 x = bv_models$x_pos[i] + 0.15,
                 y = bv_models$train_err[i],
                 label = sprintf("%.3f (Train)", bv_models$train_err[i]),
                 color = bv_models$col[i], size = 2.5, hjust = 0, vjust = -0.4),
        annotate("text",
                 x = bv_models$x_pos[i] + 0.15,
                 y = bv_models$val_err[i],
                 label = sprintf("%.3f (Val)", bv_models$val_err[i]),
                 color = bv_models$col[i], size = 2.5, hjust = 0, vjust = 1.4),
        annotate("label",
                 x = lbl2$x[i], y = lbl2$y[i],
                 label = lbl2$label[i], color = lbl2$col[i],
                 fill = "white", size = 2.8, fontface = "bold")
      ))
    }
    layers
  } +
  scale_color_manual(
    values = c("Bias\u00B2" = "#E65100",
               "Variance"    = "#1565C0",
               "Total Error" = "#6A1B9A"),
    name = NULL
  ) +
  scale_x_continuous(
    name   = "Model Flexibility  \u2192",
    limits = c(0.3, 9.5),
    breaks = bv_models$x_pos,
    labels = gsub("\\n", " ", bv_models$name),
    expand = expansion(0)
  ) +
  scale_y_continuous(
    name   = "Error  (1 \u2212 F1)  \u2192",
    limits = c(0, y_max2),
    breaks = seq(0, 0.7, 0.1),
    labels = sprintf("%.1f", seq(0, 0.7, 0.1)),
    expand = expansion(0)
  ) +
  labs(
    title    = "Bias\u2013Variance Tradeoff: All Models including XGBoost",
    subtitle = "\u25B2 = Train Error  |  \u25CF = Val Error  |  Vertical bar = Train\u2013Val gap"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey40", size = 9.5,
                                      margin = margin(b = 10)),
    legend.position    = c(0.50, 0.90),
    legend.direction   = "horizontal",
    legend.background  = element_rect(fill = "white", color = "grey80",
                                      linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.4),
    axis.text.x        = element_text(size = 8, color = "grey30"),
    axis.line          = element_line(color = "grey55", linewidth = 0.5),
    plot.margin        = margin(15, 25, 12, 15)
  )

ggsave("../outputs/plots/23_xgb_bias_variance.png", p23,
       width = 12, height = 7, dpi = 150)
cat("Saved: outputs/plots/23_xgb_bias_variance.png\n")

cat("\n==============================================\n")
cat("Phase 6 (XGBoost) complete.\n")
cat("==============================================\n")
