# =============================================================================
# Phase 7: SVM
# Project: Predicting Employee Attrition Using AI
# Notes:   Uses SCALED data (required for SVM).
#          Kernel: radial and linear.
#          Tune: kernel × cost × gamma (gamma only for radial).
# Inputs:  train_data.rds, val_data.rds
# Outputs: model_svm.rds
#          outputs/plots/24_svm_threshold.png
#          outputs/plots/25_svm_bias_variance.png
# =============================================================================

library(dplyr)
library(e1071)
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
# STEP 1: Load SCALED data
# =============================================================================

cat("==============================================\n")
cat("PHASE 7: SUPPORT VECTOR MACHINE (SVM)\n")
cat("==============================================\n\n")

train <- readRDS("train_data.rds")
val   <- readRDS("val_data.rds")

# Clean column names
names(train) <- make.names(names(train), unique = TRUE)
names(val)   <- make.names(names(val),   unique = TRUE)

y_train <- train$Attrition
y_val   <- val$Attrition

# Convert labels to factor for e1071 svm
y_train_f <- as.factor(y_train)
y_val_f   <- as.factor(y_val)

X_train <- train %>% select(-Attrition)
X_val   <- val   %>% select(-Attrition)

cat(sprintf("Train: %d rows x %d features (scaled)\n", nrow(X_train), ncol(X_train)))
cat(sprintf("Val:   %d rows x %d features (scaled)\n\n", nrow(X_val), ncol(X_val)))

# =============================================================================
# STEP 2: Grid search — tune kernel × cost × gamma on val set F1
# =============================================================================

cat("--- Tuning kernel x cost (x gamma for radial) on val set F1 ---\n")
set.seed(42)

kernels <- c("radial", "linear")
costs   <- c(1, 5, 10, 20, 50)
gammas  <- c(0.001, 0.005, 0.01, 0.05)

best_svm    <- NULL
best_f1     <- -Inf
best_kernel <- NA
best_cost   <- NA
best_gamma  <- NA
class_wts   <- c("0" = 1, "1" = 3)

for (kern in kernels) {
  gamma_vals <- if (kern == "radial") gammas else NA_real_

  for (cost_val in costs) {
    for (gamma_val in gamma_vals) {

      tryCatch({
        if (kern == "radial") {
          fit <- svm(
            x             = X_train,
            y             = y_train_f,
            kernel        = "radial",
            cost          = cost_val,
            gamma         = gamma_val,
            class.weights = class_wts,
            probability   = TRUE
          )
          gamma_label <- gamma_val
        } else {
          fit <- svm(
            x             = X_train,
            y             = y_train_f,
            kernel        = "linear",
            cost          = cost_val,
            class.weights = class_wts,
            probability   = TRUE
          )
          gamma_label <- NA
        }

        pred_val <- predict(fit, X_val, probability = TRUE)
        probs_val <- attr(pred_val, "probabilities")[, "1"]
        met       <- evaluate(probs_val, y_val)
        auc_val   <- roc_auc(probs_val, y_val)

        g_str <- if (!is.na(gamma_label)) sprintf("g=%.3f", gamma_label) else "g=N/A  "
        cat(sprintf("  kernel=%-6s cost=%5.1f %s | Val F1: %.4f | Val AUC: %.4f\n",
                    kern, cost_val, g_str, met$f1, auc_val))

        if (met$f1 > best_f1) {
          best_f1     <- met$f1
          best_svm    <- fit
          best_kernel <- kern
          best_cost   <- cost_val
          best_gamma  <- gamma_label
        }
      }, error = function(e) {
        cat(sprintf("  kernel=%-6s cost=%5.1f -- FAILED: %s\n", kern, cost_val, e$message))
      })
    }
  }
}

cat(sprintf("\nBest config: kernel=%s, cost=%.1f%s  (Val F1: %.4f)\n\n",
            best_kernel, best_cost,
            if (!is.na(best_gamma)) sprintf(", gamma=%.3f", best_gamma) else "",
            best_f1))

# =============================================================================
# STEP 3: Evaluate on BOTH train and val at threshold = 0.50
# =============================================================================

pred_train <- predict(best_svm, X_train, probability = TRUE)
pred_val   <- predict(best_svm, X_val,   probability = TRUE)

prob_svm_train <- attr(pred_train, "probabilities")[, "1"]
prob_svm_val   <- attr(pred_val,   "probabilities")[, "1"]

m_train <- evaluate(prob_svm_train, y_train)
m_val   <- evaluate(prob_svm_val,   y_val)
auc_tr  <- roc_auc(prob_svm_train, y_train)
auc_vl  <- roc_auc(prob_svm_val,   y_val)

gap_f1  <- m_train$f1 - m_val$f1
diag    <- diagnose(m_train$f1, m_val$f1)

cat("==============================================\n")
cat("BIAS-VARIANCE DIAGNOSIS (threshold = 0.50)\n")
cat("==============================================\n")
cat(sprintf("  Best kernel     : %s\n", best_kernel))
cat(sprintf("  Best cost       : %.1f\n", best_cost))
if (!is.na(best_gamma))
  cat(sprintf("  Best gamma      : %.3f\n", best_gamma))
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
  m <- evaluate(prob_svm_val, y_val, threshold = t)
  data.frame(threshold = t, precision = m$precision,
             recall = m$recall, f1 = m$f1)
}))

opt     <- sweep_df[which.max(sweep_df$f1), ]
m_val50 <- evaluate(prob_svm_val, y_val, threshold = 0.50)

cat("--- Val Metrics at threshold = 0.50 ---\n")
cat(sprintf("  Precision: %.3f | Recall: %.3f | F1: %.3f\n\n",
            m_val50$precision, m_val50$recall, m_val50$f1))

cat("--- Optimal Threshold (max F1) ---\n")
cat(sprintf("  Threshold : %.2f\n", opt$threshold))
cat(sprintf("  Precision : %.3f\n", opt$precision))
cat(sprintf("  Recall    : %.3f\n", opt$recall))
cat(sprintf("  F1        : %.3f\n\n", opt$f1))

# =============================================================================
# STEP 5: Save model
# =============================================================================

saveRDS(list(
  model         = best_svm,
  best_kernel   = best_kernel,
  best_cost     = best_cost,
  best_gamma    = best_gamma,
  train_f1      = m_train$f1,
  val_f1        = m_val$f1,
  val_precision = m_val$precision,
  val_recall    = m_val$recall,
  train_auc     = auc_tr,
  val_auc       = auc_vl,
  train_err     = 1 - m_train$accuracy,
  val_err       = 1 - m_val$accuracy
), "model_svm.rds")
cat("Saved: model_svm.rds\n\n")

# =============================================================================
# STEP 6: Plots
# =============================================================================

dir.create("../outputs/plots", recursive = TRUE, showWarnings = FALSE)

# ── Plot 24: Precision / Recall / F1 vs Threshold ───────────────────────────
thr_long <- rbind(
  data.frame(threshold = sweep_df$threshold, value = sweep_df$precision, metric = "Precision"),
  data.frame(threshold = sweep_df$threshold, value = sweep_df$recall,    metric = "Recall"),
  data.frame(threshold = sweep_df$threshold, value = sweep_df$f1,        metric = "F1")
)
thr_long$metric <- factor(thr_long$metric, levels = c("Precision", "Recall", "F1"))

p24 <- ggplot(thr_long, aes(x = threshold, y = value, color = metric)) +
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
    title    = "SVM — Precision, Recall and F1 vs Threshold",
    subtitle = sprintf("kernel=%s, cost=%.1f%s  |  Optimal F1 threshold: %.2f  |  Val F1: %.3f",
                       best_kernel, best_cost,
                       if (!is.na(best_gamma)) sprintf(", gamma=%.3f", best_gamma) else "",
                       opt$threshold, opt$f1),
    x = "Threshold", y = "Score", color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top",
        panel.grid.minor = element_blank())

ggsave("../outputs/plots/24_svm_threshold.png", p24,
       width = 8, height = 5, dpi = 150)
cat("Saved: outputs/plots/24_svm_threshold.png\n")

# ── Plot 25: Bias-Variance (same style as 04d) ───────────────────────────────
svm_train_err <- 1 - m_train$f1
svm_val_err   <- 1 - m_val$f1

bv_models <- data.frame(
  name      = c("glmnet Ridge", "Logistic\nRegression", "Decision Tree",
                "Random Forest", "XGBoost", "SVM"),
  x_pos     = c(2.0,            3.0,                    8.5,   5.5,  4.5,  6.0),
  train_err = c(0.459,          0.412,                  0.248, 0.000, 0.000, svm_train_err),
  val_err   = c(0.362,          0.389,                  0.575, 1 - 0.1667, 1 - 0.560, svm_val_err),
  col       = c("#00897B", "#AD1457", "#4527A0", "#1B5E20", "#0D47A1", "#E65100"),
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
y_max2 <- 0.90

lbl_offsets <- data.frame(
  x = c(1.20, 3.60, 7.90, 5.20, 3.80, 6.40),
  y = c(0.56, 0.52, 0.67, 0.15, 0.72, 0.60)
)

p25 <- ggplot() +
  annotate("rect", xmin = 0.3, xmax = 4.3, ymin = 0, ymax = y_max2,
           fill = "#FFF3E0", alpha = 0.40) +
  annotate("rect", xmin = 5.8, xmax = 9.5, ymin = 0, ymax = y_max2,
           fill = "#E3F2FD", alpha = 0.40) +
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
                 color = bv_models$col[i], size = 2.3, hjust = 0, vjust = -0.4),
        annotate("text",
                 x = bv_models$x_pos[i] + 0.15,
                 y = bv_models$val_err[i],
                 label = sprintf("%.3f (Val)", bv_models$val_err[i]),
                 color = bv_models$col[i], size = 2.3, hjust = 0, vjust = 1.4),
        annotate("label",
                 x = lbl_offsets$x[i], y = lbl_offsets$y[i],
                 label = bv_models$name[i], color = bv_models$col[i],
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
    breaks = seq(0, 0.9, 0.1),
    labels = sprintf("%.1f", seq(0, 0.9, 0.1)),
    expand = expansion(0)
  ) +
  labs(
    title    = "Bias\u2013Variance Tradeoff: All 6 Models",
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

ggsave("../outputs/plots/25_svm_bias_variance.png", p25,
       width = 12, height = 7, dpi = 150)
cat("Saved: outputs/plots/25_svm_bias_variance.png\n")

cat("\n==============================================\n")
cat("Phase 7 (SVM) complete.\n")
cat("==============================================\n")
