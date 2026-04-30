# =============================================================================
# Phase 7b: Combined Bias-Variance Chart — All 6 Models
# Project: Predicting Employee Attrition Using AI
# Style:   Same as 04d_bv_actual_models.R
# Inputs:  model_rf.rds, model_xgb.rds, model_svm.rds (dynamic metrics)
#          Phase-4 model positions are hardcoded from 04b results
# Outputs: outputs/plots/26_all_models_bias_variance.png
# =============================================================================

library(ggplot2)
library(dplyr)

# =============================================================================
# Helper
# =============================================================================

diagnose_simple <- function(train_f1, val_f1) {
  gap <- train_f1 - val_f1
  if (gap > 0.10 && train_f1 >= 0.65) "HIGH VARIANCE"
  else if (train_f1 < 0.65 && gap < 0.05) "HIGH BIAS"
  else if (gap < 0.05 && train_f1 >= 0.65) "GOOD FIT"
  else "MODERATE VARIANCE"
}

# =============================================================================
# STEP 1: Load metrics from saved models
# =============================================================================

rf_rds  <- readRDS("model_rf.rds")
xgb_rds <- readRDS("model_xgb.rds")
svm_rds <- readRDS("model_svm.rds")

# Compute errors (1 - F1 at t=0.50 for consistency)
rf_train_err  <- 1 - rf_rds$train_f1     # = 0.000
rf_val_err    <- 1 - rf_rds$val_f1       # = 0.833 (class-weight + high variance)
xgb_train_err <- 1 - xgb_rds$train_f1   # = 0.000
xgb_val_err   <- 1 - xgb_rds$val_f1     # = 0.440
svm_train_err <- 1 - svm_rds$train_f1   # ≈ 0.183
svm_val_err   <- 1 - svm_rds$val_f1     # ≈ 0.471

cat("=== Model Errors (1 - F1 at t=0.50) ===\n")
cat(sprintf("  glmnet Ridge      : Train=0.459  Val=0.362  (hardcoded)\n"))
cat(sprintf("  Logistic Reg.     : Train=0.412  Val=0.389  (hardcoded)\n"))
cat(sprintf("  Decision Tree     : Train=0.248  Val=0.575  (hardcoded)\n"))
cat(sprintf("  Random Forest     : Train=%.3f  Val=%.3f  (from rds)\n",   rf_train_err,  rf_val_err))
cat(sprintf("  XGBoost           : Train=%.3f  Val=%.3f  (from rds)\n",  xgb_train_err, xgb_val_err))
cat(sprintf("  SVM               : Train=%.3f  Val=%.3f  (from rds)\n",  svm_train_err, svm_val_err))

# =============================================================================
# STEP 2: Build model table
#   x_pos = conceptual flexibility position on the horizontal axis
#   Lower = simpler/more regularised; Higher = more complex/overfit-prone
# =============================================================================

bv_models <- data.frame(
  name      = c("glmnet Ridge", "Logistic\nRegression", "SVM",
                "XGBoost", "Decision\nTree", "Random\nForest"),
  x_pos     = c(2.0,            3.0,                    5.5,
                6.5,            8.0,                    9.2),
  train_err = c(0.459,          0.412,                  svm_train_err,
                xgb_train_err,  0.248,                  rf_train_err),
  val_err   = c(0.362,          0.389,                  svm_val_err,
                xgb_val_err,    0.575,                  rf_val_err),
  col       = c("#00897B", "#AD1457", "#E65100",
                "#0D47A1", "#4527A0", "#1B5E20"),
  stringsAsFactors = FALSE
)

# =============================================================================
# STEP 3: Conceptual Bias-Variance curves
#   Same parameterisation as 04d — curves are illustrative, not derived from
#   true decomposition.  They are scaled so the total-error minimum sits in
#   the "moderate flexibility" region.
# =============================================================================

x_seq      <- seq(0.3, 10.0, length.out = 600)
irr        <- 0.12
bias2_y    <- 0.55 * exp(-0.38 * x_seq) + 0.025
variance_y <- 0.009 * exp(0.45 * x_seq)
total_y    <- bias2_y + variance_y + irr
opt_idx    <- which.min(total_y)
opt_x      <- x_seq[opt_idx]
opt_y      <- total_y[opt_idx]

curves <- rbind(
  data.frame(x = x_seq, y = bias2_y,    curve = "Bias\u00B2"),
  data.frame(x = x_seq, y = variance_y, curve = "Variance"),
  data.frame(x = x_seq, y = total_y,    curve = "Total Error")
)
curves$curve <- factor(curves$curve,
                        levels = c("Bias\u00B2", "Variance", "Total Error"))

y_max <- 1.00

# Label offsets — nudge each label so they don't overlap
lbl_offsets <- data.frame(
  x = c(1.40, 3.40, 4.80, 6.40, 7.20, 8.50),
  y = c(0.56, 0.52, 0.62, 0.20, 0.73, 0.15)
)

# =============================================================================
# STEP 4: Build plot
# =============================================================================

p <- ggplot() +
  # Zone shading
  annotate("rect", xmin = 0.3, xmax = 4.5, ymin = 0, ymax = y_max,
           fill = "#FFF3E0", alpha = 0.40) +
  annotate("rect", xmin = 7.0, xmax = 10.0, ymin = 0, ymax = y_max,
           fill = "#E3F2FD", alpha = 0.40) +
  annotate("text", x = 2.0,  y = y_max - 0.03,
           label = "HIGH BIAS\nZONE", color = "#BF360C",
           size = 3.2, fontface = "italic", alpha = 0.75, vjust = 1) +
  annotate("text", x = 8.7,  y = y_max - 0.03,
           label = "HIGH VARIANCE\nZONE", color = "#0D47A1",
           size = 3.2, fontface = "italic", alpha = 0.75, vjust = 1) +
  # Irreducible error baseline
  geom_hline(yintercept = irr, linetype = "dashed",
             color = "grey45", linewidth = 0.75) +
  annotate("text", x = 9.8, y = irr + 0.030,
           label = "Irreducible Error", color = "grey45",
           size = 3.0, fontface = "italic", hjust = 1) +
  # Bias²/Variance/Total Error curves
  geom_line(data = curves, aes(x = x, y = y, color = curve), linewidth = 1.1) +
  # Optimal point marker
  geom_vline(xintercept = opt_x, linetype = "dotted",
             color = "#388E3C", linewidth = 0.7) +
  annotate("point", x = opt_x, y = opt_y,
           shape = 23, size = 4, color = "#388E3C", fill = "#C8E6C9") +
  {
    # Per-model: vertical gap bar + train/val dots + value labels + name label
    layers <- list()
    for (i in seq_len(nrow(bv_models))) {
      lo <- min(bv_models$train_err[i], bv_models$val_err[i])
      hi <- max(bv_models$train_err[i], bv_models$val_err[i])
      layers <- c(layers, list(
        # Gap bar
        annotate("segment",
                 x = bv_models$x_pos[i], xend = bv_models$x_pos[i],
                 y = lo, yend = hi,
                 color = bv_models$col[i], linewidth = 2.2, alpha = 0.60),
        # Train dot (triangle up)
        annotate("point",
                 x = bv_models$x_pos[i], y = bv_models$train_err[i],
                 shape = 17, size = 4.5, color = bv_models$col[i]),
        # Val dot (circle)
        annotate("point",
                 x = bv_models$x_pos[i], y = bv_models$val_err[i],
                 shape = 16, size = 4.5, color = bv_models$col[i]),
        # Train error text
        annotate("text",
                 x = bv_models$x_pos[i] + 0.12,
                 y = bv_models$train_err[i],
                 label = sprintf("%.3f (Train)", bv_models$train_err[i]),
                 color = bv_models$col[i], size = 2.4, hjust = 0, vjust = -0.4),
        # Val error text
        annotate("text",
                 x = bv_models$x_pos[i] + 0.12,
                 y = bv_models$val_err[i],
                 label = sprintf("%.3f (Val)", bv_models$val_err[i]),
                 color = bv_models$col[i], size = 2.4, hjust = 0, vjust = 1.4),
        # Model name label
        annotate("label",
                 x = lbl_offsets$x[i], y = lbl_offsets$y[i],
                 label = bv_models$name[i], color = bv_models$col[i],
                 fill = "white", size = 2.8, fontface = "bold")
      ))
    }
    layers
  } +
  scale_color_manual(
    values = c("Bias\u00B2"   = "#E65100",
               "Variance"     = "#1565C0",
               "Total Error"  = "#6A1B9A"),
    name = NULL
  ) +
  scale_x_continuous(
    name   = "Model Flexibility  \u2192",
    limits = c(0.3, 10.0),
    breaks = bv_models$x_pos,
    labels = gsub("\\n", " ", bv_models$name),
    expand = expansion(0)
  ) +
  scale_y_continuous(
    name   = "Error  (1 \u2212 F1)  \u2192",
    limits = c(0, y_max),
    breaks = seq(0, 1.0, 0.1),
    labels = sprintf("%.1f", seq(0, 1.0, 0.1)),
    expand = expansion(0)
  ) +
  labs(
    title    = "Bias\u2013Variance Tradeoff: All 6 Models (Phase 4\u20137)",
    subtitle = paste0("\u25B2 = Train Error (1\u2212F1 at t=0.50)  |  ",
                      "\u25CF = Val Error  |  ",
                      "Vertical bar = Train\u2013Val gap (proxy for variance)\n",
                      "Note: RF and XGBoost use class weights / scale_pos_weight; ",
                      "errors at t=0.50 may overstate variance.")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey40", size = 8.5,
                                      margin = margin(b = 12)),
    legend.position    = c(0.50, 0.90),
    legend.direction   = "horizontal",
    legend.background  = element_rect(fill = "white", color = "grey80",
                                      linewidth = 0.3),
    legend.text        = element_text(size = 9),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.4),
    axis.text.x        = element_text(size = 7.5, color = "grey30"),
    axis.line          = element_line(color = "grey55", linewidth = 0.5),
    plot.margin        = margin(15, 30, 12, 15)
  )

dir.create("../outputs/plots", recursive = TRUE, showWarnings = FALSE)
ggsave("../outputs/plots/26_all_models_bias_variance.png", p,
       width = 13, height = 7.5, dpi = 150)
cat("Saved: outputs/plots/26_all_models_bias_variance.png\n")

# =============================================================================
# STEP 5: Key Insights
# =============================================================================

cat("\n==============================================\n")
cat("KEY INSIGHTS — ALL 6 MODELS (threshold = 0.50)\n")
cat("==============================================\n")
cat(sprintf("  %-20s  Train F1  Val F1   Gap     Prec   Recall  AUC(val)  Diagnosis\n", "Model"))
cat(rep("-", 100), "\n", sep = "")

rows <- data.frame(
  model     = c("glmnet Ridge", "Logistic Reg.", "SVM",
                "XGBoost", "Decision Tree", "Random Forest"),
  train_f1  = c(1 - 0.459, 1 - 0.412, svm_rds$train_f1,
                xgb_rds$train_f1, 1 - 0.248, rf_rds$train_f1),
  val_f1    = c(1 - 0.362, 1 - 0.389, svm_rds$val_f1,
                xgb_rds$val_f1, 1 - 0.575, rf_rds$val_f1),
  # Precision @ t=0.50: Phase-4 hardcoded from 04b/04c output; new models from rds
  val_prec  = c(0.880, 0.786, svm_rds$val_precision,
                xgb_rds$val_precision, 0.472, rf_rds$val_precision),
  val_rec   = c(0.500, 0.500, svm_rds$val_recall,
                xgb_rds$val_recall, 0.386, rf_rds$val_recall),
  auc_val   = c(0.855, 0.859, svm_rds$val_auc,
                xgb_rds$val_auc, 0.705, rf_rds$val_auc),
  stringsAsFactors = FALSE
)
rows$gap   <- rows$train_f1 - rows$val_f1
rows$diag  <- mapply(diagnose_simple, rows$train_f1, rows$val_f1)

for (i in seq_len(nrow(rows))) {
  cat(sprintf("  %-20s  %5.3f     %5.3f    %+6.3f  %5.3f  %5.3f   %5.3f     %s\n",
              rows$model[i], rows$train_f1[i], rows$val_f1[i],
              rows$gap[i], rows$val_prec[i], rows$val_rec[i],
              rows$auc_val[i], rows$diag[i]))
}

cat("\nSummary:\n")
best_val <- rows[which.max(rows$val_f1), ]
best_auc <- rows[which.max(rows$auc_val), ]
cat(sprintf("  Best Val F1 at t=0.50  : %-20s (F1 = %.3f)\n",
            best_val$model, best_val$val_f1))
cat(sprintf("  Best Val AUC           : %-20s (AUC = %.4f)\n",
            best_auc$model, best_auc$auc_val))
cat(sprintf("  Fewest parameters, good AUC : glmnet Ridge (AUC=0.855, HIGH BIAS but low gap)\n"))
cat(sprintf("  Most overfitting         : RF / XGBoost (Train F1=1.0, gap >> 0.40)\n"))
cat(sprintf("  Best overall candidate   : SVM or glmnet for production (lower variance)\n"))

cat("\n==============================================\n")
cat("Phase 7b (Combined BV Chart) complete.\n")
cat("==============================================\n")
