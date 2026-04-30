# =============================================================================
# Phase 4d: Bias–Variance Tradeoff — Actual Model Positions
# Project: Predicting Employee Attrition Using AI
# Uses actual Train F1 / Val F1 from 04b to position each model on the curve
# Output:  outputs/plots/16_bias_variance_actual_models.png
# =============================================================================

library(ggplot2)

# ── Actual model metrics (from 04b_bias_variance.R) ─────────────────────────
# Error = 1 − F1  (lower is better)
models <- data.frame(
  name      = c("glmnet Ridge",       "Logistic Regression", "Decision Tree"),
  x_pos     = c(2.0,                   3.0,                   8.5),
  train_err = c(1 - 0.541,             1 - 0.588,             1 - 0.752),
  val_err   = c(1 - 0.638,             1 - 0.611,             1 - 0.425),
  diag      = c("HIGH BIAS",           "HIGH BIAS",           "HIGH VARIANCE"),
  col       = c("#00897B",             "#AD1457",             "#4527A0"),
  stringsAsFactors = FALSE
)
# train_err: 0.459, 0.412, 0.248
# val_err  : 0.362, 0.389, 0.575

# ── Conceptual curves (scaled to match actual error range) ───────────────────
# Parameters tuned so total-error curve passes near each model's val error:
#   glmnet x=2.0 → curve ≈ 0.42  (actual val 0.362 — slightly below; glmnet is well-regularised)
#   LR     x=3.0 → curve ≈ 0.36  (actual val 0.389 — close)
#   DT     x=8.5 → curve ≈ 0.58  (actual val 0.575 — excellent match)
x_seq      <- seq(0.3, 9.5, length.out = 500)
irr        <- 0.12                                   # irreducible error floor
bias2_y    <- 0.55 * exp(-0.38 * x_seq) + 0.025     # high left → zero right
variance_y <- 0.009 * exp(0.45 * x_seq)             # zero left → high right
total_y    <- bias2_y + variance_y + irr

# Optimal = minimum of total error
opt_idx <- which.min(total_y)
opt_x   <- x_seq[opt_idx]
opt_y   <- total_y[opt_idx]

curves_df <- rbind(
  data.frame(x = x_seq, y = bias2_y,    curve = "Bias\u00B2"),
  data.frame(x = x_seq, y = variance_y, curve = "Variance"),
  data.frame(x = x_seq, y = total_y,    curve = "Total Error")
)
curves_df$curve <- factor(curves_df$curve,
                          levels = c("Bias\u00B2", "Variance", "Total Error"))

# ── Model label anchor positions (staggered to avoid overlap) ───────────────
lbl <- data.frame(
  x     = c(1.30, 3.80, 7.40),
  y     = c(0.56, 0.52, 0.64),
  label = paste0(models$name, "\n(", models$diag, ")"),
  col   = models$col
)
# Arrow tip = just above the higher of train/val error for that model
tip_y <- pmax(models$train_err, models$val_err) + 0.025

y_max <- 0.73

# ── Build plot ───────────────────────────────────────────────────────────────
p <- ggplot() +

  # Zone background shading
  annotate("rect",
           xmin = 0.3, xmax = 4.3, ymin = 0, ymax = y_max,
           fill = "#FFF3E0", alpha = 0.50) +
  annotate("rect",
           xmin = 5.8, xmax = 9.5, ymin = 0, ymax = y_max,
           fill = "#E3F2FD", alpha = 0.50) +
  annotate("text",
           x = 2.0, y = y_max - 0.025,
           label = "HIGH BIAS\nZONE", color = "#BF360C",
           size = 3.3, fontface = "italic", alpha = 0.75, vjust = 1) +
  annotate("text",
           x = 7.8, y = y_max - 0.025,
           label = "HIGH VARIANCE\nZONE", color = "#0D47A1",
           size = 3.3, fontface = "italic", alpha = 0.75, vjust = 1) +

  # Irreducible error
  geom_hline(yintercept = irr, linetype = "dashed",
             color = "grey45", linewidth = 0.75) +
  annotate("text",
           x = 9.3, y = irr + 0.027,
           label = "Irreducible Error", color = "grey45",
           size = 3.1, fontface = "italic", hjust = 1) +

  # Conceptual curves
  geom_line(data = curves_df,
            aes(x = x, y = y, color = curve), linewidth = 1.2) +

  # Optimal vertical guideline + marker
  geom_vline(xintercept = opt_x, linetype = "dotted",
             color = "#388E3C", linewidth = 0.8) +
  annotate("point",
           x = opt_x, y = opt_y,
           shape = 23, size = 5, color = "#388E3C", fill = "#C8E6C9") +
  annotate("label",
           x = opt_x + 0.18, y = opt_y - 0.055,
           label = sprintf("Optimal\n(Total Error = %.3f)", opt_y),
           color = "#388E3C", fill = "white", size = 2.8,
           label.size = 0.3, fontface = "bold", hjust = 0) +

  # ── Per-model layers (loop to avoid color-scale conflicts) ─────────────────
  # glmnet Ridge
  annotate("segment",
           x = models$x_pos[1], xend = models$x_pos[1],
           y = pmin(models$train_err[1], models$val_err[1]),
           yend = pmax(models$train_err[1], models$val_err[1]),
           color = models$col[1], linewidth = 2.2, alpha = 0.65) +
  annotate("point",
           x = models$x_pos[1], y = models$train_err[1],
           shape = 17, size = 4.5, color = models$col[1]) +
  annotate("point",
           x = models$x_pos[1], y = models$val_err[1],
           shape = 16, size = 4.5, color = models$col[1]) +
  annotate("text",
           x = models$x_pos[1] + 0.15, y = models$train_err[1],
           label = sprintf("%.3f (Train)", models$train_err[1]),
           color = models$col[1], size = 2.8, hjust = 0, vjust = -0.4) +
  annotate("text",
           x = models$x_pos[1] + 0.15, y = models$val_err[1],
           label = sprintf("%.3f (Val)", models$val_err[1]),
           color = models$col[1], size = 2.8, hjust = 0, vjust = 1.4) +

  # Logistic Regression
  annotate("segment",
           x = models$x_pos[2], xend = models$x_pos[2],
           y = pmin(models$train_err[2], models$val_err[2]),
           yend = pmax(models$train_err[2], models$val_err[2]),
           color = models$col[2], linewidth = 2.2, alpha = 0.65) +
  annotate("point",
           x = models$x_pos[2], y = models$train_err[2],
           shape = 17, size = 4.5, color = models$col[2]) +
  annotate("point",
           x = models$x_pos[2], y = models$val_err[2],
           shape = 16, size = 4.5, color = models$col[2]) +
  annotate("text",
           x = models$x_pos[2] - 0.15, y = models$train_err[2],
           label = sprintf("(Train) %.3f", models$train_err[2]),
           color = models$col[2], size = 2.8, hjust = 1, vjust = -0.4) +
  annotate("text",
           x = models$x_pos[2] - 0.15, y = models$val_err[2],
           label = sprintf("(Val) %.3f", models$val_err[2]),
           color = models$col[2], size = 2.8, hjust = 1, vjust = 1.4) +

  # Decision Tree
  annotate("segment",
           x = models$x_pos[3], xend = models$x_pos[3],
           y = pmin(models$train_err[3], models$val_err[3]),
           yend = pmax(models$train_err[3], models$val_err[3]),
           color = models$col[3], linewidth = 2.2, alpha = 0.65) +
  annotate("point",
           x = models$x_pos[3], y = models$train_err[3],
           shape = 17, size = 4.5, color = models$col[3]) +
  annotate("point",
           x = models$x_pos[3], y = models$val_err[3],
           shape = 16, size = 4.5, color = models$col[3]) +
  annotate("text",
           x = models$x_pos[3] - 0.15, y = models$train_err[3],
           label = sprintf("(Train) %.3f", models$train_err[3]),
           color = models$col[3], size = 2.8, hjust = 1, vjust = -0.4) +
  annotate("text",
           x = models$x_pos[3] - 0.15, y = models$val_err[3],
           label = sprintf("(Val) %.3f", models$val_err[3]),
           color = models$col[3], size = 2.8, hjust = 1, vjust = 1.4) +

  # ── Model name labels with arrows ──────────────────────────────────────────
  # glmnet Ridge — label left of its position
  annotate("label",
           x = lbl$x[1], y = lbl$y[1],
           label = lbl$label[1], color = lbl$col[1],
           fill = "white", size = 2.9, fontface = "bold",
           label.size = 0.35, hjust = 0.5) +
  annotate("segment",
           x = lbl$x[1] + 0.35, xend = models$x_pos[1] - 0.05,
           y = lbl$y[1] - 0.035, yend = tip_y[1],
           color = lbl$col[1], linewidth = 0.5,
           arrow = arrow(length = unit(0.13, "cm"), type = "closed")) +

  # Logistic Regression — label right of its position
  annotate("label",
           x = lbl$x[2], y = lbl$y[2],
           label = lbl$label[2], color = lbl$col[2],
           fill = "white", size = 2.9, fontface = "bold",
           label.size = 0.35, hjust = 0.5) +
  annotate("segment",
           x = lbl$x[2] - 0.35, xend = models$x_pos[2] + 0.05,
           y = lbl$y[2] - 0.035, yend = tip_y[2],
           color = lbl$col[2], linewidth = 0.5,
           arrow = arrow(length = unit(0.13, "cm"), type = "closed")) +

  # Decision Tree — label left of its position
  annotate("label",
           x = lbl$x[3], y = lbl$y[3],
           label = lbl$label[3], color = lbl$col[3],
           fill = "white", size = 2.9, fontface = "bold",
           label.size = 0.35, hjust = 0.5) +
  annotate("segment",
           x = lbl$x[3] + 0.45, xend = models$x_pos[3] - 0.08,
           y = lbl$y[3] - 0.025, yend = tip_y[3],
           color = lbl$col[3], linewidth = 0.5,
           arrow = arrow(length = unit(0.13, "cm"), type = "closed")) +

  # ── Legend for point shapes (bottom-right corner) ──────────────────────────
  annotate("point", x = 8.60, y = 0.155, shape = 17, size = 3.2, color = "grey35") +
  annotate("text",  x = 8.75, y = 0.155, label = "Train Error (1\u2212F1)",
           size = 2.9, hjust = 0, color = "grey35") +
  annotate("point", x = 8.60, y = 0.105, shape = 16, size = 3.2, color = "grey35") +
  annotate("text",  x = 8.75, y = 0.105, label = "Val Error (1\u2212F1)",
           size = 2.9, hjust = 0, color = "grey35") +
  annotate("segment",
           x = 8.56, xend = 8.56, y = 0.09, yend = 0.17,
           color = "grey50", linewidth = 1.8, alpha = 0.6) +
  annotate("text",  x = 8.75, y = 0.130, label = "Gap",
           size = 2.9, hjust = 0, color = "grey50", fontface = "italic") +

  # ── Scales and theme ───────────────────────────────────────────────────────
  scale_color_manual(
    values = c("Bias\u00B2"  = "#E65100",
               "Variance"    = "#1565C0",
               "Total Error" = "#6A1B9A"),
    name = NULL
  ) +
  scale_x_continuous(
    name   = "Model Flexibility  \u2192",
    limits = c(0.3, 9.5),
    breaks = models$x_pos,
    labels = c("glmnet\nRidge", "Logistic\nRegression", "Decision\nTree"),
    expand = expansion(0)
  ) +
  scale_y_continuous(
    name   = "Error  (1 \u2212 F1)  \u2192",
    limits = c(0, y_max),
    breaks = seq(0, 0.7, 0.1),
    labels = sprintf("%.1f", seq(0, 0.7, 0.1)),
    expand = expansion(0)
  ) +

  labs(
    title    = "Bias\u2013Variance Tradeoff with Actual Model Positions",
    subtitle = paste0(
      "Conceptual curves scaled to match actual errors  |  ",
      "\u25B2 = Train Error  |  \u25CF = Val Error  |  ",
      "Vertical bar = Train\u2013Val gap"
    )
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(color = "grey40", size = 9.5,
                                      margin = margin(b = 10)),
    legend.position    = c(0.50, 0.90),
    legend.direction   = "horizontal",
    legend.background  = element_rect(fill = "white", color = "grey80",
                                      linewidth = 0.3),
    legend.text        = element_text(size = 10.5),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.4),
    axis.title.x       = element_text(size = 11, color = "grey30",
                                      margin = margin(t = 8)),
    axis.title.y       = element_text(size = 11, color = "grey30",
                                      margin = margin(r = 6)),
    axis.text.x        = element_text(size = 8.5, color = "grey30",
                                      lineheight = 0.85),
    axis.line          = element_line(color = "grey55", linewidth = 0.5),
    plot.margin        = margin(15, 25, 12, 15)
  )

# ── Save ──────────────────────────────────────────────────────────────────────
dir.create("../outputs/plots", recursive = TRUE, showWarnings = FALSE)
out <- "../outputs/plots/16_bias_variance_actual_models.png"
ggsave(out, p, width = 12, height = 7.5, dpi = 150)
cat(sprintf("Saved: outputs/plots/16_bias_variance_actual_models.png\n"))
