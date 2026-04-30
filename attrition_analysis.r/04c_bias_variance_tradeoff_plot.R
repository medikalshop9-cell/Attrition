# =============================================================================
# Phase 4c: Bias-Variance Tradeoff — Conceptual Illustration
# Project: Predicting Employee Attrition Using AI
# Output:  outputs/plots/15_bias_variance_tradeoff.png
# =============================================================================

library(ggplot2)

# =============================================================================
# Generate conceptual curves
# =============================================================================

x <- seq(0.5, 10, length.out = 300)   # model flexibility (arbitrary units)

irreducible <- 0.10

# Bias² — decreases as flexibility increases (exponential decay)
bias2 <- 0.85 * exp(-0.55 * x) + irreducible * 0.3

# Variance — increases as flexibility increases (exponential growth)
variance <- 0.015 * exp(0.42 * x)

# Total Error = Bias² + Variance + Irreducible
total <- bias2 + variance + irreducible

# Build long-format data frame for ggplot
df <- rbind(
  data.frame(x = x, y = bias2,      curve = "Bias²"),
  data.frame(x = x, y = variance,   curve = "Variance"),
  data.frame(x = x, y = total,      curve = "Total Error")
)

df$curve <- factor(df$curve, levels = c("Bias²", "Variance", "Total Error"))

# =============================================================================
# Key marker positions
# =============================================================================

# Optimal model = minimum of Total Error
opt_idx     <- which.min(total)
opt_x       <- x[opt_idx]
opt_y       <- total[opt_idx]

# Underfit = low flexibility (left region)
uf_x        <- x[round(0.10 * length(x))]
uf_y        <- total[round(0.10 * length(x))]

# Overfit = high flexibility (right region)
of_x        <- x[round(0.90 * length(x))]
of_y        <- total[round(0.90 * length(x))]

markers <- data.frame(
  x     = c(uf_x,           opt_x,           of_x),
  y     = c(uf_y,           opt_y,           of_y),
  label = c("Underfit\nModel", "Optimal\nModel", "Overfit\nModel"),
  color = c("#E53935",       "#2E7D32",        "#1565C0")
)

# =============================================================================
# Plot
# =============================================================================

curve_colors <- c(
  "Bias²"       = "#E65100",
  "Variance"    = "#1565C0",
  "Total Error" = "#6A1B9A"
)

p <- ggplot() +

  # Irreducible error — dashed horizontal line
  geom_hline(yintercept = irreducible, linetype = "dashed",
             color = "grey40", linewidth = 0.7) +
  annotate("text", x = 9.6, y = irreducible + 0.025,
           label = "Irreducible Error", color = "grey40",
           size = 3.4, hjust = 1, fontface = "italic") +

  # Main curves
  geom_line(data = df, aes(x = x, y = y, color = curve),
            linewidth = 1.1) +

  # Shaded region showing bias-variance gap around optimal
  geom_vline(xintercept = opt_x, linetype = "dotted",
             color = "#2E7D32", linewidth = 0.7) +

  # Marker points
  geom_point(data = markers, aes(x = x, y = y),
             color = markers$color, size = 4, shape = 18) +

  # Marker labels with offset boxes
  annotate("label",
           x     = c(uf_x - 0.1, opt_x + 0.05, of_x + 0.1),
           y     = c(uf_y + 0.11, opt_y + 0.10, of_y + 0.11),
           label = c("Underfit\nModel", "Optimal\nModel", "Overfit\nModel"),
           color = c("#E53935", "#2E7D32", "#1565C0"),
           fill  = "white", label.size = 0.3,
           size  = 3.3, fontface = "bold", hjust = c(0.5, 0.5, 0.5)) +

  # Scales and labels
  scale_color_manual(values = curve_colors) +
  scale_x_continuous(
    name   = "Model Flexibility  \u2192",
    breaks = NULL,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    name   = "Error  \u2192",
    limits = c(0, max(total) * 1.15),
    breaks = NULL,
    expand = expansion(mult = c(0.02, 0.02))
  ) +

  labs(
    title    = "Bias–Variance Tradeoff",
    subtitle = "As model flexibility increases, bias decreases but variance grows;\nTotal Error is minimised at the Optimal Model",
    color    = NULL
  ) +

  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "grey40", size = 10.5, margin = margin(b = 8)),
    legend.position  = c(0.50, 0.88),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "white", color = "grey80", linewidth = 0.3),
    legend.text      = element_text(size = 11),
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.title.x     = element_text(size = 11, color = "grey30", margin = margin(t = 6)),
    axis.title.y     = element_text(size = 11, color = "grey30", margin = margin(r = 6)),
    axis.line        = element_line(color = "grey50", linewidth = 0.5),
    plot.margin      = margin(15, 20, 10, 10)
  )

# =============================================================================
# Save
# =============================================================================

dir.create("../outputs/plots", recursive = TRUE, showWarnings = FALSE)
ggsave("../outputs/plots/15_bias_variance_tradeoff.png", p,
       width = 9, height = 6, dpi = 150)

cat("Saved: outputs/plots/15_bias_variance_tradeoff.png\n")
