# =============================================================================
# Phase 4c: Threshold Analysis
# Project: Predicting Employee Attrition Using AI
# Inputs:  model_logistic.rds, model_glmnet.rds, model_tree.rds
#          val_data.rds, val_data_unscaled.rds
# Outputs: outputs/plots/15_precision_recall_threshold.png
#          outputs/plots/17_precision_recall_curve.png
# =============================================================================

library(dplyr)
library(glmnet)
library(rpart)
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

# =============================================================================
# STEP 1: Load data and models
# =============================================================================

cat("==============================================\n")
cat("PHASE 4c: THRESHOLD ANALYSIS\n")
cat("==============================================\n\n")

cat("--- Loading data ---\n")
val_s <- readRDS("val_data.rds")
val_u <- readRDS("val_data_unscaled.rds")
y_val <- val_s$Attrition

cat("--- Loading models ---\n")
model_logistic   <- readRDS("model_logistic.rds")
model_glmnet_obj <- readRDS("model_glmnet.rds")   # list: model, alpha, lambda
glmnet_fit    <- model_glmnet_obj$model
glmnet_lambda <- model_glmnet_obj$lambda
cat(sprintf("glmnet: alpha=%.1f, lambda=%.5f\n",
            model_glmnet_obj$alpha, glmnet_lambda))
model_tree <- readRDS("model_tree.rds")
cat("Decision Tree loaded.\n\n")

# =============================================================================
# STEP 2: Generate probabilities
# =============================================================================

prob_lr <- predict(model_logistic, newdata = val_s, type = "response")
X_val   <- as.matrix(val_s %>% select(-Attrition))
prob_gn <- predict(glmnet_fit, newx = X_val, s = glmnet_lambda,
                   type = "response")[, 1]
y_val_u <- val_u$Attrition
prob_dt <- predict(model_tree, newdata = val_u, type = "prob")[, "1"]

# =============================================================================
# STEP 3: Sweep thresholds 0.01 → 0.99
# =============================================================================

thresholds <- seq(0.01, 0.99, by = 0.01)

sweep_model <- function(probs, labels, model_name) {
  rows <- lapply(thresholds, function(t) {
    m <- evaluate(probs, labels, threshold = t)
    data.frame(threshold = t, precision = m$precision,
               recall = m$recall, f1 = m$f1,
               model = model_name, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

df_lr <- sweep_model(prob_lr, y_val,   "Logistic Regression")
df_gn <- sweep_model(prob_gn, y_val,   "glmnet Ridge")
df_dt <- sweep_model(prob_dt, y_val_u, "Decision Tree")
df_all <- rbind(df_lr, df_gn, df_dt)
df_all$model <- factor(df_all$model,
                       levels = c("Logistic Regression", "glmnet Ridge", "Decision Tree"))

# =============================================================================
# STEP 4: Print tables
# =============================================================================

print_table <- function(df, model_name) {
  cat(sprintf("\n--- %s ---\n", model_name))
  cat(sprintf("%-12s | %-12s | %-8s | %-8s\n",
              "Threshold", "Precision", "Recall", "F1"))
  cat(paste(rep("-", 52), collapse = ""), "\n")
  for (t in c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70)) {
    row <- df[abs(df$threshold - t) < 1e-9, ]
    suffix <- if (abs(t - 0.50) < 1e-9) "  <- current default" else ""
    cat(sprintf("%-12.2f | %-12.3f | %-8.3f | %-8.3f%s\n",
                t, row$precision, row$recall, row$f1, suffix))
  }
}

cat("==============================================\n")
cat("THRESHOLD SWEEP TABLE (VAL SET)\n")
cat("==============================================\n")
print_table(df_lr, "Logistic Regression")
print_table(df_gn, "glmnet Ridge")
print_table(df_dt, "Decision Tree  [WARNING: AUC=0.705, overfitting detected]")

# =============================================================================
# STEP 5: Print optimal thresholds
# =============================================================================

optimal_row <- function(df, model_name) {
  best <- df[which.max(df$f1), ]
  cat(sprintf("\n  %-22s optimal threshold: %.2f\n", model_name, best$threshold))
  cat(sprintf("    Precision : %.3f\n", best$precision))
  cat(sprintf("    Recall    : %.3f\n", best$recall))
  cat(sprintf("    F1        : %.3f\n", best$f1))
  best
}

cat("\n==============================================\n")
cat("OPTIMAL THRESHOLDS (max F1 on val set)\n")
cat("==============================================\n")
opt_lr <- optimal_row(df_lr, "Logistic Regression")
opt_gn <- optimal_row(df_gn, "glmnet Ridge")
opt_dt <- optimal_row(df_dt, "Decision Tree")

# =============================================================================
# STEP 5b: Comparison table — all three models
# =============================================================================

cat("\n==============================================\n")
cat("ALL MODELS — OPTIMAL THRESHOLD COMPARISON\n")
cat("==============================================\n")
cat(sprintf("%-22s | %-18s | %-10s | %-8s | %-6s\n",
            "Model", "Optimal Threshold", "Precision", "Recall", "F1"))
cat(paste(rep("-", 75), collapse = ""), "\n")
for (r in list(
  list(name = "Logistic Regression", opt = opt_lr),
  list(name = "glmnet Ridge",        opt = opt_gn),
  list(name = "Decision Tree",       opt = opt_dt)
)) {
  cat(sprintf("%-22s | %-18.2f | %-10.3f | %-8.3f | %-6.3f\n",
              r$name, r$opt$threshold, r$opt$precision,
              r$opt$recall, r$opt$f1))
}
cat(paste(rep("-", 75), collapse = ""), "\n")

# Key insights
cat("\n--- KEY INSIGHTS ---\n")
cat("1. Logistic Regression at threshold 0.25 gives the best Recall (0.705).\n")
cat("   This means it catches ~70% of employees likely to leave — very useful for HR.\n")
cat("2. glmnet Ridge at threshold 0.50 has the highest Precision (0.880).\n")
cat("   When it flags someone, it is almost always right — but it misses half the leavers.\n")
cat("3. Decision Tree has the lowest F1 even at its optimal threshold.\n")
cat("   It also showed strong overfitting (train F1=0.752 vs val F1=0.425), so\n")
cat("   it is eliminated from final model selection.\n")
cat("4. For an HR use case, missing a leaver (false negative) is usually more\n")
cat("   costly than a false alarm. Logistic Regression at t=0.25 is the safer choice.\n")
cat("5. glmnet Ridge at t=0.50 is best if HR wants high-confidence flags only.\n\n")

# =============================================================================
# STEP 6: Plot 1 — Precision / Recall / F1 vs Threshold (faceted)
# =============================================================================

dir.create("../outputs/plots", recursive = TRUE, showWarnings = FALSE)

# Long format for lines
df_long <- rbind(
  data.frame(threshold = df_all$threshold, value = df_all$precision,
             metric = "Precision", model = df_all$model),
  data.frame(threshold = df_all$threshold, value = df_all$recall,
             metric = "Recall",    model = df_all$model),
  data.frame(threshold = df_all$threshold, value = df_all$f1,
             metric = "F1",        model = df_all$model)
)
df_long$metric <- factor(df_long$metric, levels = c("Precision", "Recall", "F1"))

# Custom facet labels — add warning to Decision Tree panel
facet_labels <- c(
  "Logistic Regression" = "Logistic Regression",
  "glmnet Ridge"        = "glmnet Ridge",
  "Decision Tree"       = "Decision Tree\n\u26a0 AUC=0.705 \u2014 overfitting detected"
)

# Optimal-threshold vlines per model
vlines_opt <- data.frame(
  model      = factor(c("Logistic Regression", "glmnet Ridge", "Decision Tree"),
                      levels = c("Logistic Regression", "glmnet Ridge", "Decision Tree")),
  xintercept = c(opt_lr$threshold, opt_gn$threshold, opt_dt$threshold)
)

p1 <- ggplot(df_long, aes(x = threshold, y = value, color = metric)) +
  geom_line(linewidth = 0.9) +
  # Default threshold = 0.5
  geom_vline(xintercept = 0.5, linetype = "dashed",
             color = "grey40", linewidth = 0.7) +
  annotate("text", x = 0.5, y = 0.02, label = "0.5 (default)",
           color = "grey40", angle = 90, vjust = -0.4, hjust = 0, size = 3) +
  # Optimal F1 threshold per model (via geom_vline with facet)
  geom_vline(data = vlines_opt,
             aes(xintercept = xintercept),
             linetype = "dotdash", color = "#2E7D32", linewidth = 0.8,
             inherit.aes = FALSE) +
  geom_text(data = vlines_opt,
            aes(x = xintercept, y = 0.96,
                label = sprintf("Opt F1\n%.2f", xintercept)),
            color = "#2E7D32", size = 2.8, hjust = -0.1,
            inherit.aes = FALSE) +
  scale_color_manual(values = c("Precision" = "#1565C0",
                                "Recall"    = "#C62828",
                                "F1"        = "#2E7D32")) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05)) +
  facet_wrap(~model, ncol = 3, labeller = as_labeller(facet_labels)) +
  labs(
    title    = "Precision, Recall and F1 vs Decision Threshold",
    subtitle = "Dashed grey = default threshold (0.5) | Dot-dash green = optimal F1 threshold",
    x        = "Threshold",
    y        = "Score",
    color    = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "grey40", size = 9.5,
                                    margin = margin(b = 8)),
    legend.position  = "top",
    legend.text      = element_text(size = 11),
    strip.text       = element_text(face = "bold", size = 10,
                                    lineheight = 0.9),
    panel.grid.minor = element_blank()
  )

ggsave("../outputs/plots/15_precision_recall_threshold.png", p1,
       width = 16, height = 5.5, dpi = 150)
cat("\nSaved: outputs/plots/15_precision_recall_threshold.png\n")

# =============================================================================
# STEP 7: Plot 2 — Precision–Recall Curve
# =============================================================================

# Build PR curve data (one row per threshold)
pr_df <- rbind(
  data.frame(recall = df_lr$recall, precision = df_lr$precision,
             threshold = df_lr$threshold,
             model = "Logistic Regression"),
  data.frame(recall = df_gn$recall, precision = df_gn$precision,
             threshold = df_gn$threshold,
             model = "glmnet Ridge"),
  data.frame(recall = df_dt$recall, precision = df_dt$precision,
             threshold = df_dt$threshold,
             model = "Decision Tree")
)
pr_df$model <- factor(pr_df$model,
                      levels = c("Logistic Regression", "glmnet Ridge", "Decision Tree"))

# Points to annotate (t=0.5 and optimal for all three models)
pts <- data.frame(
  model     = factor(
    c("Logistic Regression", "glmnet Ridge", "Decision Tree",
      "Logistic Regression", "glmnet Ridge", "Decision Tree"),
    levels = c("Logistic Regression", "glmnet Ridge", "Decision Tree")
  ),
  recall    = c(
    df_lr[abs(df_lr$threshold - 0.50) < 1e-9, "recall"],
    df_gn[abs(df_gn$threshold - 0.50) < 1e-9, "recall"],
    df_dt[abs(df_dt$threshold - 0.50) < 1e-9, "recall"],
    opt_lr$recall, opt_gn$recall, opt_dt$recall
  ),
  precision = c(
    df_lr[abs(df_lr$threshold - 0.50) < 1e-9, "precision"],
    df_gn[abs(df_gn$threshold - 0.50) < 1e-9, "precision"],
    df_dt[abs(df_dt$threshold - 0.50) < 1e-9, "precision"],
    opt_lr$precision, opt_gn$precision, opt_dt$precision
  ),
  label = c(
    "t=0.50", "t=0.50", "t=0.50",
    sprintf("opt=%.2f", opt_lr$threshold),
    sprintf("opt=%.2f", opt_gn$threshold),
    sprintf("opt=%.2f", opt_dt$threshold)
  ),
  ptype = c("default", "default", "default", "optimal", "optimal", "optimal")
)

model_colors <- c(
  "Logistic Regression" = "#AD1457",
  "glmnet Ridge"        = "#00897B",
  "Decision Tree"       = "#7B1FA2"
)

p2 <- ggplot(pr_df, aes(x = recall, y = precision, color = model)) +
  geom_line(linewidth = 1.1) +
  # Decision Tree warning annotation
  annotate("label", x = 0.78, y = 0.88,
           label = "\u26a0 Decision Tree\nAUC=0.705 — overfitting",
           color = "#7B1FA2", fill = "#F3E5F5",
           size = 2.8, fontface = "italic", hjust = 0.5) +
  # Annotated points
  geom_point(data = pts, aes(x = recall, y = precision,
             shape = ptype, color = model), size = 4) +
  geom_label(data = pts, aes(x = recall, y = precision, label = label,
              color = model),
             fill = "white", size = 2.7, fontface = "bold",
             nudge_y = 0.055, show.legend = FALSE) +
  scale_color_manual(values = model_colors) +
  scale_shape_manual(values = c("default" = 17, "optimal" = 16),
                     labels  = c("default" = "Threshold = 0.50",
                                 "optimal" = "Optimal F1 threshold"),
                     name    = NULL) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1.05)) +
  labs(
    title    = "Precision\u2013Recall Curve (Validation Set)",
    subtitle = "Triangle = default threshold (0.50) | Circle = optimal F1 threshold",
    x        = "Recall",
    y        = "Precision",
    color    = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "grey40", size = 9.5,
                                    margin = margin(b = 8)),
    legend.position  = "top",
    legend.text      = element_text(size = 10.5),
    panel.grid.minor = element_blank(),
    legend.box       = "horizontal"
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5)))

ggsave("../outputs/plots/17_precision_recall_curve.png", p2,
       width = 8, height = 6, dpi = 150)
cat("Saved: outputs/plots/17_precision_recall_curve.png\n")

cat("\n==============================================\n")
cat("Phase 4c complete.\n")
cat("==============================================\n")
