# =============================================================================
# Phase 4: Model Training
# Project: Predicting Employee Attrition Using AI
# Inputs:  train_data.rds (scaled)        — Logistic Regression, glmnet
#          train_data_unscaled.rds         — Decision Tree
#          val_data.rds / val_data_unscaled.rds — hyperparameter tuning
# Outputs: model_logistic.rds
#          model_glmnet.rds  (best lambda from val set)
#          model_tree.rds    (pruned)
#          model_training_results.rds  (val metrics for all models)
# =============================================================================

library(dplyr)
library(glmnet)
library(rpart)
library(rpart.plot)

# =============================================================================
# Helpers
# =============================================================================

# Evaluate predictions against true labels
evaluate <- function(probs, labels, threshold = 0.5) {
  preds <- ifelse(probs >= threshold, 1, 0)
  tp <- sum(preds == 1 & labels == 1)
  fp <- sum(preds == 1 & labels == 0)
  tn <- sum(preds == 0 & labels == 0)
  fn <- sum(preds == 0 & labels == 1)

  accuracy  <- (tp + tn) / length(labels)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall    <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1        <- if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0

  # ROC-AUC (trapezoidal)
  auc <- roc_auc(probs, labels)

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
  # trapezoidal rule — vectors are same length
  sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1)) / 2)
}

print_metrics <- function(name, m) {
  cat(sprintf(
    "  %-35s | Acc: %.3f | Prec: %.3f | Rec: %.3f | F1: %.3f | AUC: %.3f\n",
    name, m$accuracy, m$precision, m$recall, m$f1, m$auc
  ))
}

# =============================================================================
# STEP 1: Load Data
# =============================================================================

cat("==============================================\n")
cat("PHASE 4: MODEL TRAINING\n")
cat("==============================================\n\n")

cat("--- Loading Data ---\n")
train_s  <- readRDS("train_data.rds")           # scaled  — for LR + glmnet
val_s    <- readRDS("val_data.rds")             # scaled  — for LR + glmnet eval
train_u  <- readRDS("train_data_unscaled.rds")  # unscaled — for Decision Tree
val_u    <- readRDS("val_data_unscaled.rds")    # unscaled — for Decision Tree eval

cat(sprintf("Train (scaled):   %d rows x %d cols\n", nrow(train_s), ncol(train_s)))
cat(sprintf("Val   (scaled):   %d rows x %d cols\n", nrow(val_s),   ncol(val_s)))
cat(sprintf("Train (unscaled): %d rows x %d cols\n", nrow(train_u), ncol(train_u)))
cat("\n")

# Separate features and target
X_train  <- as.matrix(train_s  %>% select(-Attrition))
y_train  <- train_s$Attrition
X_val    <- as.matrix(val_s    %>% select(-Attrition))
y_val    <- val_s$Attrition

# =============================================================================
# MODEL 1: Logistic Regression (baseline)
# Uses: scaled data
# No regularization — establishes performance floor
# =============================================================================

cat("--- Model 1: Logistic Regression (Baseline) ---\n")

model_logistic <- glm(
  Attrition ~ .,
  data   = train_s,
  family = binomial(link = "logit")
)

# Check for convergence warnings
if (!model_logistic$converged) {
  cat("WARNING: Logistic Regression did not converge.\n")
} else {
  cat("Model converged successfully.\n")
}

# Validate on validation set
prob_lr_val <- predict(model_logistic, newdata = val_s, type = "response")
m_lr        <- evaluate(prob_lr_val, y_val)
print_metrics("Logistic Regression (val)", m_lr)

saveRDS(model_logistic, "model_logistic.rds")
cat("Saved: model_logistic.rds\n\n")

# =============================================================================
# MODEL 2: Regularized Logistic Regression — glmnet
# Tries both Ridge (alpha=0) and Lasso (alpha=1)
# Lambda selected by best F1 on validation set (not cross-validation on train)
# Uses: scaled data
# =============================================================================

cat("--- Model 2: Regularized Logistic Regression (glmnet) ---\n")

# Lambda grid: 100 values from 0.001 to 1 on log scale
lambda_grid <- 10^seq(-3, 0, length.out = 100)

best_glmnet    <- NULL
best_glmnet_f1 <- -Inf
best_alpha     <- NA
best_lambda    <- NA

for (alpha_val in c(0, 0.5, 1)) {
  label <- c("0" = "Ridge", "0.5" = "ElasticNet", "1" = "Lasso")[as.character(alpha_val)]

  fit <- glmnet(X_train, y_train,
                family  = "binomial",
                alpha   = alpha_val,
                lambda  = lambda_grid,
                standardize = FALSE)  # already scaled

  # Evaluate each lambda on val set
  for (lam in lambda_grid) {
    prob_val <- predict(fit, newx = X_val, s = lam, type = "response")[, 1]
    m        <- evaluate(prob_val, y_val)
    if (m$f1 > best_glmnet_f1) {
      best_glmnet_f1 <- m$f1
      best_glmnet    <- fit
      best_alpha     <- alpha_val
      best_lambda    <- lam
      best_glmnet_m  <- m
    }
  }
}

best_label <- c("0" = "Ridge", "0.5" = "ElasticNet", "1" = "Lasso")[as.character(best_alpha)]
cat(sprintf("Best glmnet config: %s (alpha=%.1f, lambda=%.5f)\n",
            best_label, best_alpha, best_lambda))
print_metrics(sprintf("glmnet %s (val)", best_label), best_glmnet_m)

# Non-zero coefficients (Lasso/ElasticNet sparsity)
coef_vec  <- coef(best_glmnet, s = best_lambda)
n_nonzero <- sum(coef_vec != 0) - 1  # exclude intercept
cat(sprintf("Non-zero coefficients: %d / %d\n", n_nonzero, ncol(X_train)))

saveRDS(list(model  = best_glmnet,
             alpha  = best_alpha,
             lambda = best_lambda), "model_glmnet.rds")
cat("Saved: model_glmnet.rds\n\n")

# =============================================================================
# MODEL 3: Decision Tree (rpart, pruned)
# Uses: UNSCALED data — trees are scale-invariant
# cp tuned on validation set to control overfitting
# =============================================================================

cat("--- Model 3: Decision Tree (rpart, pruned) ---\n")

# Grow a full tree first (cp=0 = unpruned)
tree_full <- rpart(
  Attrition ~ .,
  data    = train_u,
  method  = "class",
  control = rpart.control(cp = 0, minsplit = 10, minbucket = 5)
)

cat(sprintf("Full tree: %d leaves\n", sum(tree_full$frame$var == "<leaf>")))

# Tune cp: try values from the cptable + a fine grid around the 1-SE rule
cp_table    <- tree_full$cptable
cp_1se_idx  <- which.min(cp_table[, "xerror"])
cp_1se      <- cp_table[cp_1se_idx, "CP"]

# Wide cp grid: full cptable values + fine sweep from 0.001 to 0.05
cp_grid <- unique(sort(c(
  cp_table[, "CP"],
  seq(0.001, 0.05, length.out = 50)
)))

best_tree    <- NULL
best_tree_f1 <- -Inf
best_cp      <- NA

for (cp_val in cp_grid) {
  pruned <- prune(tree_full, cp = cp_val)
  prob_val_tree <- predict(pruned, newdata = val_u, type = "prob")[, "1"]
  # Handle case where "1" column might not exist (fully pruned to root)
  if (is.null(prob_val_tree) || all(is.na(prob_val_tree))) next
  m <- evaluate(prob_val_tree, y_val)
  if (m$f1 > best_tree_f1) {
    best_tree_f1 <- m$f1
    best_tree    <- pruned
    best_cp      <- cp_val
    best_tree_m  <- m
  }
}

n_leaves <- sum(best_tree$frame$var == "<leaf>")
cat(sprintf("Best cp: %.5f | Leaves after pruning: %d\n", best_cp, n_leaves))
print_metrics(sprintf("Decision Tree cp=%.5f (val)", best_cp), best_tree_m)

# Save tree plot
plot_dir <- "../outputs/plots"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
png(file.path(plot_dir, "11_decision_tree.png"), width = 1400, height = 900)
rpart.plot(best_tree,
           type  = 4,
           extra = 104,
           main  = "Pruned Decision Tree — Employee Attrition",
           cex   = 0.75)
dev.off()
cat("Saved: outputs/plots/11_decision_tree.png\n")

saveRDS(best_tree, "model_tree.rds")
cat("Saved: model_tree.rds\n\n")

# =============================================================================
# STEP 2: Consolidate Validation Results
# =============================================================================

cat("--- Validation Set Results Summary ---\n")

results <- list(
  logistic = list(model_name = "Logistic Regression",   metrics = m_lr),
  glmnet   = list(model_name = sprintf("glmnet (%s a=%.1f l=%.5f)", best_label, best_alpha, best_lambda),
                  metrics = best_glmnet_m),
  tree     = list(model_name = sprintf("Decision Tree (cp=%.5f)", best_cp),
                  metrics = best_tree_m)
)

saveRDS(results, "model_training_results.rds")
cat("Saved: model_training_results.rds\n\n")

# =============================================================================
# VERIFICATION SUMMARY
# =============================================================================

cat("========================================\n")
cat("PHASE 4 VERIFICATION SUMMARY\n")
cat("========================================\n")
cat(sprintf("%-38s | %s | %s | %s | %s | %s\n",
    "Model", "Acc  ", "Prec ", "Rec  ", "F1   ", "AUC  "))
cat(strrep("-", 85), "\n")
for (r in results) {
  m <- r$metrics
  cat(sprintf("  %-36s | %.3f | %.3f | %.3f | %.3f | %.3f\n",
      r$model_name, m$accuracy, m$precision, m$recall, m$f1, m$auc))
}
cat(strrep("-", 85), "\n")

best_model_name <- results[[which.max(sapply(results, function(r) r$metrics$f1))]]$model_name
cat(sprintf("\nBest model by F1 (val): %s\n", best_model_name))
cat("\nFiles saved:\n")
cat("  model_logistic.rds\n")
cat("  model_glmnet.rds\n")
cat("  model_tree.rds\n")
cat("  model_training_results.rds\n")
cat("  outputs/plots/11_decision_tree.png\n")
cat("========================================\n")
cat("Phase 4 complete. Ready for Phase 5: Model Evaluation\n")
