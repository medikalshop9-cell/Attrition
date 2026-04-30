# =============================================================================
# Threshold Tables — All 6 Models
# Prints Precision / Recall / F1 at each 0.10 step for every model
# =============================================================================

library(dplyr)
library(glmnet)
library(randomForest)

setwd("C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r")

# =============================================================================
# Helpers
# =============================================================================

evaluate <- function(probs, labels, threshold = 0.5) {
  preds <- ifelse(probs >= threshold, 1, 0)
  tp <- sum(preds == 1 & labels == 1)
  fp <- sum(preds == 1 & labels == 0)
  fn <- sum(preds == 0 & labels == 1)
  prec <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  rec  <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1   <- if ((prec + rec) > 0) 2 * prec * rec / (prec + rec) else 0
  list(prec = prec, rec = rec, f1 = f1)
}

print_table <- function(model_name, probs, labels) {
  thresholds <- seq(0.10, 0.90, by = 0.10)

  # Find optimal F1 threshold (0.01 resolution)
  fine <- seq(0.01, 0.99, by = 0.01)
  f1s  <- sapply(fine, function(t) evaluate(probs, labels, t)$f1)
  opt_t <- fine[which.max(f1s)]

  cat("\n")
  cat(rep("=", 62), "\n", sep = "")
  cat(sprintf("  MODEL: %s\n", model_name))
  cat(sprintf("  Optimal threshold (max F1): %.2f\n", opt_t))
  cat(rep("=", 62), "\n", sep = "")
  cat(sprintf("  %-12s  %-11s  %-11s  %-11s\n",
              "Threshold", "Precision", "Recall", "F1"))
  cat(rep("-", 62), "\n", sep = "")

  for (t in thresholds) {
    m    <- evaluate(probs, labels, t)
    flag <- if (abs(t - 0.50) < 0.001) "  <- default" else
            if (abs(t - opt_t) < 0.001) "  <- optimal" else ""
    cat(sprintf("  %-12.2f  %-11.3f  %-11.3f  %-11.3f%s\n",
                t, m$prec, m$rec, m$f1, flag))
  }

  # If optimal is not on a 0.10 boundary, print it separately
  if (!any(abs(thresholds - opt_t) < 0.001)) {
    m_opt <- evaluate(probs, labels, opt_t)
    cat(rep("-", 62), "\n", sep = "")
    cat(sprintf("  %-12.2f  %-11.3f  %-11.3f  %-11.3f  <- optimal\n",
                opt_t, m_opt$prec, m_opt$rec, m_opt$f1))
  }
  cat(rep("-", 62), "\n", sep = "")
}

# =============================================================================
# Load data
# =============================================================================

val_s <- readRDS("val_data.rds")
val_u <- readRDS("val_data_unscaled.rds")
y_val <- val_s$Attrition

cat(sprintf("\nVal set: %d rows | %d leavers (%.1f%%) | %d stayers\n",
            length(y_val), sum(y_val),
            100 * mean(y_val), sum(y_val == 0)))

# =============================================================================
# Phase-4 models
# =============================================================================

lr      <- readRDS("model_logistic.rds")
gn_obj  <- readRDS("model_glmnet.rds")
tree    <- readRDS("model_tree.rds")

prob_lr <- predict(lr, newdata = val_s, type = "response")
X_val   <- as.matrix(val_s %>% select(-Attrition))
prob_gn <- predict(gn_obj$model, newx = X_val,
                   s = gn_obj$lambda, type = "response")[, 1]
prob_dt <- predict(tree, newdata = val_u, type = "prob")[, "1"]

print_table("Logistic Regression",  prob_lr, y_val)
print_table("glmnet Ridge",         prob_gn, y_val)
print_table("Decision Tree",        prob_dt, y_val)

# =============================================================================
# Phase-5/6/7 models
# =============================================================================

rf_obj  <- readRDS("model_rf.rds")
xgb_obj <- readRDS("model_xgb.rds")
svm_obj <- readRDS("model_svm.rds")

# Random Forest — unscaled data, cleaned names, predict on feature matrix only
val_rf <- val_u
names(val_rf) <- make.names(names(val_rf), unique = TRUE)
X_rf    <- val_rf %>% select(-Attrition)
prob_rf <- predict(rf_obj$model, newdata = X_rf, type = "prob")[, "1"]

# XGBoost — unscaled, matrix
library(xgboost)
val_xgb <- val_u
names(val_xgb) <- make.names(names(val_xgb), unique = TRUE)
X_xgb   <- as.matrix(val_xgb %>% select(-Attrition))
dval    <- xgb.DMatrix(data = X_xgb)
prob_xgb <- predict(xgb_obj$model, dval)

# SVM — scaled data, cleaned names
val_svm <- val_s
names(val_svm) <- make.names(names(val_svm), unique = TRUE)
library(e1071)
pred_svm <- predict(svm_obj$model,
                    newdata = val_svm %>% select(-Attrition),
                    probability = TRUE)
prob_svm <- attr(pred_svm, "probabilities")[, "1"]

print_table("Random Forest", prob_rf, y_val)
print_table("XGBoost",       prob_xgb, y_val)
print_table("SVM",           prob_svm, y_val)

cat("\nDone.\n")
