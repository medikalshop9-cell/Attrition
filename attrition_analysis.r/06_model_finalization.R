# =============================================================================
# Phase 6: Model Finalization
# Selected model: Logistic Regression (optimal threshold = 0.25)
# Retrain on train + val combined, evaluate ONCE on test set
# =============================================================================

library(dplyr)

setwd("C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r")

cat("==============================================\n")
cat("PHASE 6: MODEL FINALIZATION\n")
cat("Selected model: Logistic Regression (t=0.25)\n")
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

THRESHOLD <- 0.25

# =============================================================================
# STEP 1: Combine train + val
# =============================================================================

train_s <- readRDS("train_data.rds")
val_s   <- readRDS("val_data.rds")
test_s  <- readRDS("test_data.rds")

trainval_s <- rbind(train_s, val_s)

cat(sprintf("Train rows    : %d\n", nrow(train_s)))
cat(sprintf("Val rows      : %d\n", nrow(val_s)))
cat(sprintf("Train+Val rows: %d  (attrition rate: %.1f%%)\n",
            nrow(trainval_s), 100 * mean(trainval_s$Attrition)))
cat(sprintf("Test rows     : %d  (attrition rate: %.1f%%)\n\n",
            nrow(test_s), 100 * mean(test_s$Attrition)))

# =============================================================================
# STEP 2: Retrain Logistic Regression on train + val
# =============================================================================

cat("--- Retraining Logistic Regression on train + val ---\n")

final_model <- glm(
  Attrition ~ .,
  data   = trainval_s,
  family = binomial(link = "logit")
)

cat(sprintf("  Converged: %s\n", ifelse(final_model$converged, "YES", "NO")))
cat(sprintf("  Coefficients: %d\n\n", length(coef(final_model))))

# Validate on train+val (sanity check — should not be used for selection)
prob_tv <- predict(final_model, newdata = trainval_s, type = "response")
m_tv    <- evaluate(prob_tv, trainval_s$Attrition, THRESHOLD)
cat(sprintf("Train+Val perf (t=%.2f): Acc=%.3f | Prec=%.3f | Rec=%.3f | F1=%.3f\n\n",
            THRESHOLD, m_tv$acc, m_tv$prec, m_tv$rec, m_tv$f1))

# =============================================================================
# STEP 3: Final evaluation on test set (TOUCH ONCE)
# =============================================================================

cat("--- Final evaluation on TEST SET ---\n")
cat("    (Test set touched for the first and only time)\n\n")

y_test   <- test_s$Attrition
prob_test <- predict(final_model, newdata = test_s, type = "response")

m_test  <- evaluate(prob_test, y_test, THRESHOLD)
auc_test <- roc_auc(prob_test, y_test)

cat(sprintf("  Threshold : %.2f\n",  THRESHOLD))
cat(sprintf("  Accuracy  : %.3f\n",  m_test$acc))
cat(sprintf("  Precision : %.3f\n",  m_test$prec))
cat(sprintf("  Recall    : %.3f\n",  m_test$rec))
cat(sprintf("  F1 Score  : %.3f\n",  m_test$f1))
cat(sprintf("  AUC       : %.3f\n",  auc_test))
cat("\n")

cat(sprintf("  Confusion Matrix:\n"))
cat(sprintf("                     Pred: Stay   Pred: Leave\n"))
cat(sprintf("    Actual: Stay      %5d         %5d\n",    m_test$tn, m_test$fp))
cat(sprintf("    Actual: Leave     %5d         %5d\n",    m_test$fn, m_test$tp))
cat(sprintf("\n"))
cat(sprintf("  Miss rate  (FN/P): %.1f%%  — leavers predicted as staying\n",
            100 * m_test$fn / sum(y_test == 1)))
cat(sprintf("  False alarm (FP/N): %.1f%%  — stayers predicted as leaving\n",
            100 * m_test$fp / sum(y_test == 0)))
cat("\n")

# =============================================================================
# STEP 4: Compare val vs test performance
# =============================================================================

cat("==============================================================================\n")
cat("  VAL vs TEST PERFORMANCE COMPARISON\n")
cat("==============================================================================\n")
cat(sprintf("  %-12s  %-8s  %-8s  %-8s  %-8s  %-6s\n",
            "Split", "Acc", "Prec", "Recall", "F1", "AUC"))
cat(rep("-", 60), "\n", sep = "")

# Val performance (from evaluation phase, using original LR model)
lr_orig <- readRDS("model_logistic.rds")
prob_val_orig <- predict(lr_orig, newdata = val_s, type = "response")
m_val  <- evaluate(prob_val_orig, val_s$Attrition, THRESHOLD)
auc_val <- roc_auc(prob_val_orig, val_s$Attrition)

cat(sprintf("  %-12s  %-8.3f  %-8.3f  %-8.3f  %-8.3f  %-6.3f  (original model)\n",
            "Validation", m_val$acc, m_val$prec, m_val$rec, m_val$f1, auc_val))
cat(sprintf("  %-12s  %-8.3f  %-8.3f  %-8.3f  %-8.3f  %-6.3f  (final model)\n",
            "Test", m_test$acc, m_test$prec, m_test$rec, m_test$f1, auc_test))
cat(rep("-", 60), "\n", sep = "")
f1_diff <- m_test$f1 - m_val$f1
cat(sprintf("\n  F1 change val → test: %+.3f  (%s)\n",
            f1_diff, ifelse(abs(f1_diff) <= 0.03, "stable generalisation",
                     ifelse(f1_diff < 0, "slight degradation", "improved"))))
cat("\n")

# =============================================================================
# STEP 5: Top drivers of attrition (coefficients)
# =============================================================================

cat("==============================================================================\n")
cat("  TOP ATTRITION DRIVERS (Final Model Coefficients)\n")
cat("==============================================================================\n")

coefs <- coef(final_model)
coefs <- coefs[names(coefs) != "(Intercept)"]
coef_df <- data.frame(
  Feature    = names(coefs),
  Coefficient = as.numeric(coefs),
  OddsRatio  = round(exp(as.numeric(coefs)), 3),
  stringsAsFactors = FALSE
)
coef_df <- coef_df[order(abs(coef_df$Coefficient), decreasing = TRUE), ]
rownames(coef_df) <- NULL

cat(sprintf("  %-35s  %10s  %10s\n", "Feature", "Coeff", "Odds Ratio"))
cat(rep("-", 60), "\n", sep = "")
for (i in seq_len(min(15, nrow(coef_df)))) {
  r <- coef_df[i, ]
  direction <- if (r$Coefficient > 0) "(+risk)" else "(-risk)"
  cat(sprintf("  %-35s  %10.4f  %10.3f  %s\n",
              r$Feature, r$Coefficient, r$OddsRatio, direction))
}
cat("\n")

# =============================================================================
# STEP 6: Save final model
# =============================================================================

saveRDS(list(
  model          = final_model,
  threshold      = THRESHOLD,
  test_acc       = m_test$acc,
  test_precision = m_test$prec,
  test_recall    = m_test$rec,
  test_f1        = m_test$f1,
  test_auc       = auc_test,
  conf_mat       = m_test
), "model_final.rds")
cat("Saved: model_final.rds\n\n")

cat("==============================================\n")
cat("Phase 6 (Model Finalization) complete.\n")
cat("==============================================\n")
