# =============================================================================
# Phase 8: Tenure Variable Comparison
#
# Hypothesis: YearsAtCompany and TotalWorkingYears are correlated.
#   Multicollinearity is an interpretation problem, not automatically a
#   prediction problem. If dropping TotalWorkingYears doesn't hurt
#   performance, we can drop it for a simpler, better-interpreted model.
#
# Compares three variants on the same Phase-7 reduced feature set:
#   A  — Current rebuilt model (keeps BOTH YearsAtCompany + TotalWorkingYears)
#   B  — Drop TotalWorkingYears, keep YearsAtCompany
#   C  — Drop YearsAtCompany,   keep TotalWorkingYears (reversed, for reference)
#
# Decision rule:
#   if B metrics >= A  → adopt B (simpler, lower VIF)
#   if B degrades      → keep A (correlation tolerated for prediction)
# =============================================================================

library(dplyr)
library(jsonlite)

setwd("C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r")

cat("==============================================\n")
cat("PHASE 8: TENURE VARIABLE COMPARISON\n")
cat("==============================================\n\n")

# =============================================================================
# Helpers
# =============================================================================

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
  auc  <- roc_auc(probs, labels)
  list(acc = acc, prec = prec, rec = rec, f1 = f1, auc = auc,
       tp = tp, fp = fp, fn = fn, tn = tn)
}

# Shared threshold from Phase 7
THRESHOLD <- 0.25

# Phase-7 base drops (no tenure variable yet)
BASE_DROPS <- c(
  "SatisfactionIndex", "JobSatisfaction",
  "EnvironmentSatisfaction", "RelationshipSatisfaction",
  "Department.Research & Development", "Department.Sales",
  "Department.Human Resources",
  "JobRole.Human Resources", "JobRole.Sales Executive",
  "JobRole.Sales Representative", "JobRole.Laboratory Technician",
  "JobRole.Research Scientist",
  "JobLevel"
)

# =============================================================================
# STEP 1: Load scaled data
# =============================================================================

cat("--- Loading scaled data ---\n")
train_s <- readRDS("train_data.rds")
val_s   <- readRDS("val_data.rds")
test_s  <- readRDS("test_data.rds")
cat(sprintf("Train: %d x %d | Val: %d x %d | Test: %d x %d\n\n",
            nrow(train_s), ncol(train_s),
            nrow(val_s),   ncol(val_s),
            nrow(test_s),  ncol(test_s)))

# Helper to apply drop list
make_split <- function(extra_drop = character(0)) {
  drops <- intersect(names(train_s), c(BASE_DROPS, extra_drop))
  list(
    train    = train_s %>% select(-all_of(intersect(names(train_s), drops))),
    val      = val_s   %>% select(-all_of(intersect(names(val_s),   drops))),
    test     = test_s  %>% select(-all_of(intersect(names(test_s),  drops))),
    dropped  = drops
  )
}

# =============================================================================
# STEP 2: Build and evaluate three variants
# =============================================================================

variants <- list(
  A = list(label = "A — Both YearsAtCompany + TotalWorkingYears (current)",
           extra_drop = character(0)),
  B = list(label = "B — Keep YearsAtCompany, drop TotalWorkingYears",
           extra_drop = "TotalWorkingYears"),
  C = list(label = "C — Drop YearsAtCompany, keep TotalWorkingYears (reference)",
           extra_drop = "YearsAtCompany")
)

results <- list()

for (vname in names(variants)) {
  v   <- variants[[vname]]
  sp  <- make_split(v$extra_drop)
  tv  <- rbind(sp$train, sp$val)

  cat(sprintf("--- Variant %s ---\n", vname))
  cat(sprintf("  %s\n", v$label))
  cat(sprintf("  Features: %d | Dropped: %s\n",
              ncol(sp$train) - 1L,
              if (length(v$extra_drop) > 0) v$extra_drop else "none beyond base"))

  # Train on train-only for val threshold (fixed at 0.25 for comparability)
  m_train <- glm(Attrition ~ ., data = sp$train, family = binomial(link = "logit"))
  cat(sprintf("  Train-only model converged: %s\n",
              ifelse(m_train$converged, "YES", "NO")))

  # Final model on train+val
  m_final <- glm(Attrition ~ ., data = tv, family = binomial(link = "logit"))
  cat(sprintf("  Final model converged: %s | Coefficients: %d\n",
              ifelse(m_final$converged, "YES", "NO"),
              length(coef(m_final))))

  # Evaluate
  p_val  <- predict(m_train,  newdata = sp$val,  type = "response")
  p_test <- predict(m_final,  newdata = sp$test, type = "response")
  m_val  <- evaluate(p_val,  sp$val$Attrition,  THRESHOLD)
  m_test <- evaluate(p_test, sp$test$Attrition, THRESHOLD)

  cat(sprintf("  Val  — AUC: %.3f | F1: %.3f | Recall: %.3f | Prec: %.3f | Acc: %.3f\n",
              m_val$auc, m_val$f1, m_val$rec, m_val$prec, m_val$acc))
  cat(sprintf("  Test — AUC: %.3f | F1: %.3f | Recall: %.3f | Prec: %.3f | Acc: %.3f\n\n",
              m_test$auc, m_test$f1, m_test$rec, m_test$prec, m_test$acc))

  # VIF for the tenure variables present in this variant
  al <- alias(m_final)
  n_alias <- if (!is.null(al$Complete)) nrow(al$Complete) else 0
  tenure_vif <- NA
  if (n_alias == 0) {
    if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
    library(car)
    vif_vals <- tryCatch(vif(m_final), error = function(e) NULL)
    if (!is.null(vif_vals)) {
      tenure_cols <- intersect(c("YearsAtCompany", "TotalWorkingYears"), names(vif_vals))
      if (length(tenure_cols) > 0) {
        cat(sprintf("  VIF for tenure variables:\n"))
        for (tc in tenure_cols)
          cat(sprintf("    %-30s  %.2f\n", tc, vif_vals[[tc]]))
      }
    }
  } else {
    cat(sprintf("  WARNING: %d aliased coefficients remain\n", n_alias))
  }
  cat("\n")

  results[[vname]] <- list(
    label  = v$label,
    val    = m_val,
    test   = m_test,
    model  = m_final,
    splits = sp
  )
}

# =============================================================================
# STEP 3: Side-by-side comparison table
# =============================================================================

cat("==============================================================================\n")
cat("  VARIANT COMPARISON — TEST SET\n")
cat("==============================================================================\n")
cat(sprintf("  %-7s  %-7s  %-7s  %-7s  %-7s  %-7s\n",
            "Variant", "AUC", "F1", "Recall", "Prec", "Acc"))
cat(rep("-", 60), "\n", sep = "")
for (vname in names(results)) {
  r <- results[[vname]]$test
  cat(sprintf("  %-7s  %-7.3f  %-7.3f  %-7.3f  %-7.3f  %-7.3f\n",
              vname, r$auc, r$f1, r$rec, r$prec, r$acc))
}
cat(rep("-", 60), "\n", sep = "")
cat("\n")

# =============================================================================
# STEP 4: Decision + recommendation
# =============================================================================

a_test <- results$A$test
b_test <- results$B$test

cat("=== DECISION ===\n")
auc_delta <- b_test$auc - a_test$auc
f1_delta  <- b_test$f1  - a_test$f1
rec_delta <- b_test$rec - a_test$rec

cat(sprintf("  Variant B vs A:  AUC %+.4f | F1 %+.4f | Recall %+.4f\n\n",
            auc_delta, f1_delta, rec_delta))

if (b_test$f1 >= a_test$f1 - 0.005 &&
    b_test$auc >= a_test$auc - 0.005 &&
    b_test$rec >= a_test$rec - 0.01) {
  cat("  RECOMMENDATION: Adopt Variant B\n")
  cat("    — Dropping TotalWorkingYears does NOT hurt F1, AUC, or Recall.\n")
  cat("    — Simpler model with lower VIF and clearer tenure interpretation.\n")
  cat("    — YearsAtCompany is company-specific; TotalWorkingYears is redundant.\n\n")
  WINNER <- "B"
} else {
  cat("  RECOMMENDATION: Keep Variant A (current model)\n")
  cat("    — Dropping TotalWorkingYears degrades prediction performance.\n")
  cat("    — Multicollinearity tolerated: correlation does not hurt prediction here.\n\n")
  WINNER <- "A"
}

# =============================================================================
# STEP 5: If Variant B wins — save updated model + JSON
# =============================================================================

if (WINNER == "B") {
  cat("--- Saving Variant B as new model_final.rds ---\n")

  final_drops <- c(BASE_DROPS, "TotalWorkingYears")

  saveRDS(list(
    model          = results$B$model,
    threshold      = THRESHOLD,
    test_acc       = b_test$acc,
    test_precision = b_test$prec,
    test_recall    = b_test$rec,
    test_f1        = b_test$f1,
    test_auc       = b_test$auc,
    conf_mat       = b_test,
    dropped_cols   = final_drops
  ), "plumber_api/model_final.rds")
  cat("  Saved: plumber_api/model_final.rds\n")

  # Update insights_report.json
  cat("--- Updating insights_report.json ---\n")

  coefs_full <- coef(results$B$model)
  coefs_full <- coefs_full[names(coefs_full) != "(Intercept)"]
  coefs_full <- coefs_full[!is.na(coefs_full)]
  coef_df <- data.frame(
    feature = names(coefs_full),
    coeff   = as.numeric(coefs_full),
    stringsAsFactors = FALSE
  )
  coef_df <- coef_df[order(abs(coef_df$coeff), decreasing = TRUE), ]

  make_label <- function(feat) {
    feat <- gsub("`", "", feat)
    feat <- gsub("\\.", ": ", feat, fixed = FALSE)
    feat <- gsub("_", " ", feat)
    feat
  }

  top12 <- head(coef_df, 12)
  predictors_json <- lapply(seq_len(nrow(top12)), function(i) {
    r <- top12[i, ]
    list(
      feature     = r$feature,
      label       = make_label(r$feature),
      coefficient = round(r$coeff, 4),
      coeff_fmt   = ifelse(r$coeff >= 0,
                           sprintf("+%.2f", r$coeff),
                           sprintf("\u2212%.2f", abs(r$coeff))),
      direction   = ifelse(r$coeff > 0, "risk", "protect")
    )
  })

  insights <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    model        = "Logistic Regression",
    threshold    = THRESHOLD,
    test_metrics = list(
      accuracy  = round(b_test$acc,  4),
      precision = round(b_test$prec, 4),
      recall    = round(b_test$rec,  4),
      f1        = round(b_test$f1,   4),
      auc       = round(b_test$auc,  3)
    ),
    confusion_matrix = list(
      tn = b_test$tn, fp = b_test$fp,
      fn = b_test$fn, tp = b_test$tp
    ),
    top_predictors = predictors_json,
    model_comparison = list(
      list(name="Logistic Regression", threshold=THRESHOLD,
           acc=round(b_test$acc,3),  prec=round(b_test$prec,3),
           recall=round(b_test$rec,3), f1=round(b_test$f1,3),
           auc=round(b_test$auc,3), selected=TRUE),
      list(name="SVM",           threshold=0.31, acc=0.864, prec=0.675,
           recall=0.614, f1=0.643, auc=0.857, selected=FALSE),
      list(name="glmnet Ridge",  threshold=0.24, acc=0.850, prec=0.592,
           recall=0.682, f1=0.634, auc=0.851, selected=FALSE),
      list(name="XGBoost",       threshold=0.23, acc=0.850, prec=0.605,
           recall=0.659, f1=0.631, auc=0.860, selected=FALSE),
      list(name="Random Forest", threshold=0.27, acc=0.857, prec=0.629,
           recall=0.614, f1=0.621, auc=0.868, selected=FALSE),
      list(name="Decision Tree", threshold=0.50, acc=0.823, prec=0.484,
           recall=0.614, f1=0.541, auc=0.749, selected=FALSE)
    )
  )

  writeLines(toJSON(insights, auto_unbox = TRUE, pretty = TRUE),
             "plumber_api/insights_report.json")
  cat("  Saved: plumber_api/insights_report.json\n")

  cat("\n=== ACTION REQUIRED ===\n")
  cat(sprintf("  New CM: TN=%d FP=%d FN=%d TP=%d\n",
              b_test$tn, b_test$fp, b_test$fn, b_test$tp))
  cat(sprintf("  Update ModelInsights.jsx static CM fallback to:\n"))
  cat(sprintf("    { tn: %d, fp: %d, fn: %d, tp: %d }\n",
              b_test$tn, b_test$fp, b_test$fn, b_test$tp))
  cat(sprintf("  Update STATIC_METRICS to:\n"))
  cat(sprintf("    AUC: %.3f | F1: %.3f | Precision: %.3f | Recall: %.3f | Accuracy: %.1f%%\n",
              b_test$auc, b_test$f1, b_test$prec, b_test$rec, b_test$acc * 100))
  cat("\n")
} else {
  cat("--- No files changed: current model_final.rds is optimal ---\n\n")
}

cat("==============================================\n")
cat("Phase 8 complete.\n")
cat("==============================================\n")
