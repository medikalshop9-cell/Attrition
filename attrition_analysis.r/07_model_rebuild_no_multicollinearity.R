# =============================================================================
# Phase 7: Model Rebuild — Multicollinearity Removal
# Goal: Remove perfectly/structurally collinear variables, retrain LR,
#       validate stability, and export new model + insights for the API.
#
# Variables REMOVED:
#   Perfect alias   : SatisfactionIndex, JobSatisfaction,
#                     EnvironmentSatisfaction, RelationshipSatisfaction
#   Dept/Role overlap: Department.Research & Development, Department.Sales,
#                     Department.Human Resources (if present),
#                     JobRole.Sales Executive, JobRole.Sales Representative,
#                     JobRole.Laboratory Technician, JobRole.Research Scientist,
#                     JobRole.Human Resources
#   Compensation dup: JobLevel
#   Tenure dup      : YearsAtCompany
# =============================================================================

library(dplyr)
library(jsonlite)

setwd("C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r")

cat("==============================================\n")
cat("PHASE 7: MODEL REBUILD (No Multicollinearity)\n")
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

THRESHOLD <- 0.25

# Columns to drop (exact column names in the scaled data frames)
COLS_TO_DROP <- c(
  # Perfect alias (SatisfactionIndex = avg of the three below)
  "SatisfactionIndex",
  "JobSatisfaction",
  "EnvironmentSatisfaction",
  "RelationshipSatisfaction",
  # Structural Dept/Role redundancy
  "Department.Research & Development",
  "Department.Sales",
  "Department.Human Resources",          # drop if present
  "JobRole.Human Resources",
  "JobRole.Sales Executive",
  "JobRole.Sales Representative",
  "JobRole.Laboratory Technician",
  "JobRole.Research Scientist",
  # Compensation duplication
  "JobLevel",
  # Tenure duplication
  "YearsAtCompany"
)

drop_cols <- function(df) {
  to_drop <- intersect(names(df), COLS_TO_DROP)
  if (length(to_drop) > 0)
    cat(sprintf("  Dropping %d columns: %s\n", length(to_drop),
                paste(to_drop, collapse=", ")))
  df %>% select(-all_of(to_drop))
}

# =============================================================================
# STEP 1: Load data
# =============================================================================

cat("--- Loading scaled data ---\n")
train_s <- readRDS("train_data.rds")
val_s   <- readRDS("val_data.rds")
test_s  <- readRDS("test_data.rds")

cat(sprintf("Train: %d x %d | Val: %d x %d | Test: %d x %d\n\n",
            nrow(train_s), ncol(train_s),
            nrow(val_s),   ncol(val_s),
            nrow(test_s),  ncol(test_s)))

# =============================================================================
# STEP 2: Drop collinear columns
# =============================================================================

cat("--- Dropping multicollinear columns ---\n")
cat("  [Train] "); train_r <- drop_cols(train_s)
cat("  [Val]   "); val_r   <- drop_cols(val_s)
cat("  [Test]  "); test_r  <- drop_cols(test_s)

trainval_r <- rbind(train_r, val_r)

cat(sprintf("\nReduced dims: Train %d cols | Val %d cols | Test %d cols\n\n",
            ncol(train_r), ncol(val_r), ncol(test_r)))

remaining_features <- setdiff(names(train_r), "Attrition")
cat(sprintf("Remaining features (%d):\n  %s\n\n",
            length(remaining_features),
            paste(remaining_features, collapse=", ")))

# =============================================================================
# STEP 3: BASELINE performance (original model for comparison)
# =============================================================================

cat("--- Baseline: Original final model performance ---\n")
orig_obj <- readRDS("model_final.rds")
orig_model <- orig_obj$model

prob_val_orig  <- predict(orig_model, newdata = val_s,  type = "response")
prob_test_orig <- predict(orig_model, newdata = test_s, type = "response")
m_val_orig  <- evaluate(prob_val_orig,  val_s$Attrition,  THRESHOLD)
m_test_orig <- evaluate(prob_test_orig, test_s$Attrition, THRESHOLD)

cat(sprintf("  Val  — Acc: %.3f | Prec: %.3f | Rec: %.3f | F1: %.3f | AUC: %.3f\n",
            m_val_orig$acc, m_val_orig$prec, m_val_orig$rec,
            m_val_orig$f1, m_val_orig$auc))
cat(sprintf("  Test — Acc: %.3f | Prec: %.3f | Rec: %.3f | F1: %.3f | AUC: %.3f\n\n",
            m_test_orig$acc, m_test_orig$prec, m_test_orig$rec,
            m_test_orig$f1, m_test_orig$auc))

# =============================================================================
# STEP 4: Train new model on train only (for val tuning)
# =============================================================================

cat("--- Training new LR (train set only) for threshold search ---\n")
model_new_train <- glm(Attrition ~ ., data = train_r, family = binomial(link = "logit"))
cat(sprintf("  Converged: %s | Coefficients: %d\n\n",
            ifelse(model_new_train$converged, "YES", "NO"),
            length(coef(model_new_train))))

prob_val_new <- predict(model_new_train, newdata = val_r, type = "response")

# Find best threshold on validation set (maximise F1)
best_t <- THRESHOLD
best_f1 <- 0
for (t in seq(0.10, 0.50, by = 0.01)) {
  m <- evaluate(prob_val_new, val_r$Attrition, t)
  if (m$f1 > best_f1) { best_f1 <- m$f1; best_t <- t }
}
cat(sprintf("  Best val threshold: %.2f (F1=%.3f)\n", best_t, best_f1))
m_val_new <- evaluate(prob_val_new, val_r$Attrition, best_t)
cat(sprintf("  Val — Acc: %.3f | Prec: %.3f | Rec: %.3f | F1: %.3f | AUC: %.3f\n\n",
            m_val_new$acc, m_val_new$prec, m_val_new$rec,
            m_val_new$f1, m_val_new$auc))

# =============================================================================
# STEP 5: AUC safety check — reconsider YearsAtCompany if AUC drops > 0.02
# =============================================================================

auc_drop <- m_val_orig$auc - m_val_new$auc
cat(sprintf("--- AUC change (orig val → new val): %+.4f ---\n", -auc_drop))
if (auc_drop > 0.02) {
  cat("  WARNING: AUC dropped > 0.02 — re-evaluating with YearsAtCompany kept\n")
  COLS_TO_DROP_SAFE <- setdiff(COLS_TO_DROP, "YearsAtCompany")
  train_r2 <- train_s %>% select(-all_of(intersect(names(train_s), COLS_TO_DROP_SAFE)))
  val_r2   <- val_s   %>% select(-all_of(intersect(names(val_s),   COLS_TO_DROP_SAFE)))
  test_r2  <- test_s  %>% select(-all_of(intersect(names(test_s),  COLS_TO_DROP_SAFE)))
  m2 <- glm(Attrition ~ ., data = train_r2, family = binomial(link = "logit"))
  p2 <- predict(m2, newdata = val_r2, type = "response")
  ev2 <- evaluate(p2, val_r2$Attrition, best_t)
  if (ev2$auc >= m_val_new$auc) {
    cat(sprintf("  Keeping YearsAtCompany restores AUC to %.3f — using safe set\n\n", ev2$auc))
    train_r <- train_r2; val_r <- val_r2; test_r <- test_r2
    trainval_r <- rbind(train_r, val_r)
  } else {
    cat("  Keeping YearsAtCompany doesn't help — proceeding with original removal\n\n")
  }
} else {
  cat("  AUC stable — proceeding with all planned removals\n\n")
}

# =============================================================================
# STEP 6: Final model — retrain on train+val, evaluate on test
# =============================================================================

cat("--- Final model: retrain on train+val ---\n")
final_model <- glm(Attrition ~ ., data = trainval_r, family = binomial(link = "logit"))
cat(sprintf("  Converged: %s | Coefficients: %d\n\n",
            ifelse(final_model$converged, "YES", "NO"),
            length(coef(final_model))))

prob_test_new <- predict(final_model, newdata = test_r, type = "response")
m_test_new    <- evaluate(prob_test_new, test_r$Attrition, best_t)

cat("==============================================================================\n")
cat("  BEFORE vs AFTER COMPARISON\n")
cat("==============================================================================\n")
cat(sprintf("  %-18s  %-7s  %-7s  %-7s  %-7s  %-7s\n",
            "Model", "Acc", "Prec", "Rec", "F1", "AUC"))
cat(rep("-", 60), "\n", sep="")
cat(sprintf("  %-18s  %-7.3f  %-7.3f  %-7.3f  %-7.3f  %-7.3f\n",
            "Original (test)", m_test_orig$acc, m_test_orig$prec,
            m_test_orig$rec, m_test_orig$f1, m_test_orig$auc))
cat(sprintf("  %-18s  %-7.3f  %-7.3f  %-7.3f  %-7.3f  %-7.3f\n",
            "Rebuilt  (test)", m_test_new$acc, m_test_new$prec,
            m_test_new$rec, m_test_new$f1, m_test_new$auc))
cat(rep("-", 60), "\n", sep="")
cat(sprintf("  Delta            %+7.3f  %+7.3f  %+7.3f  %+7.3f  %+7.3f\n",
            m_test_new$acc  - m_test_orig$acc,
            m_test_new$prec - m_test_orig$prec,
            m_test_new$rec  - m_test_orig$rec,
            m_test_new$f1   - m_test_orig$f1,
            m_test_new$auc  - m_test_orig$auc))
cat("\n")

# =============================================================================
# STEP 7: VIF check on rebuilt model
# =============================================================================

cat("--- VIF check on rebuilt model ---\n")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car", repos = "https://cloud.r-project.org")
library(car)

# Check for remaining aliases
al <- alias(final_model)
remaining_aliases <- if (!is.null(al$Complete)) nrow(al$Complete) else 0
cat(sprintf("  Aliased coefficients: %d\n", remaining_aliases))

if (remaining_aliases == 0) {
  vif_vals <- vif(final_model)
  sv <- sort(vif_vals, decreasing = TRUE)
  cat(sprintf("  VIF computed for %d predictors\n", length(sv)))
  cat(sprintf("  High >10: %d | Moderate 5-10: %d | OK <5: %d\n",
              sum(sv > 10), sum(sv >= 5 & sv <= 10), sum(sv < 5)))
  cat("\n  Top 10 VIF:\n")
  for (nm in names(head(sv, 10))) {
    flag <- if (sv[[nm]] > 10) "***" else if (sv[[nm]] >= 5) " * " else "   "
    cat(sprintf("    %s  %-45s  %.2f\n", flag, nm, sv[[nm]]))
  }
} else {
  cat("  WARNING: Aliases remain — check COLS_TO_DROP\n")
  print(al$Complete)
}

cat("\n")

# =============================================================================
# STEP 8: MonthlyIncome sanity check
# =============================================================================

cat("--- MonthlyIncome sanity check ---\n")
data_for_check <- rbind(train_r, val_r, test_r)

# Summary stats by class (unscaled — load original for reference)
train_u <- readRDS("train_data_unscaled.rds")
mi_stay  <- train_u$MonthlyIncome[train_u$Attrition == 0]
mi_leave <- train_u$MonthlyIncome[train_u$Attrition == 1]
fmt_usd <- function(x) format(round(x), big.mark=",", scientific=FALSE)
cat(sprintf("  Stay  — Median: $%s | Mean: $%s | SD: $%s\n",
            fmt_usd(median(mi_stay)), fmt_usd(mean(mi_stay)), fmt_usd(sd(mi_stay))))
cat(sprintf("  Leave — Median: $%s | Mean: $%s | SD: $%s\n",
            fmt_usd(median(mi_leave)), fmt_usd(mean(mi_leave)), fmt_usd(sd(mi_leave))))
overlap <- (mean(mi_leave) - mean(mi_stay)) / sd(c(mi_stay, mi_leave))
cat(sprintf("  Effect size (Cohen's d): %.2f  %s\n",
            overlap,
            ifelse(abs(overlap) < 0.5, "(small — no proxy leakage)",
                   ifelse(abs(overlap) < 0.8, "(moderate)", "(large — check for leakage)"))))
cat("\n")

# =============================================================================
# STEP 9: Coefficient stability check
# =============================================================================

cat("--- Coefficient stability (train-only vs train+val model) ---\n")
coefs_train  <- coef(model_new_train)
coefs_final  <- coef(final_model)
common_coefs <- intersect(names(coefs_train), names(coefs_final))

max_inflation <- 0
unstable <- c()
for (nm in common_coefs) {
  ratio <- abs(coefs_final[[nm]] / (coefs_train[[nm]] + 1e-9))
  if (ratio > 3 || abs(coefs_final[[nm]]) > 8) {
    unstable <- c(unstable, nm)
    max_inflation <- max(max_inflation, abs(coefs_final[[nm]]))
  }
}
if (length(unstable) == 0) {
  cat("  All coefficients stable (no extreme inflation detected)\n")
} else {
  cat(sprintf("  %d potentially unstable coefficients (|coef| > 8 or ratio > 3x):\n", length(unstable)))
  for (nm in unstable) {
    cat(sprintf("    %-40s  train: %+.3f  final: %+.3f\n",
                nm, coefs_train[[nm]], coefs_final[[nm]]))
  }
}
cat("\n")

# =============================================================================
# STEP 10: Top predictors for insights_report.json
# =============================================================================

coefs_full <- coef(final_model)
coefs_full <- coefs_full[names(coefs_full) != "(Intercept)"]
coefs_full <- coefs_full[!is.na(coefs_full)]
coef_df <- data.frame(
  feature = names(coefs_full),
  coeff   = as.numeric(coefs_full),
  stringsAsFactors = FALSE
)
coef_df <- coef_df[order(abs(coef_df$coeff), decreasing = TRUE), ]

# Pretty label helper
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
    feature   = r$feature,
    label     = make_label(r$feature),
    coefficient = round(r$coeff, 4),
    coeff_fmt = ifelse(r$coeff >= 0,
                       sprintf("+%.2f", r$coeff),
                       sprintf("\u2212%.2f", abs(r$coeff))),
    direction = ifelse(r$coeff > 0, "risk", "protect")
  )
})

# =============================================================================
# STEP 11: Save new model_final.rds → plumber_api/
# =============================================================================

cat("--- Saving new model_final.rds ---\n")
saveRDS(list(
  model          = final_model,
  threshold      = best_t,
  test_acc       = m_test_new$acc,
  test_precision = m_test_new$prec,
  test_recall    = m_test_new$rec,
  test_f1        = m_test_new$f1,
  test_auc       = m_test_new$auc,
  conf_mat       = m_test_new,
  dropped_cols   = COLS_TO_DROP
), "plumber_api/model_final.rds")
cat("  Saved: plumber_api/model_final.rds\n")

# Also save new scaling_params (unchanged — same numeric features were scaled)
# scaling_params only covers numeric columns, no changes needed there.

# =============================================================================
# STEP 12: Write insights_report.json
# =============================================================================

cat("--- Writing insights_report.json ---\n")

# Model comparison benchmarks (keep previous non-LR models, update LR row)
insights <- list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  model      = "Logistic Regression",
  threshold  = best_t,
  test_metrics = list(
    accuracy  = round(m_test_new$acc,  4),
    precision = round(m_test_new$prec, 4),
    recall    = round(m_test_new$rec,  4),
    f1        = round(m_test_new$f1,   4),
    auc       = round(m_test_new$auc,  3)
  ),
  confusion_matrix = list(
    tn = m_test_new$tn,
    fp = m_test_new$fp,
    fn = m_test_new$fn,
    tp = m_test_new$tp
  ),
  top_predictors = predictors_json,
  model_comparison = list(
    list(name="Logistic Regression", threshold=best_t,
         acc=round(m_test_new$acc,3), prec=round(m_test_new$prec,3),
         recall=round(m_test_new$rec,3), f1=round(m_test_new$f1,3),
         auc=round(m_test_new$auc,3), selected=TRUE),
    # Keep prior benchmark results for other models (unchanged)
    list(name="SVM",             threshold=0.31, acc=0.864, prec=0.675,
         recall=0.614, f1=0.643, auc=0.857, selected=FALSE),
    list(name="glmnet Ridge",    threshold=0.24, acc=0.850, prec=0.592,
         recall=0.682, f1=0.634, auc=0.851, selected=FALSE),
    list(name="Decision Tree",   threshold=0.50, acc=0.823, prec=0.484,
         recall=0.614, f1=0.541, auc=0.749, selected=FALSE),
    list(name="Random Forest",   threshold=0.27, acc=0.857, prec=0.629,
         recall=0.614, f1=0.621, auc=0.868, selected=FALSE),
    list(name="XGBoost",         threshold=0.23, acc=0.850, prec=0.605,
         recall=0.659, f1=0.631, auc=0.860, selected=FALSE)
  ),
  removed_for_multicollinearity = COLS_TO_DROP
)

write(toJSON(insights, auto_unbox = TRUE, pretty = TRUE),
      "plumber_api/insights_report.json")
cat("  Saved: plumber_api/insights_report.json\n\n")

cat("==============================================\n")
cat("Phase 7 complete.\n")
cat("==============================================\n")
