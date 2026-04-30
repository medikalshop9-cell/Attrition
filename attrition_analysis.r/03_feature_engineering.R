# =============================================================================
# Phase 3: Feature Engineering
# Project: Predicting Employee Attrition Using AI
# Inputs:  train_data_unscaled.rds, val_data_unscaled.rds, test_data_unscaled.rds
# Outputs: Updated train_data.rds, val_data.rds, test_data.rds (scaled)
#          Updated train_data_unscaled.rds, val_data_unscaled.rds, test_data_unscaled.rds
#          Updated scaling_params.rds
# =============================================================================
# IMPORTANT: All three new features are derived from raw values ONLY.
#            The same formula is applied to train/val/test identically.
#            Scaling parameters are computed on train ONLY — no leakage.
# =============================================================================

library(dplyr)
library(caret)

# =============================================================================
# STEP 1: Load Unscaled Splits
# =============================================================================

cat("==============================================\n")
cat("PHASE 3: FEATURE ENGINEERING\n")
cat("==============================================\n\n")

cat("--- Loading Unscaled Splits ---\n")
train <- readRDS("train_data_unscaled.rds")
val   <- readRDS("val_data_unscaled.rds")
test  <- readRDS("test_data_unscaled.rds")

cat(sprintf("Train: %d rows x %d cols\n", nrow(train), ncol(train)))
cat(sprintf("Val:   %d rows x %d cols\n", nrow(val),   ncol(val)))
cat(sprintf("Test:  %d rows x %d cols\n", nrow(test),  ncol(test)))

# Confirm source columns are present (still in unscaled data)
required_cols <- c("YearsAtCompany", "JobSatisfaction", "EnvironmentSatisfaction",
                   "RelationshipSatisfaction", "MonthlyIncome", "TotalWorkingYears")
missing <- setdiff(required_cols, names(train))
if (length(missing) > 0) {
  stop("Missing required source columns: ", paste(missing, collapse = ", "))
} else {
  cat("PASS: All required source columns present.\n\n")
}

# =============================================================================
# FEATURE ENGINEERING FUNCTION
# Apply the exact same transformations to any split
# =============================================================================

engineer_features <- function(df) {

  # ------------------------------------------------------------------
  # Feature 1: TenureGroup
  # Ordinal segmentation of YearsAtCompany
  # Early (0-3 years) = 0 | Mid (4-10 years) = 1 | Senior (11+) = 2
  # Rationale: EDA showed shorter tenures correlate strongly with attrition.
  #            Capturing non-linear tenure thresholds as an ordinal feature.
  # ------------------------------------------------------------------
  df$TenureGroup <- case_when(
    df$YearsAtCompany <= 3  ~ 0L,
    df$YearsAtCompany <= 10 ~ 1L,
    TRUE                    ~ 2L
  )

  # ------------------------------------------------------------------
  # Feature 2: SatisfactionIndex
  # Composite mean of three satisfaction scores (each rated 1-4)
  #   JobSatisfaction + EnvironmentSatisfaction + RelationshipSatisfaction
  # Rationale: Combines correlated satisfaction signals into one feature
  #            to reduce multicollinearity and improve signal-to-noise.
  # Range: 1.0 – 4.0 (continuous)
  # ------------------------------------------------------------------
  df$SatisfactionIndex <- rowMeans(
    df[, c("JobSatisfaction", "EnvironmentSatisfaction", "RelationshipSatisfaction")],
    na.rm = TRUE
  )

  # ------------------------------------------------------------------
  # Feature 3: IncomePerTenureYear
  # MonthlyIncome / (TotalWorkingYears + 1)
  # +1 avoids division by zero for employees with <1 year experience
  # Rationale: Captures whether pay is commensurate with experience.
  #            Low ratio = underpaid relative to experience = attrition risk.
  # ------------------------------------------------------------------
  df$IncomePerTenureYear <- df$MonthlyIncome / (df$TotalWorkingYears + 1)

  return(df)
}

# =============================================================================
# STEP 2: Apply Feature Engineering to All Splits
# =============================================================================

cat("--- Engineering Features ---\n")

train <- engineer_features(train)
val   <- engineer_features(val)
test  <- engineer_features(test)

new_features <- c("TenureGroup", "SatisfactionIndex", "IncomePerTenureYear")
cat(sprintf("New features added: %s\n", paste(new_features, collapse = ", ")))
cat(sprintf("Train now: %d cols | Val: %d cols | Test: %d cols\n\n",
            ncol(train), ncol(val), ncol(test)))

# =============================================================================
# STEP 3: Sanity Check — New Features
# =============================================================================

cat("--- Sanity Checks: New Features ---\n")

# TenureGroup: should only contain 0, 1, 2
tg_vals <- unique(train$TenureGroup)
if (all(tg_vals %in% c(0L, 1L, 2L))) {
  cat("PASS: TenureGroup values are 0, 1, 2\n")
  cat("      Distribution (train):\n")
  tg_dist <- table(train$TenureGroup)
  for (v in names(tg_dist)) {
    cat(sprintf("        %s (%s): %d (%.1f%%)\n",
        v,
        c("0"="Early 0-3yr","1"="Mid 4-10yr","2"="Senior 11+yr")[v],
        tg_dist[[v]],
        tg_dist[[v]] / nrow(train) * 100))
  }
} else {
  cat("FAIL: Unexpected TenureGroup values:", paste(tg_vals, collapse=", "), "\n")
}

# SatisfactionIndex: should be between 1.0 and 4.0, no NAs
si_min <- min(train$SatisfactionIndex, na.rm = TRUE)
si_max <- max(train$SatisfactionIndex, na.rm = TRUE)
si_na  <- sum(is.na(train$SatisfactionIndex))
if (si_min >= 1.0 && si_max <= 4.0 && si_na == 0) {
  cat(sprintf("PASS: SatisfactionIndex range [%.2f, %.2f], NAs: %d\n", si_min, si_max, si_na))
} else {
  cat(sprintf("FAIL: SatisfactionIndex range [%.2f, %.2f], NAs: %d\n", si_min, si_max, si_na))
}

# IncomePerTenureYear: should be positive, no NAs, no Inf
ipt_na  <- sum(is.na(train$IncomePerTenureYear))
ipt_inf <- sum(is.infinite(train$IncomePerTenureYear))
ipt_min <- min(train$IncomePerTenureYear, na.rm = TRUE)
if (ipt_na == 0 && ipt_inf == 0 && ipt_min > 0) {
  cat(sprintf("PASS: IncomePerTenureYear — NAs: %d, Inf: %d, min: %.2f\n", ipt_na, ipt_inf, ipt_min))
} else {
  cat(sprintf("FAIL: IncomePerTenureYear — NAs: %d, Inf: %d, min: %.2f\n", ipt_na, ipt_inf, ipt_min))
}

cat("\n")

# =============================================================================
# STEP 4: Save Updated Unscaled Splits
# =============================================================================

cat("--- Saving Updated Unscaled Splits ---\n")
saveRDS(train, "train_data_unscaled.rds")
saveRDS(val,   "val_data_unscaled.rds")
saveRDS(test,  "test_data_unscaled.rds")
cat("Saved: train_data_unscaled.rds, val_data_unscaled.rds, test_data_unscaled.rds\n\n")

# =============================================================================
# STEP 5: Re-compute Scaling Parameters on Updated Train (no leakage)
# =============================================================================

cat("--- Re-computing Scaling Parameters on Train Only ---\n")

# Identify all continuous columns to scale
# Exclude: Attrition (target), binary/ordinal cols (<=3 unique values)
cols_to_scale <- names(train)[sapply(train, function(x) {
  is.numeric(x) && length(unique(x)) > 3
})]
cols_to_scale <- cols_to_scale[cols_to_scale != "Attrition"]

cat("Columns being scaled:", length(cols_to_scale), "\n")

# Compute from TRAIN ONLY
preProc <- preProcess(train[, cols_to_scale], method = c("center", "scale"))
saveRDS(preProc, "scaling_params.rds")
cat("Scaling parameters recomputed from train set only — saved to scaling_params.rds\n\n")

# =============================================================================
# STEP 6: Apply Scaling to All Splits and Save
# =============================================================================

cat("--- Applying Scaling and Saving Updated Splits ---\n")

apply_scaling <- function(df, preProc, cols) {
  df[, cols] <- predict(preProc, df[, cols])
  return(df)
}

train_scaled <- apply_scaling(train, preProc, cols_to_scale)
val_scaled   <- apply_scaling(val,   preProc, cols_to_scale)
test_scaled  <- apply_scaling(test,  preProc, cols_to_scale)

saveRDS(train_scaled, "train_data.rds")
saveRDS(val_scaled,   "val_data.rds")
saveRDS(test_scaled,  "test_data.rds")
cat("Saved: train_data.rds, val_data.rds, test_data.rds (scaled, with new features)\n\n")

# =============================================================================
# VERIFICATION SUMMARY
# =============================================================================

cat("========================================\n")
cat("PHASE 3 VERIFICATION SUMMARY\n")
cat("========================================\n")
cat("New features created:     ", paste(new_features, collapse = ", "), "\n")
cat("Columns (train, scaled):  ", ncol(train_scaled), "\n")
cat("Columns (train, unscaled):", ncol(train), "\n")
cat("NAs in train_data.rds:    ", sum(is.na(train_scaled)), "\n")
cat("NAs in val_data.rds:      ", sum(is.na(val_scaled)), "\n")
cat("NAs in test_data.rds:     ", sum(is.na(test_scaled)), "\n")
cat("All numeric (train):      ", all(sapply(train_scaled, is.numeric)), "\n")
cat("\nFeature summary (train, unscaled):\n")
cat(sprintf("  TenureGroup        — Early: %d | Mid: %d | Senior: %d\n",
    sum(train$TenureGroup == 0),
    sum(train$TenureGroup == 1),
    sum(train$TenureGroup == 2)))
cat(sprintf("  SatisfactionIndex  — mean: %.2f | sd: %.2f\n",
    mean(train$SatisfactionIndex), sd(train$SatisfactionIndex)))
cat(sprintf("  IncomePerTenureYear — mean: %.0f | sd: %.0f\n",
    mean(train$IncomePerTenureYear), sd(train$IncomePerTenureYear)))
cat("========================================\n")
cat("Phase 3 complete. Ready for Phase 4: Model Training\n")
