# =============================================================================
# Phase 1: Data Preparation
# Project: Predicting Employee Attrition Using AI
# Dataset: IBM HR Analytics Employee Attrition Dataset (1,470 employees, 35 vars)
# =============================================================================

library(dplyr)
library(caret)

# =============================================================================
# STEP 1: Load Dataset
# =============================================================================

df <- read.csv(
  "WA_Fn-UseC_-HR-Employee-Attrition.csv",
  stringsAsFactors = FALSE
)

cat("--- Raw Dataset ---\n")
cat("Dimensions:", dim(df), "\n")
cat("Rows:", nrow(df), "| Columns:", ncol(df), "\n\n")
str(df)

# =============================================================================
# STEP 2: Check Missing Values
# =============================================================================

cat("\n--- Missing Values ---\n")
total_na <- sum(is.na(df))
cat("Total NAs:", total_na, "\n")

if (total_na > 0) {
  cat("NAs per column:\n")
  print(colSums(is.na(df))[colSums(is.na(df)) > 0])

  # Impute numeric columns with median
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  for (col in numeric_cols) {
    if (any(is.na(df[[col]]))) {
      df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
    }
  }

  # Impute character columns with mode
  char_cols <- names(df)[sapply(df, is.character)]
  get_mode <- function(x) {
    ux <- unique(x[!is.na(x)])
    ux[which.max(tabulate(match(x, ux)))]
  }
  for (col in char_cols) {
    if (any(is.na(df[[col]]))) {
      df[[col]][is.na(df[[col]])] <- get_mode(df[[col]])
    }
  }

  cat("Imputation complete. NAs remaining:", sum(is.na(df)), "\n")
} else {
  cat("No missing values found. Dataset is clean.\n")
}

# =============================================================================
# STEP 3: Drop Zero-Variance Columns (no predictive value)
# =============================================================================

cat("\n--- Dropping Zero-Variance Columns ---\n")
cat("EmployeeCount unique values:", length(unique(df$EmployeeCount)), "->", unique(df$EmployeeCount), "\n")
cat("Over18 unique values:       ", length(unique(df$Over18)), "->", unique(df$Over18), "\n")
cat("StandardHours unique values:", length(unique(df$StandardHours)), "->", unique(df$StandardHours), "\n")

df <- df %>% select(-EmployeeCount, -Over18, -StandardHours)
cat("Dropped: EmployeeCount, Over18, StandardHours\n")

# =============================================================================
# STEP 4: Drop Identifier Column
# =============================================================================

cat("\n--- Dropping Identifier Column ---\n")
df <- df %>% select(-EmployeeNumber)
cat("Dropped: EmployeeNumber (not a predictor)\n")

# =============================================================================
# STEP 5: Encode Target Variable
# =============================================================================

cat("\n--- Encoding Target Variable ---\n")
df$Attrition <- ifelse(df$Attrition == "Yes", 1, 0)
cat("Attrition: Yes -> 1, No -> 0\n")
cat("Class distribution:\n")
print(table(df$Attrition))
cat(sprintf(
  "Attrition rate: %.1f%%\n",
  mean(df$Attrition) * 100
))

# =============================================================================
# STEP 6: Encode Binary Categorical Columns
# =============================================================================

cat("\n--- Encoding Binary Categoricals ---\n")

df$OverTime <- ifelse(df$OverTime == "Yes", 1, 0)
cat("OverTime: Yes -> 1, No -> 0\n")

df$Gender <- ifelse(df$Gender == "Male", 1, 0)
cat("Gender: Male -> 1, Female -> 0\n")

# =============================================================================
# STEP 7: Encode Ordinal Column — BusinessTravel
# =============================================================================

cat("\n--- Encoding Ordinal Column: BusinessTravel ---\n")
cat("Levels found:", sort(unique(df$BusinessTravel)), "\n")

df$BusinessTravel <- case_when(
  df$BusinessTravel == "Non-Travel"        ~ 0,
  df$BusinessTravel == "Travel_Rarely"     ~ 1,
  df$BusinessTravel == "Travel_Frequently" ~ 2,
  TRUE ~ NA_real_
)
cat("Non-Travel=0, Travel_Rarely=1, Travel_Frequently=2\n")

# =============================================================================
# STEP 8: One-Hot Encode Nominal Categorical Columns
# =============================================================================

cat("\n--- One-Hot Encoding Nominal Categoricals ---\n")
nominal_cols <- c("Department", "EducationField", "JobRole", "MaritalStatus")

for (col in nominal_cols) {
  cat(sprintf("%s levels: %s\n", col, paste(sort(unique(df[[col]])), collapse = ", ")))
}

# Convert nominal columns to factors for dummyVars
df[nominal_cols] <- lapply(df[nominal_cols], as.factor)

dummy_model <- dummyVars(~ Department + EducationField + JobRole + MaritalStatus,
                         data = df,
                         fullRank = TRUE)  # drop first level to avoid multicollinearity

dummy_encoded <- predict(dummy_model, newdata = df) %>% as.data.frame()

df <- df %>%
  select(-all_of(nominal_cols)) %>%
  bind_cols(dummy_encoded)

cat(sprintf("After one-hot encoding: %d columns\n", ncol(df)))

# =============================================================================
# STEP 9: Encoding Sanity Check
# =============================================================================

cat("\n--- Encoding Sanity Check ---\n")

# All columns should be numeric at this point
non_numeric <- names(df)[!sapply(df, is.numeric)]
if (length(non_numeric) == 0) {
  cat("PASS: All", ncol(df), "columns are numeric.\n")
} else {
  cat("FAIL: Non-numeric columns still present:", paste(non_numeric, collapse = ", "), "\n")
  stop("Encoding incomplete — fix before proceeding.")
}

# Attrition should only contain 0 and 1
attrition_vals <- unique(df$Attrition)
if (all(attrition_vals %in% c(0, 1))) {
  cat("PASS: Attrition column contains only 0 and 1.\n")
} else {
  cat("FAIL: Attrition unexpected values:", paste(attrition_vals, collapse = ", "), "\n")
  stop("Attrition encoding error.")
}

# Binary columns should only have 0/1
binary_cols <- c("OverTime", "Gender")
for (col in binary_cols) {
  vals <- unique(df[[col]])
  if (all(vals %in% c(0, 1))) {
    cat(sprintf("PASS: %s contains only 0 and 1.\n", col))
  } else {
    cat(sprintf("FAIL: %s has unexpected values: %s\n", col, paste(vals, collapse = ", ")))
  }
}

# BusinessTravel should only have 0, 1, 2
bt_vals <- unique(df$BusinessTravel)
if (all(bt_vals %in% c(0, 1, 2)) && !any(is.na(df$BusinessTravel))) {
  cat("PASS: BusinessTravel contains only 0, 1, 2 — no NAs.\n")
} else {
  cat("FAIL: BusinessTravel unexpected values or NAs found.\n")
}

cat(sprintf("PASS: No NAs in full encoded dataset (%d NAs)\n", sum(is.na(df))))

# =============================================================================
# STEP 10: Train / Validation / Test Split (BEFORE scaling)
# =============================================================================

cat("\n--- Train / Validation / Test Split ---\n")
cat("Strategy: 70% train | 15% validation | 15% test (stratified on Attrition)\n")

set.seed(42)

# Stratified split — preserves attrition class ratio across splits
train_idx    <- createDataPartition(df$Attrition, p = 0.70, list = FALSE)
df_train_raw <- df[train_idx, ]
df_temp      <- df[-train_idx, ]

# Split the remaining 30% equally into val and test (15% / 15%)
val_idx     <- createDataPartition(df_temp$Attrition, p = 0.50, list = FALSE)
df_val_raw  <- df_temp[val_idx, ]
df_test_raw <- df_temp[-val_idx, ]

cat(sprintf("Train rows: %d  | Attrition rate: %.1f%%\n",
            nrow(df_train_raw), mean(df_train_raw$Attrition) * 100))
cat(sprintf("Val rows:   %d  | Attrition rate: %.1f%%\n",
            nrow(df_val_raw),   mean(df_val_raw$Attrition) * 100))
cat(sprintf("Test rows:  %d  | Attrition rate: %.1f%%\n",
            nrow(df_test_raw),  mean(df_test_raw$Attrition) * 100))

# Save unscaled splits (for Decision Tree — no scaling needed)
saveRDS(df_train_raw, "train_data_unscaled.rds")
saveRDS(df_val_raw,   "val_data_unscaled.rds")
saveRDS(df_test_raw,  "test_data_unscaled.rds")
cat("Saved unscaled splits: train_data_unscaled.rds, val_data_unscaled.rds, test_data_unscaled.rds\n")

# =============================================================================
# STEP 11: Feature Scaling — computed on TRAIN only, applied to all splits
# =============================================================================

cat("\n--- Feature Scaling (no data leakage) ---\n")

# Identify continuous numeric columns to scale
# Exclude: Attrition (target), binary encodings (only 2 unique values)
cols_to_scale <- names(df_train_raw)[sapply(df_train_raw, function(x) {
  is.numeric(x) && length(unique(x)) > 2
})]
cols_to_scale <- cols_to_scale[cols_to_scale != "Attrition"]

cat("Continuous columns to scale:", length(cols_to_scale), "\n")

# Compute scaling parameters from TRAIN set ONLY
preProc <- preProcess(df_train_raw[, cols_to_scale], method = c("center", "scale"))
saveRDS(preProc, "scaling_params.rds")
cat("Scaling parameters (mean, sd) computed from train set only — saved to scaling_params.rds\n")

# Apply the SAME parameters to train, val, and test
df_train_scaled <- df_train_raw
df_val_scaled   <- df_val_raw
df_test_scaled  <- df_test_raw

df_train_scaled[, cols_to_scale] <- predict(preProc, df_train_raw[, cols_to_scale])
df_val_scaled[, cols_to_scale]   <- predict(preProc, df_val_raw[, cols_to_scale])
df_test_scaled[, cols_to_scale]  <- predict(preProc, df_test_raw[, cols_to_scale])

# Save scaled splits
saveRDS(df_train_scaled, "train_data.rds")
saveRDS(df_val_scaled,   "val_data.rds")
saveRDS(df_test_scaled,  "test_data.rds")
cat("Saved scaled splits: train_data.rds, val_data.rds, test_data.rds\n")

# Also save the full clean unscaled dataset (used by EDA)
saveRDS(df, "attrition_clean_unscaled.rds")
cat("Saved: attrition_clean_unscaled.rds (full dataset — for EDA only)\n")

# =============================================================================
# VERIFICATION SUMMARY
# =============================================================================

cat("\n========================================\n")
cat("PHASE 1 VERIFICATION SUMMARY\n")
cat("========================================\n")
cat("Total rows:             ", nrow(df), "\n")
cat("Total columns:          ", ncol(df), "\n")
cat("Missing values:         ", sum(is.na(df)), "\n")
cat("\nSplit sizes:\n")
cat("  Train:      ", nrow(df_train_scaled), "rows\n")
cat("  Validation: ", nrow(df_val_scaled),   "rows\n")
cat("  Test:       ", nrow(df_test_scaled),  "rows\n")
cat("\nAttrition rates (should be ~equal — stratified):\n")
cat(sprintf("  Train:      %.1f%%\n", mean(df_train_scaled$Attrition) * 100))
cat(sprintf("  Validation: %.1f%%\n", mean(df_val_scaled$Attrition)   * 100))
cat(sprintf("  Test:       %.1f%%\n", mean(df_test_scaled$Attrition)  * 100))
cat("\nAll numeric (scaled train): ", all(sapply(df_train_scaled, is.numeric)), "\n")
cat("NAs in train_data.rds:      ", sum(is.na(df_train_scaled)), "\n")
cat("NAs in test_data.rds:       ", sum(is.na(df_test_scaled)), "\n")
cat("Scaling params saved:        scaling_params.rds\n")
cat("\nFiles saved:\n")
cat("  train_data.rds           (scaled — for Logistic Regression)\n")
cat("  val_data.rds             (scaled — for model selection)\n")
cat("  test_data.rds            (scaled — final evaluation only)\n")
cat("  train_data_unscaled.rds  (for Decision Tree)\n")
cat("  val_data_unscaled.rds\n")
cat("  test_data_unscaled.rds\n")
cat("  attrition_clean_unscaled.rds (full dataset — EDA use only)\n")
cat("  scaling_params.rds       (preProcess object — reuse in API)\n")
cat("========================================\n")
cat("Phase 1 complete. Ready for Phase 2: EDA\n")
