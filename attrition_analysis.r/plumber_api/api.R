# =============================================================================
# Phase 7: Plumber REST API
# Model: Logistic Regression (threshold = 0.26)
# Endpoints: GET /health, POST /predict
# =============================================================================

library(plumber)
library(dplyr)
library(caret)

# =============================================================================
# Load model and scaling params once at startup
# =============================================================================

# Resolve paths relative to this file's directory (works locally and in Docker)
BASE_DIR    <- Sys.getenv("API_DIR", unset = getwd())
MODEL_PATH  <- file.path(BASE_DIR, "model_final.rds")
SCALER_PATH <- file.path(BASE_DIR, "scaling_params.rds")

final_obj    <- readRDS(MODEL_PATH)
final_model  <- final_obj$model
THRESHOLD    <- final_obj$threshold
scaling_params <- readRDS(SCALER_PATH)

# Columns scaled during training (from 03_feature_engineering.R)
SCALED_COLS <- names(scaling_params$mean)

# Exact dummy column names produced by caret dummyVars (fullRank = TRUE)
# Reference: alphabetical factor levels, first level dropped per variable
DEPT_REF       <- "Human Resources"           # dropped
EDU_REF        <- "Human Resources"           # dropped
ROLE_REF       <- "Healthcare Representative" # dropped
MARITAL_REF    <- "Divorced"                  # dropped

DEPT_LEVELS    <- c("Human Resources", "Research & Development", "Sales")
EDU_LEVELS     <- c("Human Resources", "Life Sciences", "Marketing",
                    "Medical", "Other", "Technical Degree")
ROLE_LEVELS    <- c("Healthcare Representative", "Human Resources",
                    "Laboratory Technician", "Manager",
                    "Manufacturing Director", "Research Director",
                    "Research Scientist", "Sales Executive",
                    "Sales Representative")
MARITAL_LEVELS <- c("Divorced", "Married", "Single")

# =============================================================================
# Preprocessing function — mirrors 01_data_preparation.R + 03_feature_engineering.R
# =============================================================================

preprocess_input <- function(emp) {

  # ── 1. Binary encodings ────────────────────────────────────────────────────
  emp$OverTime       <- ifelse(emp$OverTime == "Yes", 1L, 0L)
  emp$Gender         <- ifelse(emp$Gender   == "Male", 1L, 0L)
  emp$BusinessTravel <- dplyr::case_when(
    emp$BusinessTravel == "Non-Travel"        ~ 0L,
    emp$BusinessTravel == "Travel_Rarely"     ~ 1L,
    emp$BusinessTravel == "Travel_Frequently" ~ 2L,
    TRUE ~ NA_integer_
  )

  # ── 2. Feature engineering ─────────────────────────────────────────────────
  emp$TenureGroup <- dplyr::case_when(
    emp$YearsAtCompany <= 3  ~ 0L,
    emp$YearsAtCompany <= 10 ~ 1L,
    TRUE                     ~ 2L
  )
  emp$SatisfactionIndex    <- (emp$JobSatisfaction +
                                emp$EnvironmentSatisfaction +
                                emp$RelationshipSatisfaction) / 3
  emp$IncomePerTenureYear  <- emp$MonthlyIncome / (emp$TotalWorkingYears + 1)

  # ── 3. One-hot encoding (fullRank = TRUE, mirrors training) ────────────────
  dept    <- emp$Department
  edu     <- emp$EducationField
  role    <- emp$JobRole
  marital <- emp$MaritalStatus

  for (lvl in setdiff(DEPT_LEVELS, DEPT_REF)) {
    col_name <- paste0("Department.", lvl)
    emp[[col_name]] <- as.integer(dept == lvl)
  }
  for (lvl in setdiff(EDU_LEVELS, EDU_REF)) {
    col_name <- paste0("EducationField.", lvl)
    emp[[col_name]] <- as.integer(edu == lvl)
  }
  for (lvl in setdiff(ROLE_LEVELS, ROLE_REF)) {
    col_name <- paste0("JobRole.", lvl)
    emp[[col_name]] <- as.integer(role == lvl)
  }
  for (lvl in setdiff(MARITAL_LEVELS, MARITAL_REF)) {
    col_name <- paste0("MaritalStatus.", lvl)
    emp[[col_name]] <- as.integer(marital == lvl)
  }

  # Drop original nominal columns
  emp <- emp %>% select(-Department, -EducationField, -JobRole, -MaritalStatus)

  # ── 4. Apply z-score scaling (train parameters only) ──────────────────────
  cols_present <- intersect(SCALED_COLS, names(emp))
  emp[cols_present] <- predict(scaling_params, newdata = emp)[cols_present]

  emp
}

# Columns removed during Phase 7+8 multicollinearity rebuild
DROPPED_COLS <- c(
  "SatisfactionIndex", "JobSatisfaction",
  "EnvironmentSatisfaction", "RelationshipSatisfaction",
  "Department.Research & Development", "Department.Sales",
  "Department.Human Resources",
  "JobRole.Human Resources", "JobRole.Sales Executive",
  "JobRole.Sales Representative", "JobRole.Laboratory Technician",
  "JobRole.Research Scientist",
  "JobLevel",
  "TotalWorkingYears"   # Phase 8: redundant with YearsAtCompany; dropping improves AUC +0.004
)

# Validate all expected model features are present
EXPECTED_FEATURES <- setdiff(names(final_model$model), "Attrition")

# =============================================================================
# ── GET /health ───────────────────────────────────────────────────────────────
#* @get /health
#* @serializer unboxedJSON
function() {
  list(
    status    = "ok",
    model     = "Logistic Regression",
    threshold = THRESHOLD,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

# =============================================================================
# ── POST /predict ─────────────────────────────────────────────────────────────
#* @post /predict
#* @serializer unboxedJSON
#* @param req The HTTP request body (JSON employee record)
function(req) {

  # Parse body
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody),
    error = function(e) NULL
  )

  if (is.null(body)) {
    return(list(error = "Invalid JSON in request body"))
  }

  # Required raw fields
  required <- c(
    "Age", "BusinessTravel", "DailyRate", "Department",
    "DistanceFromHome", "Education", "EducationField",
    "EnvironmentSatisfaction", "Gender", "HourlyRate",
    "JobInvolvement", "JobLevel", "JobRole", "JobSatisfaction",
    "MaritalStatus", "MonthlyIncome", "MonthlyRate",
    "NumCompaniesWorked", "OverTime", "PercentSalaryHike",
    "PerformanceRating", "RelationshipSatisfaction",
    "StockOptionLevel", "TotalWorkingYears",
    "TrainingTimesLastYear", "WorkLifeBalance",
    "YearsAtCompany", "YearsInCurrentRole",
    "YearsSinceLastPromotion", "YearsWithCurrManager"
  )

  missing_fields <- setdiff(required, names(body))
  if (length(missing_fields) > 0) {
    return(list(
      error          = "Missing required fields",
      missing_fields = missing_fields
    ))
  }

  # Build single-row data frame
  emp <- tryCatch(
    as.data.frame(body, stringsAsFactors = FALSE),
    error = function(e) NULL
  )

  if (is.null(emp)) {
    return(list(error = "Could not parse employee fields into data frame"))
  }

  # Preprocess
  emp_processed <- tryCatch(
    preprocess_input(emp),
    error = function(e) {
      list(.__error__ = conditionMessage(e))
    }
  )

  if (!is.null(emp_processed$.__error__)) {
    return(list(error = emp_processed$.__error__))
  }

  # Drop multicollinear columns (Phase 7 rebuild)
  cols_to_drop <- intersect(DROPPED_COLS, names(emp_processed))
  if (length(cols_to_drop) > 0) {
    emp_processed <- emp_processed %>% select(-all_of(cols_to_drop))
  }

  # Predict
  prob <- tryCatch(
    as.numeric(predict(final_model, newdata = emp_processed, type = "response")),
    error = function(e) NULL
  )

  if (is.null(prob)) {
    return(list(error = "Prediction failed — check input values"))
  }

  prediction <- ifelse(prob >= THRESHOLD, "Yes", "No")

  list(
    prediction  = prediction,
    probability = round(prob, 4),
    threshold   = THRESHOLD,
    risk_level  = dplyr::case_when(
      prob >= 0.70 ~ "High",
      prob >= 0.40 ~ "Medium",
      TRUE         ~ "Low"
    )
  )
}

# =============================================================================
# ── GET /insights ─────────────────────────────────────────────────────────────
#* @get /insights
#* @serializer unboxedJSON
function() {
  report_path <- file.path(BASE_DIR, "insights_report.json")
  if (!file.exists(report_path)) {
    return(list(error = "insights_report.json not found — run 08_reporting.R first"))
  }
  jsonlite::fromJSON(report_path, simplifyVector = FALSE)
}

# =============================================================================
# ── GET /feature_importance ───────────────────────────────────────────────────
#* @get /feature_importance
#* @serializer unboxedJSON
function() {
  fi_path <- file.path(BASE_DIR, "feature_importance.json")
  if (!file.exists(fi_path)) {
    return(list(error = "feature_importance.json not found — run 12_feature_importance.R"))
  }
  jsonlite::fromJSON(fi_path, simplifyVector = FALSE)
}

# =============================================================================
# ── GET /shap_metadata ────────────────────────────────────────────────────────
#* @get /shap_metadata
#* @serializer unboxedJSON
function() {
  shap_path <- file.path(BASE_DIR, "shap_metadata.json")
  if (!file.exists(shap_path)) {
    return(list(error = "shap_metadata.json not found — run 12b_export_shap_metadata.R"))
  }
  jsonlite::fromJSON(shap_path, simplifyVector = FALSE)
}

# =============================================================================
# ── POST /shap ────────────────────────────────────────────────────────────────
# Returns per-feature LinearSHAP values for a single employee.
# Uses the same preprocessing as POST /predict, then computes
# shap_i = beta_i * (x_i - mean_i) / denom_i (consistent with the
# frontend computeShap() function).
#* @post /shap
#* @serializer unboxedJSON
#* @param req The HTTP request body (JSON employee record)
function(req) {

  shap_path <- file.path(BASE_DIR, "shap_metadata.json")
  if (!file.exists(shap_path)) {
    return(list(error = "shap_metadata.json not found"))
  }
  shap_meta <- jsonlite::fromJSON(shap_path)

  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)
  if (is.null(body)) return(list(error = "Invalid JSON in request body"))

  required <- c(
    "Age", "BusinessTravel", "DailyRate", "Department",
    "DistanceFromHome", "Education", "EducationField",
    "EnvironmentSatisfaction", "Gender", "HourlyRate",
    "JobInvolvement", "JobLevel", "JobRole", "JobSatisfaction",
    "MaritalStatus", "MonthlyIncome", "MonthlyRate",
    "NumCompaniesWorked", "OverTime", "PercentSalaryHike",
    "PerformanceRating", "RelationshipSatisfaction",
    "StockOptionLevel", "TotalWorkingYears",
    "TrainingTimesLastYear", "WorkLifeBalance",
    "YearsAtCompany", "YearsInCurrentRole",
    "YearsSinceLastPromotion", "YearsWithCurrManager"
  )

  missing_fields <- setdiff(required, names(body))
  if (length(missing_fields) > 0) {
    return(list(
      error          = "Missing required fields",
      missing_fields = paste(missing_fields, collapse = ", ")
    ))
  }

  emp <- tryCatch(as.data.frame(body, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(emp)) return(list(error = "Could not parse employee fields"))

  emp_p <- tryCatch(
    preprocess_input(emp),
    error = function(e) list(.__error__ = conditionMessage(e))
  )
  if (!is.null(emp_p$.__error__)) return(list(error = emp_p$.__error__))

  cols_to_drop <- intersect(DROPPED_COLS, names(emp_p))
  if (length(cols_to_drop) > 0) emp_p <- emp_p %>% select(-all_of(cols_to_drop))

  prob <- tryCatch(
    as.numeric(predict(final_model, newdata = emp_p, type = "response")),
    error = function(e) NULL
  )
  if (is.null(prob)) return(list(error = "Prediction failed"))

  # Compute LinearSHAP using feature specs from shap_metadata.json
  # Formula: shap_i = beta_i * (x_i - mean_i) / denom_i
  #          where denom = 2*sd for binary features, sd otherwise
  feat_df <- shap_meta$features  # data.frame: name, beta, mean, sd, is_binary, direction

  shap_vals <- lapply(seq_len(nrow(feat_df)), function(i) {
    feat     <- feat_df[i, ]
    x        <- if (feat$name %in% names(emp_p)) as.numeric(emp_p[[feat$name]]) else 0
    denom    <- if (isTRUE(feat$is_binary)) 2 * feat$sd else feat$sd
    x_scaled <- if (denom > 0) (x - feat$mean) / denom else 0
    list(
      feature   = feat$name,
      impact    = round(feat$beta * x_scaled, 6),
      raw_value = round(x, 4),
      direction = feat$direction
    )
  })

  # Sort by |impact| descending
  shap_vals <- shap_vals[order(sapply(shap_vals, function(v) abs(v$impact)), decreasing = TRUE)]

  list(
    risk_score    = round(prob, 4),
    risk_label    = dplyr::case_when(
      prob >= 0.70 ~ "High",
      prob >= 0.40 ~ "Medium",
      TRUE         ~ "Low"
    ),
    baseline_prob = shap_meta$baseline_prob,
    intercept     = shap_meta$intercept,
    shap_values   = shap_vals
  )
}
