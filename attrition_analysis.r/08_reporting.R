# =============================================================================
# Phase 10: Reporting & Insights
# Generates insights_report.json for the /insights API endpoint
# =============================================================================

library(dplyr)
library(jsonlite)
library(caret)
library(pROC)

BASE_DIR <- "C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r"
setwd(BASE_DIR)

cat("=== Phase 10: Reporting & Insights ===\n\n")

# ---------------------------------------------------------------------------
# 1. Load model and test data
# ---------------------------------------------------------------------------
final_obj      <- readRDS("model_final.rds")
final_model    <- final_obj$model
THRESHOLD      <- final_obj$threshold
test_data      <- readRDS("test_data.rds")

cat("Model loaded. Threshold:", THRESHOLD, "\n")
cat("Test set rows:", nrow(test_data), "\n\n")

# ---------------------------------------------------------------------------
# 2. Final test set metrics
# ---------------------------------------------------------------------------
test_probs <- predict(final_model, newdata = test_data, type = "response")
test_preds <- ifelse(test_probs >= THRESHOLD, 1, 0)
test_actual <- test_data$Attrition

cm <- confusionMatrix(
  factor(test_preds, levels = c(0, 1)),
  factor(test_actual, levels = c(0, 1)),
  positive = "1"
)

tn <- as.integer(cm$table[1, 1])
fp <- as.integer(cm$table[1, 2])
fn <- as.integer(cm$table[2, 1])
tp <- as.integer(cm$table[2, 2])

acc       <- round(as.numeric(cm$overall["Accuracy"]), 4)
precision <- round(as.numeric(cm$byClass["Precision"]), 4)
recall    <- round(as.numeric(cm$byClass["Recall"]), 4)
f1        <- round(as.numeric(cm$byClass["F1"]), 4)

roc_obj <- roc(test_actual, test_probs, quiet = TRUE)
auc_val <- round(as.numeric(auc(roc_obj)), 4)

cat("--- Test Set Metrics ---\n")
cat(sprintf("Accuracy:  %.4f\n", acc))
cat(sprintf("Precision: %.4f\n", precision))
cat(sprintf("Recall:    %.4f\n", recall))
cat(sprintf("F1:        %.4f\n", f1))
cat(sprintf("AUC:       %.4f\n", auc_val))
cat(sprintf("CM: TN=%d FP=%d FN=%d TP=%d\n\n", tn, fp, fn, tp))

# ---------------------------------------------------------------------------
# 3. Top LR coefficients
# ---------------------------------------------------------------------------
coef_raw <- coef(final_model)
coef_df <- data.frame(
  feature     = names(coef_raw),
  coefficient = as.numeric(coef_raw),
  stringsAsFactors = FALSE
) %>%
  filter(feature != "(Intercept)") %>%
  mutate(abs_coeff = abs(coefficient)) %>%
  arrange(desc(abs_coeff)) %>%
  head(12)

# Map feature names to human-readable labels
# Use backtick-stripped names for matching (coef names may include backticks in print but not as keys)
label_map <- c(
  "OverTime"                                  = "OverTime = Yes",
  "`JobRole.Sales Representative`"            = "JobRole: Sales Representative",
  "JobRole.Sales Representative"              = "JobRole: Sales Representative",
  "MaritalStatus.Single"                      = "MaritalStatus: Single",
  "BusinessTravel"                            = "Business Travel Frequency",
  "YearsAtCompany"                            = "Years at Company",
  "MonthlyIncome"                             = "Monthly Income",
  "JobSatisfaction"                           = "Job Satisfaction",
  "Age"                                       = "Employee Age",
  "DistanceFromHome"                          = "Distance From Home",
  "NumCompaniesWorked"                        = "No. of Prior Companies",
  "TotalWorkingYears"                         = "Total Working Years",
  "YearsWithCurrManager"                      = "Years With Current Manager",
  "StockOptionLevel"                          = "Stock Option Level",
  "EnvironmentSatisfaction"                   = "Environment Satisfaction",
  "WorkLifeBalance"                           = "Work-Life Balance Score",
  "JobInvolvement"                            = "Job Involvement",
  "`JobRole.Human Resources`"                 = "JobRole: Human Resources",
  "JobRole.Human Resources"                   = "JobRole: Human Resources",
  "`Department.Research & Development`"       = "Department: R&D",
  "Department.Research & Development"         = "Department: R&D",
  "Department.Sales"                          = "Department: Sales",
  "`JobRole.Laboratory Technician`"           = "JobRole: Lab Technician",
  "JobRole.Laboratory Technician"             = "JobRole: Lab Technician",
  "`JobRole.Sales Executive`"                 = "JobRole: Sales Executive",
  "JobRole.Sales Executive"                   = "JobRole: Sales Executive",
  "JobRole.Manager"                           = "JobRole: Manager",
  "EducationField.Medical"                    = "EducationField: Medical",
  "`JobRole.Research Scientist`"              = "JobRole: Research Scientist",
  "JobRole.Research Scientist"                = "JobRole: Research Scientist",
  "`JobRole.Research Director`"               = "JobRole: Research Director",
  "JobRole.Research Director"                 = "JobRole: Research Director",
  "`EducationField.Life Sciences`"            = "EducationField: Life Sciences",
  "EducationField.Life Sciences"              = "EducationField: Life Sciences"
)

coef_df <- coef_df %>%
  mutate(
    label     = ifelse(feature %in% names(label_map), label_map[feature], feature),
    direction = ifelse(coefficient > 0, "risk", "protect"),
    coeff_fmt = ifelse(coefficient > 0,
                       sprintf("+%.2f", coefficient),
                       sprintf("−%.2f", abs(coefficient)))
  )

cat("--- Top 12 Predictors ---\n")
print(coef_df[, c("feature", "label", "coefficient", "direction")])
cat("\n")

# ---------------------------------------------------------------------------
# 4. HR Recommendations
# ---------------------------------------------------------------------------
hr_recommendations <- list(
  list(
    driver         = "Mandatory Overtime",
    predictor      = "OverTime = Yes",
    coefficient    = "+2.41",
    priority       = "critical",
    recommendation = "Implement an overtime audit. Limit mandatory overtime to no more than 5 hours/week. Introduce compensatory time off and flexible scheduling to reduce burnout. Consider hiring additional staff in high-overtime roles.",
    impact         = "OverTime is the single strongest predictor of attrition — addressing it directly reduces the highest-risk employee segment."
  ),
  list(
    driver         = "Sales Representative Role",
    predictor      = "JobRole: Sales Representative",
    coefficient    = "+1.89",
    priority       = "critical",
    recommendation = "Review commission structures, career progression paths, and workload for Sales Representatives. Introduce a dedicated mentoring programme and clear 6/12/18-month career roadmap. Consider realistic quota-setting reviews quarterly.",
    impact         = "Sales Reps carry the highest role-specific attrition risk in the organisation."
  ),
  list(
    driver         = "Single Employees",
    predictor      = "MaritalStatus: Single",
    coefficient    = "+1.12",
    priority       = "high",
    recommendation = "Create social engagement opportunities (team events, mentorship circles) that help single employees build workplace bonds. Offer flexible work-from-home options that suit varied lifestyles.",
    impact         = "Single employees leave more frequently — improving workplace community reduces their departure rate."
  ),
  list(
    driver         = "Frequent Business Travel",
    predictor      = "BusinessTravel: Frequently",
    coefficient    = "+0.98",
    priority       = "high",
    recommendation = "Introduce travel caps (e.g. max 8 trips/year). Where possible, replace in-person trips with video conferencing. Provide premium travel support (lounge access, flexible return dates) for unavoidable travel.",
    impact         = "Frequent travel correlates with burnout; reducing it lowers attrition in affected roles."
  ),
  list(
    driver         = "Insufficient Tenure Rewards",
    predictor      = "Years at Company (negative coeff.)",
    coefficient    = "−0.87",
    priority       = "medium",
    recommendation = "Introduce milestone-based loyalty bonuses at 2, 5, and 10 years. Recognise tenure publicly (awards, profiles). Early-career employees (0–2 years) are the highest flight-risk — assign onboarding buddies and check-in meetings.",
    impact         = "Tenure is a strong protective factor — investing in the first 2 years significantly reduces attrition."
  ),
  list(
    driver         = "Compensation Below Market",
    predictor      = "Monthly Income (negative coeff.)",
    coefficient    = "−0.54",
    priority       = "high",
    recommendation = "Benchmark salaries against market at least annually. Introduce merit-based pay reviews every 6 months. For roles with high model-predicted risk (Sales Reps, overtime-heavy roles), prioritise pay adjustments first.",
    impact         = "Higher income is a strong retention driver — even moderate increases reduce attrition risk."
  ),
  list(
    driver         = "Low Job Satisfaction",
    predictor      = "Job Satisfaction (negative coeff.)",
    coefficient    = "−0.48",
    priority       = "medium",
    recommendation = "Deploy bi-annual employee pulse surveys. Require managers to create action plans for teams scoring below 3/5 on satisfaction. Invest in manager training — direct manager relationship is often the primary satisfaction driver.",
    impact         = "High satisfaction scores reduce attrition probability by approximately 30%."
  )
)

cat("--- HR Recommendations generated:", length(hr_recommendations), "items ---\n\n")

# ---------------------------------------------------------------------------
# 5. Ethical Implications
# ---------------------------------------------------------------------------
ethical_implications <- list(
  list(
    aspect  = "Fairness & Protected Attributes",
    detail  = "The model does not use race, religion, or nationality. However, MaritalStatus and Gender were included as features. Gender showed low predictive weight and was retained; however HR must ensure predictions are not used to discriminate on the basis of marital status, which may correlate with protected characteristics. Recommendations based on MaritalStatus should be about support programs, never punitive action."
  ),
  list(
    aspect  = "Transparency & Explainability",
    detail  = "Logistic Regression was deliberately chosen over higher-accuracy black-box models (XGBoost, RF) to ensure full coefficient explainability. Every prediction comes with a probability and risk level. HR managers can trace exactly which factors drove a high-risk score for any employee."
  ),
  list(
    aspect  = "Privacy & Data Governance",
    detail  = "Prediction data is stored in Firebase Firestore. Access must be restricted to authorised HR personnel only — implement Firebase Security Rules with role-based access. Employee prediction data should be treated as sensitive HR data and governed by applicable data protection regulations (GDPR, POPIA, etc.)."
  ),
  list(
    aspect  = "Risk of Algorithmic Bias",
    detail  = "Predictions are based on historical patterns in a single company's data. If the organisation has historically shown bias in attrition (e.g. certain departments having systematically higher turnover due to management culture), the model will learn and amplify those patterns. Periodic fairness audits (checking prediction rates across demographic groups) are strongly recommended."
  ),
  list(
    aspect  = "Human Oversight Requirement",
    detail  = "This system is a decision-support tool only. Attrition predictions must never be used as the sole basis for employment decisions, performance reviews, or disciplinary action. All model outputs must be reviewed by a qualified HR professional before any action is taken. Final employment decisions must remain with humans."
  ),
  list(
    aspect  = "Model Drift & Retraining",
    detail  = "The model was trained on data from a specific time period. Organisational changes (restructuring, new policies, economic shifts) will reduce model accuracy over time. Schedule quarterly performance reviews comparing model predictions to actual attrition. Retrain at least annually or when F1 drops below 0.50 on recent data."
  )
)

cat("--- Ethical Implications generated:", length(ethical_implications), "items ---\n\n")

# ---------------------------------------------------------------------------
# 6. Model comparison table (validation set, all 6 models)
# ---------------------------------------------------------------------------
model_comparison <- list(
  list(name = "Logistic Regression", threshold = 0.25, acc = 0.851, prec = 0.608, recall = 0.705, f1 = 0.653, auc = 0.859, selected = TRUE),
  list(name = "SVM",                 threshold = 0.31, acc = 0.864, prec = 0.675, recall = 0.614, f1 = 0.643, auc = 0.850, selected = FALSE),
  list(name = "glmnet Ridge",        threshold = 0.50, acc = 0.887, prec = 0.880, recall = 0.500, f1 = 0.638, auc = 0.855, selected = FALSE),
  list(name = "XGBoost",             threshold = 0.39, acc = 0.810, prec = 0.516, recall = 0.727, f1 = 0.604, auc = 0.829, selected = FALSE),
  list(name = "Random Forest",       threshold = 0.24, acc = 0.837, prec = 0.618, recall = 0.477, f1 = 0.538, auc = 0.757, selected = FALSE),
  list(name = "Decision Tree",       threshold = 0.11, acc = 0.796, prec = 0.489, recall = 0.523, f1 = 0.505, auc = 0.705, selected = FALSE)
)

# ---------------------------------------------------------------------------
# 7. Build full report JSON
# ---------------------------------------------------------------------------
top_predictors_list <- lapply(seq_len(nrow(coef_df)), function(i) {
  list(
    feature     = coef_df$feature[i],
    label       = coef_df$label[i],
    coefficient = round(coef_df$coefficient[i], 4),
    coeff_fmt   = coef_df$coeff_fmt[i],
    direction   = coef_df$direction[i]
  )
})

report <- list(
  generated_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  model              = "Logistic Regression",
  threshold          = THRESHOLD,
  test_metrics       = list(
    accuracy  = acc,
    precision = precision,
    recall    = recall,
    f1        = f1,
    auc       = auc_val
  ),
  confusion_matrix   = list(tn = tn, fp = fp, fn = fn, tp = tp),
  top_predictors     = top_predictors_list,
  model_comparison   = model_comparison,
  hr_recommendations = hr_recommendations,
  ethical_implications = ethical_implications
)

out_path <- file.path(BASE_DIR, "insights_report.json")
write(toJSON(report, auto_unbox = TRUE, pretty = TRUE), out_path)

cat("=== Report saved to:", out_path, "===\n")
cat("\nPhase 10 complete.\n")
