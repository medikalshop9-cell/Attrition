# =============================================================================
# deploy.R — Deploy the Plumber API to shinyapps.io
# Run this script from: C:\Users\ayhin\Desktop\Attrition\attrition_analysis.r
# =============================================================================

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}
library(rsconnect)

# 1. Authenticate with shinyapps.io
rsconnect::setAccountInfo(
  name   = "attrition",
  token  = "REDACTED_TOKEN",
  secret = "REDACTED_SECRET"
)

# 2. Deploy the plumber_api folder
#    - api.R is the entry point
#    - model_final.rds, scaling_params.rds, insights_report.json are bundled alongside
APP_DIR <- file.path(
  "C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r/plumber_api"
)

rsconnect::deployApp(
  appDir         = APP_DIR,
  appPrimaryDoc  = "api.R",
  appName        = "plumber_api",
  account        = "attrition",
  launch.browser = FALSE
)

cat("\n✅ API deployed!\n")
cat("   URL: https://attrition.shinyapps.io/plumber_api\n")
cat("\nNext step: build and deploy the React frontend.\n")
cat("   Frontend points to: https://attrition.shinyapps.io/plumber_api\n")
