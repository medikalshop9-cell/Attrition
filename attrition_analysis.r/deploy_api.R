# =============================================================================
# deploy_api.R — one-click deploy to shinyapps.io
#
# Option A — Positron / terminal (from project root):
#   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "attrition_analysis.r/deploy_api.R"
#
# Option B — Positron R console (open the file, then in the console):
#   source("attrition_analysis.r/deploy_api.R")
# =============================================================================

library(rsconnect)

# ── 1. Account credentials ────────────────────────────────────────────────────
#    Already stored from the first deployment, but safe to re-set each time.
rsconnect::setAccountInfo(
  name   = "attrition",
  token  = "89FB35F9A93F841C886D25D0741C6AED",
  secret = "9QQR3XcRByBZ2xwfmb4DvgoQ+rS36bMq2zuiV5jW"
)

# ── 2. App directory — everything shinyapps.io needs ─────────────────────────
API_DIR <- normalizePath(
  "C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r/plumber_api",
  mustWork = TRUE
)

cat("Deploying from:", API_DIR, "\n")

# Verify required files exist before uploading
required_files <- c("plumber.R", "api.R", "model_final.rds", "scaling_params.rds",
                     "shap_metadata.json", "feature_importance.json",
                     "insights_report.json")
missing <- required_files[!file.exists(file.path(API_DIR, required_files))]
if (length(missing)) {
  stop("Missing files in plumber_api/: ", paste(missing, collapse = ", "))
}

# ── 3. Deploy ─────────────────────────────────────────────────────────────────
rsconnect::deployAPI(
  api        = API_DIR,
  appName    = "plumber_api",
  account    = "attrition",
  server     = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = FALSE
)

cat("\n✓ Deployed → https://attrition.shinyapps.io/plumber_api/\n")
cat("  Test: curl https://attrition.shinyapps.io/plumber_api/health\n\n")
cat("  Reminder: set ALLOWED_ORIGIN in the shinyapps.io dashboard\n")
cat("  App → Settings → Environment variables → ALLOWED_ORIGIN=https://your-app.netlify.app\n")
