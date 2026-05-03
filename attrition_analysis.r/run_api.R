# =============================================================================
# Run Plumber API  (local development)
# Usage: Rscript run_api.R
# Server: http://localhost:8000
# =============================================================================

library(plumber)

setwd("C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r/plumber_api")

# plumber.R returns the configured router (CORS + routes).
# Same entry point as shinyapps.io — guarantees identical behaviour locally.
pr <- source("plumber.R")$value

cat("==============================================\n")
cat("  Attrition Prediction API\n")
cat("  http://localhost:8000\n")
cat("  GET  /health\n")
cat("  POST /predict\n")
cat("  POST /shap\n")
cat("==============================================\n\n")

pr$run(host = "0.0.0.0", port = 8000, docs = TRUE)
