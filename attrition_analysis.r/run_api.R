# =============================================================================
# Run Plumber API
# Usage: Rscript run_api.R
# Server: http://localhost:8000
# =============================================================================

library(plumber)

setwd("C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r")

pr <- plumb("plumber_api/api.R")

cat("==============================================\n")
cat("  Attrition Prediction API\n")
cat("  http://localhost:8000\n")
cat("  GET  /health\n")
cat("  POST /predict\n")
cat("==============================================\n\n")

pr$run(host = "0.0.0.0", port = 8000, docs = TRUE)
