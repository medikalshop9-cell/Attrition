# =============================================================================
# app.R — shinyapps.io entry point for the Attrition Plumber API
#
# This file is the composition layer.  It:
#   1. Builds the plumber router from api.R (pure route definitions)
#   2. Attaches a CORS filter as the FIRST filter so it runs before every route
#   3. Returns the router — shinyapps.io calls $run() automatically.
#
# Deploy:
#   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "attrition_analysis.r/deploy_api.R"
# Local test (identical environment):
#   Rscript -e "pr <- source('app.R')[['value']]; pr$run(port = 8000)"
# =============================================================================

library(plumber)

# ---------------------------------------------------------------------------
# Hardcoded allowed origins — no .Renviron / env-var dependency
# ---------------------------------------------------------------------------
ALLOWED_ORIGINS <- c(
  "https://friendly-monstera-389b24.netlify.app",
  "http://localhost:5173",   # Vite dev server
  "http://localhost:3000"    # CRA / other local dev
)

# ---------------------------------------------------------------------------
# CORS filter — registered first via pr_filter() so it runs before all routes.
# Echoes the exact request origin back only if it is in the allowlist.
# Handles OPTIONS pre-flight automatically.
# ---------------------------------------------------------------------------
cors_filter <- function(req, res) {
  origin <- if (!is.null(req$HTTP_ORIGIN)) req$HTTP_ORIGIN else ""

  if (origin %in% ALLOWED_ORIGINS) {
    res$setHeader("Access-Control-Allow-Origin",  origin)
    res$setHeader("Vary",                         "Origin")
  } else if (nchar(origin) == 0) {
    # Non-browser call (curl, Postman, R) — no Origin header sent
    res$setHeader("Access-Control-Allow-Origin",  "*")
  }
  # Unknown browser origins: header intentionally omitted → browser blocks it

  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  res$setHeader("Access-Control-Max-Age",       "600")

  if (identical(req$REQUEST_METHOD, "OPTIONS")) {
    res$status <- 204L
    return(list())
  }

  plumber::forward()
}

# ---------------------------------------------------------------------------
# Build and return the router.
# pr_filter() prepends the CORS filter before the built-in plumber filters,
# ensuring it is first in the chain.
# ---------------------------------------------------------------------------
pr("api.R") |>
  pr_filter("cors", cors_filter)
