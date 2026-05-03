# =============================================================================
# Phase 12b: Export SHAP Metadata for Frontend Dashboard
#
# Outputs two files consumed by ModelSHAP.jsx:
#   plumber_api/shap_metadata.json   ← authoritative copy
#   ../frontend/src/data/shap_metadata.json   ← Vite static import
#   ../frontend/src/data/feature_importance.json  ← copy of Phase 12 output
#
# Contents of shap_metadata.json:
#   intercept        — from standardised-coefficient LR model
#   baseline_prob    — σ(intercept) = average model output
#   features[33]     — name, beta, mean, sd, is_binary, direction, rank
#   beeswarm[10]     — per-employee contributions + raw values for top 10 features
# =============================================================================

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(jsonlite))

setwd("C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r")

cat("==============================================\n")
cat("PHASE 12b: EXPORT SHAP METADATA\n")
cat("==============================================\n\n")

# ---------------------------------------------------------------------------
# 1. Load data + model
# ---------------------------------------------------------------------------
obj       <- readRDS("plumber_api/model_final.rds")
THRESHOLD <- obj[["threshold"]]

train_raw <- readRDS("train_data.rds")
val_raw   <- readRDS("val_data.rds")
test_raw  <- readRDS("test_data.rds")

DROPPED_COLS <- c(
  "SatisfactionIndex", "JobSatisfaction",
  "EnvironmentSatisfaction", "RelationshipSatisfaction",
  "Department.Research & Development", "Department.Sales",
  "Department.Human Resources",
  "JobRole.Human Resources", "JobRole.Sales Executive",
  "JobRole.Sales Representative", "JobRole.Laboratory Technician",
  "JobRole.Research Scientist",
  "JobLevel",
  "TotalWorkingYears"
)

drop_safe <- function(df) df %>% select(-all_of(intersect(names(df), DROPPED_COLS)))

train_s <- drop_safe(train_raw)
val_s   <- drop_safe(val_raw)
test_s  <- drop_safe(test_raw)

full_s  <- bind_rows(train_s, val_s)
all_s   <- bind_rows(train_s, val_s, test_s)   # 1470 employees for beeswarm

feature_cols <- setdiff(names(test_s), "Attrition")
cat(sprintf("Features: %d | All employees: %d\n\n", length(feature_cols), nrow(all_s)))

# ---------------------------------------------------------------------------
# 2. Fit standardised model (Gelman 2SD convention)
# ---------------------------------------------------------------------------
cat("--- Fitting standardised model ---\n")

scale_df <- function(df) {
  out <- df
  for (col in feature_cols) {
    x    <- df[[col]]
    sd_x <- sd(x, na.rm = TRUE)
    if (sd_x == 0) { out[[col]] <- 0; next }
    is_bin <- all(x %in% c(0, 1), na.rm = TRUE)
    denom  <- if (is_bin) 2 * sd_x else sd_x
    out[[col]] <- (x - mean(x, na.rm = TRUE)) / denom
  }
  out
}

train_sc <- scale_df(full_s)
fit_std  <- glm(Attrition ~ ., data = train_sc, family = binomial())

coefs   <- coef(fit_std)
intercept <- as.numeric(coefs["(Intercept)"])
betas_raw <- coefs[names(coefs) != "(Intercept)"]
names(betas_raw) <- gsub("^`|`$", "", names(betas_raw))

baseline_prob <- 1 / (1 + exp(-intercept))
cat(sprintf("Intercept: %.4f  |  Baseline prob (avg employee): %.4f\n\n", intercept, baseline_prob))

# ---------------------------------------------------------------------------
# 3. Load feature_importance.json for direction / rank metadata
# ---------------------------------------------------------------------------
fi    <- fromJSON("plumber_api/feature_importance.json", simplifyDataFrame = TRUE)
fi_df <- as.data.frame(fi$features)

# ---------------------------------------------------------------------------
# 4. Build features metadata list
# ---------------------------------------------------------------------------
cat("--- Building feature metadata ---\n")

feature_meta <- lapply(feature_cols, function(col) {
  x      <- full_s[[col]]
  mn     <- mean(x, na.rm = TRUE)
  sd_x   <- sd(x, na.rm = TRUE)
  is_bin <- all(x %in% c(0, 1), na.rm = TRUE)
  beta   <- if (col %in% names(betas_raw)) as.numeric(betas_raw[col]) else 0

  # Pull rank + direction from feature_importance.json
  row <- fi_df[fi_df$feature == col, ]
  rank_val  <- if (nrow(row) > 0) as.integer(row$rank[1])      else 99L
  dir_val   <- if (nrow(row) > 0) as.character(row$direction[1]) else "unknown"

  list(
    rank      = rank_val,
    name      = col,
    direction = dir_val,
    beta      = round(beta, 6),
    mean      = round(mn,   6),
    sd        = round(sd_x, 6),
    is_binary = is_bin
  )
})

# Sort by rank
feature_meta <- feature_meta[order(sapply(feature_meta, `[[`, "rank"))]
cat(sprintf("  Exported metadata for %d features\n\n", length(feature_meta)))

# ---------------------------------------------------------------------------
# 5. Beeswarm data — top 10 features, up to 400 employee samples
# ---------------------------------------------------------------------------
cat("--- Computing beeswarm data (top 10 features) ---\n")

set.seed(42)
top_features <- sapply(head(feature_meta, 10), `[[`, "name")

# Sample employees for beeswarm (stratified by Attrition)
n_sample <- min(400, nrow(all_s))
stay_idx <- which(all_s$Attrition == 0)
leave_idx <- which(all_s$Attrition == 1)
n_leave <- min(length(leave_idx), round(n_sample * 0.2))
n_stay  <- n_sample - n_leave
samp_idx <- c(sample(stay_idx, n_stay), sample(leave_idx, n_leave))
samp_data <- all_s[samp_idx, ]

beeswarm <- lapply(top_features, function(feat) {
  meta   <- Filter(function(m) m$name == feat, feature_meta)[[1]]
  x_raw  <- as.numeric(samp_data[[feat]])
  denom  <- if (meta$is_binary) 2 * meta$sd else meta$sd
  x_sc   <- if (denom > 0) (x_raw - meta$mean) / denom else rep(0, length(x_raw))
  contrib <- round(meta$beta * x_sc, 5)

  # Normalise feature values 0-1 for colour scale
  x_min <- min(x_raw, na.rm = TRUE)
  x_max <- max(x_raw, na.rm = TRUE)
  x_norm <- if (x_max > x_min) (x_raw - x_min) / (x_max - x_min) else rep(0.5, length(x_raw))

  list(
    feature           = feat,
    direction         = meta$direction,
    contributions     = contrib,
    feature_values    = round(x_norm, 4),
    feature_values_raw = x_raw
  )
})

cat(sprintf("  Beeswarm: %d features × %d employees each\n\n", length(beeswarm), n_sample))

# ---------------------------------------------------------------------------
# 6. Assemble and export JSON
# ---------------------------------------------------------------------------
cat("--- Exporting shap_metadata.json ---\n")

out <- list(
  generated_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  model         = "Logistic Regression (Phase 8 Variant B)",
  threshold     = THRESHOLD,
  intercept     = round(intercept,      6),
  baseline_prob = round(baseline_prob,  6),
  n_features    = length(feature_cols),
  features      = feature_meta,
  beeswarm      = beeswarm
)

json_str <- toJSON(out, auto_unbox = TRUE, pretty = FALSE, digits = 6)

# Write to plumber_api (authoritative)
api_path <- "plumber_api/shap_metadata.json"
write(json_str, api_path)
cat(sprintf("  Written → %s\n", api_path))

# Write to frontend/src/data/ for Vite static import
data_dir <- "../frontend/src/data"
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

shap_frontend_path <- file.path(data_dir, "shap_metadata.json")
write(json_str, shap_frontend_path)
cat(sprintf("  Written → %s\n", shap_frontend_path))

# Also copy feature_importance.json to frontend/src/data/
fi_src  <- "plumber_api/feature_importance.json"
fi_dest <- file.path(data_dir, "feature_importance.json")
file.copy(fi_src, fi_dest, overwrite = TRUE)
cat(sprintf("  Copied  → %s\n", fi_dest))

cat("\n==============================================\n")
cat("Phase 12b complete.\n")
cat("==============================================\n")
