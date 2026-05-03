# =============================================================================
# Phase 12: Feature Importance — SHAP-style Analysis for Logistic Regression
#
# Goal: Quantify and rank predictor importance using three complementary
#       methods, then export a JSON artefact consumed by the frontend.
#
# Methods:
#   1. Standardised Coefficients  — scale each numeric predictor to μ=0 σ=1
#      before fitting so coefficient magnitudes are directly comparable.
#      Binary / dummy predictors scaled by 2sd (Gelman convention) so they
#      sit on the same scale as continuous predictors.
#
#   2. Permutation Importance     — shuffle each feature on the test set,
#      measure AUC drop vs. baseline; repeat 30 times and report mean ± sd.
#      Model-agnostic, captures non-linear interaction effects too.
#
#   3. McFadden Partial R²        — refit model omitting one feature at a time;
#      report the drop in log-likelihood as a fraction of the null deviance.
#      Equivalent to "how much explanatory power does this variable add?"
#
# Outputs:
#   • Console table (all three ranks side-by-side)
#   • attrition_analysis.r/plumber_api/feature_importance.json
#     → consumed by ModelInsights.jsx "Predictors" tab
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})

setwd("C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r")

cat("==============================================\n")
cat("PHASE 12: FEATURE IMPORTANCE ANALYSIS\n")
cat("==============================================\n\n")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

roc_auc <- function(probs, labels) {
  ord   <- order(probs, decreasing = TRUE)
  labs  <- labels[ord]
  n_pos <- sum(labels == 1)
  n_neg <- sum(labels == 0)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  tpr <- c(0, cumsum(labs == 1) / n_pos, 1)
  fpr <- c(0, cumsum(labs == 0) / n_neg, 1)
  sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1)) / 2)
}

# ---------------------------------------------------------------------------
# 1. Load data + model
# ---------------------------------------------------------------------------

cat("--- Loading model and data ---\n")
obj       <- readRDS("plumber_api/model_final.rds")
model     <- obj[["model"]]
threshold <- obj[["threshold"]]

train_raw  <- readRDS("train_data.rds")
val_raw    <- readRDS("val_data.rds")
test_raw   <- readRDS("test_data.rds")

# Dropped columns (Phase 7 + Phase 8)
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

# Combine train+val for coefficient fitting (same split used in production)
full_s  <- bind_rows(train_s, val_s)

feature_cols <- setdiff(names(test_s), "Attrition")
cat(sprintf("Features: %d | Test rows: %d\n\n", length(feature_cols), nrow(test_s)))

# Baseline test AUC
baseline_probs <- predict(model, newdata = test_s, type = "response")
baseline_auc   <- roc_auc(baseline_probs, test_s[["Attrition"]])
cat(sprintf("Baseline Test AUC: %.4f\n\n", baseline_auc))

# ---------------------------------------------------------------------------
# 2. Standardised Coefficients
# ---------------------------------------------------------------------------

cat("--- [1/3] Standardised Coefficients ---\n")

# Scale each predictor: numeric → (x - mean) / sd; binary → (x - mean) / (2*sd)
scale_df <- function(df) {
  out <- df
  for (col in feature_cols) {
    x  <- df[[col]]
    sd_x <- sd(x, na.rm = TRUE)
    if (sd_x == 0) {
      out[[col]] <- 0
      next
    }
    is_binary <- all(x %in% c(0, 1), na.rm = TRUE)
    denom <- if (is_binary) 2 * sd_x else sd_x
    out[[col]] <- (x - mean(x, na.rm = TRUE)) / denom
  }
  out
}

train_sc <- scale_df(full_s)
fit_std  <- glm(Attrition ~ ., data = train_sc, family = binomial())

coefs_std <- coef(fit_std)
coefs_std <- coefs_std[names(coefs_std) != "(Intercept)"]
# Strip backticks R adds to non-syntactic column names
names(coefs_std) <- gsub("^`|`$", "", names(coefs_std))

std_coef_df <- data.frame(
  feature    = names(coefs_std),
  std_coef   = as.numeric(coefs_std),
  abs_std    = abs(as.numeric(coefs_std)),
  direction  = ifelse(coefs_std > 0, "risk", "protective"),
  stringsAsFactors = FALSE
) %>% arrange(desc(abs_std))

std_coef_df$rank_std <- seq_len(nrow(std_coef_df))

cat("Top 15 by |standardised coefficient|:\n")
cat(sprintf("  %-45s  %8s  %s\n", "Feature", "Std.Coef", "Direction"))
cat(strrep("-", 65), "\n")
for (i in seq_len(min(15, nrow(std_coef_df)))) {
  r <- std_coef_df[i, ]
  cat(sprintf("  %-45s  %+8.4f  %s\n", r$feature, r$std_coef, r$direction))
}
cat("\n")

# ---------------------------------------------------------------------------
# 3. Permutation Importance (30 shuffles per feature)
# ---------------------------------------------------------------------------

cat("--- [2/3] Permutation Importance (30 repeats) ---\n")

set.seed(42)
N_REPS <- 30

perm_results <- lapply(feature_cols, function(col) {
  drops <- numeric(N_REPS)
  for (r in seq_len(N_REPS)) {
    test_perm         <- test_s
    test_perm[[col]]  <- sample(test_perm[[col]])
    perm_probs        <- predict(model, newdata = test_perm, type = "response")
    perm_auc          <- roc_auc(perm_probs, test_s[["Attrition"]])
    drops[r]          <- baseline_auc - perm_auc
  }
  data.frame(
    feature      = col,
    perm_mean    = mean(drops),
    perm_sd      = sd(drops),
    stringsAsFactors = FALSE
  )
})

perm_df <- bind_rows(perm_results) %>% arrange(desc(perm_mean))
perm_df$rank_perm <- seq_len(nrow(perm_df))

cat("Top 15 by mean AUC drop on permutation:\n")
cat(sprintf("  %-45s  %9s  %8s\n", "Feature", "AUC Drop", "± SD"))
cat(strrep("-", 68), "\n")
for (i in seq_len(min(15, nrow(perm_df)))) {
  r <- perm_df[i, ]
  cat(sprintf("  %-45s  %+9.5f  %8.5f\n", r$feature, r$perm_mean, r$perm_sd))
}
cat("\n")

# ---------------------------------------------------------------------------
# 4. McFadden Partial R² (log-likelihood drop)
# ---------------------------------------------------------------------------

cat("--- [3/3] McFadden Partial R² ---\n")

null_fit   <- glm(Attrition ~ 1, data = full_s, family = binomial())
full_fit   <- glm(Attrition ~ ., data = full_s, family = binomial())
null_ll    <- as.numeric(logLik(null_fit))
full_ll    <- as.numeric(logLik(full_fit))
null_dev   <- -2 * null_ll   # total null deviance

mcf_results <- lapply(feature_cols, function(col) {
  reduced_data <- full_s %>% select(-all_of(col))
  reduced_fit  <- glm(Attrition ~ ., data = reduced_data, family = binomial())
  red_ll       <- as.numeric(logLik(reduced_fit))
  # Partial R²: how much deviance is uniquely explained by this predictor
  partial_r2   <- (red_ll - full_ll) / (-0.5 * null_dev)  # positive → feature helps
  # Alternatively: drop in McFadden R2
  mcf_full     <- 1 - full_ll / null_ll
  mcf_red      <- 1 - red_ll  / null_ll
  data.frame(
    feature    = col,
    partial_r2 = mcf_full - mcf_red,
    stringsAsFactors = FALSE
  )
})

mcf_df <- bind_rows(mcf_results) %>% arrange(desc(partial_r2))
mcf_df$rank_mcf <- seq_len(nrow(mcf_df))

cat("Top 15 by McFadden partial R² (drop when removed):\n")
cat(sprintf("  %-45s  %10s\n", "Feature", "Partial R²"))
cat(strrep("-", 60), "\n")
for (i in seq_len(min(15, nrow(mcf_df)))) {
  r <- mcf_df[i, ]
  cat(sprintf("  %-45s  %10.6f\n", r$feature, r$partial_r2))
}
cat("\n")

# ---------------------------------------------------------------------------
# 5. Consensus ranking table
# ---------------------------------------------------------------------------

cat("==============================================\n")
cat("CONSENSUS RANKING (average of 3 method ranks)\n")
cat("==============================================\n")

consensus <- std_coef_df %>%
  select(feature, rank_std, std_coef, direction) %>%
  left_join(perm_df  %>% select(feature, rank_perm, perm_mean, perm_sd), by = "feature") %>%
  left_join(mcf_df   %>% select(feature, rank_mcf,  partial_r2),          by = "feature") %>%
  mutate(avg_rank = (rank_std + rank_perm + rank_mcf) / 3) %>%
  arrange(avg_rank)

consensus$consensus_rank <- seq_len(nrow(consensus))

cat(sprintf("  %-3s  %-40s  %5s  %5s  %5s  %7s  %s\n",
            "Rk", "Feature", "StdRk", "PrmRk", "McfRk", "AvgRk", "Direction"))
cat(strrep("-", 82), "\n")
for (i in seq_len(nrow(consensus))) {
  r <- consensus[i, ]
  cat(sprintf("  %-3d  %-40s  %5d  %5d  %5d  %7.1f  %s\n",
              i, r$feature, r$rank_std, r$rank_perm, r$rank_mcf, r$avg_rank, r$direction))
}
cat("\n")

# ---------------------------------------------------------------------------
# 6. Export JSON for frontend
# ---------------------------------------------------------------------------

cat("--- Exporting feature_importance.json ---\n")

# Build output list — top N features, sorted by consensus rank
top_n <- nrow(consensus)   # export all features

export_list <- lapply(seq_len(top_n), function(i) {
  r <- consensus[i, ]
  list(
    rank          = r$consensus_rank,
    feature       = r$feature,
    direction     = r$direction,
    std_coef      = round(r$std_coef,    4),
    perm_auc_drop = round(r$perm_mean,   5),
    perm_auc_sd   = round(r$perm_sd,     5),
    partial_r2    = round(r$partial_r2,  6),
    rank_std      = r$rank_std,
    rank_perm     = r$rank_perm,
    rank_mcf      = r$rank_mcf
  )
})

out_json <- list(
  generated_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  model           = "Logistic Regression (Phase 8 Variant B)",
  threshold       = threshold,
  baseline_auc    = round(baseline_auc, 4),
  n_features      = length(feature_cols),
  method_notes    = list(
    std_coef   = "Coefficients from model refit on scaled predictors (Gelman 2sd for binary)",
    perm_imp   = "Mean AUC drop over 30 permutation repeats on held-out test set",
    partial_r2 = "Drop in McFadden R² when feature removed from full model (train+val)"
  ),
  features = export_list
)

json_path <- "plumber_api/feature_importance.json"
write(toJSON(out_json, auto_unbox = TRUE, pretty = TRUE), json_path)
cat(sprintf("  Saved → %s\n\n", json_path))

cat("==============================================\n")
cat("Phase 12 complete.\n")
cat("==============================================\n")
