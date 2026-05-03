if (!requireNamespace("car", quietly = TRUE)) install.packages("car", repos = "https://cloud.r-project.org")
library(car)

setwd("C:/Users/ayhin/Desktop/Attrition")
obj   <- readRDS("attrition_analysis.r/plumber_api/model_final.rds")
model <- obj[["model"]]

# Step 1: Find aliased (perfectly collinear) coefficients
al <- alias(model)
if (!is.null(al$Complete)) {
  cat("\n=== PERFECTLY COLLINEAR (Aliased) Coefficients ===\n")
  cat("These predictors are exact linear combinations of others:\n\n")
  print(al$Complete)
} else {
  cat("\nNo perfectly aliased coefficients found.\n")
}

# Step 2: Compute VIF manually from design matrix (avoids refitting)
# VIF_j = 1 / (1 - R^2_j) where R^2_j = R^2 from regressing X_j on all other X columns

X <- model.matrix(model)          # design matrix (includes intercept)
X <- X[, !is.na(coef(model))]    # drop aliased columns (NA coefs)
X <- X[, colnames(X) != "(Intercept)"]  # drop intercept

cat(sprintf("\nComputing VIF for %d non-aliased predictors...\n\n", ncol(X)))

vif_manual <- sapply(seq_len(ncol(X)), function(j) {
  y_j    <- X[, j]
  X_rest <- X[, -j, drop = FALSE]
  r2     <- summary(lm(y_j ~ X_rest))$r.squared
  1 / (1 - r2)
})
names(vif_manual) <- colnames(X)
sv <- sort(vif_manual, decreasing = TRUE)

v  <- vif_manual
sv <- sort(v, decreasing = TRUE)

cat("\n=== VIF Results (non-aliased terms, sorted) ===\n")
for (nm in names(sv)) {
  flag <- if (sv[[nm]] > 10) "  *** HIGH" else if (sv[[nm]] >= 5) "  * moderate" else ""
  cat(sprintf("  %-52s %6.2f%s\n", nm, sv[[nm]], flag))
}
cat(sprintf("\nHigh >10 : %d\nModerate 5-10: %d\nOK <5    : %d\n",
            sum(sv > 10), sum(sv >= 5 & sv <= 10), sum(sv < 5)))
