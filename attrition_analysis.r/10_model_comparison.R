# =============================================================================
# Phase 10: Multi-Model Comparison on Multicollinearity-Free Dataset
#
# Models: Logistic Regression, glmnet Ridge, SVM, XGBoost,
#         Decision Tree, Random Forest
#
# Data:   Phase-7+8 cleaned splits (YearsAtCompany kept,
#         TotalWorkingYears + collinear vars removed)
#
# LR threshold fixed at 0.26 (user decision from Phase 9 sweep).
# All other models: threshold optimised on val set (max F1, 0.10–0.90).
# =============================================================================

# ── Package bootstrap ─────────────────────────────────────────────────────────
pkgs <- c("dplyr", "glmnet", "e1071", "xgboost", "rpart", "randomForest")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org", quiet = TRUE)
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}

setwd("C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r")

cat("==============================================\n")
cat("PHASE 10: MULTI-MODEL COMPARISON\n")
cat("==============================================\n\n")

# =============================================================================
# Helpers
# =============================================================================

roc_auc <- function(probs, labels) {
  ord   <- order(probs, decreasing = TRUE)
  labs  <- as.integer(labels)[ord]
  n_pos <- sum(labs == 1); n_neg <- sum(labs == 0)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  tpr <- c(0, cumsum(labs == 1) / n_pos, 1)
  fpr <- c(0, cumsum(labs == 0) / n_neg, 1)
  sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1)) / 2)
}

evaluate <- function(probs, labels, threshold) {
  preds <- as.integer(probs >= threshold)
  labs  <- as.integer(labels)
  tp  <- sum(preds == 1 & labs == 1)
  fp  <- sum(preds == 1 & labs == 0)
  fn  <- sum(preds == 0 & labs == 1)
  tn  <- sum(preds == 0 & labs == 0)
  prec <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  rec  <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1   <- if ((prec + rec) > 0) 2 * prec * rec / (prec + rec) else 0
  acc  <- (tp + tn) / length(labs)
  list(f1 = f1, prec = prec, rec = rec, acc = acc,
       tp = tp, fp = fp, fn = fn, tn = tn)
}

best_threshold <- function(probs, labels, fixed_t = NULL) {
  if (!is.null(fixed_t)) return(list(t = fixed_t, f1 = evaluate(probs, labels, fixed_t)$f1))
  best_t <- 0.5; best_f1 <- 0
  for (t in seq(0.10, 0.90, by = 0.01)) {
    f1 <- evaluate(probs, labels, t)$f1
    if (f1 > best_f1) { best_f1 <- f1; best_t <- t }
  }
  list(t = best_t, f1 = best_f1)
}

diagnose <- function(train_f1, val_f1, gap_thresh = 0.08) {
  gap <- train_f1 - val_f1
  if (train_f1 < 0.55 && abs(gap) < 0.05) return("High Bias")
  if (gap > gap_thresh)                    return("High Variance")
  "OK"
}

# =============================================================================
# STEP 1: Load + clean data
# =============================================================================

cat("--- Loading data ---\n")
train_s <- readRDS("train_data.rds")
val_s   <- readRDS("val_data.rds")
test_s  <- readRDS("test_data.rds")

DROPS <- c(
  "SatisfactionIndex", "JobSatisfaction",
  "EnvironmentSatisfaction", "RelationshipSatisfaction",
  "Department.Research & Development", "Department.Sales",
  "Department.Human Resources",
  "JobRole.Human Resources", "JobRole.Sales Executive",
  "JobRole.Sales Representative", "JobRole.Laboratory Technician",
  "JobRole.Research Scientist",
  "JobLevel", "TotalWorkingYears"
)

clean <- function(df) df %>% select(-all_of(intersect(names(df), DROPS)))
train_r <- clean(train_s)
val_r   <- clean(val_s)
test_r  <- clean(test_s)

cat(sprintf("Clean dims — Train: %d x %d | Val: %d x %d | Test: %d x %d\n\n",
            nrow(train_r), ncol(train_r),
            nrow(val_r),   ncol(val_r),
            nrow(test_r),  ncol(test_r)))

y_train <- train_r$Attrition
y_val   <- val_r$Attrition
y_test  <- test_r$Attrition

X_train <- train_r %>% select(-Attrition)
X_val   <- val_r   %>% select(-Attrition)
X_test  <- test_r  %>% select(-Attrition)

# Matrix form for glmnet / xgboost
Xm_train <- as.matrix(X_train)
Xm_val   <- as.matrix(X_val)
Xm_test  <- as.matrix(X_test)

results <- list()

# =============================================================================
# MODEL 1: Logistic Regression  (threshold fixed at 0.26 per Phase 9 analysis)
# =============================================================================

cat("--- [1/6] Logistic Regression ---\n")
lr_model <- glm(Attrition ~ ., data = train_r, family = binomial(link = "logit"))
cat(sprintf("  Converged: %s\n", ifelse(lr_model$converged, "YES", "NO")))

p_tr  <- predict(lr_model, newdata = train_r, type = "response")
p_val <- predict(lr_model, newdata = val_r,   type = "response")
p_te  <- predict(lr_model, newdata = test_r,  type = "response")

LR_THRESH <- 0.26
tr_f1  <- evaluate(p_tr,  y_train, LR_THRESH)$f1
val_f1 <- evaluate(p_val, y_val,   LR_THRESH)$f1
te_f1  <- evaluate(p_te,  y_test,  LR_THRESH)$f1
val_auc <- roc_auc(p_val, y_val)
te_auc  <- roc_auc(p_te,  y_test)

cat(sprintf("  Threshold: %.2f (fixed)\n", LR_THRESH))
cat(sprintf("  Train F1: %.3f | Val F1: %.3f | Test F1: %.3f\n", tr_f1, val_f1, te_f1))
cat(sprintf("  Val AUC: %.3f | Test AUC: %.3f\n\n", val_auc, te_auc))

results[["Logistic Regression"]] <- list(
  train_f1 = tr_f1, val_f1 = val_f1, test_f1 = te_f1,
  val_auc = val_auc, test_auc = te_auc, thresh = LR_THRESH,
  p_test = p_te, y_test = y_test
)

# =============================================================================
# MODEL 2: glmnet Ridge  (alpha = 0, lambda via CV)
# =============================================================================

cat("--- [2/6] glmnet Ridge ---\n")
set.seed(42)
cv_ridge <- cv.glmnet(Xm_train, y_train, family = "binomial", alpha = 0,
                      nfolds = 5, type.measure = "auc")
best_lam <- cv_ridge$lambda.1se
cat(sprintf("  Best lambda (1se): %.5f\n", best_lam))

p_tr  <- as.numeric(predict(cv_ridge, newx = Xm_train, s = best_lam, type = "response"))
p_val <- as.numeric(predict(cv_ridge, newx = Xm_val,   s = best_lam, type = "response"))
p_te  <- as.numeric(predict(cv_ridge, newx = Xm_test,  s = best_lam, type = "response"))

bt      <- best_threshold(p_val, y_val)
tr_f1   <- evaluate(p_tr,  y_train, bt$t)$f1
val_f1  <- evaluate(p_val, y_val,   bt$t)$f1
te_f1   <- evaluate(p_te,  y_test,  bt$t)$f1
val_auc <- roc_auc(p_val, y_val)
te_auc  <- roc_auc(p_te,  y_test)

cat(sprintf("  Optimal threshold: %.2f (max val F1)\n", bt$t))
cat(sprintf("  Train F1: %.3f | Val F1: %.3f | Test F1: %.3f\n", tr_f1, val_f1, te_f1))
cat(sprintf("  Val AUC: %.3f | Test AUC: %.3f\n\n", val_auc, te_auc))

results[["glmnet Ridge"]] <- list(
  train_f1 = tr_f1, val_f1 = val_f1, test_f1 = te_f1,
  val_auc = val_auc, test_auc = te_auc, thresh = bt$t,
  p_test = p_te, y_test = y_test
)

# =============================================================================
# MODEL 3: SVM (radial kernel, probability = TRUE)
# =============================================================================

cat("--- [3/6] SVM (radial, probability=TRUE) ---\n")
set.seed(42)
train_r_svm <- train_r %>% mutate(Attrition = factor(Attrition))
# Tune cost/gamma quickly with a small grid
tune_svm <- tune(svm, Attrition ~ ., data = train_r_svm,
                 kernel = "radial", probability = TRUE,
                 ranges = list(cost = c(0.1, 1, 10), gamma = c(0.01, 0.1)),
                 tunecontrol = tune.control(sampling = "cross", cross = 3))
svm_model <- tune_svm$best.model
cat(sprintf("  Best cost: %s | gamma: %s\n",
            svm_model$cost, svm_model$gamma))

svm_prob <- function(model, newdata) {
  pmat <- attr(predict(model, newdata = newdata, probability = TRUE), "probabilities")
  # column may be named "1" or the positive factor level
  pos_col <- if ("1" %in% colnames(pmat)) "1" else colnames(pmat)[2]
  as.numeric(pmat[, pos_col])
}

p_tr  <- svm_prob(svm_model, train_r %>% select(-Attrition))
p_val <- svm_prob(svm_model, val_r   %>% select(-Attrition))
p_te  <- svm_prob(svm_model, test_r  %>% select(-Attrition))

bt      <- best_threshold(p_val, y_val)
tr_f1   <- evaluate(p_tr,  y_train, bt$t)$f1
val_f1  <- evaluate(p_val, y_val,   bt$t)$f1
te_f1   <- evaluate(p_te,  y_test,  bt$t)$f1
val_auc <- roc_auc(p_val, y_val)
te_auc  <- roc_auc(p_te,  y_test)

cat(sprintf("  Optimal threshold: %.2f\n", bt$t))
cat(sprintf("  Train F1: %.3f | Val F1: %.3f | Test F1: %.3f\n", tr_f1, val_f1, te_f1))
cat(sprintf("  Val AUC: %.3f | Test AUC: %.3f\n\n", val_auc, te_auc))

results[["SVM"]] <- list(
  train_f1 = tr_f1, val_f1 = val_f1, test_f1 = te_f1,
  val_auc = val_auc, test_auc = te_auc, thresh = bt$t,
  p_test = p_te, y_test = y_test
)

# =============================================================================
# MODEL 4: XGBoost
# =============================================================================

cat("--- [4/6] XGBoost ---\n")
dtrain <- xgb.DMatrix(data = Xm_train, label = as.integer(y_train))
dval   <- xgb.DMatrix(data = Xm_val,   label = as.integer(y_val))
dtest  <- xgb.DMatrix(data = Xm_test,  label = as.integer(y_test))

# Class imbalance weight
n_neg <- sum(y_train == 0); n_pos <- sum(y_train == 1)
scale_pos <- n_neg / n_pos

set.seed(42)
xgb_model <- xgb.train(
  params = list(
    objective        = "binary:logistic",
    eval_metric      = "auc",
    eta              = 0.05,
    max_depth        = 4,
    subsample        = 0.8,
    colsample_bytree = 0.8,
    scale_pos_weight = scale_pos,
    nthread          = 1
  ),
  data      = dtrain,
  nrounds   = 300,
  watchlist = list(train = dtrain, val = dval),
  early_stopping_rounds = 30,
  verbose   = 0
)
cat(sprintf("  Best iteration: %d | Best val AUC: %.4f\n",
            xgb_model$best_iteration, xgb_model$best_score))

p_tr  <- predict(xgb_model, dtrain)
p_val <- predict(xgb_model, dval)
p_te  <- predict(xgb_model, dtest)

bt      <- best_threshold(p_val, y_val)
tr_f1   <- evaluate(p_tr,  y_train, bt$t)$f1
val_f1  <- evaluate(p_val, y_val,   bt$t)$f1
te_f1   <- evaluate(p_te,  y_test,  bt$t)$f1
val_auc <- roc_auc(p_val, y_val)
te_auc  <- roc_auc(p_te,  y_test)

cat(sprintf("  Optimal threshold: %.2f\n", bt$t))
cat(sprintf("  Train F1: %.3f | Val F1: %.3f | Test F1: %.3f\n", tr_f1, val_f1, te_f1))
cat(sprintf("  Val AUC: %.3f | Test AUC: %.3f\n\n", val_auc, te_auc))

results[["XGBoost"]] <- list(
  train_f1 = tr_f1, val_f1 = val_f1, test_f1 = te_f1,
  val_auc = val_auc, test_auc = te_auc, thresh = bt$t,
  p_test = p_te, y_test = y_test
)

# =============================================================================
# MODEL 5: Decision Tree (rpart, min complexity pruned)
# =============================================================================

cat("--- [5/6] Decision Tree ---\n")
set.seed(42)
dt_model <- rpart(
  Attrition ~ ., data = train_r, method = "class",
  parms  = list(prior = c(`0` = n_neg / (n_neg + n_pos),
                           `1` = n_pos / (n_neg + n_pos))),
  control = rpart.control(cp = 0.001, minsplit = 10, maxdepth = 8)
)

# Prune to cp with lowest xerror
cp_table  <- dt_model$cptable
best_cp   <- cp_table[which.min(cp_table[, "xerror"]), "CP"]
dt_pruned <- prune(dt_model, cp = best_cp)
cat(sprintf("  Pruned cp: %.5f | tree size: %d leaves\n",
            best_cp, sum(dt_pruned$frame$var == "<leaf>")))

dt_prob <- function(model, newdata) {
  predict(model, newdata = newdata, type = "prob")[, "1"]
}

p_tr  <- dt_prob(dt_pruned, train_r %>% select(-Attrition))
p_val <- dt_prob(dt_pruned, val_r   %>% select(-Attrition))
p_te  <- dt_prob(dt_pruned, test_r  %>% select(-Attrition))

bt      <- best_threshold(p_val, y_val)
tr_f1   <- evaluate(p_tr,  y_train, bt$t)$f1
val_f1  <- evaluate(p_val, y_val,   bt$t)$f1
te_f1   <- evaluate(p_te,  y_test,  bt$t)$f1
val_auc <- roc_auc(p_val, y_val)
te_auc  <- roc_auc(p_te,  y_test)

cat(sprintf("  Optimal threshold: %.2f\n", bt$t))
cat(sprintf("  Train F1: %.3f | Val F1: %.3f | Test F1: %.3f\n", tr_f1, val_f1, te_f1))
cat(sprintf("  Val AUC: %.3f | Test AUC: %.3f\n\n", val_auc, te_auc))

results[["Decision Tree"]] <- list(
  train_f1 = tr_f1, val_f1 = val_f1, test_f1 = te_f1,
  val_auc = val_auc, test_auc = te_auc, thresh = bt$t,
  p_test = p_te, y_test = y_test
)

# =============================================================================
# MODEL 6: Random Forest
# =============================================================================

cat("--- [6/6] Random Forest ---\n")
set.seed(42)
rf_model <- randomForest(
  x         = X_train,
  y         = factor(y_train),
  ntree     = 500,
  mtry      = max(1L, floor(sqrt(ncol(X_train)))),
  classwt   = c(`0` = 1, `1` = n_neg / n_pos),
  importance= FALSE
)

rf_prob <- function(model, newdata) {
  predict(model, newdata = newdata, type = "prob")[, "1"]
}

p_tr  <- rf_prob(rf_model, X_train)
p_val <- rf_prob(rf_model, X_val)
p_te  <- rf_prob(rf_model, X_test)

bt      <- best_threshold(p_val, y_val)
tr_f1   <- evaluate(p_tr,  y_train, bt$t)$f1
val_f1  <- evaluate(p_val, y_val,   bt$t)$f1
te_f1   <- evaluate(p_te,  y_test,  bt$t)$f1
val_auc <- roc_auc(p_val, y_val)
te_auc  <- roc_auc(p_te,  y_test)

cat(sprintf("  Optimal threshold: %.2f\n", bt$t))
cat(sprintf("  Train F1: %.3f | Val F1: %.3f | Test F1: %.3f\n", tr_f1, val_f1, te_f1))
cat(sprintf("  Val AUC: %.3f | Test AUC: %.3f\n\n", val_auc, te_auc))

results[["Random Forest"]] <- list(
  train_f1 = tr_f1, val_f1 = val_f1, test_f1 = te_f1,
  val_auc = val_auc, test_auc = te_auc, thresh = bt$t,
  p_test = p_te, y_test = y_test
)

# =============================================================================
# FINAL COMPARISON TABLE
# =============================================================================

cat("\n")
cat("==============================================================================\n")
cat("  MULTI-MODEL COMPARISON — VALIDATION SET (threshold-optimised)\n")
cat("==============================================================================\n")
cat(sprintf("  %-22s  %8s  %8s  %6s  %7s  %10s  %8s  %-14s\n",
            "Model", "TrainF1", "ValF1", "Gap", "ValAUC", "OptThresh", "TestF1", "Diagnosis"))
cat(rep("-", 100), "\n", sep = "")

MODEL_ORDER <- c("Logistic Regression", "glmnet Ridge", "SVM",
                 "XGBoost", "Decision Tree", "Random Forest")

for (nm in MODEL_ORDER) {
  r   <- results[[nm]]
  gap <- r$train_f1 - r$val_f1
  dx  <- diagnose(r$train_f1, r$val_f1)
  cat(sprintf("  %-22s  %8.3f  %8.3f  %+6.3f  %7.3f  %10.2f  %8.3f  %-14s\n",
              nm, r$train_f1, r$val_f1, gap, r$val_auc, r$thresh, r$test_f1, dx))
}
cat(rep("-", 100), "\n\n", sep = "")

# =============================================================================
# TEST SET TABLE (secondary, for final reference)
# =============================================================================

cat("  TEST SET RESULTS (fixed threshold from val optimisation)\n")
cat(sprintf("  %-22s  %8s  %8s\n", "Model", "TestF1", "TestAUC"))
cat(rep("-", 50), "\n", sep = "")
for (nm in MODEL_ORDER) {
  r <- results[[nm]]
  cat(sprintf("  %-22s  %8.3f  %8.3f\n", nm, r$test_f1, r$test_auc))
}
cat(rep("-", 50), "\n\n", sep = "")

cat("==============================================\n")
cat("Phase 10 complete.\n")
cat("==============================================\n")
