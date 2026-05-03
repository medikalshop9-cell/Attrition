# Phase 11: Threshold sweep 0.01–0.99 for all non-LR models
# Uses the same cleaned dataset (Phase 7+8 drops)

pkgs <- c("dplyr", "glmnet", "e1071", "xgboost", "rpart", "randomForest")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org", quiet = TRUE)
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}

setwd("C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r")

# ── Data ─────────────────────────────────────────────────────────────────────
train_s <- readRDS("train_data.rds")
val_s   <- readRDS("val_data.rds")
test_s  <- readRDS("test_data.rds")

DROPS <- c(
  "SatisfactionIndex","JobSatisfaction","EnvironmentSatisfaction",
  "RelationshipSatisfaction","Department.Research & Development",
  "Department.Sales","Department.Human Resources",
  "JobRole.Human Resources","JobRole.Sales Executive",
  "JobRole.Sales Representative","JobRole.Laboratory Technician",
  "JobRole.Research Scientist","JobLevel","TotalWorkingYears"
)
clean <- function(df) df %>% select(-all_of(intersect(names(df), DROPS)))
train_r <- clean(train_s); val_r <- clean(val_s); test_r <- clean(test_s)

y_val  <- val_r$Attrition
y_test <- test_r$Attrition
n_val  <- length(y_val)
n_test <- length(y_test)

X_train <- train_r %>% select(-Attrition)
X_val   <- val_r   %>% select(-Attrition)
X_test  <- test_r  %>% select(-Attrition)
Xm_train <- as.matrix(X_train)
Xm_val   <- as.matrix(X_val)
Xm_test  <- as.matrix(X_test)
y_train  <- train_r$Attrition

n_neg <- sum(y_train == 0); n_pos <- sum(y_train == 1)

# ── Helpers ───────────────────────────────────────────────────────────────────
eval_t <- function(probs, labels, t) {
  preds <- as.integer(probs >= t)
  labs  <- as.integer(labels)
  tp <- sum(preds == 1 & labs == 1); fp <- sum(preds == 1 & labs == 0)
  fn <- sum(preds == 0 & labs == 1); tn <- sum(preds == 0 & labs == 0)
  prec <- if ((tp+fp) > 0) tp/(tp+fp) else NA
  rec  <- if ((tp+fn) > 0) tp/(tp+fn) else NA
  f1   <- if (!is.na(prec) && !is.na(rec) && (prec+rec) > 0) 2*prec*rec/(prec+rec) else NA
  acc  <- (tp+tn)/length(labs)
  list(prec=prec, rec=rec, f1=f1, acc=acc)
}

print_sweep <- function(model_name, p_val, p_test, y_val, y_test, opt_thresh) {
  cat(sprintf("\n%s\n%s\n", model_name, strrep("=", nchar(model_name))))
  cat(sprintf("  (Optimal threshold from Phase 10 val search: %.2f)\n\n", opt_thresh))

  header <- sprintf("  %-8s  %-9s  %-9s  %-9s  %-9s   ||  %-9s  %-9s  %-9s  %-9s",
                    "Thresh","Prec(V)","Rec(V)","F1(V)","Acc(V)",
                    "Prec(T)","Rec(T)","F1(T)","Acc(T)")
  cat(header, "\n")
  cat("  ", strrep("-", nchar(header)-2), "\n", sep="")

  best_f1_v <- 0; best_t_v <- NA
  best_rec_v <- 0; best_t_rec <- NA

  for (t in seq(0.01, 0.99, by = 0.01)) {
    mv <- eval_t(p_val,  y_val,  t)
    mt <- eval_t(p_test, y_test, t)
    if (!is.na(mv$f1)  && mv$f1  > best_f1_v)  { best_f1_v <- mv$f1;  best_t_v   <- t }
    if (!is.na(mv$rec) && mv$rec > best_rec_v) { best_rec_v <- mv$rec; best_t_rec <- t }
    marker <- if (abs(t - opt_thresh) < 0.005) " <- opt" else ""
    cat(sprintf("  %-8.2f  %-9s  %-9s  %-9s  %-9s   ||  %-9s  %-9s  %-9s  %-9s%s\n",
      t,
      if(is.na(mv$prec)) "  -  " else sprintf("%.3f", mv$prec),
      if(is.na(mv$rec))  "  -  " else sprintf("%.3f", mv$rec),
      if(is.na(mv$f1))   "  -  " else sprintf("%.3f", mv$f1),
      sprintf("%.3f", mv$acc),
      if(is.na(mt$prec)) "  -  " else sprintf("%.3f", mt$prec),
      if(is.na(mt$rec))  "  -  " else sprintf("%.3f", mt$rec),
      if(is.na(mt$f1))   "  -  " else sprintf("%.3f", mt$f1),
      sprintf("%.3f", mt$acc),
      marker))
  }
  cat("  ", strrep("-", nchar(header)-2), "\n", sep="")
  cat(sprintf("  Best Val F1    @ t=%.2f  (Val F1=%.3f)\n", best_t_v,   best_f1_v))
  cat(sprintf("  Best Val Recall@ t=%.2f  (Val Rec=%.3f)\n", best_t_rec, best_rec_v))
  cat("\n")
}

# =============================================================================
# MODEL 1: glmnet Ridge
# =============================================================================
cat("Training glmnet Ridge...\n")
set.seed(42)
cv_ridge <- cv.glmnet(Xm_train, y_train, family="binomial", alpha=0,
                      nfolds=5, type.measure="auc")
p_val_ridge  <- as.numeric(predict(cv_ridge, newx=Xm_val,  s=cv_ridge$lambda.1se, type="response"))
p_test_ridge <- as.numeric(predict(cv_ridge, newx=Xm_test, s=cv_ridge$lambda.1se, type="response"))
print_sweep("glmnet Ridge", p_val_ridge, p_test_ridge, y_val, y_test, opt_thresh=0.18)

# =============================================================================
# MODEL 2: SVM
# =============================================================================
cat("Training SVM (tuning cost/gamma)...\n")
set.seed(42)
train_r_svm <- train_r %>% mutate(Attrition = factor(Attrition))
tune_svm <- tune(svm, Attrition ~ ., data = train_r_svm,
                 kernel = "radial", probability = TRUE,
                 ranges = list(cost = c(0.1, 1, 10), gamma = c(0.01, 0.1)),
                 tunecontrol = tune.control(sampling = "cross", cross = 3))
svm_model <- tune_svm$best.model
svm_prob <- function(newdata) {
  pmat <- attr(predict(svm_model, newdata=newdata, probability=TRUE), "probabilities")
  pos_col <- if ("1" %in% colnames(pmat)) "1" else colnames(pmat)[2]
  as.numeric(pmat[, pos_col])
}
p_val_svm  <- svm_prob(val_r   %>% select(-Attrition))
p_test_svm <- svm_prob(test_r  %>% select(-Attrition))
print_sweep("SVM", p_val_svm, p_test_svm, y_val, y_test, opt_thresh=0.18)

# =============================================================================
# MODEL 3: XGBoost
# =============================================================================
cat("Training XGBoost...\n")
dtrain <- xgb.DMatrix(data=Xm_train, label=as.integer(y_train))
dval   <- xgb.DMatrix(data=Xm_val,   label=as.integer(y_val))
dtest  <- xgb.DMatrix(data=Xm_test,  label=as.integer(y_test))
set.seed(42)
xgb_model <- xgb.train(
  params = list(objective="binary:logistic", eval_metric="auc",
                eta=0.05, max_depth=4, subsample=0.8,
                colsample_bytree=0.8, scale_pos_weight=n_neg/n_pos, nthread=1),
  data=dtrain, nrounds=300,
  evals=list(train=dtrain, val=dval),
  early_stopping_rounds=30, verbose=0
)
p_val_xgb  <- predict(xgb_model, dval)
p_test_xgb <- predict(xgb_model, dtest)
print_sweep("XGBoost", p_val_xgb, p_test_xgb, y_val, y_test, opt_thresh=0.45)

# =============================================================================
# MODEL 4: Decision Tree
# =============================================================================
cat("Training Decision Tree...\n")
set.seed(42)
dt_model <- rpart(Attrition ~ ., data=train_r, method="class",
                  parms=list(prior=c(`0`=n_neg/(n_neg+n_pos), `1`=n_pos/(n_neg+n_pos))),
                  control=rpart.control(cp=0.001, minsplit=10, maxdepth=8))
best_cp   <- dt_model$cptable[which.min(dt_model$cptable[,"xerror"]), "CP"]
dt_pruned <- prune(dt_model, cp=best_cp)
dt_prob <- function(newdata) predict(dt_pruned, newdata=newdata, type="prob")[,"1"]
p_val_dt  <- dt_prob(val_r  %>% select(-Attrition))
p_test_dt <- dt_prob(test_r %>% select(-Attrition))
print_sweep("Decision Tree", p_val_dt, p_test_dt, y_val, y_test, opt_thresh=0.10)

# =============================================================================
# MODEL 5: Random Forest
# =============================================================================
cat("Training Random Forest...\n")
set.seed(42)
rf_model <- randomForest(x=X_train, y=factor(y_train), ntree=500,
                         mtry=max(1L, floor(sqrt(ncol(X_train)))),
                         classwt=c(`0`=1, `1`=n_neg/n_pos), importance=FALSE)
rf_prob <- function(newdata) predict(rf_model, newdata=newdata, type="prob")[,"1"]
p_val_rf  <- rf_prob(X_val)
p_test_rf <- rf_prob(X_test)
print_sweep("Random Forest", p_val_rf, p_test_rf, y_val, y_test, opt_thresh=0.16)

cat("==============================================\n")
cat("Phase 11 complete.\n")
cat("==============================================\n")
