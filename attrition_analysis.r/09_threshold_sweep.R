library(dplyr)
setwd("C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r")

obj    <- readRDS("plumber_api/model_final.rds")
model  <- obj[["model"]]
test_s <- readRDS("test_data.rds")

drops <- c(
  "SatisfactionIndex", "JobSatisfaction", "EnvironmentSatisfaction",
  "RelationshipSatisfaction", "Department.Research & Development",
  "Department.Sales", "Department.Human Resources",
  "JobRole.Human Resources", "JobRole.Sales Executive",
  "JobRole.Sales Representative", "JobRole.Laboratory Technician",
  "JobRole.Research Scientist", "JobLevel", "TotalWorkingYears"
)
test_r <- test_s %>% select(-all_of(intersect(names(test_s), drops)))

probs  <- predict(model, newdata = test_r, type = "response")
labels <- test_r[["Attrition"]]
n      <- length(labels)

cat(sprintf("\n%-8s  %-9s  %-9s  %-9s  %-9s\n", "Thresh", "Precision", "Recall", "F1", "Accuracy"))
cat(strrep("-", 55), "\n")

best_f1  <- 0; best_t_f1  <- NA
best_rec <- 0; best_t_rec <- NA

for (t in seq(0.01, 0.99, by = 0.01)) {
  preds <- as.integer(probs >= t)
  tp <- sum(preds == 1 & labels == 1)
  fp <- sum(preds == 1 & labels == 0)
  fn <- sum(preds == 0 & labels == 1)
  tn <- sum(preds == 0 & labels == 0)
  acc  <- (tp + tn) / n
  prec <- if ((tp + fp) > 0) tp / (tp + fp) else NA
  rec  <- if ((tp + fn) > 0) tp / (tp + fn) else NA
  f1   <- if (!is.na(prec) && !is.na(rec) && (prec + rec) > 0) 2 * prec * rec / (prec + rec) else NA

  if (!is.na(f1)  && f1  > best_f1)  { best_f1  <- f1;  best_t_f1  <- t }
  if (!is.na(rec) && rec > best_rec) { best_rec <- rec; best_t_rec <- t }

  marker <- if (abs(t - 0.25) < 0.001) "  <- current" else ""

  cat(sprintf("%-8.2f  %-9s  %-9s  %-9s  %-9s%s\n",
    t,
    if (is.na(prec)) "   -   " else sprintf("%.3f", prec),
    if (is.na(rec))  "   -   " else sprintf("%.3f", rec),
    if (is.na(f1))   "   -   " else sprintf("%.3f", f1),
    sprintf("%.3f", acc),
    marker))
}

cat(strrep("-", 55), "\n")
cat(sprintf("Best F1    threshold: %.2f  (F1=%.3f)\n", best_t_f1,  best_f1))
cat(sprintf("Best Recall threshold: %.2f  (Recall=%.3f)\n", best_t_rec, best_rec))
