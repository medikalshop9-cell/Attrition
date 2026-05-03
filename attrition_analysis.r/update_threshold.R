setwd("C:/Users/ayhin/Desktop/Attrition/attrition_analysis.r")
obj <- readRDS("plumber_api/model_final.rds")
obj[["threshold"]] <- 0.26
saveRDS(obj, "plumber_api/model_final.rds")
library(jsonlite)
j <- fromJSON("plumber_api/insights_report.json")
j[["threshold"]] <- 0.26
writeLines(toJSON(j, auto_unbox=TRUE, pretty=TRUE), "plumber_api/insights_report.json")
cat("Done: threshold updated to 0.26\n")
