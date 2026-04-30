# =============================================================================
# Phase 2: Exploratory Data Analysis (EDA)
# Project: Predicting Employee Attrition Using AI
# Uses: Raw CSV (original column names — for readable visualizations)
#       EDA is done on the FULL dataset (before split) — for exploration only.
#       No model decisions are made here. Train/test split is untouched.
# Output: Plots saved to ../outputs/plots/
# =============================================================================

library(dplyr)
library(ggplot2)

# =============================================================================
# Setup
# =============================================================================

# Create output directory if it doesn't exist
plot_dir <- "../outputs/plots"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
  cat("Created output directory:", plot_dir, "\n")
}

save_plot <- function(filename, width = 8, height = 5) {
  path <- file.path(plot_dir, filename)
  ggsave(path, width = width, height = height, dpi = 150)
  cat("Saved:", path, "\n")
}

# Load original (raw) CSV — use original column names for readability
df_raw <- read.csv(
  "WA_Fn-UseC_-HR-Employee-Attrition.csv",
  stringsAsFactors = FALSE
)

# Make Attrition a factor for nicer ggplot grouping
df_raw$Attrition <- factor(df_raw$Attrition, levels = c("No", "Yes"))

cat("==============================================\n")
cat("PHASE 2: EXPLORATORY DATA ANALYSIS\n")
cat("==============================================\n")
cat("Dataset shape:", nrow(df_raw), "rows x", ncol(df_raw), "cols\n")
cat(sprintf("Attrition rate: %.1f%% (%d of %d employees)\n",
    mean(df_raw$Attrition == "Yes") * 100,
    sum(df_raw$Attrition == "Yes"),
    nrow(df_raw)))
cat("\n")

# =============================================================================
# PLOT 1: Attrition Distribution (overview)
# =============================================================================

cat("Generating Plot 1: Attrition Distribution...\n")

attrition_counts <- df_raw %>%
  count(Attrition) %>%
  mutate(pct = n / sum(n) * 100,
         label = sprintf("%d\n(%.1f%%)", n, pct))

ggplot(attrition_counts, aes(x = Attrition, y = n, fill = Attrition)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = label), vjust = -0.3, size = 4.5) +
  scale_fill_manual(values = c("No" = "#2196F3", "Yes" = "#F44336")) +
  labs(
    title = "Employee Attrition Distribution",
    subtitle = "Target variable: Yes = left, No = stayed",
    x = "Attrition", y = "Number of Employees"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

save_plot("01_attrition_distribution.png")

# =============================================================================
# PLOT 2: Age Distribution by Attrition
# =============================================================================

cat("Generating Plot 2: Age vs Attrition...\n")

ggplot(df_raw, aes(x = Age, fill = Attrition)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("No" = "#2196F3", "Yes" = "#F44336")) +
  labs(
    title = "Age Distribution by Attrition",
    subtitle = "Younger employees tend to have higher attrition",
    x = "Age", y = "Density", fill = "Attrition"
  ) +
  theme_minimal(base_size = 13)

save_plot("02_age_vs_attrition.png")

# =============================================================================
# PLOT 3: Monthly Income by Attrition
# =============================================================================

cat("Generating Plot 3: Monthly Income vs Attrition...\n")

ggplot(df_raw, aes(x = Attrition, y = MonthlyIncome, fill = Attrition)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = c("No" = "#2196F3", "Yes" = "#F44336")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Monthly Income by Attrition",
    subtitle = "Employees who left earned less on average",
    x = "Attrition", y = "Monthly Income (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

save_plot("03_income_vs_attrition.png")

# =============================================================================
# PLOT 4: Attrition Rate by Department
# =============================================================================

cat("Generating Plot 4: Attrition by Department...\n")

dept_summary <- df_raw %>%
  group_by(Department, Attrition) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Department) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(dept_summary, aes(x = Department, y = pct, fill = Attrition)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("No" = "#2196F3", "Yes" = "#F44336")) +
  labs(
    title = "Attrition Rate by Department",
    subtitle = "Sales has the highest attrition proportion",
    x = "Department", y = "Proportion", fill = "Attrition"
  ) +
  theme_minimal(base_size = 13)

save_plot("04_attrition_by_department.png")

# =============================================================================
# PLOT 5: OverTime vs Attrition
# =============================================================================

cat("Generating Plot 5: OverTime vs Attrition...\n")

overtime_summary <- df_raw %>%
  group_by(OverTime, Attrition) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(OverTime) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(overtime_summary, aes(x = OverTime, y = pct, fill = Attrition)) +
  geom_col(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("No" = "#2196F3", "Yes" = "#F44336")) +
  labs(
    title = "Attrition Rate by OverTime Status",
    subtitle = "Employees working overtime leave at significantly higher rates",
    x = "OverTime", y = "Proportion", fill = "Attrition"
  ) +
  theme_minimal(base_size = 13)

save_plot("05_overtime_vs_attrition.png")

# =============================================================================
# PLOT 6: Job Satisfaction by Attrition
# =============================================================================

cat("Generating Plot 6: Job Satisfaction vs Attrition...\n")

jsat_summary <- df_raw %>%
  group_by(JobSatisfaction, Attrition) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(JobSatisfaction) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(jsat_summary, aes(x = factor(JobSatisfaction), y = pct, fill = Attrition)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("No" = "#2196F3", "Yes" = "#F44336")) +
  labs(
    title = "Attrition Rate by Job Satisfaction",
    subtitle = "1 = Low, 2 = Medium, 3 = High, 4 = Very High",
    x = "Job Satisfaction Level", y = "Proportion", fill = "Attrition"
  ) +
  theme_minimal(base_size = 13)

save_plot("06_jobsatisfaction_vs_attrition.png")

# =============================================================================
# PLOT 7: Years at Company by Attrition
# =============================================================================

cat("Generating Plot 7: Years at Company vs Attrition...\n")

ggplot(df_raw, aes(x = Attrition, y = YearsAtCompany, fill = Attrition)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = c("No" = "#2196F3", "Yes" = "#F44336")) +
  labs(
    title = "Years at Company by Attrition",
    subtitle = "Employees who left had shorter tenures on average",
    x = "Attrition", y = "Years at Company"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

save_plot("07_tenure_vs_attrition.png")

# =============================================================================
# PLOT 8: Distance from Home by Attrition
# =============================================================================

cat("Generating Plot 8: Distance from Home vs Attrition...\n")

ggplot(df_raw, aes(x = DistanceFromHome, fill = Attrition)) +
  geom_histogram(binwidth = 3, position = "identity", alpha = 0.6) +
  scale_fill_manual(values = c("No" = "#2196F3", "Yes" = "#F44336")) +
  labs(
    title = "Distance from Home by Attrition",
    subtitle = "Longer commutes correlate with higher attrition",
    x = "Distance from Home (km)", y = "Count", fill = "Attrition"
  ) +
  theme_minimal(base_size = 13)

save_plot("08_distance_vs_attrition.png")

# =============================================================================
# PLOT 9: Attrition Rate by Job Role
# =============================================================================

cat("Generating Plot 9: Attrition by Job Role...\n")

role_summary <- df_raw %>%
  group_by(JobRole) %>%
  summarise(
    total = n(),
    left  = sum(Attrition == "Yes"),
    rate  = left / total * 100
  ) %>%
  arrange(desc(rate))

ggplot(role_summary, aes(x = reorder(JobRole, rate), y = rate, fill = rate)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#90CAF9", high = "#B71C1C") +
  labs(
    title = "Attrition Rate by Job Role",
    subtitle = "Sales Representatives have the highest attrition rate",
    x = "Job Role", y = "Attrition Rate (%)", fill = "Rate (%)"
  ) +
  theme_minimal(base_size = 12)

save_plot("09_attrition_by_jobrole.png", width = 9, height = 5)

# =============================================================================
# PLOT 10: Work-Life Balance by Attrition
# =============================================================================

cat("Generating Plot 10: Work-Life Balance vs Attrition...\n")

wlb_summary <- df_raw %>%
  group_by(WorkLifeBalance, Attrition) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(WorkLifeBalance) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(wlb_summary, aes(x = factor(WorkLifeBalance), y = pct, fill = Attrition)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("No" = "#2196F3", "Yes" = "#F44336")) +
  labs(
    title = "Attrition Rate by Work-Life Balance",
    subtitle = "1 = Bad, 2 = Good, 3 = Better, 4 = Best",
    x = "Work-Life Balance Level", y = "Proportion", fill = "Attrition"
  ) +
  theme_minimal(base_size = 13)

save_plot("10_worklifebalance_vs_attrition.png")

# =============================================================================
# SUMMARY: KEY FINDINGS
# =============================================================================

cat("\n==============================================\n")
cat("PHASE 2 EDA — KEY FINDINGS\n")
cat("==============================================\n")

# Attrition rate by overtime
ot <- df_raw %>% group_by(OverTime) %>%
  summarise(rate = mean(Attrition == "Yes") * 100)
cat(sprintf("OverTime attrition rate:     Yes=%.1f%%  No=%.1f%%\n",
    ot$rate[ot$OverTime == "Yes"], ot$rate[ot$OverTime == "No"]))

# Attrition rate by department
dept <- df_raw %>% group_by(Department) %>%
  summarise(rate = mean(Attrition == "Yes") * 100) %>% arrange(desc(rate))
cat("Attrition rate by department:\n")
for (i in seq_len(nrow(dept))) {
  cat(sprintf("  %-30s %.1f%%\n", dept$Department[i], dept$rate[i]))
}

# Median income comparison
inc <- df_raw %>% group_by(Attrition) %>%
  summarise(median_income = median(MonthlyIncome))
cat(sprintf("Median income — Stayed: $%s  Left: $%s\n",
    format(inc$median_income[inc$Attrition == "No"], big.mark = ","),
    format(inc$median_income[inc$Attrition == "Yes"], big.mark = ",")))

# Top attrition job role
top_role <- role_summary$JobRole[1]
top_rate  <- role_summary$rate[1]
cat(sprintf("Highest attrition role: %s (%.1f%%)\n", top_role, top_rate))

cat("\n10 plots saved to:", normalizePath(plot_dir), "\n")
cat("==============================================\n")
cat("Phase 2 complete. Ready for Phase 3: Feature Engineering\n")
