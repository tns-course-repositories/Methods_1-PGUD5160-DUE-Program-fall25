#c10_lab10-script-1.R

##########################################
#Lab 10 | Part I — Correlation and Seasonality: NYPD Arrests by Season
##########################################

# Packages used in this lab
# install.packages(c("tidyverse","lubridate","scales","ggcorrplot","corrplot"))
library(tidyverse)
library(lubridate)
library(scales)
library(ggcorrplot)  


# Read input data
ar <- read_csv("~/Desktop/c10_lab/data/NYPD_Arrests_Data_(Historic)_20251027.csv")

# Basic structure check
glimpse(ar)

# Mutate for new column names and new seasons variable
ar_clean <- ar |>
  mutate(ARREST_DATE = mdy(ARREST_DATE),
         month = month(ARREST_DATE, label = TRUE, abbr = TRUE),
         month_num = month(ARREST_DATE),
         season = case_when(
           month_num %in% c(12,1,2) ~ "Winter",
           month_num %in% c(3,4,5) ~ "Spring",
           month_num %in% c(6,7,8) ~ "Summer",
           TRUE ~ "Fall"
         )) |>
  drop_na(ARREST_DATE, month_num)

# Count Arrests by Month
ar_monthly <- ar_clean |>
  count(month_num, month, season, name = "arrests")

head(ar_monthly)

# Visualize Monthly Pattern
p_month <- ggplot(ar_monthly, aes(month, arrests, group = 1)) +
  geom_line(linewidth = 0.9, color = "steelblue") +
  geom_point(color = "black") +
  labs(title = "Monthly Arrest Totals — NYPD (2019-2024)",
       x = NULL, y = "Number of Arrests") +
  theme_minimal(base_size = 13)

print(p_month)

# Correlation with Month Number(1-12)
r_pearson  <- cor(ar_monthly$month_num, ar_monthly$arrests, method = "pearson")
r_spearman <- cor(ar_monthly$month_num, ar_monthly$arrests, method = "spearman")

r_pearson; r_spearman

# P-values and Confidence Intervals
pearson_test  <- cor.test(ar_monthly$month_num, ar_monthly$arrests, method = "pearson")
spearman_test <- cor.test(ar_monthly$month_num, ar_monthly$arrests, method = "spearman", exact = FALSE)

cat("\n--- Pearson test ---\n")
cat("r =", round(pearson_test$estimate, 3),
    "| 95% CI [", round(pearson_test$conf.int[1], 3), ",", round(pearson_test$conf.int[2], 3), "]",
    "| p =", signif(pearson_test$p.value, 3), "\n")

cat("\n--- Spearman test ---\n")
cat("rho =", round(spearman_test$estimate, 3),
    "| p =", signif(spearman_test$p.value, 3), "\n")

# Reprint monthly plot with correlation results in subtitle
p_month_annot <- ggplot(ar_monthly, aes(month, arrests, group = 1)) +
  geom_line(linewidth = 0.9, color = "steelblue") +
  geom_point(color = "black") +
  labs(
    title    = "Monthly Arrest Totals — NYPD (2019-2024)",
    subtitle = paste0("Pearson r = ", round(r_pearson, 2),
                      " | Spearman ρ = ", round(r_spearman, 2),
                      " | p_Pearson = ", signif(pearson_test$p.value, 3),
                      " | p_Spearman = ", signif(spearman_test$p.value, 3)),
    x = NULL, y = "Number of Arrests"
  ) +
  theme_minimal(base_size = 13)

print(p_month_annot)

# -------------------- Correlation Matrix & Corrplots --------------------
# Matrix is 2x2 here (month_num, arrests); useful to show both Pearson and Spearman.
vars <- ar_monthly %>% select(month_num, arrests)

M_pearson  <- cor(vars, use = "pairwise.complete.obs", method = "pearson")
M_spearman <- cor(vars, use = "pairwise.complete.obs", method = "spearman")

print(M_pearson)
print(M_spearman)

# ggcorrplot (ggplot2 style)
ggcorrplot(M_pearson,  lab = TRUE, type = "lower", title = "Correlation Matrix (Pearson)")
ggcorrplot(M_spearman, lab = TRUE, type = "lower", title = "Correlation Matrix (Spearman)")

# Aggregate by Season + Visualization by Season
ar_season <- ar_clean |>
  count(season, name = "arrests") |>
  mutate(season = fct_relevel(season, "Winter","Spring","Summer","Fall"))

print(ar_season)

ggplot(ar_season, aes(season, arrests, fill = season)) +
  geom_col(color = "white") +
  scale_fill_brewer(palette = "Set3", guide = "none") +
  labs(title = "NYPD Arrests by Season (2019 - 2025)",
       x = NULL, y = "Arrests") +
  theme_minimal(base_size = 13)

##########################################
#Lab 10 | Part II — ANOVA Season-Based Comparison Test 
##########################################

# Goal: Instead of correlating numeric month values, test whether
# mean arrests differ significantly between the 4 seasonal groups.

# We'll use the original cleaned data (ar_clean) so that each arrest event
# contributes to its season, not just aggregated totals.

# Create a season-level summary (mean arrests per day, to normalize slightly)
ar_daily <- ar_clean %>%
  count(ARREST_DATE, season, name = "arrests")

# Check data distribution visually
ggplot(ar_daily, aes(x = season, y = arrests, fill = season)) +
  geom_boxplot(color = "gray30", alpha = 0.8) +
  scale_fill_brewer(palette = "Set3", guide = "none") +
  labs(title = "Distribution of Daily Arrests by Season",
       x = NULL, y = "Daily Arrest Count") +
  theme_minimal(base_size = 13)

# Filter and Explore Winter Outlier
outliers <- ar_daily %>%
  group_by(season) %>%
  mutate(Q1 = quantile(arrests, 0.25, na.rm = TRUE),
         Q3 = quantile(arrests, 0.75, na.rm = TRUE),
         IQR = Q3 - Q1,
         lower = Q1 - 1.5*IQR,
         upper = Q3 + 1.5*IQR,
         is_outlier = arrests < lower | arrests > upper) %>%
  filter(season == "Winter", is_outlier) %>%
  arrange(desc(arrests))

outliers

# ANOVA TEST
anova_result <- aov(arrests ~ season, data = ar_daily)
summary(anova_result)







