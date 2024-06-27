# Set directory, create dataframe and attach data
setwd("C:/Users/Suzanne van Esch/PycharmProjects/pythonProject/output")
WOFOST73_CWB_potato <- read.csv("merged_final_output_timing_R.csv")
attach(WOFOST73_CWB_potato)
class(WOFOST73_CWB_potato)
WOFOST73_CWB_potato$scenario <- as.factor(WOFOST73_CWB_potato$scenario)
WOFOST73_CWB_potato$timing <- as.factor(WOFOST73_CWB_potato$timing)
WOFOST73_CWB_potato$compl_scenario <- as.factor(WOFOST73_CWB_potato$compl_scenario)


# Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(rstatix)
library(ggcorrplot)
library(cmstatr)
library(AICcmodavg)
library(ggpubr)
library(tidyr)
library(openxlsx)

# Create an index column to uniquely identify each row within each group
WOFOST73_CWB_potato <- WOFOST73_CWB_potato %>%
  group_by(scenario) %>%
  mutate(row_id = row_number()) %>%
  ungroup()

# Pivot the data to wide format 
WOFOST73_CWB_potato_wide_TWSO <- WOFOST73_CWB_potato %>%
  select(row_id, scenario, TWSO) %>%
  pivot_wider(names_from = scenario, values_from = TWSO)

WOFOST73_CWB_potato_wide_WP <- WOFOST73_CWB_potato %>%
  select(row_id, scenario, WP) %>%
  pivot_wider(names_from = scenario, values_from = WP)

WOFOST73_CWB_potato_wide_IRRTOT <- WOFOST73_CWB_potato %>%
  select(row_id, scenario, IRRTOT) %>%
  pivot_wider(names_from = scenario, values_from = IRRTOT)

WOFOST73_CWB_potato_wide_PRTOT <- WOFOST73_CWB_potato %>%
  select(row_id, scenario, PRTOT) %>%
  pivot_wider(names_from = scenario, values_from = PRTOT)

library(dplyr)

# Perform ANOVA for each scenario (groups: timing) and test normality of residuals
ks.test_TWSO_results <- WOFOST73_CWB_potato %>%
  group_by(scenario) %>%
  do({
    model <- aov(TWSO ~ timing, data = .)
    residuals <- residuals(model)
    ks_test <- ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
    data.frame(ks_statistic = ks_test$statistic, p_value = ks_test$p.value)
  })

ks.test_WP_results <- WOFOST73_CWB_potato %>%
  group_by(scenario) %>%
  do({
    model <- aov(WP ~ timing, data = .)
    residuals <- residuals(model)
    ks_test <- ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
    data.frame(ks_statistic = ks_test$statistic, p_value = ks_test$p.value)
  })

# Perform Bartlett test (groups: timing) 
bartlett_test_TWSO_results <- WOFOST73_CWB_potato %>%
  group_by(scenario) %>%
  do({
    bartlett_test <- bartlett.test(.$TWSO ~ .$timing, data = .)
    data.frame(
      scenario = .$scenario[1],  # Take the scenario name from the first row of the group
      p_value = bartlett_test$p.value
    )
  }) %>%
  ungroup() %>%
  bind_rows()

bartlett_test_WP_results <- WOFOST73_CWB_potato %>%
  group_by(scenario) %>%
  do({
    bartlett_test <- bartlett.test(.$WP ~ .$timing, data = .)
    data.frame(
      scenario = .$scenario[1],  # Take the scenario name from the first row of the group
      p_value = bartlett_test$p.value
    )
  }) %>%
  ungroup() %>%
  bind_rows()

library(tidyverse)
library(emmeans)

# Load required libraries
library(dplyr)
library(emmeans)
library(openxlsx)



# Define the scenarios you want to include in the analysis for TWSO
scenarios_TWSO <- c("Hd_2100_I", "Hd_2150_I", "Hn_2100_I", "Hn_2100_NI", "Hn_2150_NI",
                    "Ld_2100_I", "Ld_2100_NI", "Ln_2100_I", "Md_2050_I", "Md_2100_I",
                    "Mn_2050_I", "Mn_2100_I")
# Define the scenarios you want to include in the analysis for WP
scenarios_WP <- c("Hd_2050_I", "Hd_2050_NI", "Hd_2150_I", "Hn_2050_I", "Hn_2050_NI",
                  "Hn_2100_I", "Hn_2100_NI", "Hn_2150_I", "Hn_2150_NI", "Ld_2100_I",
                  "Ld_2100_NI", "Ln_2100_I", "Ln_2100_NI", "Md_2050_I", "Md_2100_I",
                  "Md_2100_NI", "Md_2150_I", "Md_2150_NI", "Mn_2050_I", "Mn_2100_I",
                  "Mn_2100_NI", "Mn_2150_I", "Mn_2150_NI")

# Filter the dataframe for TWSO and WP scenarios
filtered_TWSO <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_TWSO)

filtered_WP <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_WP)

# Function to perform ANOVA and Tukey HSD test
perform_ANOVA_Tukey <- function(data, response_var) {
  results <- data %>%
    group_by(scenario) %>%
    do({
      ANOVA <- aov(as.formula(paste(response_var, "~ timing")), data = .)
      Tukey <- TukeyHSD(ANOVA, conf.level = 0.95)
      Tukey_results <- as.data.frame(Tukey$timing)
      Tukey_results$comparison <- rownames(Tukey_results)
      Tukey_results$scenario <- .$scenario[1]  # Take the scenario name from the first row of the group
      Tukey_results
    }) %>%
    ungroup()
  
  return(results)
}

# Perform ANOVA and Tukey HSD test for TWSO
ANOVA_Tukey_results_TWSO <- perform_ANOVA_Tukey(filtered_TWSO, "TWSO")

# Perform ANOVA and Tukey HSD test for WP
ANOVA_Tukey_results_WP <- perform_ANOVA_Tukey(filtered_WP, "WP")

# Save TWSO results to an Excel file
output_file_TWSO <- "ANOVA_Tukey_results_TWSO.xlsx"
write.xlsx(ANOVA_Tukey_results_TWSO, file = output_file_TWSO, rowNames = FALSE)

# Save WP results to an Excel file
output_file_WP <- "ANOVA_Tukey_results_WP.xlsx"
write.xlsx(ANOVA_Tukey_results_WP, file = output_file_WP, rowNames = FALSE)

#Welch ANOVA with Games-Howell post hoc test

library(dplyr)
library(DescTools)
library(openxlsx)

# Define scenarios for TWSO and WP
scenarios_TWSO <- c("Md_2050_NI")

scenarios_WP <- c("Md_2050_NI", "Md_2100_NI")

# Filter the dataframe for TWSO and WP scenarios
filtered_TWSO <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_TWSO)

filtered_WP <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_WP)

# Function to perform Welch ANOVA and Games-Howell post hoc test
perform_ANOVA_GamesHowell <- function(data, response_var) {
  results <- data %>%
    group_by(scenario) %>%
    do({
      # Perform Welch ANOVA
      welch_anova <- oneway.test(as.formula(paste(response_var, "~ timing")), data = ., var.equal = FALSE)
      
      # Perform Games-Howell post hoc test
      gh_test <- games_howell_test(as.formula(paste(response_var, "~ timing")), data = .)
      
      # Extract and format results (p-values and estimates)
      gh_results <- data.frame(
        scenario = .$scenario[1],  # Take the scenario name from the first row of the group
        group1 = gh_test$group1,
        group2 = gh_test$group2,
        p_adj = gh_test$p.adj,
        estimate = gh_test$estimate
      )
      
      gh_results
    }) %>%
    ungroup()
  
  return(results)
}

# Perform Welch ANOVA and Games-Howell post hoc test for TWSO
ANOVA_GamesHowell_results_TWSO <- perform_ANOVA_GamesHowell(filtered_TWSO, "TWSO")

# Perform Welch ANOVA and Games-Howell post hoc test for WP
ANOVA_GamesHowell_results_WP <- perform_ANOVA_GamesHowell(filtered_WP, "WP")

# Save TWSO results to an Excel file
output_file_TWSO <- "ANOVA_GamesHowell_results_TWSO.xlsx"
write.xlsx(ANOVA_GamesHowell_results_TWSO, file = output_file_TWSO, rowNames = FALSE)

# Save WP results to an Excel file
output_file_WP <- "ANOVA_GamesHowell_results_WP.xlsx"
write.xlsx(ANOVA_GamesHowell_results_WP, file = output_file_WP, rowNames = FALSE)

# Load required libraries
library(dplyr)
library(dunn.test)
library(openxlsx)

# Define scenarios for TWSO and WP
scenarios_TWSO <- c("Hd_2050_I", "Hd_2050_NI", "Hd_2100_NI", "Hd_2150_NI",
                    "Hn_2050_I", "Hn_2050_NI", "Hn_2150_I", "Ln_2100_NI",
                    "Md_2100_NI", "Md_2150_I", "Md_2150_NI", "Mn_2050_NI",
                    "Mn_2100_NI", "Mn_2150_I", "Mn_2150_NI")

scenarios_WP <- c("Hd_2100_I", "Hd_2100_NI", "Hd_2150_NI")

# Filter the dataframe for TWSO and WP scenarios
filtered_TWSO <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_TWSO)

filtered_WP <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_WP)

# Function to perform Welch ANOVA and Games-Howell post hoc test
perform_ANOVA_GamesHowell <- function(data, response_var) {
  results <- data %>%
    group_by(scenario) %>%
    do({
      # Perform Welch ANOVA
      welch_anova <- oneway.test(as.formula(paste(response_var, "~ timing")), data = ., var.equal = FALSE)
      
      # Perform Games-Howell post hoc test
      gh_test <- games_howell_test(as.formula(paste(response_var, "~ timing")), data = .)
      
      # Extract and format results (p-values and estimates)
      gh_results <- data.frame(
        scenario = .$scenario[1],  # Take the scenario name from the first row of the group
        group1 = gh_test$group1,
        group2 = gh_test$group2,
        p_adj = gh_test$p.adj,
        estimate = gh_test$estimate
      )
      
      gh_results
    }) %>%
    ungroup()
  
  return(results)
}

# Perform Welch ANOVA and Games-Howell post hoc test for TWSO
ANOVA_GamesHowell_results_TWSO <- perform_ANOVA_GamesHowell(filtered_TWSO, "TWSO")

# Perform Welch ANOVA and Games-Howell post hoc test for WP
ANOVA_GamesHowell_results_WP <- perform_ANOVA_GamesHowell(filtered_WP, "WP")

# Save TWSO results to an Excel file
output_file_TWSO <- "ANOVA_GamesHowell_results_TWSO1.xlsx"
write.xlsx(ANOVA_GamesHowell_results_TWSO, file = output_file_TWSO, rowNames = FALSE)

# Save WP results to an Excel file
output_file_WP <- "ANOVA_GamesHowell_results_WP1.xlsx"
write.xlsx(ANOVA_GamesHowell_results_WP, file = output_file_WP, rowNames = FALSE)
