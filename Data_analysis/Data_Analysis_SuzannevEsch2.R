# Set directory, create dataframe and attach data
setwd("C:/Users/Suzanne van Esch/PycharmProjects/pythonProject/output")
WOFOST73_CWB_potato <- read.csv("merged_final_output_figures_R.csv")
attach(WOFOST73_CWB_potato)
class(WOFOST73_CWB_potato)
WOFOST73_CWB_potato$scenario <- as.factor(WOFOST73_CWB_potato$scenario)

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
library(patchwork)
library(svglite)
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
WOFOST73_CWB_potato_wide_IRRTOT <- WOFOST73_CWB_potato_wide_IRRTOT %>%
  select(-c(8:13, 20:25, 32:37, 44:49, 56:61, 68:73, 80:85, 87))

WOFOST73_CWB_potato_wide_PRTOT <- WOFOST73_CWB_potato %>%
  select(row_id, scenario, PRTOT) %>%
  pivot_wider(names_from = scenario, values_from = PRTOT)

# Function to perform Kolmogorov-Smirnov test for each column in a dataframe
ks_tests <- function(data) {
  # Initialize a list to store results
  ks_results <- list()
  
  # Loop through each column in the dataframe
  for (col_name in names(data)) {
    # Ensure the column is numeric
    if (is.numeric(data[[col_name]])) {
      # Calculate mean and standard deviation for the column
      col_mean <- mean(data[[col_name]], na.rm = TRUE)
      col_sd <- sd(data[[col_name]], na.rm = TRUE)
      
      # Perform Kolmogorov-Smirnov test against a normal distribution
      test_result <- ks.test(data[[col_name]], "pnorm", mean = col_mean, sd = col_sd)
      
      # Store the result in the list
      ks_results[[col_name]] <- test_result
    } else {
      # Store NA for non-numeric columns
      ks_results[[col_name]] <- list(statistic = NA, p.value = NA)
    }
  }
  
  # Convert the list of results to a dataframe
  results_df <- do.call(rbind, lapply(ks_results, function(x) {
    data.frame(statistic = x$statistic, p.value = x$p.value)
  }))
  
  # Add column names as row names
  rownames(results_df) <- names(ks_results)
  
  return(results_df)
}

# Example usage with your actual dataframe
# Perform Kolmogorov-Smirnov tests on your actual dataframe
ks_results_TWSO <- ks_tests(WOFOST73_CWB_potato_wide_TWSO)
ks_results_WP <- ks_tests(WOFOST73_CWB_potato_wide_WP)
ks_results_IRRTOT <- ks_tests(WOFOST73_CWB_potato_wide_IRRTOT)
ks_results_PRTOT <- ks_tests(WOFOST73_CWB_potato_wide_PRTOT)

# Function to filter results based on p-value threshold
filter_results <- function(results_df, threshold) {
  higher_equal <- results_df %>% filter(p.value >= threshold)
  lower <- results_df %>% filter(p.value < threshold)
  return(list(higher_equal = higher_equal, lower = lower))
}

# Define the p-value threshold
p_value_threshold <- 0.05

# Filter the KS test results based on the p-value threshold
filtered_TWSO <- filter_results(ks_results_TWSO, p_value_threshold)
filtered_WP <- filter_results(ks_results_WP, p_value_threshold)
filtered_IRRTOT <- filter_results(ks_results_IRRTOT, p_value_threshold)
filtered_PRTOT <- filter_results(ks_results_PRTOT, p_value_threshold)

# Combine all filtered results into a single dataframe
filtered_results <- bind_rows(
  mutate(filtered_TWSO$higher_equal, variable = "TWSO", p_value_category = ">= 0.05"),
  mutate(filtered_TWSO$lower, variable = "TWSO", p_value_category = "< 0.05"),
  mutate(filtered_WP$higher_equal, variable = "WP", p_value_category = ">= 0.05"),
  mutate(filtered_WP$lower, variable = "WP", p_value_category = "< 0.05"),
  mutate(filtered_IRRTOT$higher_equal, variable = "IRRTOT", p_value_category = ">= 0.05"),
  mutate(filtered_IRRTOT$lower, variable = "IRRTOT", p_value_category = "< 0.05"),
  mutate(filtered_PRTOT$higher_equal, variable = "PRTOT", p_value_category = ">= 0.05"),
  mutate(filtered_PRTOT$lower, variable = "PRTOT", p_value_category = "< 0.05")
)

# Print the combined filtered results dataframe
print(filtered_results)

# Create different dataframes based on distribution

# Filter scenarios and create dataframes for TWSO
normal_scenarios_TWSO <- rownames(ks_results_TWSO)[ks_results_TWSO$p.value >= 0.05]
not_normal_scenarios_TWSO <- rownames(ks_results_TWSO)[ks_results_TWSO$p.value < 0.05]

normal_WOFOST73_CWB_potato_wide_TWSO <- WOFOST73_CWB_potato_wide_TWSO %>%
  select(all_of(normal_scenarios_TWSO))
Wilcoxon_WOFOST73_CWB_potato_wide_TWSO <- WOFOST73_CWB_potato_wide_TWSO %>%
  select(all_of(not_normal_scenarios_TWSO))

# Filter scenarios and create dataframes for WP
normal_scenarios_WP <- rownames(ks_results_WP)[ks_results_WP$p.value >= 0.05]
not_normal_scenarios_WP <- rownames(ks_results_WP)[ks_results_WP$p.value < 0.05]

normal_WOFOST73_CWB_potato_wide_WP <- WOFOST73_CWB_potato_wide_WP %>%
  select(all_of(normal_scenarios_WP))
Wilcoxon_WOFOST73_CWB_potato_wide_WP <- WOFOST73_CWB_potato_wide_WP %>%
  select(all_of(not_normal_scenarios_WP))

# Filter scenarios and create dataframes for IRRTOT
normal_scenarios_IRRTOT <- rownames(ks_results_IRRTOT)[ks_results_IRRTOT$p.value >= 0.05]
not_normal_scenarios_IRRTOT <- rownames(ks_results_IRRTOT)[ks_results_IRRTOT$p.value < 0.05]

normal_WOFOST73_CWB_potato_wide_IRRTOT <- WOFOST73_CWB_potato_wide_IRRTOT %>%
  select(all_of(normal_scenarios_IRRTOT))
Wilcoxon_WOFOST73_CWB_potato_wide_IRRTOT <- WOFOST73_CWB_potato_wide_IRRTOT %>%
  select(all_of(not_normal_scenarios_IRRTOT))

# Filter scenarios and create dataframes for PRTOT
normal_scenarios_PRTOT <- rownames(ks_results_PRTOT)[ks_results_PRTOT$p.value >= 0.05]
not_normal_scenarios_PRTOT <- rownames(ks_results_PRTOT)[ks_results_PRTOT$p.value < 0.05]

normal_WOFOST73_CWB_potato_wide_PRTOT <- WOFOST73_CWB_potato_wide_PRTOT %>%
  select(all_of(normal_scenarios_PRTOT))
Wilcoxon_WOFOST73_CWB_potato_wide_PRTOT <- WOFOST73_CWB_potato_wide_PRTOT %>%
  select(all_of(not_normal_scenarios_PRTOT))

column_names_TWSO <- names(normal_WOFOST73_CWB_potato_wide_TWSO)
column_names_WP <- names(normal_WOFOST73_CWB_potato_wide_WP)
column_names_IRRTOT <- names(normal_WOFOST73_CWB_potato_wide_IRRTOT)
column_names_PRTOT <- names(normal_WOFOST73_CWB_potato_wide_PRTOT)

print(column_names_TWSO)
print(column_names_WP)
print(column_names_IRRTOT)
print(column_names_IRRTOT)

# Function to perform Bartlett test for each variable comparing scenarios
bartlett_tests <- function(data, variable_names, ref_scenario1, ref_scenario2, p_value_threshold = 0.05) {
  # Initialize an empty dataframe to store results
  results_df <- data.frame()
  
  # Loop through each variable name
  for (variable_name in variable_names) {
    # Select scenario and the current variable
    variable_data <- data %>%
      select(scenario, {{ variable_name }}) %>%
      drop_na({{ variable_name }})  # Drop rows with NA for the current variable
    
    # Filter data for reference scenarios
    ref1_data <- variable_data %>%
      filter(scenario == ref_scenario1) %>%
      pull({{ variable_name }})
    
    ref2_data <- variable_data %>%
      filter(scenario == ref_scenario2) %>%
      pull({{ variable_name }})
    
    # Perform Bartlett test between reference_scenario1 and reference_scenario2
    test_result <- bartlett.test(list(ref1_data, ref2_data))
    bartlett_results_ref <- test_result$p.value
    
    # Filter data for other scenarios
    other_scenarios_data <- variable_data %>%
      filter(scenario != ref_scenario1 & scenario != ref_scenario2)
    
    # Initialize an empty list to store results
    bartlett_results <- list()
    
    # Compare each scenario against reference_scenario1
    for (scenario in unique(other_scenarios_data$scenario)) {
      # Extract data for the current scenario
      scenario_data <- other_scenarios_data %>%
        filter(scenario == {{ scenario }}) %>%
        pull({{ variable_name }})
      
      # Perform Bartlett test between ref1_data and scenario_data
      test_result <- bartlett.test(list(ref1_data, scenario_data))
      
      # Store the p-value in the list
      bartlett_results[[paste0("vs_", scenario, "_ref1")]] <- test_result$p.value
    }
    
    # Compare each scenario against reference_scenario2
    for (scenario in unique(other_scenarios_data$scenario)) {
      # Extract data for the current scenario
      scenario_data <- other_scenarios_data %>%
        filter(scenario == {{ scenario }}) %>%
        pull({{ variable_name }})
      
      # Perform Bartlett test between ref2_data and scenario_data
      test_result <- bartlett.test(list(ref2_data, scenario_data))
      
      # Store the p-value in the list
      bartlett_results[[paste0("vs_", scenario, "_ref2")]] <- test_result$p.value
    }
    
    # Convert the list to a dataframe
    results_df_variable <- data.frame(
      scenario_comparison = c(names(bartlett_results), "ref1_vs_ref2"),
      bartlett_pvalue = c(unlist(bartlett_results), bartlett_results_ref),
      variable = {{ variable_name }},
      reference1 = ref_scenario1,
      reference2 = ref_scenario2
    )
    
    # Append results for the current variable to results_df
    results_df <- bind_rows(results_df, results_df_variable)
  }
  
  # Filter results based on p-value threshold
  results_above_threshold <- results_df %>% filter(bartlett_pvalue >= p_value_threshold)
  results_below_threshold <- results_df %>% filter(bartlett_pvalue < p_value_threshold)
  
  # Return the two dataframes
  return(list(above_threshold = results_above_threshold, below_threshold = results_below_threshold))
}

# Specify variable names for which Bartlett test should be performed
variable_names <- c("TWSO", "WP", "IRRTOT", "PRTOT")

# Specify reference scenarios
reference_scenario1 <- "1991-2020_Reference_I_medium"
reference_scenario2 <- "1991-2020_Reference_NI_medium"

# Perform Bartlett tests on the specified variables
bartlett_results <- bartlett_tests(WOFOST73_CWB_potato, variable_names, reference_scenario1, reference_scenario2)

# Extract the dataframes for p-values above and below the threshold
results_above_threshold <- bartlett_results$above_threshold
results_below_threshold <- bartlett_results$below_threshold

# Print the dataframes
print("Results with p-values >= 0.05")
print(results_above_threshold)

print("Results with p-values < 0.05")
print(results_below_threshold)


# Function to perform independent two-tailed t-test for each variable comparing scenarios
ttest_scenarios <- function(data, variable_names, ref_scenario1, ref_scenario2) {
  # Initialize an empty dataframe to store results
  results_df <- data.frame()
  
  # Loop through each variable name
  for (variable_name in variable_names) {
    # Select scenario and the current variable
    variable_data <- data %>%
      select(scenario, {{ variable_name }}) %>%
      drop_na({{ variable_name }})  # Drop rows with NA for the current variable
    
    # Filter data for reference scenarios
    ref1_data <- variable_data %>%
      filter(scenario == ref_scenario1) %>%
      pull({{ variable_name }})
    
    ref2_data <- variable_data %>%
      filter(scenario == ref_scenario2) %>%
      pull({{ variable_name }})
    
    # Perform t-test between reference_scenario1 and reference_scenario2
    test_result <- t.test(ref1_data, ref2_data, var.equal = TRUE)
    ttest_results_ref <- test_result$p.value
    
    # Filter data for other scenarios
    other_scenarios_data <- variable_data %>%
      filter(scenario != ref_scenario1 & scenario != ref_scenario2)
    
    # Initialize an empty list to store results
    ttest_results <- list()
    
    # Compare each scenario against reference_scenario1
    for (scenario in unique(other_scenarios_data$scenario)) {
      # Extract data for the current scenario
      scenario_data <- other_scenarios_data %>%
        filter(scenario == {{ scenario }}) %>%
        pull({{ variable_name }})
      
      # Perform t-test between ref1_data and scenario_data
      test_result <- t.test(ref1_data, scenario_data, var.equal = TRUE)
      
      # Store the p-value in the list
      ttest_results[[paste0("vs_", scenario, "_ref1")]] <- test_result$p.value
    }
    
    # Compare each scenario against reference_scenario2
    for (scenario in unique(other_scenarios_data$scenario)) {
      # Extract data for the current scenario
      scenario_data <- other_scenarios_data %>%
        filter(scenario == {{ scenario }}) %>%
        pull({{ variable_name }})
      
      # Perform t-test between ref2_data and scenario_data
      test_result <- t.test(ref2_data, scenario_data, var.equal = TRUE)
      
      # Store the p-value in the list
      ttest_results[[paste0("vs_", scenario, "_ref2")]] <- test_result$p.value
    }
    
    # Convert the list to a dataframe
    results_df_variable <- data.frame(
      scenario_comparison = c(names(ttest_results), "ref1_vs_ref2"),
      ttest_pvalue = c(unlist(ttest_results), ttest_results_ref),
      variable = {{ variable_name }},
      reference1 = ref_scenario1,
      reference2 = ref_scenario2
    )
    
    # Append results for the current variable to results_df
    results_df <- bind_rows(results_df, results_df_variable)
  }
  
  # Return the dataframe of results
  return(results_df)
}

# Specify variable names for which t-tests should be performed
variable_names <- c("TWSO", "WP", "IRRTOT", "PRTOT")

# Specify reference scenarios
reference_scenario1 <- "1991-2020_Reference_I_medium"
reference_scenario2 <- "1991-2020_Reference_NI_medium"

# Perform t-tests on the specified variables
ttest_results <- ttest_scenarios(WOFOST73_CWB_potato, variable_names, reference_scenario1, reference_scenario2)

# Print the dataframe of t-test results
print(ttest_results)

# Function to perform Welch test for each variable comparing scenarios
welch_tests <- function(data, variable_names, ref_scenario1, ref_scenario2) {
  # Initialize an empty dataframe to store results
  results_df <- data.frame()
  
  # Loop through each variable name
  for (variable_name in variable_names) {
    # Select scenario and the current variable
    variable_data <- data %>%
      select(scenario, {{ variable_name }}) %>%
      drop_na({{ variable_name }})  # Drop rows with NA for the current variable
    
    # Filter data for reference scenarios
    ref1_data <- variable_data %>%
      filter(scenario == ref_scenario1) %>%
      pull({{ variable_name }})
    
    ref2_data <- variable_data %>%
      filter(scenario == ref_scenario2) %>%
      pull({{ variable_name }})
    
    # Perform Welch test between reference_scenario1 and reference_scenario2
    test_result <- t.test(ref1_data, ref2_data, var.equal = FALSE)
    welch_results_ref <- test_result$p.value
    
    # Filter data for other scenarios
    other_scenarios_data <- variable_data %>%
      filter(scenario != ref_scenario1 & scenario != ref_scenario2)
    
    # Initialize an empty list to store results
    welch_results <- list()
    
    # Compare each scenario against reference_scenario1
    for (scenario in unique(other_scenarios_data$scenario)) {
      # Extract data for the current scenario
      scenario_data <- other_scenarios_data %>%
        filter(scenario == {{ scenario }}) %>%
        pull({{ variable_name }})
      
      # Perform Welch test between ref1_data and scenario_data
      test_result <- t.test(ref1_data, scenario_data, var.equal = FALSE)
      
      # Store the p-value in the list
      welch_results[[paste0("vs_", scenario, "_ref1")]] <- test_result$p.value
    }
    
    # Compare each scenario against reference_scenario2
    for (scenario in unique(other_scenarios_data$scenario)) {
      # Extract data for the current scenario
      scenario_data <- other_scenarios_data %>%
        filter(scenario == {{ scenario }}) %>%
        pull({{ variable_name }})
      
      # Perform Welch test between ref2_data and scenario_data
      test_result <- t.test(ref2_data, scenario_data, var.equal = FALSE)
      
      # Store the p-value in the list
      welch_results[[paste0("vs_", scenario, "_ref2")]] <- test_result$p.value
    }
    
    # Convert the list to a dataframe
    results_df_variable <- data.frame(
      scenario_comparison = c(names(welch_results), "ref1_vs_ref2"),
      welch_pvalue = c(unlist(welch_results), welch_results_ref),
      variable = {{ variable_name }},
      reference1 = ref_scenario1,
      reference2 = ref_scenario2
    )
    
    # Append results for the current variable to results_df
    results_df <- bind_rows(results_df, results_df_variable)
  }
  
  # Return the dataframe of results
  return(results_df)
}

# Specify variable names for which Welch tests should be performed
variable_names <- c("TWSO", "WP", "IRRTOT", "PRTOT")

# Specify reference scenarios
reference_scenario1 <- "1991-2020_Reference_I_medium"
reference_scenario2 <- "1991-2020_Reference_NI_medium"

# Perform Welch tests on the specified variables
welch_results <- welch_tests(WOFOST73_CWB_potato, variable_names, reference_scenario1, reference_scenario2)

# Print the dataframe of Welch test results
print(welch_results)

# Function to perform Wilcoxon test for each variable comparing scenarios
wilcoxon_tests <- function(data, variable_names, ref_scenario1, ref_scenario2) {
  # Initialize an empty dataframe to store results
  results_df <- data.frame()
  
  # Loop through each variable name
  for (variable_name in variable_names) {
    # Select scenario and the current variable
    variable_data <- data %>%
      select(scenario, {{ variable_name }}) %>%
      drop_na({{ variable_name }})  # Drop rows with NA for the current variable
    
    # Filter data for reference scenarios
    ref1_data <- variable_data %>%
      filter(scenario == ref_scenario1) %>%
      pull({{ variable_name }})
    
    ref2_data <- variable_data %>%
      filter(scenario == ref_scenario2) %>%
      pull({{ variable_name }})
    
    # Perform Wilcoxon test between reference_scenario1 and reference_scenario2
    test_result <- wilcox.test(ref1_data, ref2_data)
    wilcoxon_results_ref <- test_result$p.value
    
    # Filter data for other scenarios
    other_scenarios_data <- variable_data %>%
      filter(scenario != ref_scenario1 & scenario != ref_scenario2)
    
    # Initialize an empty list to store results
    wilcoxon_results <- list()
    
    # Compare each scenario against reference_scenario1
    for (scenario in unique(other_scenarios_data$scenario)) {
      # Extract data for the current scenario
      scenario_data <- other_scenarios_data %>%
        filter(scenario == {{ scenario }}) %>%
        pull({{ variable_name }})
      
      # Perform Wilcoxon test between ref1_data and scenario_data
      test_result <- wilcox.test(ref1_data, scenario_data)
      
      # Store the p-value in the list
      wilcoxon_results[[paste0("vs_", scenario, "_ref1")]] <- test_result$p.value
    }
    
    # Compare each scenario against reference_scenario2
    for (scenario in unique(other_scenarios_data$scenario)) {
      # Extract data for the current scenario
      scenario_data <- other_scenarios_data %>%
        filter(scenario == {{ scenario }}) %>%
        pull({{ variable_name }})
      
      # Perform Wilcoxon test between ref2_data and scenario_data
      test_result <- wilcox.test(ref2_data, scenario_data)
      
      # Store the p-value in the list
      wilcoxon_results[[paste0("vs_", scenario, "_ref2")]] <- test_result$p.value
    }
    
    # Convert the list to a dataframe
    results_df_variable <- data.frame(
      scenario_comparison = c(names(wilcoxon_results), "ref1_vs_ref2"),
      wilcoxon_pvalue = c(unlist(wilcoxon_results), wilcoxon_results_ref),
      variable = {{ variable_name }},
      reference1 = ref_scenario1,
      reference2 = ref_scenario2
    )
    
    # Append results for the current variable to results_df
    results_df <- bind_rows(results_df, results_df_variable)
  }
  
  # Return the dataframe of results
  return(results_df)
}

# Specify variable names for which Wilcoxon tests should be performed
variable_names <- c("TWSO", "WP", "IRRTOT", "PRTOT")

# Specify reference scenarios
reference_scenario1 <- "1991-2020_Reference_I_medium"
reference_scenario2 <- "1991-2020_Reference_NI_medium"

# Perform Wilcoxon tests on the specified variables
wilcoxon_results <- wilcoxon_tests(WOFOST73_CWB_potato, variable_names, reference_scenario1, reference_scenario2)

# Print the dataframe of Wilcoxon test results
print(wilcoxon_results)

# Function to calculate mean differences for each variable comparing scenarios
mean_differences <- function(data, variable_names, ref_scenario1, ref_scenario2) {
  # Initialize an empty dataframe to store results
  results_df <- data.frame()
  
  # Loop through each variable name
  for (variable_name in variable_names) {
    # Select scenario and the current variable
    variable_data <- data %>%
      select(scenario, !!sym(variable_name)) %>%
      drop_na(!!sym(variable_name))  # Drop rows with NA for the current variable
    
    # Calculate mean for each scenario
    scenario_means <- variable_data %>%
      group_by(scenario) %>%
      summarize(mean_value = mean(!!sym(variable_name), na.rm = TRUE)) %>%
      ungroup()
    
    # Print scenario means for debugging
    print(paste("Scenario means for", variable_name))
    print(scenario_means)
    
    # Extract means for reference scenarios
    ref1_mean <- scenario_means %>%
      filter(scenario == ref_scenario1) %>%
      pull(mean_value) %>%
      mean(na.rm = TRUE)
    
    ref2_mean <- scenario_means %>%
      filter(scenario == ref_scenario2) %>%
      pull(mean_value) %>%
      mean(na.rm = TRUE)
    
    # Print reference means for debugging
    print(paste("Reference means for", variable_name))
    print(paste(ref_scenario1, "mean:", ref1_mean))
    print(paste(ref_scenario2, "mean:", ref2_mean))
    
    # Calculate percentage mean difference between reference scenarios
    ref_mean_diff <- (ref2_mean - ref1_mean) / ref1_mean
    
    # Filter data for other scenarios
    other_scenarios_means <- scenario_means %>%
      filter(scenario != ref_scenario1 & scenario != ref_scenario2)
    
    # Initialize an empty list to store results
    mean_diff_results <- list()
    
    # Calculate percentage mean difference for each scenario against reference_scenario1 and reference_scenario2
    for (row in 1:nrow(other_scenarios_means)) {
      scenario <- other_scenarios_means$scenario[row]
      scenario_mean <- other_scenarios_means$mean_value[row]
      
      mean_diff_ref1 <- (scenario_mean - ref1_mean) / ref1_mean
      mean_diff_ref2 <- (scenario_mean - ref2_mean) / ref2_mean
      
      mean_diff_results[[paste0("vs_", scenario, "_ref1")]] <- mean_diff_ref1
      mean_diff_results[[paste0("vs_", scenario, "_ref2")]] <- mean_diff_ref2
    }
    
    # Convert the list to a dataframe
    results_df_variable <- data.frame(
      scenario_comparison = c(names(mean_diff_results), "ref2_vs_ref1"),
      mean_difference = c(unlist(mean_diff_results), ref_mean_diff),
      variable = variable_name,
      reference1 = ref_scenario1,
      reference2 = ref_scenario2
    )
    
    # Append results for the current variable to results_df
    results_df <- bind_rows(results_df, results_df_variable)
  }
  
  # Return the dataframe of results
  return(results_df)
}

# Specify variable names for which mean differences should be calculated
variable_names <- c("TWSO", "WP", "IRRTOT", "PRTOT")

# Specify reference scenarios
reference_scenario1 <- "1991-2020_Reference_I_medium"
reference_scenario2 <- "1991-2020_Reference_NI_medium"

# Calculate mean differences on the specified variables
mean_diff_results <- mean_differences(WOFOST73_CWB_potato, variable_names, reference_scenario1, reference_scenario2)

# Print the dataframe of mean difference results
print(mean_diff_results)

write.xlsx(mean_diff_results, file = "mean_difference_results.xlsx", sheetName = "Results", rowNames = FALSE)

#Create violin plots with boxplots

# Define the scenarios for irrigated (I) and non-irrigated (NI)
scenarios_I <- c("1991-2020_Reference_I_medium", "Hd_2100_I_medium", "Hn_2100_I_medium",
                 "Md_2100_I_medium", "Mn_2100_I_medium", "Ld_2100_I_medium", "Ln_2100_I_medium")

scenarios_NI <- c("1991-2020_Reference_NI_medium", "Hd_2100_NI_medium", "Hn_2100_NI_medium",
                  "Md_2100_NI_medium", "Mn_2100_NI_medium", "Ld_2100_NI_medium", "Ln_2100_NI_medium")

# Define a color palette for the scenarios
scenario_colors <- c("1991-2020_Reference_I_medium" = "#9467bd", "1991-2020_Reference_NI_medium" = "#ffbb78",
                     "Hd_2100_I_medium" = "#ff9896", "Hd_2100_NI_medium" = "#ff9896",
                     "Hn_2100_I_medium" = "#aec7e8", "Hn_2100_NI_medium" = "#aec7e8",
                     "Md_2100_I_medium" = "#ff9896", "Md_2100_NI_medium" = "#ff9896",
                     "Mn_2100_I_medium" = "#aec7e8", "Mn_2100_NI_medium" = "#aec7e8",
                     "Ld_2100_I_medium" = "#ff9896", "Ld_2100_NI_medium" = "#ff9896",
                     "Ln_2100_I_medium" = "#aec7e8", "Ln_2100_NI_medium" = "#aec7e8")

# Filter the dataframe for irrigated (I) scenarios
filtered_data_I <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_I)

# Filter the dataframe for non-irrigated (NI) scenarios
filtered_data_NI <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_NI)

# Create a function to add custom grid lines
custom_grid <- function(base_size = 12, base_family = "") {
  theme_light(base_size = base_size, base_family = base_family) +
    theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_line(color = "grey90", size = 0.25),
          panel.grid.major.y = element_line(color = "grey80", size = 0.5),
          panel.grid.minor.y = element_line(color = "grey90", size = 0.25))
}

# Create a violin plot for TWSO with boxplots for irrigated (I) scenarios
twso_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = TWSO, fill = scenario)) +
  geom_violin(trim = FALSE, show.legend = FALSE) + # Disable trimming to include all observations and remove legend
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA, show.legend = FALSE) + # Add boxplot inside violin plot without outliers and remove legend
  labs(title = "Violin Plot of TWSO by Scenario (Irrigated)",
       x = "Scenario",
       y = "TWSO") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_grid() + # Apply custom grid
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) # Increase the number of y-axis ticks

# Create a violin plot for WP with boxplots for irrigated (I) scenarios
wp_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = WP, fill = scenario)) +
  geom_violin(trim = FALSE, show.legend = FALSE) + # Disable trimming to include all observations and remove legend
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA, show.legend = FALSE) + # Add boxplot inside violin plot without outliers and remove legend
  labs(title = "Violin Plot of WP by Scenario (Irrigated)",
       x = "Scenario",
       y = "WP") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_grid() + # Apply custom grid
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) # Increase the number of y-axis ticks

# Create a violin plot for TWSO with boxplots for non-irrigated (NI) scenarios
twso_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = TWSO, fill = scenario)) +
  geom_violin(trim = FALSE, show.legend = FALSE) + # Disable trimming to include all observations and remove legend
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA, show.legend = FALSE) + # Add boxplot inside violin plot without outliers and remove legend
  labs(title = "Violin Plot of TWSO by Scenario (Non-Irrigated)",
       x = "Scenario",
       y = "TWSO") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_grid() + # Apply custom grid
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) # Increase the number of y-axis ticks

# Create a violin plot for WP with boxplots for non-irrigated (NI) scenarios
wp_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = WP, fill = scenario)) +
  geom_violin(trim = FALSE, show.legend = FALSE) + # Disable trimming to include all observations and remove legend
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA, show.legend = FALSE) + # Add boxplot inside violin plot without outliers and remove legend
  labs(title = "Violin Plot of WP by Scenario (Non-Irrigated)",
       x = "Scenario",
       y = "WP") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_grid() + # Apply custom grid
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) # Increase the number of y-axis ticks

# Save the plots with adjusted width and height
ggsave("TWSO_violin_plot_I.png", plot = twso_plot_I, width = 16, height = 9, dpi = 300)
ggsave("WP_violin_plot_I.png", plot = wp_plot_I, width = 16, height = 9, dpi = 300)
ggsave("TWSO_violin_plot_NI.png", plot = twso_plot_NI, width = 16, height = 9, dpi = 300)
ggsave("WP_violin_plot_NI.png", plot = wp_plot_NI, width = 16, height = 9, dpi = 300)

#Create boxplots only, 2100, medium

# Define the scenarios for irrigated (I) and non-irrigated (NI)
scenarios_I <- c("1991-2020_Reference_I_medium", "Hd_2100_I_medium", "Hn_2100_I_medium",
                 "Md_2100_I_medium", "Mn_2100_I_medium", "Ld_2100_I_medium", "Ln_2100_I_medium")

scenarios_NI <- c("1991-2020_Reference_NI_medium", "Hd_2100_NI_medium", "Hn_2100_NI_medium",
                  "Md_2100_NI_medium", "Mn_2100_NI_medium", "Ld_2100_NI_medium", "Ln_2100_NI_medium")

# Define a color palette for the scenarios
scenario_colors <- c("1991-2020_Reference_I_medium" = "#9467bd", "1991-2020_Reference_NI_medium" = "#ffbb78",
                     "Hd_2100_I_medium" = "#ff9896", "Hd_2100_NI_medium" = "#ff9896",
                     "Hn_2100_I_medium" = "#aec7e8", "Hn_2100_NI_medium" = "#aec7e8",
                     "Md_2100_I_medium" = "#ff9896", "Md_2100_NI_medium" = "#ff9896",
                     "Mn_2100_I_medium" = "#aec7e8", "Mn_2100_NI_medium" = "#aec7e8",
                     "Ld_2100_I_medium" = "#ff9896", "Ld_2100_NI_medium" = "#ff9896",
                     "Ln_2100_I_medium" = "#aec7e8", "Ln_2100_NI_medium" = "#aec7e8")

# Filter the dataframe for irrigated (I) scenarios
filtered_data_I <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_I)

# Filter the dataframe for non-irrigated (NI) scenarios
filtered_data_NI <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_NI)

# Create a function to add custom grid lines and adjust theme
custom_theme <- function(base_size = 12, base_family = "Times New Roman") {
  theme_light(base_size = base_size * 3, base_family = base_family) +
    theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_line(color = "grey90", size = 0.25),
          panel.grid.major.y = element_line(color = "grey80", size = 0.5),
          panel.grid.minor.y = element_line(color = "grey90", size = 0.25),
          text = element_text(size = base_size * 3, family = base_family),
          plot.title = element_text(size = base_size * 3 - 2, family = base_family, face = "bold"))
}

# Create a boxplot for TWSO with colors from violin plot for irrigated (I) scenarios
twso_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = TWSO, fill = scenario)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) + # Add boxplot without outliers
  labs(title = "Boxplot of TWSO by Scenario (2100, irrigated, medium timing)",
       x = "Scenario",
       y = "TWSO (kg/ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(fill = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a boxplot for WP with colors from violin plot for irrigated (I) scenarios
wp_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = WP, fill = scenario)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) + # Add boxplot without outliers
  labs(title = "Boxplot of WP by Scenario (2100, irrigated, medium timing)",
       x = "Scenario",
       y = "WP (kg/(ha*cm))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(fill = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a boxplot for TWSO with colors from violin plot for non-irrigated (NI) scenarios
twso_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = TWSO, fill = scenario)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) + # Add boxplot without outliers
  labs(title = "Boxplot of TWSO by Scenario (2100, non-irrigated, medium timing)",
       x = "Scenario",
       y = "TWSO (kg/ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(fill = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a boxplot for WP with colors from violin plot for non-irrigated (NI) scenarios
wp_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = WP, fill = scenario)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) + # Add boxplot without outliers
  labs(title = "Boxplot of WP by Scenario (2100, non-irrigated, medium timing)",
       x = "Scenario",
       y = "WP (kg/(ha*cm))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(fill = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Save the plots with adjusted width and height
ggsave("2100_TWSO_boxplot_I.png", plot = twso_plot_I, width = 16, height = 9, dpi = 300)
ggsave("2100_WP_boxplot_I.png", plot = wp_plot_I, width = 16, height = 9, dpi = 300)
ggsave("2100_TWSO_boxplot_NI.png", plot = twso_plot_NI, width = 16, height = 9, dpi = 300)
ggsave("2100_WP_boxplot_NI.png", plot = wp_plot_NI, width = 16, height = 9, dpi = 300)

#Create boxplots only, 2050, medium

# Define the scenarios for irrigated (I) and non-irrigated (NI)
scenarios_I <- c("1991-2020_Reference_I_medium", "Hd_2050_I_medium", "Hn_2050_I_medium",
                 "Md_2050_I_medium", "Mn_2050_I_medium")

scenarios_NI <- c("1991-2020_Reference_NI_medium", "Hd_2050_NI_medium", "Hn_2050_NI_medium",
                  "Md_2050_NI_medium", "Mn_2050_NI_medium")

# Define a color palette for the scenarios
scenario_colors <- c("1991-2020_Reference_I_medium" = "#9467bd", "1991-2020_Reference_NI_medium" = "#ffbb78",
                     "Hd_2050_I_medium" = "#ff9896", "Hd_2050_NI_medium" = "#ff9896",
                     "Hn_2050_I_medium" = "#aec7e8", "Hn_2050_NI_medium" = "#aec7e8",
                     "Md_2050_I_medium" = "#ff9896", "Md_2050_NI_medium" = "#ff9896",
                     "Mn_2050_I_medium" = "#aec7e8", "Mn_2050_NI_medium" = "#aec7e8")

# Filter the dataframe for irrigated (I) scenarios
filtered_data_I <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_I)

# Filter the dataframe for non-irrigated (NI) scenarios
filtered_data_NI <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_NI)

# Create a function to add custom grid lines and adjust theme
custom_theme <- function(base_size = 12, base_family = "Times New Roman") {
  theme_light(base_size = base_size * 3, base_family = base_family) +
    theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_line(color = "grey90", size = 0.25),
          panel.grid.major.y = element_line(color = "grey80", size = 0.5),
          panel.grid.minor.y = element_line(color = "grey90", size = 0.25),
          text = element_text(size = base_size * 3, family = base_family),
          plot.title = element_text(size = base_size * 3 - 2, family = base_family, face = "bold"))
}

# Create a boxplot for TWSO with colors from violin plot for irrigated (I) scenarios
twso_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = TWSO, fill = scenario)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) + # Add boxplot without outliers
  labs(title = "Boxplot of TWSO by Scenario (2050, irrigated, medium timing)",
       x = "Scenario",
       y = "TWSO (kg/ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(fill = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a boxplot for WP with colors from violin plot for irrigated (I) scenarios
wp_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = WP, fill = scenario)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) + # Add boxplot without outliers
  labs(title = "Boxplot of WP by Scenario (2050, irrigated, medium timing)",
       x = "Scenario",
       y = "WP (kg/(ha*cm))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(fill = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a boxplot for TWSO with colors from violin plot for non-irrigated (NI) scenarios
twso_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = TWSO, fill = scenario)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) + # Add boxplot without outliers
  labs(title = "Boxplot of TWSO by Scenario (2050, non-irrigated, medium timing)",
       x = "Scenario",
       y = "TWSO (kg/ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(fill = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a boxplot for WP with colors from violin plot for non-irrigated (NI) scenarios
wp_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = WP, fill = scenario)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) + # Add boxplot without outliers
  labs(title = "Boxplot of WP by Scenario (2050, non-irrigated, medium timing)",
       x = "Scenario",
       y = "WP (kg/(ha*cm))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(fill = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Save the plots with adjusted width and height
ggsave("2050_TWSO_boxplot_I.png", plot = twso_plot_I, width = 16, height = 9, dpi = 300)
ggsave("2050_WP_boxplot_I.png", plot = wp_plot_I, width = 16, height = 9, dpi = 300)
ggsave("2050_TWSO_boxplot_NI.png", plot = twso_plot_NI, width = 16, height = 9, dpi = 300)
ggsave("2050_WP_boxplot_NI.png", plot = wp_plot_NI, width = 16, height = 9, dpi = 300)


#Create boxplots only, 2150, medium

# Define the scenarios for irrigated (I) and non-irrigated (NI)
scenarios_I <- c("1991-2020_Reference_I_medium", "Hd_2150_I_medium", "Hn_2150_I_medium",
                 "Md_2150_I_medium", "Mn_2150_I_medium")

scenarios_NI <- c("1991-2020_Reference_NI_medium", "Hd_2150_NI_medium", "Hn_2150_NI_medium",
                  "Md_2150_NI_medium", "Mn_2150_NI_medium")

# Define a color palette for the scenarios
scenario_colors <- c("1991-2020_Reference_I_medium" = "#9467bd", "1991-2020_Reference_NI_medium" = "#ffbb78",
                     "Hd_2150_I_medium" = "#ff9896", "Hd_2150_NI_medium" = "#ff9896",
                     "Hn_2150_I_medium" = "#aec7e8", "Hn_2150_NI_medium" = "#aec7e8",
                     "Md_2150_I_medium" = "#ff9896", "Md_2150_NI_medium" = "#ff9896",
                     "Mn_2150_I_medium" = "#aec7e8", "Mn_2150_NI_medium" = "#aec7e8")

# Filter the dataframe for irrigated (I) scenarios
filtered_data_I <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_I)

# Filter the dataframe for non-irrigated (NI) scenarios
filtered_data_NI <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_NI)

# Create a function to add custom grid lines and adjust theme
custom_theme <- function(base_size = 12, base_family = "Times New Roman") {
  theme_light(base_size = base_size * 3, base_family = base_family) +
    theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_line(color = "grey90", size = 0.25),
          panel.grid.major.y = element_line(color = "grey80", size = 0.5),
          panel.grid.minor.y = element_line(color = "grey90", size = 0.25),
          text = element_text(size = base_size * 3, family = base_family),
          plot.title = element_text(size = base_size * 3 - 2, family = base_family, face = "bold"))
}

# Create a boxplot for TWSO with colors from violin plot for irrigated (I) scenarios
twso_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = TWSO, fill = scenario)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) + # Add boxplot without outliers
  labs(title = "Boxplot of TWSO by Scenario (2150, irrigated, medium timing)",
       x = "Scenario",
       y = "TWSO (kg/ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(fill = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a boxplot for WP with colors from violin plot for irrigated (I) scenarios
wp_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = WP, fill = scenario)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) + # Add boxplot without outliers
  labs(title = "Boxplot of WP by Scenario (2150, irrigated, medium timing)",
       x = "Scenario",
       y = "WP (kg/(ha*cm))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(fill = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a boxplot for TWSO with colors from violin plot for non-irrigated (NI) scenarios
twso_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = TWSO, fill = scenario)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) + # Add boxplot without outliers
  labs(title = "Boxplot of TWSO by Scenario (2150, non-irrigated, medium timing)",
       x = "Scenario",
       y = "TWSO (kg/ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(fill = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a boxplot for WP with colors from violin plot for non-irrigated (NI) scenarios
wp_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = WP, fill = scenario)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) + # Add boxplot without outliers
  labs(title = "Boxplot of WP by Scenario (2150, non-irrigated, medium timing)",
       x = "Scenario",
       y = "WP (kg/(ha*cm))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_fill_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(fill = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Save the plots with adjusted width and height
ggsave("2150_TWSO_boxplot_I.png", plot = twso_plot_I, width = 16, height = 9, dpi = 300)
ggsave("2150_WP_boxplot_I.png", plot = wp_plot_I, width = 16, height = 9, dpi = 300)
ggsave("2150_TWSO_boxplot_NI.png", plot = twso_plot_NI, width = 16, height = 9, dpi = 300)
ggsave("2150_WP_boxplot_NI.png", plot = wp_plot_NI, width = 16, height = 9, dpi = 300)

#Create jitter plots
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define the scenarios for irrigated (I) and non-irrigated (NI)
scenarios_I <- c("1991-2020_Reference_I_medium", "Hd_2050_I_medium", "Hn_2050_I_medium",
                 "Md_2050_I_medium", "Mn_2050_I_medium")

scenarios_NI <- c("1991-2020_Reference_NI_medium", "Hd_2050_NI_medium", "Hn_2050_NI_medium",
                  "Md_2050_NI_medium", "Mn_2050_NI_medium")

# Define a color palette for the scenarios (darker colors)
scenario_colors <- c("1991-2020_Reference_I_medium" = "#5b379e", "1991-2020_Reference_NI_medium" = "#d78745",
                     "Hd_2050_I_medium" = "#d9534f", "Hd_2050_NI_medium" = "#d9534f",
                     "Hn_2050_I_medium" = "#3d85c6", "Hn_2050_NI_medium" = "#3d85c6",
                     "Md_2050_I_medium" = "#d9534f", "Md_2050_NI_medium" = "#d9534f",
                     "Mn_2050_I_medium" = "#3d85c6", "Mn_2050_NI_medium" = "#3d85c6")

# Filter the dataframe for irrigated (I) scenarios
filtered_data_I <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_I)

# Filter the dataframe for non-irrigated (NI) scenarios
filtered_data_NI <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_NI)

# Create a function to add custom grid lines and adjust theme
custom_theme <- function(base_size = 12, base_family = "Times New Roman") {
  theme_light(base_size = base_size * 3, base_family = base_family) +
    theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_line(color = "grey90", size = 0.25),
          panel.grid.major.y = element_line(color = "grey80", size = 0.5),
          panel.grid.minor.y = element_line(color = "grey90", size = 0.25),
          text = element_text(size = base_size * 3, family = base_family),
          plot.title = element_text(size = base_size * 3 - 2, family = base_family, face = "bold"))
}

# Create a jitter plot for TWSO with colors for irrigated (I) scenarios
twso_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = TWSO, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of TWSO by Scenario (2050, irrigated, medium timing)",
       x = "Scenario",
       y = "TWSO (kg/ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for WP with colors for irrigated (I) scenarios
wp_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = WP, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of WP by Scenario (2050, irrigated, medium timing)",
       x = "Scenario",
       y = "WP (kg/(ha*cm))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for TWSO with colors for non-irrigated (NI) scenarios
twso_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = TWSO, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of TWSO by Scenario (2050, non-irrigated, medium timing)",
       x = "Scenario",
       y = "TWSO (kg/ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for WP with colors for non-irrigated (NI) scenarios
wp_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = WP, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of WP by Scenario (2050, non-irrigated, medium timing)",
       x = "Scenario",
       y = "WP (kg/(ha*cm))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Save the plots with adjusted width and height
ggsave("TWSO_jitter_plot_I_2050.png", plot = twso_plot_I, width = 20, height = 9, dpi = 300)
ggsave("WP_jitter_plot_I_2050.png", plot = wp_plot_I, width = 20, height = 9, dpi = 300)
ggsave("TWSO_jitter_plot_NI_2050.png", plot = twso_plot_NI, width = 20, height = 9, dpi = 300)
ggsave("WP_jitter_plot_NI_2050.png", plot = wp_plot_NI, width = 20, height = 9, dpi = 300)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define the scenarios for irrigated (I) and non-irrigated (NI)
scenarios_I <- c("1991-2020_Reference_I_medium", "Hd_2100_I_medium", "Hn_2100_I_medium",
                 "Md_2100_I_medium", "Mn_2100_I_medium", "Ld_2100_I_medium", "Ln_2100_I_medium")

scenarios_NI <- c("1991-2020_Reference_NI_medium", "Hd_2100_NI_medium", "Hn_2100_NI_medium",
                  "Md_2100_NI_medium", "Mn_2100_NI_medium", "Ld_2100_NI_medium", "Ln_2100_NI_medium")

# Define a darker color palette for the scenarios
scenario_colors <- c("1991-2020_Reference_I_medium" = "#5b379e", "1991-2020_Reference_NI_medium" = "#d78745",
                     "Hd_2100_I_medium" = "#d9534f", "Hd_2100_NI_medium" = "#d9534f",
                     "Hn_2100_I_medium" = "#3d85c6", "Hn_2100_NI_medium" = "#3d85c6",
                     "Md_2100_I_medium" = "#d9534f", "Md_2100_NI_medium" = "#d9534f",
                     "Mn_2100_I_medium" = "#3d85c6", "Mn_2100_NI_medium" = "#3d85c6",
                     "Ld_2100_I_medium" = "#d9534f", "Ld_2100_NI_medium" = "#d9534f",
                     "Ln_2100_I_medium" = "#3d85c6", "Ln_2100_NI_medium" = "#3d85c6")

# Filter the dataframe for irrigated (I) scenarios
filtered_data_I <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_I)

# Filter the dataframe for non-irrigated (NI) scenarios
filtered_data_NI <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_NI)

# Create a function to add custom grid lines and adjust theme
custom_theme <- function(base_size = 12, base_family = "Times New Roman") {
  theme_light(base_size = base_size * 3, base_family = base_family) +
    theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_line(color = "grey90", size = 0.25),
          panel.grid.major.y = element_line(color = "grey80", size = 0.5),
          panel.grid.minor.y = element_line(color = "grey90", size = 0.25),
          text = element_text(size = base_size * 3, family = base_family),
          plot.title = element_text(size = base_size * 3 - 2, family = base_family, face = "bold"))
}

# Create a jitter plot for TWSO with colors from violin plot for irrigated (I) scenarios
twso_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = TWSO, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of TWSO by Scenario (2100, irrigated, medium timing)",
       x = "Scenario",
       y = "TWSO (kg/ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for WP with colors from violin plot for irrigated (I) scenarios
wp_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = WP, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of WP by Scenario (2100, irrigated, medium timing)",
       x = "Scenario",
       y = "WP (kg/(ha*cm))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for TWSO with colors from violin plot for non-irrigated (NI) scenarios
twso_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = TWSO, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of TWSO by Scenario (2100, non-irrigated, medium timing)",
       x = "Scenario",
       y = "TWSO (kg/ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for WP with colors from violin plot for non-irrigated (NI) scenarios
wp_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = WP, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of WP by Scenario (2100, non-irrigated, medium timing)",
       x = "Scenario",
       y = "WP (kg/(ha*cm))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Save the plots with adjusted width and height
ggsave("TWSO_jitter_plot_I_2100.png", plot = twso_plot_I, width = 24, height = 9, dpi = 300)
ggsave("WP_jitter_plot_I_2100.png", plot = wp_plot_I, width = 24, height = 9, dpi = 300)
ggsave("TWSO_jitter_plot_NI_2100.png", plot = twso_plot_NI, width = 24, height = 9, dpi = 300)
ggsave("WP_jitter_plot_NI_2100.png", plot = wp_plot_NI, width = 24, height = 9, dpi = 300)

# Define the scenarios for irrigated (I) and non-irrigated (NI)
scenarios_I <- c("1991-2020_Reference_I_medium", "Hd_2150_I_medium", "Hn_2150_I_medium",
                 "Md_2150_I_medium", "Mn_2150_I_medium")

scenarios_NI <- c("1991-2020_Reference_NI_medium", "Hd_2150_NI_medium", "Hn_2150_NI_medium",
                  "Md_2150_NI_medium", "Mn_2150_NI_medium")

# Define a color palette for the scenarios (darker colors)
scenario_colors <- c("1991-2020_Reference_I_medium" = "#5b379e", "1991-2020_Reference_NI_medium" = "#d78745",
                     "Hd_2150_I_medium" = "#d9534f", "Hd_2150_NI_medium" = "#d9534f",
                     "Hn_2150_I_medium" = "#3d85c6", "Hn_2150_NI_medium" = "#3d85c6",
                     "Md_2150_I_medium" = "#d9534f", "Md_2150_NI_medium" = "#d9534f",
                     "Mn_2150_I_medium" = "#3d85c6", "Mn_2150_NI_medium" = "#3d85c6")

# Filter the dataframe for irrigated (I) scenarios
filtered_data_I <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_I)

# Filter the dataframe for non-irrigated (NI) scenarios
filtered_data_NI <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_NI)

# Create a function to add custom grid lines and adjust theme
custom_theme <- function(base_size = 12, base_family = "Times New Roman") {
  theme_light(base_size = base_size * 3, base_family = base_family) +
    theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_line(color = "grey90", size = 0.25),
          panel.grid.major.y = element_line(color = "grey80", size = 0.5),
          panel.grid.minor.y = element_line(color = "grey90", size = 0.25),
          text = element_text(size = base_size * 3, family = base_family),
          plot.title = element_text(size = base_size * 3 - 2, family = base_family, face = "bold"))
}

# Create a jitter plot for TWSO with colors for irrigated (I) scenarios
twso_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = TWSO, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of TWSO by Scenario (2150, irrigated, medium timing)",
       x = "Scenario",
       y = "TWSO (kg/ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for WP with colors for irrigated (I) scenarios
wp_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = WP, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of WP by Scenario (2150, irrigated, medium timing)",
       x = "Scenario",
       y = "WP (kg/(ha*cm))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for TWSO with colors for non-irrigated (NI) scenarios
twso_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = TWSO, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of TWSO by Scenario (2150, non-irrigated, medium timing)",
       x = "Scenario",
       y = "TWSO (kg/ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for WP with colors for non-irrigated (NI) scenarios
wp_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = WP, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of WP by Scenario (2150, non-irrigated, medium timing)",
       x = "Scenario",
       y = "WP (kg/(ha*cm))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Save the plots with adjusted width and height
ggsave("TWSO_jitter_plot_I_2150.png", plot = twso_plot_I, width = 20, height = 9, dpi = 300)
ggsave("WP_jitter_plot_I_2150.png", plot = wp_plot_I, width = 20, height = 9, dpi = 300)
ggsave("TWSO_jitter_plot_NI_2150.png", plot = twso_plot_NI, width = 20, height = 9, dpi = 300)
ggsave("WP_jitter_plot_NI_2150.png", plot = wp_plot_NI, width = 20, height = 9, dpi = 300)

#Create combined jitter plots
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(patchwork)

# Define the scenarios for irrigated (I) and non-irrigated (NI) for all years
scenarios_I <- c("1991-2020_Reference_I_medium", "Hd_2050_I_medium", "Hn_2050_I_medium",
                 "Md_2050_I_medium", "Mn_2050_I_medium", "Hd_2100_I_medium", "Hn_2100_I_medium",
                 "Md_2100_I_medium", "Mn_2100_I_medium", "Ld_2100_I_medium", "Ln_2100_I_medium",
                 "Hd_2150_I_medium", "Hn_2150_I_medium", "Md_2150_I_medium", "Mn_2150_I_medium")

scenarios_NI <- c("1991-2020_Reference_NI_medium", "Hd_2050_NI_medium", "Hn_2050_NI_medium",
                  "Md_2050_NI_medium", "Mn_2050_NI_medium", "Hd_2100_NI_medium", "Hn_2100_NI_medium",
                  "Md_2100_NI_medium", "Mn_2100_NI_medium", "Ld_2100_NI_medium", "Ln_2100_NI_medium",
                  "Hd_2150_NI_medium", "Hn_2150_NI_medium", "Md_2150_NI_medium", "Mn_2150_NI_medium")

# Define a color palette for the scenarios (darker colors)
scenario_colors <- c("1991-2020_Reference_I_medium" = "#5b379e", "1991-2020_Reference_NI_medium" = "#d78745",
                     "Hd_2050_I_medium" = "#d9534f", "Hd_2050_NI_medium" = "#d9534f",
                     "Hn_2050_I_medium" = "#3d85c6", "Hn_2050_NI_medium" = "#3d85c6",
                     "Md_2050_I_medium" = "#d9534f", "Md_2050_NI_medium" = "#d9534f",
                     "Mn_2050_I_medium" = "#3d85c6", "Mn_2050_NI_medium" = "#3d85c6",
                     "Hd_2100_I_medium" = "#d9534f", "Hd_2100_NI_medium" = "#d9534f",
                     "Hn_2100_I_medium" = "#3d85c6", "Hn_2100_NI_medium" = "#3d85c6",
                     "Md_2100_I_medium" = "#d9534f", "Md_2100_NI_medium" = "#d9534f",
                     "Mn_2100_I_medium" = "#3d85c6", "Mn_2100_NI_medium" = "#3d85c6",
                     "Ld_2100_I_medium" = "#d9534f", "Ld_2100_NI_medium" = "#d9534f",
                     "Ln_2100_I_medium" = "#3d85c6", "Ln_2100_NI_medium" = "#3d85c6",
                     "Hd_2150_I_medium" = "#d9534f", "Hd_2150_NI_medium" = "#d9534f",
                     "Hn_2150_I_medium" = "#3d85c6", "Hn_2150_NI_medium" = "#3d85c6",
                     "Md_2150_I_medium" = "#d9534f", "Md_2150_NI_medium" = "#d9534f",
                     "Mn_2150_I_medium" = "#3d85c6", "Mn_2150_NI_medium" = "#3d85c6")

# Filter the dataframe for irrigated (I) scenarios
filtered_data_I <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_I)

# Filter the dataframe for non-irrigated (NI) scenarios
filtered_data_NI <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_NI)

# Create a function to add custom grid lines and adjust theme
custom_theme <- function(base_size = 12, base_family = "Times New Roman") {
  theme_light(base_size = base_size * 3, base_family = base_family) +
    theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_line(color = "grey90", size = 0.25),
          panel.grid.major.y = element_line(color = "grey80", size = 0.5),
          panel.grid.minor.y = element_line(color = "grey90", size = 0.25),
          text = element_text(size = base_size * 3, family = base_family),
          plot.title = element_text(size = base_size * 3, family = base_family, face = "bold"))
}

# Create a jitter plot function to avoid repetition
create_jitter_plot <- function(data, y_var, title_text, y_label, remove_y = FALSE, remove_x = FALSE, irrigated = TRUE) {
  plot <- ggplot(data, aes(x = scenario, y = !!sym(y_var), color = scenario)) +
    geom_jitter(width = 0.3, height = 0, size = 2) +
    labs(title = title_text, x = if (remove_x) NULL else "Scenario", y = if (remove_y) "" else y_label) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, margin = margin(t = 10)),
          axis.title.y = element_text(size = 12 * 3, margin = margin(r = 15))) +
    scale_color_manual(values = scenario_colors) +
    custom_theme() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    guides(color = FALSE) +
    scale_x_discrete(labels = function(x) gsub("_.*", "", x)) +
    theme(plot.title = element_text(size = 36, face = "bold"))
  
  if (remove_y) {
    plot <- plot + theme(axis.title.y = element_blank())
  }
  if (!irrigated) {
    plot <- plot + labs(title = NULL) + theme(plot.title = element_blank(), axis.title.y = element_blank())
  }
  
  plot
}
# Create irrigated plots
for (year in c("2050", "2100", "2150")) {
  plots[[paste0("twso_", year, "_I")]] <- create_jitter_plot(
    filtered_data_I %>% filter(grepl(year, scenario)),
    "TWSO",
    year,
    expression(paste("Ywp (t DM ha"^"-1", ")")),
    remove_y = FALSE,
    remove_x = year != "2150",
    irrigated = TRUE
  )
  plots[[paste0("wp_", year, "_I")]] <- create_jitter_plot(
    filtered_data_I %>% filter(grepl(year, scenario)),
    "WP",
    year,
    expression(paste("WP (t DM ha"^"-1", " cm"^"-1", ")")),
    remove_y = FALSE,
    remove_x = year != "2150",
    irrigated = TRUE
  )
  
  # Add reference scenarios annotation for irrigated plots
  plots[[paste0("twso_", year, "_I")]] <- plots[[paste0("twso_", year, "_I")]] +
    annotate("text", x = 1, y = max(filtered_data_I$TWSO), label = "Reference", 
             vjust = 1.5, hjust = -0.1, color = "black", size = 6)
  
  plots[[paste0("wp_", year, "_I")]] <- plots[[paste0("wp_", year, "_I")]] +
    annotate("text", x = 1, y = max(filtered_data_I$WP), label = "Reference", 
             vjust = 1.5, hjust = -0.1, color = "black", size = 6)
}

# Save the plots
ggsave("combined_twso_plot.png", plot = combined_twso_plot, width = 20, height = 25, dpi = 300)
ggsave("combined_wp_plot.png", plot = combined_wp_plot, width = 20, height = 25, dpi = 300)
ggsave("combined_twso_plot.svg", plot = combined_twso_plot, width = 40, height = 55, units = "cm")
ggsave("combined_wp_plot.svg", plot = combined_wp_plot, width = 40, height = 55, units = "cm")

#2create jitters
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define the scenarios for irrigated (I) and non-irrigated (NI)
scenarios_I <- c("1991-2020_Reference_I_medium", "Hd_2050_I_medium", "Hn_2050_I_medium",
                 "Md_2050_I_medium", "Mn_2050_I_medium")

scenarios_NI <- c("1991-2020_Reference_NI_medium", "Hd_2050_NI_medium", "Hn_2050_NI_medium",
                  "Md_2050_NI_medium", "Mn_2050_NI_medium")

# Define a color palette for the scenarios (darker colors)
scenario_colors <- c("1991-2020_Reference_I_medium" = "#5b379e", "1991-2020_Reference_NI_medium" = "#d78745",
                     "Hd_2050_I_medium" = "#d9534f", "Hd_2050_NI_medium" = "#d9534f",
                     "Hn_2050_I_medium" = "#3d85c6", "Hn_2050_NI_medium" = "#3d85c6",
                     "Md_2050_I_medium" = "#d9534f", "Md_2050_NI_medium" = "#d9534f",
                     "Mn_2050_I_medium" = "#3d85c6", "Mn_2050_NI_medium" = "#3d85c6")

# Filter the dataframe for irrigated (I) scenarios
filtered_data_I <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_I)

# Filter the dataframe for non-irrigated (NI) scenarios
filtered_data_NI <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_NI)

# Create a function to add custom grid lines and adjust theme
custom_theme <- function(base_size = 12, base_family = "Times New Roman") {
  theme_light(base_size = base_size * 3, base_family = base_family) +
    theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_line(color = "grey90", size = 0.25),
          panel.grid.major.y = element_line(color = "grey80", size = 0.5),
          panel.grid.minor.y = element_line(color = "grey90", size = 0.25),
          text = element_text(size = base_size * 3, family = base_family),
          plot.title = element_text(size = base_size * 3 - 2, family = base_family, face = "bold"))
}

# Adjusted labels for TWSO and WP
label_TWSO <- expression(paste("Ywp (t DM ha"^"-1", ")"))
label_WP <- expression(paste("WP (t DM ha"^"-1", " cm"^"-1", ")"))

# Create a jitter plot for TWSO with colors for irrigated (I) scenarios
twso_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = TWSO, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of TWSO by Scenario (2050, irrigated, medium timing)",
       x = "Scenario",
       y = label_TWSO) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for WP with colors for irrigated (I) scenarios
wp_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = WP, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of WP by Scenario (2050, irrigated, medium timing)",
       x = "Scenario",
       y = label_WP) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for TWSO with colors for non-irrigated (NI) scenarios
twso_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = TWSO, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of TWSO by Scenario (2050, non-irrigated, medium timing)",
       x = "Scenario",
       y = label_TWSO) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for WP with colors for non-irrigated (NI) scenarios
wp_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = WP, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of WP by Scenario (2050, non-irrigated, medium timing)",
       x = "Scenario",
       y = label_WP) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Save the plots with adjusted width and height
ggsave("TWSO_jitter_plot_I_2050.png", plot = twso_plot_I, width = 20, height = 9, dpi = 300)
ggsave("WP_jitter_plot_I_2050.png", plot = wp_plot_I, width = 20, height = 9, dpi = 300)
ggsave("TWSO_jitter_plot_NI_2050.png", plot = twso_plot_NI, width = 20, height = 9, dpi = 300)
ggsave("WP_jitter_plot_NI_2050.png", plot = wp_plot_NI, width = 20, height = 9, dpi = 300)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define the scenarios for irrigated (I) and non-irrigated (NI)
scenarios_I <- c("1991-2020_Reference_I_medium", "Hd_2100_I_medium", "Hn_2100_I_medium",
                 "Md_2100_I_medium", "Mn_2100_I_medium", "Ld_2100_I_medium", "Ln_2100_I_medium")

scenarios_NI <- c("1991-2020_Reference_NI_medium", "Hd_2100_NI_medium", "Hn_2100_NI_medium",
                  "Md_2100_NI_medium", "Mn_2100_NI_medium", "Ld_2100_NI_medium", "Ln_2100_NI_medium")

# Define a darker color palette for the scenarios (continued)
scenario_colors <- c(scenario_colors,
                     "1991-2020_Reference_I_medium" = "#5b379e", "1991-2020_Reference_NI_medium" = "#d78745",
                     "Hd_2050_I_medium" = "#d9534f", "Hd_2050_NI_medium" = "#d9534f",
                     "Hn_2050_I_medium" = "#3d85c6", "Hn_2050_NI_medium" = "#3d85c6",
                     "Md_2100_I_medium" = "#d9534f", "Md_2100_NI_medium" = "#d9534f",
                     "Mn_2100_I_medium" = "#3d85c6", "Mn_2100_NI_medium" = "#3d85c6",
                     "Ld_2100_I_medium" = "#d9534f", "Ld_2100_NI_medium" = "#d9534f",
                     "Ln_2100_I_medium" = "#3d85c6", "Ln_2100_NI_medium" = "#3d85c6")
# Filter the dataframe for irrigated (I) scenarios
filtered_data_I <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_I)

# Filter the dataframe for non-irrigated (NI) scenarios
filtered_data_NI <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_NI)

# Create a function to add custom grid lines and adjust theme
custom_theme <- function(base_size = 12, base_family = "Times New Roman") {
  theme_light(base_size = base_size * 3, base_family = base_family) +
    theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_line(color = "grey90", size = 0.25),
          panel.grid.major.y = element_line(color = "grey80", size = 0.5),
          panel.grid.minor.y = element_line(color = "grey90", size = 0.25),
          text = element_text(size = base_size * 3, family = base_family),
          plot.title = element_text(size = base_size * 3 - 2, family = base_family, face = "bold"))
}

# Adjusted labels for TWSO and WP
label_TWSO <- expression(paste("Ywp (t DM ha"^"-1", ")"))
label_WP <- expression(paste("WP (t DM ha"^"-1", " cm"^"-1", ")"))

# Create a jitter plot for TWSO with colors for irrigated (I) scenarios
twso_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = TWSO, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of TWSO by Scenario (2100, irrigated, medium timing)",
       x = "Scenario",
       y = label_TWSO) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for WP with colors for irrigated (I) scenarios
wp_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = WP, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of WP by Scenario (2100, irrigated, medium timing)",
       x = "Scenario",
       y = label_WP) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for TWSO with colors for non-irrigated (NI) scenarios
twso_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = TWSO, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of TWSO by Scenario (2100, non-irrigated, medium timing)",
       x = "Scenario",
       y = label_TWSO) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for WP with colors for non-irrigated (NI) scenarios
wp_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = WP, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of WP by Scenario (2100, non-irrigated, medium timing)",
       x = "Scenario",
       y = label_WP) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Save the plots with adjusted width and height
ggsave("TWSO_jitter_plot_I_2100.png", plot = twso_plot_I, width = 20, height = 9, dpi = 300)
ggsave("WP_jitter_plot_I_2100.png", plot = wp_plot_I, width = 20, height = 9, dpi = 300)
ggsave("TWSO_jitter_plot_NI_2100.png", plot = twso_plot_NI, width = 20, height = 9, dpi = 300)
ggsave("WP_jitter_plot_NI_2100.png", plot = wp_plot_NI, width = 20, height = 9, dpi = 300)


#2create jitters
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define the scenarios for irrigated (I) and non-irrigated (NI)
scenarios_I <- c("1991-2020_Reference_I_medium", "Hd_2150_I_medium", "Hn_2150_I_medium",
                 "Md_2150_I_medium", "Mn_2150_I_medium")

scenarios_NI <- c("1991-2020_Reference_NI_medium", "Hd_2150_NI_medium", "Hn_2150_NI_medium",
                  "Md_2150_NI_medium", "Mn_2150_NI_medium")

# Define a color palette for the scenarios (darker colors)
scenario_colors <- c("1991-2020_Reference_I_medium" = "#5b379e", "1991-2020_Reference_NI_medium" = "#d78745",
                     "Hd_2150_I_medium" = "#d9534f", "Hd_2150_NI_medium" = "#d9534f",
                     "Hn_2150_I_medium" = "#3d85c6", "Hn_2150_NI_medium" = "#3d85c6",
                     "Md_2150_I_medium" = "#d9534f", "Md_2150_NI_medium" = "#d9534f",
                     "Mn_2150_I_medium" = "#3d85c6", "Mn_2150_NI_medium" = "#3d85c6")

# Filter the dataframe for irrigated (I) scenarios
filtered_data_I <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_I)

# Filter the dataframe for non-irrigated (NI) scenarios
filtered_data_NI <- WOFOST73_CWB_potato %>%
  filter(scenario %in% scenarios_NI)

# Create a function to add custom grid lines and adjust theme
custom_theme <- function(base_size = 12, base_family = "Times New Roman") {
  theme_light(base_size = base_size * 3, base_family = base_family) +
    theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_line(color = "grey90", size = 0.25),
          panel.grid.major.y = element_line(color = "grey80", size = 0.5),
          panel.grid.minor.y = element_line(color = "grey90", size = 0.25),
          text = element_text(size = base_size * 3, family = base_family),
          plot.title = element_text(size = base_size * 3 - 2, family = base_family, face = "bold"))
}

# Adjusted labels for TWSO and WP
label_TWSO <- expression(paste("Ywp (t DM ha"^"-1", ")"))
label_WP <- expression(paste("WP (t DM ha"^"-1", " cm"^"-1", ")"))

# Create a jitter plot for TWSO with colors for irrigated (I) scenarios
twso_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = TWSO, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of TWSO by Scenario (2150, irrigated, medium timing)",
       x = "Scenario",
       y = label_TWSO) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for WP with colors for irrigated (I) scenarios
wp_plot_I <- ggplot(filtered_data_I, aes(x = scenario, y = WP, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of WP by Scenario (2150, irrigated, medium timing)",
       x = "Scenario",
       y = label_WP) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for TWSO with colors for non-irrigated (NI) scenarios
twso_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = TWSO, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of TWSO by Scenario (2150, non-irrigated, medium timing)",
       x = "Scenario",
       y = label_TWSO) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Create a jitter plot for WP with colors for non-irrigated (NI) scenarios
wp_plot_NI <- ggplot(filtered_data_NI, aes(x = scenario, y = WP, color = scenario)) +
  geom_jitter(width = 0.3, height = 0, size = 2) + # Add jitter plot
  labs(title = "Jitter Plot of WP by Scenario (2150, non-irrigated, medium timing)",
       x = "Scenario",
       y = label_WP) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman")) +
  scale_color_manual(values = scenario_colors) + # Set custom colors
  custom_theme() + # Apply custom theme
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + # Increase the number of y-axis ticks
  guides(color = FALSE) + # Remove legend
  scale_x_discrete(labels = function(x) gsub("_.*", "", x)) # Remove _NI_medium and _I_medium parts

# Save the plots with adjusted width and height
ggsave("TWSO_jitter_plot_I_2150.png", plot = twso_plot_I, width = 20, height = 9, dpi = 300)
ggsave("WP_jitter_plot_I_2150.png", plot = wp_plot_I, width = 20, height = 9, dpi = 300)
ggsave("TWSO_jitter_plot_NI_2150.png", plot = twso_plot_NI, width = 20, height = 9, dpi = 300)
ggsave("WP_jitter_plot_NI_2150.png", plot = wp_plot_NI, width = 20, height = 9, dpi = 300)
