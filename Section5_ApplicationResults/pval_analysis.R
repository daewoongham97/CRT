# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Obtaining p-value for all results presented in Section 5

# Given a path that contains all the saved observed test statistics and resampled test statistics we compute the p-value
get_final_pval = function(path) {
  obs_test_stat = read.csv(paste0(path, 'obs_test_stat.csv'), header = TRUE)
  
  # Obtaining p-value
  resampled_test_stats = vector()
  for (i in 1:400) {
    file_name = paste0(path, '_', i, ".csv")
    
    resampled_test_stats[i] = read.csv(file_name, header = TRUE)[, 2]
  }
  
  p_val = (length(which(resampled_test_stats >=  obs_test_stat[, 2])) + 1)/(length(resampled_test_stats) + 1)
  return(p_val)
}

## Immigration analysis 
immigration_path = "data/Table1/immigration/main_CRT_results/"

obs_test_stat = read.csv("data/Table1/immigration/main_CRT_results/obs_test_stat.csv", header = TRUE)

# Shows which interaction was most influential
obs_test_stat

# Getting p-value in row 1 column 1 of Table 1
immigration_pval = get_final_pval(immigration_path)

immigration_pval

# Getting p-value in row 1 column 3 of Table 1
immigration_profile_order_pval = get_final_pval("data/Table1/immigration/profile_order_effect/")

immigration_profile_order_pval

# Getting p-value in row 1 column 4 of Table 1
immigration_carryover_pval = get_final_pval("data/Table1/immigration/carryover_effect/")

immigration_carryover_pval

# Getting p-value in row 1 column 5 of Table 1
immigration_fatigue_pval = get_final_pval("data/Table1/immigration/fatigue_effect/")

immigration_fatigue_pval

# Supplementary Analysis 1) Using Lasso Logistic Regression as test statistic to diagnose importance of interaction 
immigration_lasso_pval = get_final_pval("data/Section5_Supplementary_Analysis/immigration_lasso/")

immigration_lasso_pval

# Supplementary Analysis 2) Including ethnocentrism
obs_test_stat = read.csv("data/Section5_Supplementary_Analysis/immigration_ethnocentrism/obs_test_stat.csv", header = TRUE)

# Shows which interaction was most influential
obs_test_stat

immigration_ethno_pval = get_final_pval("data/Section5_Supplementary_Analysis/immigration_ethnocentrism/")

immigration_ethno_pval


## Gender analysis
gender_path = "data/Table1/gender/main_CRT_results/"

obs_test_stat = read.csv("data/Table1/gender/main_CRT_results/obs_test_stat.csv", header = TRUE)

# Shows which interaction was most influential
obs_test_stat

# Getting p-value in row 2 column 1 of Table 1
gender_pval = get_final_pval(gender_path)

gender_pval

# Getting p-value in row 2 column 3 of Table 1
gender_profile_order_pval = get_final_pval("data/Table1/gender/profile_order_effect/")

gender_profile_order_pval

# Getting p-value in row 2 column 4 of Table 1
gender_carryover_pval = get_final_pval("data/Table1/gender/carryover_effect/")

gender_carryover_pval

# Getting p-value in row 2 column 5 of Table 1
gender_fatigue_pval = get_final_pval("data/Table1/gender/fatigue_effect/")

gender_fatigue_pval

# Supplementary Analysis 1) Using Lasso Logistic Regression as test statistic to diagnose importance of interaction 
gender_lasso_pval = get_final_pval("data/Section5_Supplementary_Analysis/gender_lasso/")

gender_lasso_pval

# Supplementary Analysis 2) Forcing interaction with abortion instead
gender_lasso_pval = get_final_pval("data/Section5_Supplementary_Analysis/gender_abortion/")

gender_lasso_pval



