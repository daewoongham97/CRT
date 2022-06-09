# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Obtaining p-value for exploratory lasso results presented in Section 5
# written on 06/09/2022

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

# Supplementary Analysis 1) Using Lasso Logistic Regression as test statistic to diagnose importance of interaction
immigration_lasso_pval = get_final_pval("data/Section5_Supplementary_Analysis/immigration_lasso/")

immigration_lasso_pval


# Supplementary Analysis 2) Using Lasso Logistic Regression as test statistic to diagnose importance of interaction
gender_lasso_pval = get_final_pval("data/Section5_Supplementary_Analysis/gender_lasso/")

gender_lasso_pval





