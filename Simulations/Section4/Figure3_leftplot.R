# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for producing results in Figure 3
# Runtime: Approximately 2400 minutes/50 hours with 200 cores of parallel computing - for each simulation parameter calculating 1000 p-values takes 400 minutes (6 simulation parameters)
# This script is executed through left_fig_simulation.sh

source("source/simulation_source.R")
source("source/hiernet_source.R")

# Simulation parameters (sample size, main effects, and varying interaction sizes)
n = 3000
main_x = 0.1
main_z = c(0.1, -0.1, 0.1, -0.1, 0.1, -0.1, 0.1, -0.1)
num_z = 10

# x-axis
interaction_signal = c(0, 0.025, 0.05, 0.075, 0.1, 0.125)

# Parallel computing cluster 
args = as.numeric(commandArgs(trailingOnly = TRUE))
loop_num = args[1]
param_num = args[2]

# Looping through x-axis (interaction signals)
int_signal = interaction_signal[param_num]
num_within = 3
within_signal = rep(int_signal, num_within)

num_between = 3
between_signal = -rep(int_signal, num_between)

s = param_num*10000 + loop_num

df = simulate_dat(n = n, main_x = main_x, main_z = main_z, num_z = num_z, num_within = num_within, num_between = num_between, within_signal = within_signal, between_signal = between_signal, s= s)

# AMCE_estimator
AMCE_df = get_AMCEdf(df)
AMCE_df[colnames(AMCE_df)[-ncol(AMCE_df)]] = lapply(AMCE_df[colnames(AMCE_df)[-ncol(AMCE_df)]] , factor)

AMCE_short = lm(y ~ x_1, data = AMCE_df)
AMCE_short_pval = summary(AMCE_short)$coefficients[2, 4]

# HierNet Estimator
aug_df = augment_df(df)
aug_df[colnames(aug_df)[-ncol(aug_df)]] = lapply(aug_df[colnames(aug_df)[-ncol(aug_df)]] , factor)

X_long = model.matrix(y ~ . , aug_df, contrasts.arg = lapply(aug_df[, c(1:22)], contrasts, contrasts = FALSE))[, -1]
Y = aug_df$y

lambda = c(20, 30, 40, 50)

best_lam_long = get_lam(lambda, X = X_long, y_var = Y)

invisible(capture.output(fit_2 <- hierNet.logistic(as.matrix(X_long), Y, lam= best_lam_long, diagonal = FALSE,trace = 0)))

idx_long = grep("x", colnames(X_long))[1:2]

obs_hier_long = hiernet_group(fit_2, idx_long, X = X_long, analysis = FALSE)

# Resampling Procedure
B = 400

e = foreach(j = 1:B) %dopar% {
  holding = df
  x_1 = sample(c(-0.5, 0.5), size = n, replace = TRUE)
  x_2 = sample(c(-0.5, 0.5), size = n, replace = TRUE)
  holding$x_1 = x_1
  holding$x_2 = x_2
  
  aug_df = augment_df(holding)
  aug_df[colnames(aug_df)[-ncol(aug_df)]] = lapply(aug_df[colnames(aug_df)[-ncol(aug_df)]] , factor)

  X_long = model.matrix(y ~ . , aug_df, contrasts.arg = lapply(aug_df[, c(1:22)], contrasts, contrasts = FALSE))[, -1]
  
  best_lam_long = get_lam(lambda, X = X_long, y_var = Y)
  # 
  invisible(capture.output(fit_2 <- hierNet.logistic(as.matrix(X_long), Y, lam= best_lam_long, diagonal = FALSE,trace = 0)))
  
  e4 = hiernet_group(fit_2, idx_long, X = X_long, analysis = FALSE)
  
  c(e4)
}

e_4 = sapply(e, "[[", 1)

hier_new = (length(which(e_4 >= obs_hier_long)) + 1)/(B + 1)

final_df = data.frame(method = c("AMCE", "CRT"), pval = c(AMCE_short_pval, hier_new))

file_name = paste0("data/Fig3_leftplot_results/pval", param_num, "_", loop_num, ".csv")

write.csv(final_df, file = file_name)
