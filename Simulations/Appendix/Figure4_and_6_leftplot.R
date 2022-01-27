# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for producing results in Figure 4 and 6 in Appendix
# Runtime: Approximately 2400 minutes/50 hours with 200 cores of parallel computing - for each simulation parameter calculating 1000 p-values takes 400 minutes (6 simulation parameters)
# This script is executed through left_fig_simulation.sh

source("source/simulation_source.R")
source("source/hiernet_source.R")
source("source/dI_source.R")

n = 3000
main_x = 0.1
main_z = c(0.1, -0.1, 0.1, -0.1, 0.1, -0.1, 0.1, -0.1)
num_z = 10

ratio = c(0, 0.025, 0.05, 0.075, 0.1, 0.125)
args = as.numeric(commandArgs(trailingOnly = TRUE))
loop_num = args[1]
param_num = args[2]

int_signal = ratio[param_num]
num_within = 3
within_signal = rep(int_signal, num_within)

num_between = 3
between_signal = -rep(int_signal,num_between)

s = param_num*10000 + loop_num

df = simulate_dat(n = n, main_x = main_x, main_z = main_z, num_z = num_z, num_within = num_within, num_between = num_between, within_signal = within_signal, between_signal = between_signal, s= s)
held = df
aug_df = augment_df(df)

df[colnames(df)[-ncol(df)]] = lapply(df[colnames(df)[-ncol(df)]] , factor)

lambda = c(20, 30, 40, 50)

# Unconstrained test statistic
X_unconstrained = model.matrix(y ~ . , df, contrasts.arg = lapply(df[, c(1:22)], contrasts, contrasts = FALSE))[, -1]

idx_unconstrained = grep("x", colnames(X_unconstrained))

y_var_unconstrained = df$y

best_lam_unconstrained = get_lam(lambda, X = X_unconstrained, y_var = y_var_unconstrained, unconstrained = TRUE)

invisible(capture.output(fit_uncons <- hierNet.logistic(as.matrix(X_unconstrained), y_var_unconstrained, lam= best_lam_unconstrained, diagonal = FALSE,trace = 0)))

obs_hier_unconstrained = hiernet_group(fit_uncons, idx_unconstrained, X = X_unconstrained, analysis = FALSE, group = list(c(1,2), c(3,4)))

#dI-CRT
obs_dI = dI_CRT(df = aug_df, variable = colnames(aug_df)[1:2], other_variable = colnames(aug_df)[3:(ncol(aug_df) - 1)], Y = factor(aug_df$y), lambda=  NULL, I = 2)

B = 400

e = foreach(j = 1:B) %dopar% {
  holding = held
  
  x_1 = sample(c(-0.5, 0.5), size = n, replace = TRUE)
  x_2 = sample(c(-0.5, 0.5), size = n, replace = TRUE)
  holding$x_1 = x_1
  holding$x_2 = x_2
  
  aug_df = augment_df(holding)
  
  #unconstrained
  holding[colnames(holding)[-ncol(holding)]] = lapply(holding[colnames(holding)[-ncol(holding)]] , factor)
  
  X_unconstrained = model.matrix(y ~ . , holding, contrasts.arg = lapply(holding[, c(1:22)], contrasts, contrasts = FALSE))[, -1]
  
  best_lam_unconstrained = get_lam(lambda, X = X_unconstrained, y_var = y_var_unconstrained, unconstrained = TRUE)
  
  invisible(capture.output(fit_uncons <- hierNet.logistic(as.matrix(X_unconstrained), y_var_unconstrained, lam= best_lam_unconstrained, diagonal = FALSE,trace = 0)))
  
  e1 = hiernet_group(fit_uncons, idx_unconstrained, X = X_unconstrained, analysis = FALSE, group = list(c(1,2), c(3,4)))
  
  ##dI
  e2 = dI_CRT(df = aug_df, variable = colnames(aug_df)[1:2], other_variable = colnames(aug_df)[3:(ncol(aug_df) - 1)], Y = factor(aug_df$y), lambda=  NULL, I = 2)
  
  c(e1, e2)
}

e_1 = sapply(e, "[[", 1)
e_2 = sapply(e, "[[", 2)

hier_unconstraint = (length(which(e_1 >= obs_hier_unconstrained)) + 1)/(B + 1)

dI_pval = (length(which(e_2 >= obs_dI)) + 1)/(B + 1)

final_df = data.frame(method = c("Hier_Unconstrained", "dI_CRT"), pval = c(hier_unconstraint, dI_pval))

file_name = paste0("data/Appendix_simulations/Fig4_and_6_leftplot_results/pval", param_num, "_", loop_num, ".csv")

write.csv(final_df, file = file_name)



