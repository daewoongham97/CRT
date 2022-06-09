# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for producing results in Figure 3
# written on 06/09/2022
# Runtime: Approximately 80 minutes with 200 cores of parallel computing - for each simulation parameter calculating 1000 p-values takes 20 minutes (4 simulation parameters)
# This script is executed through right_fig_simulation.sh

source("source/sim_source.R")

library(glmnet); library(CRTConjoint)

# Simulation parameters (sample size, main effects, and varying interaction sizes)
n = 3000
main_x = 0.1
main_z = c(0.1, -0.1, 0.1, -0.1)
num_z = 10
num_int = c(0, 6, 12, 18)

# Parallel computing cluster
args = as.numeric(commandArgs(trailingOnly = TRUE))
loop_num = args[1]
param_num = args[2]

if (param_num == 1) {


  num_within = 1
  within_signal = rep(0, num_within)

  num_between = 1
  between_signal = rep(-0, num_between)
} else {

  int_signal = 0.06
  num_within = num_int[param_num]/2
  within_signal = rep(int_signal, num_within)

  num_between = num_int[param_num]/2
  between_signal = rep(-int_signal, num_between)
}
#

s = param_num*10000 + loop_num

df = simulate_dat(n = n, main_x = main_x, main_z = main_z, num_z = num_z, num_within = num_within, num_between = num_between, within_signal = within_signal, between_signal = between_signal, s= s)

# AMCE_estimator
AMCE_df = get_AMCEdf(df)
AMCE_df[colnames(AMCE_df)[-ncol(AMCE_df)]] = lapply(AMCE_df[colnames(AMCE_df)[-ncol(AMCE_df)]] , factor)

AMCE_short = lm(y ~ x_1, data = AMCE_df)
AMCE_short_pval = summary(AMCE_short)$coefficients[2, 4]

# HierNet Estimator
CRT_df = df
CRT_df[colnames(CRT_df)[-ncol(CRT_df)]] = lapply(CRT_df[colnames(CRT_df)[-ncol(CRT_df)]] , factor)

form = formula("y ~ x_1 + z1_1 + z2_1 + z3_1 + z4_1 + z5_1 + z6_1 + z7_1 + z8_1 + z9_1 + z10_1")
left = colnames(df)[seq(1, ncol(df) - 1, 2)]
right = colnames(df)[seq(2, ncol(df), 2)]

CRT_hiernet = CRT_pval(formula = form, data = CRT_df, X = "x_1", left = left, right = right, parallel = FALSE)

final_df = data.frame(matrix(c(AMCE_short_pval, CRT_hiernet$p_val), nrow = 1))

file_name = paste0("data/Fig3_leftplot_results/pval", param_num, "_", loop_num, ".csv")

write.csv(final_df, file = file_name)




