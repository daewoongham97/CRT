# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for producing Apppendix results for Figure 4-8
# written on 06/09/2022
# Runtime: Approximately 448 minutes with 200 cores of parallel computing - for each simulation parameter calculating 1000 p-values takes 112 minutes (4 simulation parameters)
# This script is executed through right_fig_simulation.sh

source("source/sim_source.R")

library(glmnet); library(CRTConjoint); library(miceadds); library(lmtest); library(sandwich)

# Simulation parameters (sample size, main effects, and varying interaction sizes)
n = 3000
main_x = 0.1
main_z = c(0.1, -0.1, 0.1, -0.1)
num_z = 10
num_int = c(0, 6, 12, 18)
int_signal = 0.06

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

  num_within = num_int[param_num]/2
  within_signal = rep(int_signal, num_within)

  num_between = num_int[param_num]/2
  between_signal = rep(-int_signal, num_between)
}
#

s = param_num*10000 + loop_num

df = simulate_dat(n = n, main_x = main_x, main_z = main_z, num_z = num_z, num_within = num_within, num_between = num_between, within_signal = within_signal, between_signal = between_signal, s= s)

CRT_df = df
CRT_df[colnames(CRT_df)[-ncol(CRT_df)]] = lapply(CRT_df[colnames(CRT_df)[-ncol(CRT_df)]] , factor)

form = formula("y ~ x_1 + z1_1 + z2_1 + z3_1 + z4_1 + z5_1 + z6_1 + z7_1 + z8_1 + z9_1 + z10_1")
left = colnames(df)[seq(1, ncol(df) - 1, 2)]
right = colnames(df)[seq(2, ncol(df), 2)]

# Generating Appendix Figure 4: Unconstrained HierNet
CRT_hiernet_uncons = CRT_pval(formula = form, data = CRT_df, X = "x_1", left = left, right = right, profileorder_constraint = FALSE, parallel = FALSE)
# Approximately 4 minutes per p-value


# Generating Appendix Figure 8: Slower HierNet
CRT_hiernet_slower = CRT_pval(formula = form, data = CRT_df, X = "x_1", left = left, right = right, speedup = FALSE, tol = 1e-6, parallel = FALSE)
# Approximately 100 minutes per p-value

# Generating Appendix Figure 6: AMCE with all (X,Z)
AMCE_df = get_AMCEdf(df)
AMCE_df[colnames(AMCE_df)[-ncol(AMCE_df)]] = lapply(AMCE_df[colnames(AMCE_df)[-ncol(AMCE_df)]] , factor)

AMCE_long = lm(y ~ ., data = AMCE_df)
AMCE_long_pval = summary(AMCE_long)$coefficients[2, 4]

# Generating Appendix Figure 7: Random Effects Model
df_RE = simulate_dat_RE(n = n, main_x = main_x, main_z = main_z, num_z = num_z, num_within = num_within, num_between = num_between, within_signal = within_signal, between_signal = between_signal, J = 5, RE_var = 0.1,s= s)

df = df_RE[, -ncol(df_RE)]
AMCE_df = get_AMCEdf(df)
AMCE_df[colnames(AMCE_df)[-ncol(AMCE_df)]] = lapply(AMCE_df[colnames(AMCE_df)[-ncol(AMCE_df)]] , factor)

AMCE_df$respondent_idx = c(df_RE$respondent_idx, df_RE$respondent_idx)

# use lm.cluster clustering by respondent index
AMCE_short = lm(y ~ x_1, data = AMCE_df)
AMCE_result = coeftest(AMCE_short, vcov = vcovCL, type = "HC1", cluster = ~respondent_idx)
AMCE_short_pval_RE = AMCE_result[2, 4]

CRT_df = df
CRT_df[colnames(CRT_df)[-ncol(CRT_df)]] = lapply(CRT_df[colnames(CRT_df)[-ncol(CRT_df)]] , factor)

CRT_hiernet_RE = CRT_pval(formula = form, data = CRT_df, X = "x_1", left = left, right = right, parallel = FALSE)
# Approximately 4 minutes per p-value


# Generating Appendix Figure 5: Heterogeneous Interaction Strengths
if (param_num == 1) {


  num_within = 1
  within_signal = rep(0, num_within)

  num_between = 1
  between_signal = rep(-0, num_between)
} else {

  num_within = num_int[param_num]/2
  within_signal = rep(int_signal, num_within)*sqrt(4/3)

  num_between = num_int[param_num]/2
  between_signal = rep(-int_signal, num_between)*sqrt(2/3)
}



df = simulate_dat(n = n, main_x = main_x, main_z = main_z, num_z = num_z, num_within = num_within, num_between = num_between, within_signal = within_signal, between_signal = between_signal, s= s)

CRT_df = df
CRT_df[colnames(CRT_df)[-ncol(CRT_df)]] = lapply(CRT_df[colnames(CRT_df)[-ncol(CRT_df)]] , factor)

CRT_hiernet_hetero = CRT_pval(formula = form, data = CRT_df, X = "x_1", left = left, right = right, parallel = FALSE)
# Approximately 4 minutes per p-value

final_df = data.frame(matrix(c(CRT_hiernet_uncons$p_val, CRT_hiernet_slower$p_val, CRT_hiernet_hetero$p_val, AMCE_long_pval, AMCE_short_pval_RE, CRT_hiernet_RE$p_val), nrow = 1))

file_name = paste0("data/Appendix_simulations/rightplot_result/pval", param_num, "_", loop_num, ".csv")

write.csv(final_df, file = file_name)




