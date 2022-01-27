# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for producing results in Figure 7 in Appendix
# Runtime: Less than 5 minutes with 200 cores of parallel computing
# This script is executed via right_fig_simulation.sh

library(stringr)

# Creates simple no signal simulation data given (X,Z) 
# n: sample size
# num_levels: number of levels of x,z (set to 4)
# num_z: number of Z (varies according to x-axis of Figure 7)
create_df = function(n, num_levels, num_z, seed) {
  set.seed(seed)
  x = sample(c(1:num_levels), size = n, replace = TRUE)
  z_mat = replicate(num_z, sample(c(1:num_levels), size = n, replace = TRUE))
  Y = sample(c(0, 1), size = n, replace = TRUE)
  df = data.frame(z_mat)
  colnames(df) = str_replace(colnames(df), "X", "Z")
  df$X = x; df$Y = Y
  for (i in 1:ncol(df)) {
    df[,i] = factor(df[, i])
  }
  return(df)
}

#Varying dimension of z
s = c(3, 5, 10, 11, 12, 13)

n = 5000; num_levels = 4

args = as.numeric(commandArgs(trailingOnly = TRUE))
loop_num = args[1]
param_num = args[2]

seed = loop_num*10000 + j*param_num
df = create_df(n = n, num_levels = num_levels, num_z = s[loop_num], seed = seed )
full_mod = glm(Y ~ .*.,data = df, family = "binomial")
col_idx = which(colnames(df) == "X")
sub_mod = glm(Y ~ .*., data = df[, -col_idx], family = "binomial")
p_value = anova(full_mod, sub_mod, test = "Chisq")[2, 5] 

file_name = paste0("data/Appendix_simulations/Fig7_results/pval", param_num, "_", loop_num, ".csv")
write.csv(e, file = file_name)

