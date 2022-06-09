# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for generating resampled test statistic when using Lasso logistic regression test statistic (used as a supplementary tool to discern interaction impact)
# written on 06/09/2022
# Done using 200 cores of parallel computing
# This script is executed through p_val.sh

library(glmnet)
source("source/gender/gender_df.R")

df = int_df
first = c(1:13)
second = c(14:26)
n = nrow(df)
empty_df = df
y_new = factor(1- (as.numeric(int_df$Y) - 1))

for (i in 1:length(first)) {
  empty_df[, first[i]] = df[, second[i]]
  empty_df[, second[i]] = df[, first[i]]
}

empty_df$Y = y_new

final_df = rbind(int_df, empty_df)

col_names = colnames(final_df)

final_df[col_names[c(27, 28, 30:36)]] = lapply(final_df[col_names[c(27, 28, 30:36)]] , factor)

X = model.matrix(Y~ . + Sex*Party.affiliation + Sex*Party.affiliation_2 +Sex_2*Party.affiliation + Sex_2*Party.affiliation_2, final_df, contrasts.arg = lapply(final_df[, c(1:28, 30:36)], contrasts, contrasts = FALSE))[, -1]

Y = final_df$Y
group = list(c(1, 2), c(130, 131), c(132, 133), c(134, 135), c(136, 137))

##resampling part
args = as.numeric(commandArgs(trailingOnly = TRUE))
loop_num = args[1]

set.seed(loop_num)
g1 = factor(sample(c("Male", "Female"), size = nrow(int_df), replace = TRUE))
g2 = factor(sample(c("Male", "Female"), size = nrow(int_df), replace = TRUE))
int_df[, 1] = g1
int_df[, 14] = g2
df = int_df
n = nrow(df)
empty_df = df
for (i in 1:length(first)) {
  empty_df[, first[i]] = df[, second[i]]
  empty_df[, second[i]] = df[, first[i]]
}

empty_df$Y = y_new
final_df = rbind(int_df, empty_df)

final_df[col_names[c(27, 28, 30:36)]] = lapply(final_df[col_names[c(27, 28, 30:36)]] , factor)

X = model.matrix(Y~ . + Sex*Party.affiliation + Sex*Party.affiliation_2 +Sex_2*Party.affiliation + Sex_2*Party.affiliation_2, final_df, contrasts.arg = lapply(final_df[, c(1:28, 30:36)], contrasts, contrasts = FALSE))[, -1]

a = cv.glmnet(X, Y, alpha = 1, lambda = NULL, family = "binomial")
b = glmnet(X, Y, alpha = 1, lambda = a$lambda.min, family = "binomial")

contribution = vector()
for (i in 1:length(group)) {
  in_coefs = b$beta[group[[i]]]
  contribution[i] = (in_coefs[1] - in_coefs[2])^2
}

resampled_test_stat =  sum(contribution)

file_name = paste0("data/Section5_Supplementary_Analysis/gender_lasso", "_", loop_num, ".csv")

write.csv(resampled_test_stat, file = file_name)




