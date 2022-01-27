# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for generating resampled test statistic when computing p-value in Table 1 row 1 column 1
# Done using 200 cores of parallel computing 
# This script is executed through p_val.sh

library(glmnet)
source("source/immigration/immigration_df.R")

int_df$FeatCountry = as.character(int_df$FeatCountry)
int_df$FeatCountry_2 = as.character(int_df$FeatCountry_2)

int_df$FeatCountry[int_df$FeatCountry %in% c("Germany", "France", "Poland")] = "White"
int_df$FeatCountry_2[int_df$FeatCountry_2 %in% c("Germany", "France", "Poland")] = "White"

int_df$FeatCountry = factor(int_df$FeatCountry)
int_df$FeatCountry_2 = factor(int_df$FeatCountry_2)

x_df = int_df

first = c(1:9)
second = c(10:18)
n = nrow(x_df)
empty_df = x_df
y_new = abs(1 - (as.numeric(x_df$Y) - 1))
for (i in 1:length(first)) {
  empty_df[, first[i]] = x_df[, second[i]]
  empty_df[, second[i]] = x_df[, first[i]]
}

empty_df$Y = y_new

final_df = rbind(x_df, empty_df)

X = model.matrix(Y ~ . , final_df, contrasts.arg = lapply(final_df[, c(1:18, 20:22)], contrasts, contrasts = FALSE))[, -1]
Y = final_df$Y

#resampling
idx_1 = which(x_df$FeatCountry == "Mexico" | x_df$FeatCountry == "White")
idx_2 = which(x_df$FeatCountry_2 == "Mexico" | x_df$FeatCountry_2 == "White")
num_sample_1 = length(idx_1)
num_sample_2 = length(idx_2)

args = as.numeric(commandArgs(trailingOnly = TRUE))
loop_num = args[1]

set.seed(loop_num)
country_1 = factor(sample(c("Mexico", "White"), size = num_sample_1, replace = TRUE, p = c(0.25, 0.75)))
country_2 = factor(sample(c("Mexico", "White"), size = num_sample_2, replace = TRUE, p = c(0.25, 0.75)))

x_df[, 3][idx_1] = country_1
x_df[, 12][idx_2] = country_2
empty_df = x_df
y_new = abs(1 - (as.numeric(x_df$Y) - 1))
for (i in 1:length(first)) {
  empty_df[, first[i]] = x_df[, second[i]]
  empty_df[, second[i]] = x_df[, first[i]]
}

empty_df$Y = y_new

final_df = rbind(x_df, empty_df)

X = model.matrix(Y ~ . , final_df, contrasts.arg = lapply(final_df[, c(1:18, 20:22)], contrasts, contrasts = FALSE))[, -1]

a = cv.glmnet(X, Y, alpha = 1, lambda = NULL, family = "binomial")
b = glmnet(X, Y, alpha = 1, lambda = a$lambda.min, family = "binomial")

resampled_test_stat =  (b$beta[13] - b$beta[17])^2

file_name = paste0("data/Section5_Supplementary_Analysis/immigration_lasso", "_", loop_num, ".csv")

write.csv(resampled_test_stat, file = file_name)






