# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for generating observed test statistic when using Lasso logistic regression test statistic (used as a supplementary tool to discern interaction impact)

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

a = cv.glmnet(X, Y, alpha = 1, lambda = NULL, family = "binomial")
b = glmnet(X, Y, alpha = 1, lambda = a$lambda.min, family = "binomial")

#pulling relevant main effects
obs_test_stat = (b$beta[13] - b$beta[17])^2

write.csv(obs_test_stat, file = "data/Section5_Supplementary_Analysis/immigration_lasso/obs_test_stat.csv")






