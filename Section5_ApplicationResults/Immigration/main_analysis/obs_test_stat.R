# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for generating observed test statistic when computing p-value in Table 1 row 1 column 1
# Runtime: Approximately 10 minutes

source("source/immigration/immigration_df.R")
source("source/hiernet_source.R")

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

y_var = as.numeric(final_df$Y)- 1

left_idx = grep("Mexico|White", colnames(X))[1:2]

lambda = c(20, 25, 30, 40)

best_lam = get_lam(lambda, X = X, y_var = y_var)

invisible(capture.output(fit <- hierNet.logistic(as.matrix(X), y_var, lam= best_lam, diagonal = FALSE,trace = 0)))

obs_test_stat = hiernet_group(fit, idx = left_idx, X = X, analysis = TRUE)

write.csv(obs_test_stat, file = "data/Table1/immigration/main_CRT_results/obs_test_stat.csv")





