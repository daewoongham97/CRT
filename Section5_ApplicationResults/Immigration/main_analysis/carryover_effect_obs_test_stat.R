# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for generating observed test statistic when computing p-value in Table 1 row 1 column 4
# Runtime: Approximately 10 minutes

source("source/immigration/immigration_df.R")
source("source/hiernet_source.R")

lambda = c(40, 50)

df = int_df[, -c(19:22)]

df$task = rep(1:5, 1396)

# Taking even numbered tasks as Z and odd numbered tasks as X
Z_df = df[(df$task == 2 | df$task == 4), ][, c(1:18)]
colnames(Z_df) = paste0("Z_", colnames(Z_df))
X_df = df[(df$task == 1 | df$task == 3), ][, c(1:18)]
colnames(X_df) = paste0("X_", colnames(X_df))
Y = df[(df$task == 2 | df$task == 4), ]$Y
original_Y = Y
original_Xdf = X_df
original_Zdf = Z_df

final_df = cbind(X_df, Z_df)
final_df$Y = Y

# Enforcing constraints
y_new = abs(1 - (as.numeric(final_df$Y) - 1))
x_df = X_df
first = (1:(length(colnames(X_df))))[-c(grep("_2",colnames(X_df)))]
second = grep("_2",colnames(X_df))
n = nrow(x_df)
empty_df = x_df
for (i in 1:length(first)) {
  empty_df[, first[i]] = x_df[, second[i]]
  empty_df[, second[i]] = x_df[, first[i]]
}
X_df = rbind(x_df, empty_df, x_df, empty_df)

z_df = Z_df
first = (1:(length(colnames(Z_df))))[-c(grep("_2",colnames(Z_df)))]
second = grep("_2",colnames(Z_df))
n = nrow(z_df)
empty_df = z_df
for (i in 1:length(first)) {
  empty_df[, first[i]] = z_df[, second[i]]
  empty_df[, second[i]] = z_df[, first[i]]
}
Z_df = rbind(Z_df, empty_df, empty_df, Z_df)

full_Y = factor(c((as.numeric(final_df$Y) - 1), y_new, y_new, (as.numeric(final_df$Y) - 1)))

final_df = cbind(X_df, Z_df, Y = full_Y)

X = model.matrix(Y ~ ., data = final_df, contrasts.arg = lapply(final_df[, c(1:36)], contrasts, contrasts = FALSE))[, -1]
y_var = as.numeric(final_df$Y) - 1

idx = grep("X", colnames(X))

best_lam = get_lam(lambda, X = X, y_var = y_var)

invisible(capture.output(fit <- hierNet.logistic(as.matrix(X), y_var, lam= best_lam, diagonal = FALSE,trace = 0)))

obs_test_stat = CO_stat(fit, idx)

write.csv(obs_test_stat, "data/Table1/immigration/carryover_effect/obs_test_stat.csv")

