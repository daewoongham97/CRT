# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for generating observed test statistic when computing p-value in Table 1 row 1 column 5
# Runtime: Approximately 8 minutes

source("source/immigration/immigration_df.R")
source("source/hiernet_source.R")

int_df = int_df[, c(1:18, 23)]
int_df$task = rep(1:5, nrow(int_df)/5)

df = int_df
first = c(1:9)
second = c(10:18)
n = nrow(df)
empty_df = df
y_new = 1 - (as.numeric(int_df$Y) - 1)
for (i in 1:length(first)) {
  empty_df[, first[i]] = df[, second[i]]
  empty_df[, second[i]] = df[, first[i]]
}

empty_df$Y = y_new

final_df = rbind(int_df, empty_df)

X = model.matrix(Y~ . , final_df, contrasts.arg = lapply(final_df[, c(1:18)], contrasts, contrasts = FALSE))[, -1]
y_var = as.numeric(final_df$Y)-1

# We are interesting in the task evaluation as X
idx = grep("task", colnames(X))
lambda = c(20, 30, 40)

best_lam = get_lam(lambda, X = X, y_var = y_var)

invisible(capture.output(fit <- hierNet.logistic(as.matrix(X), y_var, lam= best_lam, diagonal = FALSE,trace = 0)))


I_1 = as.vector(fit$th[idx, ])

I_2 = as.vector(t(fit$th[, idx]))

I = (I_1 + I_2)/2

# To not double count
obs_test_stat = sum(unique(round(I^2, digits = 13)))

write.csv(obs_test_stat, "data/Table1/immigration/fatigue_effect/obs_test_stat.csv")

