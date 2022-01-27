# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for generating observed test statistic when computing p-value in Table 1 row 1 column 3
# Runtime: Approximately 3 minutes

source("source/hiernet_source.R")
source("source/immigration/immigration_df.R")

X = model.matrix(Y~ . , int_df, contrasts.arg = lapply(int_df[, c(1:18, 20:22)], contrasts, contrasts = FALSE))[, -1]
y_var = as.numeric(int_df$Y) - 1

idx_1 = (1:ncol(X))[-grep("_2",colnames(X))][1:50]
idx_2 = grep("_2",colnames(X))

lambda = c(20, 25, 30)

best_lam = get_lam(lambda, X = X, y_var = y_var, unconstrained = TRUE, seed = 7)

invisible(capture.output(fit <- hierNet.logistic(as.matrix(X), y_var, lam= best_lam, diagonal = FALSE,trace = 0)))

obs_test_stat = PO_stat(fit, idx_1, idx_2, respond_idx = 101:112)

write.csv(obs_test_stat, "data/Table1/immigration/profile_order_effect/obs_test_stat.csv")




