# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for generating observed test statistic when computing p-value in Table 1 row 1 column 3
# Runtime: Approximately 6 minutes with 200 cores of parallel computing
# This script is executed through p_val.sh

source("source/hiernet_source.R")
source("source/immigration/immigration_df.R")

X = model.matrix(Y~ . , int_df, contrasts.arg = lapply(int_df[, c(1:18, 20:22)], contrasts, contrasts = FALSE))[, -1]
y_var = as.numeric(int_df$Y) - 1

idx_1 = (1:ncol(X))[-grep("_2",colnames(X))][1:50]
idx_2 = grep("_2",colnames(X))

lambda = c(20, 25, 30)

# resampling part
args = as.numeric(commandArgs(trailingOnly = TRUE))
loop_num = args[1]

resampling_df = int_df
n = nrow(int_df)
set.seed(loop_num)
a = sample(c(0,1), size =n, replace = TRUE)
sample_idx = which(a == 1)

kept = resampling_df[-sample_idx, ]

b = resampling_df[sample_idx, ]
new_y = factor(1 - (as.numeric(b$Y) - 1))
new_first = b[, c(1:9)]
new_second = b[, c(10:18)]
name_1 = colnames(new_first)
name_2 = colnames(new_second)
colnames(new_first) = name_2
colnames(new_second) = name_1
new_df = cbind(new_second, new_first)
new_df = cbind(new_df, b[, 19:22])

new_df$Y = new_y
final_df = rbind(kept, new_df)

X = model.matrix(Y~ . , final_df, contrasts.arg = lapply(final_df[, c(1:18, 20:22)], contrasts, contrasts = FALSE))[, -1]

y_var = as.numeric(final_df$Y) - 1
best_lam = get_lam(lambda, X = X, y_var = y_var, seed = loop_num + 10000, unconstrained = TRUE)

invisible(capture.output(fit <- hierNet.logistic(as.matrix(X), y_var, lam= best_lam, diagonal = FALSE,trace = 0)))
e = PO_stat(fit, idx_1, idx_2, respond_idx = 101:112)

file_name = paste0("data/Table1/immigration/profile_order_effect/", "_", loop_num, ".csv")

write.csv(e, file = file_name)




