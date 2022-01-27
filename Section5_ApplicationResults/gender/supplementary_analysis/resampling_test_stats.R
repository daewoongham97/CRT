# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for generating resampled test statistic when computing supplementary immigration conjoint p-value by forcing interaction with position on abortion
# Runtime: Approximately 24 minutes with 200 cores of parallel computing
# This script is executed through p_val.sh

source("source/gender/gender_df.R")
source("source/hiernet_source.R")
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

X = model.matrix(Y~ . + Sex*Position.on.abortion + Sex*Position.on.abortion_2 +Sex_2*Position.on.abortion + Sex_2*Position.on.abortion_2, final_df, contrasts.arg = lapply(final_df[, c(1:28, 30:36)], contrasts, contrasts = FALSE))[, -1]

lambda = c(25, 30, 40)

left_idx = grep("Sex", colnames(X))[c(1, 2, 7:18)]
odds = seq(1, length(left_idx), 2)
group = list()
for (i in 1:length(odds)) {
  group[[i]] = c(odds[i], odds[i]+1)
}

y_var = as.numeric(final_df$Y)- 1

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

X = model.matrix(Y~ . + Sex*Position.on.abortion + Sex*Position.on.abortion_2 +Sex_2*Position.on.abortion + Sex_2*Position.on.abortion_2, final_df, contrasts.arg = lapply(final_df[, c(1:28, 30:36)], contrasts, contrasts = FALSE))[, -1]

best_lam = get_lam(lambda, X = X, y_var = y_var, seed = loop_num)

print(best_lam)

invisible(capture.output(fit <- hierNet.logistic(as.matrix(X), y_var, lam= best_lam, diagonal = FALSE,trace = 0)))

e = hiernet_group(fit, left_idx = left_idx, forced = c(39:41, 88:90, 130:153), X = X, group = group,  analysis = FALSE)

file_name = paste0("data/Section5_Supplementary_Analysis/gender_abortion", "_", loop_num, ".csv")

write.csv(e, file = file_name)



