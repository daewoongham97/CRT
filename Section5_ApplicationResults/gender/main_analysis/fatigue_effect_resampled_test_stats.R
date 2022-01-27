# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for generating resampled test statistic when computing p-value in Table 1 row 2 column 5
# Runtime: Approximately 30 minutes with 200 cores of parallel computing
# This script is executed through p_val.sh

source("source/hiernet_source.R")

load("data/POBE_R_data.RData")

col_names = names(x)

n = nrow(x)
x[col_names[4:18]] = lapply(x[col_names[4:18]] , factor)

x_1 = x[x$profile == 1, ]
x_2 = x[x$profile == 2, ]
colnames(x_2) <- paste0(colnames(x_2), "_2")

gender = cbind(x_1, x_2)
gender_pres = gender
variable = colnames(gender)[c(4:10, 12:17, 36:42, 44:49)]

Y = gender_pres$selected

df = gender_pres
int_df = data.frame(df[, variable])
int_df$Y = df$selected
int_df$task = x$task[seq(1, nrow(x), by = 2)]

df = int_df
first = c(1:13)
second = c(14:26)
n = nrow(df)
empty_df = df
y_new = 1 - (as.numeric(int_df$Y) - 1)
for (i in 1:length(first)) {
  empty_df[, first[i]] = df[, second[i]]
  empty_df[, second[i]] = df[, first[i]]
}

empty_df$Y = y_new

final_df = rbind(int_df, empty_df)

X = model.matrix(Y~ . , final_df, contrasts.arg = lapply(final_df[, c(1:26)], contrasts, contrasts = FALSE))[, -1]
y_var = as.numeric(final_df$Y)-1

idx = grep("task", colnames(X))

args = as.numeric(commandArgs(trailingOnly = TRUE))
loop_num = args[1]

n = length(unique(x$respondentIndex))
set.seed(loop_num)
a = as.vector(replicate(n, sample(1:10, replace = FALSE)))
resampled = final_df
resampled$task = c(a,a)
X = model.matrix(Y~ . , resampled, contrasts.arg = lapply(resampled[, c(1:26)], contrasts, contrasts = FALSE))[, -1]

best_lam = get_lam(lambda, X = X, y_var = y_var)

print(best_lam)

invisible(capture.output(fit <- hierNet.logistic(as.matrix(X), y_var, lam= best_lam, diagonal = FALSE,trace = 0)))

I_1 = as.vector(fit$th[idx, ])

I_2 = as.vector(t(fit$th[, idx]))

I = (I_1 + I_2)/2

e = sum(unique(round(I, digits = 13))^2)

file_name = paste0("data/Table1/gender/fatigue_effect/", "_", loop_num, ".csv")

write.csv(e, file = file_name)




