# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for generating observed test statistic when computing p-value in Table 1 row 2 column 4
# Runtime: Approximately 60 minutes with 200 cores of parallel computing
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

lambda = c(50, 60)
df = int_df
df$task = x$task[seq(1, nrow(x), by = 2)]
df$Y = factor(x$selected[seq(1, nrow(x), by = 2)])

Z_df = df[(df$task == 2 | df$task == 4 | df$task == 6 | df$task == 8 | df$task == 10), ][, c(1:26)]
colnames(Z_df) = paste0("Z_", colnames(Z_df))
X_df = df[(df$task == 1 | df$task == 3| df$task == 5| df$task == 7| df$task == 9), ][, c(1:26)]
colnames(X_df) = paste0("X_", colnames(X_df))
Y = df[(df$task == 2 | df$task == 4 | df$task == 6 | df$task == 8 | df$task == 10), ]$Y
original_Y = Y
original_Xdf = X_df
original_Zdf = Z_df

final_df = cbind(X_df, Z_df)
final_df$Y = Y

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

X = model.matrix(Y ~ ., data = final_df, contrasts.arg = lapply(final_df[, c(1:52)], contrasts, contrasts = FALSE))[, -1]
y_var = as.numeric(final_df$Y) - 1

idx = grep("X", colnames(X))

#resampling the entire dataframe function
resample_func_gender = function(df, variable, variable_lev = NULL, p = NULL, subset_var = NULL, subset_var_lev = NULL, seeds) {
  len = length(variable)
  resampled = list()
  n = nrow(df)
  len_2 = length(subset_var_lev)
  for (i in 1:len) {
    var = df[, variable[i]]
    var = factor(var)
    lev = levels(var)
    if (length(variable_lev) == 0) {
      set.seed(seeds + i)
      resampled[[i]] = sample(lev, size = n, replace = TRUE, prob = p[[i]][1])
    } else {
      new_lev = lev[!lev %in% variable_lev]
      set.seed(seeds + i)

      resampled[[i]] = sample(new_lev, size = n, replace = TRUE, prob = p[[i]][1])
      a = df[, subset_var]
      in_index = (a %in% subset_var_lev)
      set.seed(100*seeds)

      resampled[[i]][in_index] = sample(lev, size = sum(in_index), prob = p[[i]][2], replace = TRUE)

    }
    resampled[[i]] = factor(resampled[[i]])
  }

  resampled_df = data.frame(resampled[[1]])
  for (i in 2:len) {
    resampled_df = cbind(resampled_df, resampled[[i]])
  }
  colnames(resampled_df) = colnames(df)

  return(resampled_df)
}

args = as.numeric(commandArgs(trailingOnly = TRUE))
loop_num = args[1]

resampled_X_df = resample_func_gender(df = original_Xdf, variable = colnames(X_df), seeds = loop_num)

first = (1:(length(colnames(resampled_X_df))))[-c(grep("_2",colnames(resampled_X_df)))]
second = grep("_2",colnames(resampled_X_df))
n = nrow(resampled_X_df)
empty_df = resampled_X_df
for (i in 1:length(first)) {
  empty_df[, first[i]] = resampled_X_df[, second[i]]
  empty_df[, second[i]] = resampled_X_df[, first[i]]
}
resampled_X_df = rbind(resampled_X_df, empty_df, resampled_X_df, empty_df)

final_df = cbind(resampled_X_df, Z_df, Y = full_Y)

X = model.matrix(Y ~ ., data = final_df, contrasts.arg = lapply(final_df[, c(1:52)], contrasts, contrasts = FALSE))[, -1]
y_var = as.numeric(final_df$Y) - 1

best_lam = get_lam(lambda, X = X, y_var = y_var, seed = loop_num)

invisible(capture.output(fit <- hierNet.logistic(as.matrix(X), y_var, lam= best_lam, diagonal = FALSE,trace = 0)))

e = CO_stat(fit, idx)

file_name = paste0("data/Table1/gender/carryover_effect/", "_", loop_num, ".csv")

write.csv(e, file = file_name)
