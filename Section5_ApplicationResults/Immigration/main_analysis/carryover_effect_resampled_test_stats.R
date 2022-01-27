# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for generating resampled test statistic when computing p-value in Table 1 row 1 column 4
# Runtime: Approximately 20 minutes with 200 cores of parallel computing
# This script is executed through p_val.sh

source("source/immigration/immigration_df.R")
source("source/hiernet_source.R")

lambda = c(40, 50)

df = int_df[, -c(19:22)]

df$task = rep(1:5, 1396)

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

args = as.numeric(commandArgs(trailingOnly = TRUE))
loop_num = args[1]

# this function resamples entire randomized factors as should be done in Algorithm 4
resample_func_immigration = function(df, variable, variable_lev = NULL, p = NULL, subset_var = NULL, subset_var_lev = NULL, seeds) {
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
      set.seed(seeds +i)

      resampled[[i]] = sample(new_lev, size = n, replace = TRUE, prob = p[[i]][1])
      a = df[, subset_var]
      in_index = (a %in% subset_var_lev)
      set.seed(100*seeds + i)

      resampled[[i]][in_index] = sample(lev, size = sum(in_index), prob = p[[i]][2], replace = TRUE)

    }
    resampled[[i]] = factor(resampled[[i]])
  }

  resampled_df = data.frame(resampled[[1]])
  for (i in 2:len) {
    resampled_df = cbind(resampled_df, resampled[[i]])
  }
  colnames(resampled_df) = colnames(df)

  #escape persecution fix
  country_1 = resampled_df[, 3]
  country_2 = resampled_df[, 12]
  i_1 = which((country_1 == "Iraq" | country_1 == "Sudan" | country_1 == "Somalia" | country_1 == "China"))
  i_2 = which((country_2 == "Iraq" | country_2 == "Sudan" | country_2 == "Somalia" | country_2 == "China"))

  reason_1 = resampled_df[, 4]
  reason_2 = resampled_df[, 13]
  levs = levels(reason_1)
  r_levs = levs[c(2,3)]

  set.seed(seeds*10 + 1)

  reason_1 = sample(r_levs, size = n, replace = TRUE)
  set.seed(seeds*11 + 1)

  reason_1[i_1] = sample(levs, size = length(i_1), replace = TRUE)
  set.seed(seeds*12 + 1)

  reason_2 = sample(r_levs, size = n, replace = TRUE)
  set.seed(seeds*13 + 1)

  reason_2[i_2] = sample(levs, size = length(i_2), replace = TRUE)

  resampled_df[, 4] = reason_1
  resampled_df[, 13] = reason_2

  #profession high skill fix
  educ_1 = resampled_df[, 1]
  educ_2 = resampled_df[, 10]
  i_1 = which((educ_1 == "Equivalent to completing two years of college in the US" | educ_1 == "Equivalent to completing a college degree in the US" | educ_1 == "Equivalent to completing a graduate degree in the US"))
  i_2 = which((educ_2 == "Equivalent to completing two years of college in the US" | educ_2 == "Equivalent to completing a college degree in the US" | educ_2 == "Equivalent to completing a graduate degree in the US"))


  job_1 = resampled_df[, 5]
  job_2 = resampled_df[, 14]
  levs = levels(job_1)
  r_levs = levs[-c(2,4,5)]

  set.seed(seeds*14 + 1)

  job_1 = sample(r_levs, size = n, replace = TRUE)

  set.seed(seeds*15 + 1)

  job_1[i_1] = sample(levs, size = length(i_1), replace = TRUE)
  set.seed(seeds*16 + 1)

  job_2 = sample(r_levs, size = n, replace = TRUE)
  set.seed(seeds*17 + 1)

  job_2[i_2] = sample(levs, size = length(i_2), replace = TRUE)

  resampled_df[, 5] = job_1
  resampled_df[, 14] = job_2
  return(resampled_df)
}

resampled_X_df = resample_func_immigration(df = original_Xdf, variable = colnames(original_Xdf), seeds = loop_num)

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

col_names = colnames(final_df)
final_df[col_names[c(1:36)]] = lapply(final_df[col_names[1:36]] , factor)

X = model.matrix(Y ~ ., data = final_df, contrasts.arg = lapply(final_df[, c(1:36)], contrasts, contrasts = FALSE))[, -1]
y_var = as.numeric(final_df$Y) - 1

best_lam = get_lam(lambda, X = X, y_var = y_var, seed = loop_num)
print(best_lam)

invisible(capture.output(fit <- hierNet.logistic(as.matrix(X), y_var, lam= best_lam, diagonal = FALSE,trace = 0)))

e = CO_stat(fit, idx)

file_name = paste0("data/Table1/immigration/carryover_effect/", "_", loop_num, ".csv")

write.csv(e, file = file_name)



