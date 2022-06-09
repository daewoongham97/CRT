# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# source file for generating all simulation datasets
# written on 06/09/2022

# Returns dataframe to obtain AMCE estimates as done in original Haimueller, Hopkins, Yamamoto (2013) paper
get_AMCEdf = function(x_df) {
  second_idx = which(sapply(strsplit(colnames(x_df)[-ncol(x_df)], "_"), "[[", 2) == "2")

  y_1 = x_df$y
  first = x_df[, -second_idx]
  second = x_df[, second_idx]
  y_2 = 1-y_1

  second$y = y_2
  names(second) = names(first)
  AMCE_df = rbind(first, second)
  return(AMCE_df)
}

### Simulation data generating function
## Inputs:
# n: sample size
# main_x: scalar indicating the main effect signal of X
# main_z: vector indicating all non-zero main effects of Z
# num_z: total dimension of Z
# num_within: total number of within-profile interaction with X and Z
# num_between: total number of between-profile interaction with X and Z
# within_signal: vector of within-profile interaction signal sizes (length of vector should match num_within)
# between_signal: vector of between-profile interaction signal sizes (length of vector should match num_between)
# z_interact: vector of signals of all interactions among Z
# s: seed number for replicability
simulate_dat = function(n, main_x, main_z, num_z, num_within, num_between, within_signal, between_signal,
                        z_interact = rep(0.05, 15), s = sample(1:1000, size = 1)) {
  set.seed(s)
  x1 = sample(c(-0.5, 0.5), size = n, replace = TRUE)
  x2 = sample(c(-0.5, 0.5), size = n, replace = TRUE)

  z_df = list()
  for (i in 1:num_z) {
    pair_1 = sample(c(-0.5, 0.5), size = n, replace = TRUE)
    pair_2 = sample(c(-0.5, 0.5), size = n, replace = TRUE)
    a = data.frame(pair_1, pair_2)
    name_1 = paste0("z", i, "_1")
    name_2 = paste0("z", i, "_2")
    colnames(a) = c(name_1, name_2)

    z_df[[i]] = a
  }

  final_z = z_df[[1]]
  for (i in 2:length(z_df)) {
    final_z = cbind(final_z, z_df[[i]])
  }

  df = cbind(x1, x2, final_z)

  #main x_signal
  x_main = rep(0, n)
  x_diff = x1 - x2
  x_main[x_diff == 1] =  main_x
  x_main[x_diff == -1] =  -main_x

  #main z_signal
  z_main = rep(0, n)
  picked_z = 1:length(main_z)


  idx_1 = grep("_1", colnames(final_z))
  idx_2 = grep("_2", colnames(final_z))

  z_signals = list()
  for (i in 1:length(main_z)) {
    z_signals[[i]] = rep(0, n)
    left_z = final_z[, idx_1[picked_z[i]]]
    right_z = final_z[, idx_2[picked_z[i]]]
    z_diff = left_z - right_z
    z_signals[[i]][z_diff == 1] = main_z[i]
    z_signals[[i]][z_diff == -1] = -main_z[i]
  }

  #within profile interact
  picked_within = sample(1:num_z, size = num_within, replace = FALSE)
  within_signals = list()
  for (i in 1:num_within) {
    within_signals[[i]] = rep(0, n)
    left_z = final_z[, idx_1[picked_within[i]]]
    right_z = final_z[, idx_2[picked_within[i]]]
    x_z_within = x1*left_z - x2*right_z
    within_signals[[i]][x_z_within == 0.5] = within_signal[i]
    within_signals[[i]][x_z_within == -0.5] = -within_signal[i]
  }

  #between profile interact
  picked_between = sample(1:num_z, size = num_between, replace = FALSE)
  between_signals = list()
  for (i in 1:num_between) {
    between_signals[[i]] = rep(0, n)
    left_z = final_z[, idx_1[picked_between[i]]]
    right_z = final_z[, idx_2[picked_between[i]]]
    x_z_between = x1*right_z - x2*left_z
    between_signals[[i]][x_z_between == 0.5] = between_signal[i]
    between_signals[[i]][x_z_between == -0.5] = -between_signal[i]
  }

  #z interacts
  all_possible_interacts = combn(num_z, 2)
  picked_z = sample(1:dim(all_possible_interacts)[2], size = length(z_interact), replace = FALSE)
  z_interacts = list()
  for (i in 1:length(z_interact)) {
    z_interacts[[i]] = rep(0, n)
    left_z1 = final_z[, idx_1[all_possible_interacts[, picked_z[i]][1]]]
    right_z1 = final_z[, idx_2[all_possible_interacts[, picked_z[i]][1]]]
    left_z2 = final_z[, idx_1[all_possible_interacts[, picked_z[i]][2]]]
    right_z2 = final_z[, idx_2[all_possible_interacts[, picked_z[i]][2]]]

    z_z_within = left_z1*left_z2 - right_z1*right_z2
    z_interacts[[i]][z_z_within == 0.5] = z_interact[i]
    z_interacts[[i]][z_z_within== -0.5] = -z_interact[i]
  }

  y_probs = x_main + Reduce("+", z_signals) + Reduce("+", within_signals) + Reduce("+", between_signals) + Reduce("+", z_interacts)
  y_probs = 1/(1+exp(-y_probs))
  y = vector()

  for (i in 1:n) {
    y[i] = sample(c(0,1), size =1, replace = TRUE, prob = c(1- y_probs[i], y_probs[i]))
  }

  df$y = y
  colnames(df)[1:2] = c("x_1", "x_2")
  return(df)
}

# similar functionality as simulate_dat() except we have J responses per n/J respondent
# Each respondent has random effect
simulate_dat_RE = function(n, main_x, main_z, num_z, num_within, num_between, within_signal, between_signal,
                           z_interact = rep(0.05, 15), J = 5, RE_var = 0.1, s = sample(1:1000, size = 1)) {
  set.seed(s)
  x1 = sample(c(-0.5, 0.5), size = n, replace = TRUE)
  x2 = sample(c(-0.5, 0.5), size = n, replace = TRUE)

  z_df = list()
  for (i in 1:num_z) {
    pair_1 = sample(c(-0.5, 0.5), size = n, replace = TRUE)
    pair_2 = sample(c(-0.5, 0.5), size = n, replace = TRUE)
    a = data.frame(pair_1, pair_2)
    name_1 = paste0("z", i, "_1")
    name_2 = paste0("z", i, "_2")
    colnames(a) = c(name_1, name_2)

    z_df[[i]] = a
  }

  final_z = z_df[[1]]
  for (i in 2:length(z_df)) {
    final_z = cbind(final_z, z_df[[i]])
  }

  df = cbind(x1, x2, final_z)

  #main x_signal
  x_main = rep(0, n)
  x_diff = x1 - x2
  x_main[x_diff == 1] =  main_x
  x_main[x_diff == -1] =  -main_x

  #main z_signal
  z_main = rep(0, n)
  picked_z = 1:length(main_z)


  idx_1 = grep("_1", colnames(final_z))
  idx_2 = grep("_2", colnames(final_z))

  z_signals = list()
  for (i in 1:length(main_z)) {
    z_signals[[i]] = rep(0, n)
    left_z = final_z[, idx_1[picked_z[i]]]
    right_z = final_z[, idx_2[picked_z[i]]]
    z_diff = left_z - right_z
    z_signals[[i]][z_diff == 1] = main_z[i]
    z_signals[[i]][z_diff == -1] = -main_z[i]
  }

  #within profile interact
  picked_within = sample(1:num_z, size = num_within, replace = FALSE)
  within_signals = list()
  for (i in 1:num_within) {
    within_signals[[i]] = rep(0, n)
    left_z = final_z[, idx_1[picked_within[i]]]
    right_z = final_z[, idx_2[picked_within[i]]]
    x_z_within = x1*left_z - x2*right_z
    within_signals[[i]][x_z_within == 0.5] = within_signal[i]
    within_signals[[i]][x_z_within == -0.5] = -within_signal[i]
  }

  #between profile interact
  picked_between = sample(1:num_z, size = num_between, replace = FALSE)
  between_signals = list()
  for (i in 1:num_between) {
    between_signals[[i]] = rep(0, n)
    left_z = final_z[, idx_1[picked_between[i]]]
    right_z = final_z[, idx_2[picked_between[i]]]
    x_z_between = x1*right_z - x2*left_z
    between_signals[[i]][x_z_between == 0.5] = between_signal[i]
    between_signals[[i]][x_z_between == -0.5] = -between_signal[i]
  }

  #z interacts
  all_possible_interacts = combn(num_z, 2)
  picked_z = sample(1:dim(all_possible_interacts)[2], size = length(z_interact), replace = FALSE)
  z_interacts = list()
  for (i in 1:length(z_interact)) {
    z_interacts[[i]] = rep(0, n)
    left_z1 = final_z[, idx_1[all_possible_interacts[, picked_z[i]][1]]]
    right_z1 = final_z[, idx_2[all_possible_interacts[, picked_z[i]][1]]]
    left_z2 = final_z[, idx_1[all_possible_interacts[, picked_z[i]][2]]]
    right_z2 = final_z[, idx_2[all_possible_interacts[, picked_z[i]][2]]]

    z_z_within = left_z1*left_z2 - right_z1*right_z2
    z_interacts[[i]][z_z_within == 0.5] = z_interact[i]
    z_interacts[[i]][z_z_within== -0.5] = -z_interact[i]
  }

  # adding random effects
  RE = rnorm(n/J, mean = 0, sd = RE_var)
  RE = rep(RE, each = J)
  y_probs = x_main + Reduce("+", z_signals) + Reduce("+", within_signals) + Reduce("+", between_signals) + Reduce("+", z_interacts) +RE
  y_probs = 1/(1+exp(-y_probs))
  y = vector()

  for (i in 1:n) {
    y[i] = sample(c(0,1), size =1, replace = TRUE, prob = c(1- y_probs[i], y_probs[i]))
  }

  df$y = y
  colnames(df)[1:2] = c("x_1", "x_2")
  df$respondent_idx = rep(1:(n/J), each = J)
  return(df)
}








