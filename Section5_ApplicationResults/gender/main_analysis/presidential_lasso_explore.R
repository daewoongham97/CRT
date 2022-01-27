# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code to find which interaction is strongest in Presidential dataset 
# This script is executed through p_val.sh (change i in {1..400} to {1..22})

library(glmnet)

source("source/gender/gender_df.R")

# Enforcing constraints
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

# Randomized factors
facs = c(2:13, 27:36)

args = as.numeric(commandArgs(trailingOnly = TRUE))
loop_num = args[1]

# loop_num <= 12 are all randomized factors while above 12 are respondent factors
if (loop_num <= 12) {
  a = formula(paste0("Y ~ . +", "Sex*",colnames(final_df)[facs[loop_num]], "+", "Sex*",colnames(final_df)[facs[loop_num] + 13], "+", "Sex_2*",colnames(final_df)[facs[loop_num]],"+","Sex_2*",colnames(final_df)[facs[loop_num] + 13]))
} else {
  a = formula(paste0("Y ~ . +", "Sex*",colnames(final_df)[facs[loop_num]], "+", "Sex_2*",colnames(final_df)[facs[loop_num]]))
}

X = model.matrix(a, final_df, contrasts.arg = lapply(final_df[, c(1:28, 30:36)], contrasts, contrasts = FALSE))[, -1]
Y = final_df$Y
a = cv.glmnet(X, Y, alpha = 1, lambda = NULL, family = "binomial")
b = glmnet(X, Y, alpha = 1, lambda = a$lambda.min, family = "binomial")

in_idx = grep(":", rownames(b$beta))
left_idx = in_idx[1:(length(in_idx)/2)]

odd = seq(1,(length(left_idx) - 1), 2)
in_coefs = b$beta[left_idx]

contribution = vector()

for (i in 1:length(odd)) {
  coefs = in_coefs[c(odd[i], odd[i] + 1)]
  contribution[i] = sum((coefs[1] - coefs[2])^2)
}

obs_test_stat = sum(contribution)

if (obs_test_stat == 0) {
  p_val = 1
} else {
  B = 400
  e = foreach(j = 1:B) %dopar% {
    set.seed(j)
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
    if (loop_num <= 12) {
      a = formula(paste0("Y ~ . +", "Sex*",colnames(final_df)[facs[loop_num]], "+", "Sex*",colnames(final_df)[facs[loop_num] + 13], "+", "Sex_2*",colnames(final_df)[facs[loop_num]],"+","Sex_2*",colnames(final_df)[facs[loop_num] + 13]))
    } else {
      a = formula(paste0("Y ~ . +", "Sex*",colnames(final_df)[facs[loop_num]], "+", "Sex_2*",colnames(final_df)[facs[loop_num]]))
    }    
    
    X = model.matrix(a, final_df, contrasts.arg = lapply(final_df[, c(1:28, 30:36)], contrasts, contrasts = FALSE))[, -1]
    a = cv.glmnet(X, Y, alpha = 1, lambda = NULL, family = "binomial")
    b = glmnet(X, Y, alpha = 1, lambda = a$lambda.min, family = "binomial")
    
    in_coefs = b$beta[left_idx]
    
    contribution = vector()
    
    for (i in 1:length(odd)) {
      coefs = in_coefs[c(odd[i], odd[i] + 1)]
      contribution[i] = sum((coefs[1] - coefs[2])^2)
    }
    
    e = sum(contribution)
    
    e
  }
  
  p_val = (length(which(unlist(e) >=  obs_test_stat)) + 1)/(length(e) + 1)
  
}

write.csv(p_val, file = paste0("data/gender_presidential_pvals/_", loop_num, ".csv"))

# corresponding variable with p_val index:
# colnames(final_df)[facs]



