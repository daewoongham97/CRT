# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# source code for obtaining computationally efficient dI_CRT test statistics introduced in Appendix G

library(glmnet)

# Return dI_CRT test statistic as shown in Equation 14 in Appendix G (this function is built specifically for the conditions in our simulation in Appendix G and not generalizable)
# Inputs
# df: takes the dataframe containing all relevant covariates (X,Z)
# variable: index of column(s) in df that correspond to the variable of interest X
# other_variable: index of column(s) in df that correspond to all other variables Z
# Y: vector of response variable (should be same length as the number of rows of df)
# I: hyperparameter of dI_CRT (the I in dI_CRT). Corresponds to the number of variables chosen to interact in the second stage regression
# lambda: set of lambda to cross validate when running the lasso regression (default to NULL so glmnet chooses their own parameters)
dI_CRT = function(df, variable, other_variable, Y, I = 2, lambda = NULL) {
  
  #Seperating X and Z variables
  X = df[, variable]
  
  Z = data.frame(df[, other_variable])
  Z = (model.matrix(Y~ . , Z)[, -1])
  
  #Distilling Z (first stage regression)
  Y_Z_model = cv.glmnet(x = Z, y = Y,family = "binomial", alpha = 1, lambda = lambda)
  Y_Z = glmnet(Z, Y, lambda = Y_Z_model$lambda.min, family = "binomial", alpha = 1)
  beta_offsets = Z%*%as.vector(Y_Z$beta) 
  in_order = sort(abs(as.vector(Y_Z$beta)), decreasing = TRUE)[1:(2*I)]
  X_mat = model.matrix(Y~ .,data.frame(X))[, -1]
  
  #Choosing the top I variables to interact with (this function assumes all factors have two levels only, which is the case for our simulation, thus only one coefficients are estimated per variable)
  if (sum(as.numeric(in_order == 0)) == 2*I) {
    X_mat_concats = X_mat
  } else {
    order_indx = order(abs(as.vector(Y_Z$beta)), decreasing = TRUE)[1:(2*I)] #we take both within and between profile
    order_indx = order_indx[in_order !=0]
    
    in_Z = data.frame(Z[, order_indx])
    X_mat_concat = matrix(, nrow = nrow(X_mat), ncol = (ncol(X_mat)) * ncol(in_Z))
    concat_list = list()
    names = matrix(NA, nrow = ncol(X_mat), ncol = ncol(in_Z))
    for (i in 1:(ncol(X_mat))) {
      for (j in 1:ncol(in_Z)) {
        concat_list= append(concat_list, list(as.numeric(X_mat[,i]*in_Z[,j])))
        names[i,j] = paste0(colnames(X_mat)[i], ":", colnames(in_Z)[j])
      }
      
    }
    
    for (i in 1:ncol(X_mat_concat)) {
      X_mat_concat[,i] = concat_list[[i]]
    }
    
    X_mat_concats = cbind(X_mat, X_mat_concat)
    l = ncol(X_mat_concats)
    colnames(X_mat_concats)[(ncol(X_mat)+ 1):l] = as.vector(t(names))
    
  }
  
  
  new_df = data.frame(X_mat_concats)
  new_df[colnames(new_df)] = lapply(new_df[colnames(new_df)] , factor)
  new_df$y = Y
  
  #Fit second stage regression with X and the I forced interactions with Z (both within and between profile interactions)
  X_dI = model.matrix(y ~ . , new_df, contrasts.arg = lapply(new_df[, c(1:(ncol(new_df)-1))], contrasts, contrasts = FALSE))[, -1]
  
  Y_X_Z_model = cv.glmnet(x = X_dI, y = Y, offset = beta_offsets, family = "binomial", alpha = 1, lambda = NULL)
  
  Y_X_Z_model = glmnet(x = X_dI, y = Y, offset = beta_offsets, family = "binomial", alpha = 1, lambda = Y_X_Z_model$lambda.min)
  
  coefs_beta = as.vector(Y_X_Z_model$beta)
  
  #main effect contribution
  main = coefs_beta[1:2]
  main_means = mean(main)
  main_diff = sum((main - main_means)^2)
  #all interaction effects
  
  if (sum(as.numeric(in_order == 0)) == 2*I) {
    obs_test_stat = main_diff
  } else {
    groups = (length(coefs_beta)-4)/4
    I_int = list()
    for (i in 0:(groups-1)) {
      I_int[[i+1]] = c(coefs_beta[5+ 2*i], coefs_beta[5+2*i+1])
    }
    int_diff = vector()
    for (i in 1:length(I_int)) {
      int_diff[i] = sum((I_int[[i]] - mean(I_int[[i]]))^2)
    }
    if (sum(int_diff) == 0) {
      int_diff = 0
    } else { 
      int_diff = int_diff[int_diff != 0]
    }
    
    obs_test_stat = main_diff + mean(int_diff)
  }
  return(obs_test_stat)
}







