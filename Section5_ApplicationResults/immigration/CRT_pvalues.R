# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for generating all CRT p-values in Table 1 row 1 along with supplementary analysis related to immigration conjoint study
# written on 06/09/2022
# Approximate runtime listed under each p-value computation

library(CRTConjoint); library(data.table)

num_cores = 50
B = 2000

# obtaining immigration data
if (TRUE) {

  load("data/hh_data.RData")

  #Removing outlier cases
  x = x[!x$CaseID %in% c(757,  771, 1192 ,1412 ,1421, 1572, 1585 ,1607 ), ]

  hh_data = x[!is.na(x$Chosen_Immigrant), ]

  removed_df = hh_data

  hh_data$profiles = rep(c(1,2), nrow(hh_data)/2)

  #aligning left and right profiles
  hh_1 = hh_data[hh_data$profiles == 1, ]
  hh_2 = hh_data[hh_data$profiles == 2, ]
  colnames(hh_2) = paste0(colnames(hh_2), "_2")
  n = nrow(hh_data)
  hh_data = cbind(hh_1, hh_2)

  other_variables = colnames(hh_data)[c(4:12, 54:62, 26:29)]
  Y = hh_data$Chosen_Immigrant

  int_df = hh_data[, c(other_variables)]
  df = int_df
  classes = sapply(df, class)
  factor_idx = grepl("factor", classes)
  factor_idx = which(factor_idx == TRUE)
  for (i in 1:length(factor_idx)) {
    df[, factor_idx[i]] = factor(as.character(df[, factor_idx[i]]))
  }
  df$ppage = as.numeric(as.character(df$ppage))

  for (i in c(20:22)) {
    df[,i] = factor(as.character(df[,i]))
  }

  int_df = df
  int_df$Y = Y
}

form = formula("Y ~ FeatEd + FeatGender + FeatCountry + FeatReason + FeatJob +
FeatExp + FeatPlans + FeatTrips + FeatLang + ppage + ppeducat + ppethm + ppgender")
left = colnames(int_df)[1:9]
right = colnames(int_df)[10:18]

## main result: Comparing Mexican and European immigrants

# collapsing levels of Germany, France, and Poland to Europe
immigration_df = int_df
immigration_df$FeatCountry = as.character(immigration_df$FeatCountry)
immigration_df$FeatCountry_2 = as.character(immigration_df$FeatCountry_2)

immigration_df$FeatCountry[immigration_df$FeatCountry %in% c("Germany", "France", "Poland")] = "Europe"
immigration_df$FeatCountry_2[immigration_df$FeatCountry_2 %in% c("Germany", "France", "Poland")] = "Europe"

immigration_df$FeatCountry = factor(immigration_df$FeatCountry)
immigration_df$FeatCountry_2 = factor(immigration_df$FeatCountry_2)

immigration_result = CRT_pval(formula = form, data = immigration_df, X = "FeatCountry", left = left,
                              right = right, design = "Nonuniform", in_levs = c("Mexico", "Europe"),
                              p = c(0.25, 0.75), non_factor = "ppage", B = B, num_cores = num_cores,
                              analysis = 2, seed = 1)

save(immigration_result, file = "data/Table1/immigration/immigration_result.RData")
# 7 minutes for B = 2000 on 50 cores

## Profile order effect test
immigration_PO_effect = CRT_profileordereffect(formula = form, data = immigration_df, left = left,
                                 right = right, B = B, num_cores = num_cores, seed = 2)
# 6 minutes for B = 2000 on 50 cores

save(immigration_PO_effect, file = "data/Table1/immigration/immigration_PO_effect.RData")

## Carryover effect test
J = 5
carryover_df = int_df
carryover_df$task = rep(1:J, nrow(carryover_df)/J)

# manual resampling function
resample_func_immigration = function(x, seed = sample(c(0, 1000), size = 1), left_idx, right_idx) {
  set.seed(seed)
  df = x[, c(left_idx, right_idx)]
  variable = colnames(x)[c(left_idx, right_idx)]
  len = length(variable)
  resampled = list()
  n = nrow(df)
  for (i in 1:len) {
    var = df[, variable[i]]
    lev = levels(var)
    resampled[[i]] = factor(sample(lev, size = n, replace = TRUE))
  }

  resampled_df = data.frame(resampled[[1]])
  for (i in 2:len) {
    resampled_df = cbind(resampled_df, resampled[[i]])
  }
  colnames(resampled_df) = colnames(df)

  #escape persecution was dependently randomized
  country_1 = resampled_df[, "FeatCountry"]
  country_2 = resampled_df[, "FeatCountry_2"]
  i_1 = which((country_1 == "Iraq" | country_1 == "Sudan" | country_1 == "Somalia"))
  i_2 = which((country_2 == "Iraq" | country_2 == "Sudan" | country_2 == "Somalia"))

  reason_1 = resampled_df[, "FeatReason"]
  reason_2 = resampled_df[, "FeatReason_2"]
  levs = levels(reason_1)
  r_levs = levs[c(2,3)]

  reason_1 = sample(r_levs, size = n, replace = TRUE)

  reason_1[i_1] = sample(levs, size = length(i_1), replace = TRUE)

  reason_2 = sample(r_levs, size = n, replace = TRUE)

  reason_2[i_2] = sample(levs, size = length(i_2), replace = TRUE)

  resampled_df[, "FeatReason"] = reason_1
  resampled_df[, "FeatReason_2"] = reason_2

  #profession high skill fix
  educ_1 = resampled_df[, "FeatEd"]
  educ_2 = resampled_df[, "FeatEd_2"]
  i_1 = which((educ_1 == "Equivalent to completing two years of college in the US" |
                 educ_1 == "Equivalent to completing a college degree in the US" |
                 educ_1 == "Equivalent to completing a graduate degree in the US"))
  i_2 = which((educ_2 == "Equivalent to completing two years of college in the US" |
                 educ_2 == "Equivalent to completing a college degree in the US" |
                 educ_2 == "Equivalent to completing a graduate degree in the US"))


  job_1 = resampled_df[, "FeatJob"]
  job_2 = resampled_df[, "FeatJob_2"]
  levs = levels(job_1)
  # take out computer programmer, doctor, financial analyst, and research scientist
  r_levs = levs[-c(2,4,5, 9)]

  job_1 = sample(r_levs, size = n, replace = TRUE)

  job_1[i_1] = sample(levs, size = length(i_1), replace = TRUE)

  job_2 = sample(r_levs, size = n, replace = TRUE)

  job_2[i_2] = sample(levs, size = length(i_2), replace = TRUE)

  resampled_df[, "FeatJob"] = job_1
  resampled_df[, "FeatJob_2"] = job_2

  resampled_df[colnames(resampled_df)] = lapply(resampled_df[colnames(resampled_df)], factor )

  return(resampled_df)
}

own_resamples = list()
for (i in 1:B) {
  newdf = resample_func_immigration(carryover_df, left_idx = 1:9, right_idx = 10:18, seed = i)
  own_resamples[[i]] = newdf
}
carryover_test = CRT_carryovereffect(formula = form, data = carryover_df, left = left,
                                     right = right, task = "task", supplyown_resamples = own_resamples,
                                     B = B, num_cores = num_cores, seed = 2)
# 12 minutes for B = 2000 on 50 cores

save(immigration_carryover_effect, file = "data/Table1/immigration/immigration_carryover_effect.RData")

## Fatigue effect test
fatigue_df = immigrationdata
fatigue_df$task = rep(1:J, nrow(fatigue_df)/J)
fatigue_df$respondent = rep(1:(nrow(fatigue_df)/J), each = J)
fatigue_test = CRT_fatigueeffect(formula = form, data = fatigue_df, left = left,
                                 right = right, task = "task", respondent = "respondent",
                                 B = B, num_cores = num_cores, seed = 2)
# 6 minutes for B = 2000 on 50 cores
save(immigration_fatigue_effect, file = "data/Table1/immigration/immigration_fatigue_effect.RData")



## supplementary analysis with ethnocentrism
if (TRUE) {
  load("data/hh_data.RData")

  #Removing outlier cases
  x = x[!x$CaseID %in% c(757,  771, 1192 ,1412 ,1421, 1572, 1585 ,1607 ), ]

  hh_data = x[!is.na(x$Chosen_Immigrant), ]

  hh_data$profiles = rep(c(1,2), nrow(hh_data)/2)
  hh_1 = hh_data[hh_data$profiles == 1, ]
  hh_2 = hh_data[hh_data$profiles == 2, ]
  colnames(hh_2) = paste0(colnames(hh_2), "_2")
  n = nrow(hh_data)
  hh_data = cbind(hh_1, hh_2)


  black_white = which((hh_data$ppethm == "White, Non-Hispanic") | (hh_data$ppethm == "Black, Non-Hispanic"))
  BW_df = hh_data[black_white, ]

  ingroupTM = vector()
  ingroupTM[BW_df$ppethm == "White, Non-Hispanic"] =  BW_df$W1_Q8a[BW_df$ppethm == "White, Non-Hispanic"]
  ingroupTM[BW_df$ppethm == "Black, Non-Hispanic"] =  BW_df$W1_Q8b[BW_df$ppethm == "Black, Non-Hispanic"]


  outgroupTM = (BW_df$W1_Q5 + BW_df$W1_Q6 + BW_df$W1_Q7)/3

  ethno_var = ingroupTM - outgroupTM

  BW_df$ethno = ethno_var
  BW_df = BW_df[!is.na(BW_df$ethno), ]

  BW_df_mex = BW_df

  BW_df_mex$FeatCountry = as.character(BW_df_mex$FeatCountry)
  BW_df_mex$FeatCountry_2 = as.character(BW_df_mex$FeatCountry_2)

  BW_df_mex$FeatCountry[BW_df_mex$FeatCountry %in% c("Germany", "France", "Poland")] = "Europe"
  BW_df_mex$FeatCountry_2[BW_df_mex$FeatCountry_2 %in% c("Germany", "France", "Poland")] = "Europe"

  BW_df_mex$FeatCountry = factor(BW_df_mex$FeatCountry)
  BW_df_mex$FeatCountry_2 = factor(BW_df_mex$FeatCountry_2)


  Y = BW_df_mex$Chosen_Immigrant

  int_df = BW_df_mex[, c(4:12, 54:62, 101, 26:29)]
  int_df$Y = Y

  col_names = colnames(int_df)

  int_df[col_names[c(1:18, 21:23)]] = lapply(int_df[col_names[c(1:18, 21:23)]] , as.character)

  int_df[col_names[c(1:18, 21:23)]] = lapply(int_df[col_names[c(1:18, 21:23)]] , factor)

  int_df$ppage = as.numeric(as.character(int_df$ppage))
}

int_df$FeatCountry = as.character(int_df$FeatCountry)
int_df$FeatCountry_2 = as.character(int_df$FeatCountry_2)

int_df$FeatCountry[int_df$FeatCountry %in% c("Germany", "France", "Poland")] = "Europe"
int_df$FeatCountry_2[int_df$FeatCountry_2 %in% c("Germany", "France", "Poland")] = "Europe"

int_df$FeatCountry = factor(int_df$FeatCountry)
int_df$FeatCountry_2 = factor(int_df$FeatCountry_2)

ethno_df = int_df

form = formula("Y ~ FeatEd + FeatGender + FeatCountry + FeatReason + FeatJob +
FeatExp + FeatPlans + FeatTrips + FeatLang + ppage + ppeducat + ppethm + ppgender + ethno")
immigration_result_ethno = CRT_pval(formula = form, data = ethno_df, X = "FeatCountry", left = left,
                              right = right, design = "Nonuniform", in_levs = c("Mexico", "Europe"),
                              p = c(0.25, 0.75), non_factor = c("ppage", "ethno"), B = B, num_cores = num_cores,
                              analysis = 2, seed = 2)
# 6 minutes for B = 2000 on 50 cores
save(immigration_result_ethno, file = "data/Table1/immigration/immigration_result_ethno.RData")

