# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for generating all CRT p-values in Table 1 row 2 along with supplementary analysis related to gender conjoint study
# written on 06/09/2022
# Approximate runtime listed under each p-value computation

library(CRTConjoint); library(data.table)

num_cores = 50
B = 2000

if (TRUE) {

  load("data/POBE_R_data.RData")

  col_names = names(x)

  n = nrow(x)
  x[col_names[4:18]] = lapply(x[col_names[4:18]] , factor)

  #aligning left and right profiles
  x_1 = x[x$profile == 1, ]
  x_2 = x[x$profile == 2, ]
  colnames(x_2) <- paste0(colnames(x_2), "_2")

  gender = cbind(x_1, x_2)
  #Extracting Congressional candidates
  gender_pres = gender[gender$Office == "Congress", ]
  #taking relevant covariates
  variable = colnames(gender)[c(4:10, 12:17, 36:42, 44:49, 22:24, 26:32)]

  Y = gender_pres$selected

  df = gender_pres
  int_df = data.frame(df[, variable])
  int_df$Y = as.numeric(df$selected) - 1

  int_df = int_df[int_df$R_Hillary != "NA", ]
  int_df = int_df[int_df$R_Partisanship != "NA", ]

  int_df = na.omit(int_df)
  col_names = colnames(int_df)
  int_df[col_names[c(27, 28, 30:36)]] = lapply(int_df[col_names[c(27, 28, 30:36)]] , factor)


}

form = formula("Y ~ Sex + Age + Family + Race + Experience.in.public.office + Salient.personal.characteristics +
               Party.affiliation + Policy.area.of.expertise + Position.on.national.security + Position.on.immigrants +
               Position.on.abortion + Position.on.government.deficit + Favorability.rating.among.the.public +
               R_Sex + R_Education + R_Age + R_Class + R_Region + R_Race + R_Partisanship + R_Hillary + R_Interest +
               R_Ideology")

gender_df = int_df
left = colnames(gender_df)[1:13]
right = colnames(gender_df)[14:26]

## main result: Testing whether gender matters
gender_result = CRT_pval(formula = form, data = gender_df, X = "Sex", left = left, right = right,
                         forced_var = "Party.affiliation",non_factor = "R_Age", analysis = 2, seed = 2,
                         B = B, num_cores = num_cores)
#7 minutes for B = 2000 on 50 cores

save(gender_result, file = "data/Table1/gender/gender_result.RData")



## Profile order effect test
gender_PO_effect = CRT_profileordereffect(formula = form, data = gender_df, left = left, right = right, B = B, seed = 2, num_cores = num_cores)
#6 minutes for B = 2000 on 50 cores

save(gender_PO_effect, file = "data/Table1/gender/gender_PO_effect.RData")



## Carryover effect test
# Test on whole data not just Congressional candidates
if (TRUE) {

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

  Y = as.numeric(gender_pres$selected) - 1

  df = gender_pres
  int_df = data.frame(df[, variable])
  int_df$Y = Y
  int_df$task = x$task[seq(1, nrow(x), by = 2)]

  int_df = na.omit(int_df)
  col_names = colnames(int_df)
}

carryover_df = int_df

gender_carryover_effect = CRT_carryovereffect(formula = form, data = carryover_df, left = left, right = right, task = "task", B = B, seed = 2, num_cores = num_cores)
# 20 minutes for B = 2000 on 50 cores

save(gender_carryover_effect, file = "data/Table1/gender/gender_carryover_effect.RData")

## Fatigue effect test
J = 10
fatigue_df = carryover_df
fatigue_df$respondent_idx = rep(1:(nrow(fatigue_df)/J), each = J)
gender_fatigue_effect = CRT_fatigueeffect(formula = form, data = fatigue_df, left = left, right = right, task = "task", respondent = "respondent_idx", B = B, seed = 2, num_cores = num_cores)
# 6.5 minutes for B = 2000 on 50 cores

save(gender_fatigue_effect, file = "data/Table1/gender/gender_fatigue_effect.RData")


## Supplementary Analysis forced with abortion
gender_result_abortion = CRT_pval(formula = form, data = gender_df, X = "Sex", left = left, right = right, forced_var = "Position.on.abortion", non_factor = "R_Age", analysis = 2, seed = 2, B = B, num_cores = num_cores)
#7 minutes for B = 2000 on 50 cores

save(gender_result_abortion, file = "data/Table1/gender/gender_result_abortion.RData")





