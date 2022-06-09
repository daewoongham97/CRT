# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Generating AMCE Data to obtain original AMCE p-value in Ono and Burden (2019) for Table 1 row 2 column 2
# written on 06/09/2022

library(data.table)
library(foreign)
load("data/POBE_R_data.RData")

col_names = names(x)

n = nrow(x)
x[col_names[4:18]] = lapply(x[col_names[4:18]] , factor)

gender = x

gender_pres = gender[gender$Office == "Congress", ]

Y = gender_pres$selected

int_df = gender_pres[, c(1, 4:18, 29)]

int_df$selected = as.numeric(int_df$selected) - 1

write.dta(int_df, "data/full_gender_df.dta")



