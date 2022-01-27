# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Generating AMCE data to obtain original AMCE p-value in Ono and Burden (2019) for Table 1 row 2 column 2 

library(data.table)
library(foreign)
load("data/POBE_R_data.RData")

col_names = names(x)

n = nrow(x)
x[col_names[4:18]] = lapply(x[col_names[4:18]] , factor)

gender_congress = x[x$Office == "Congress", ]

Y = gender_congress$selected

int_df = gender_congress[, c(1, 4:18, 29)]

int_df$selected = as.numeric(int_df$selected) - 1

write.dta(int_df, "data/congress_data.dta")



