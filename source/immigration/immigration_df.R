# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# helper source code for reading in Immigration Conjoint Analysis data from Hainmueller and Hopkins (2014)

library(data.table)
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
Y = factor(hh_data$Chosen_Immigrant)

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



