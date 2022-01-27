# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for generating observed test statistic when computing supplementary immigration conjoint p-value including ethnocentrism
# Runtime: Approximately 10 minutes

source("source/hiernet_source.R")

load("data/hh_data.RData")
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

# Getting ethnocentrism variable
outgroupTM = (BW_df$W1_Q5 + BW_df$W1_Q6 + BW_df$W1_Q7)/3

ethno_var = ingroupTM - outgroupTM

BW_df$ethno = ethno_var
BW_df = BW_df[!is.na(BW_df$ethno), ]

BW_df_mex = BW_df

BW_df_mex$FeatCountry = as.character(BW_df_mex$FeatCountry)
BW_df_mex$FeatCountry_2 = as.character(BW_df_mex$FeatCountry_2)

BW_df_mex$FeatCountry[BW_df_mex$FeatCountry %in% c("Germany", "France", "Poland")] = "White"
BW_df_mex$FeatCountry_2[BW_df_mex$FeatCountry_2 %in% c("Germany", "France", "Poland")] = "White"

BW_df_mex$FeatCountry = factor(BW_df_mex$FeatCountry)
BW_df_mex$FeatCountry_2 = factor(BW_df_mex$FeatCountry_2)


Y = factor(BW_df_mex$Chosen_Immigrant)

int_df = BW_df_mex[, c(4:12, 54:62, 101, 26:29)]
int_df$Y = Y

col_names = colnames(int_df)

int_df[col_names[1:18]] = lapply(int_df[col_names[1:18]] , as.character)

int_df[col_names[1:18]] = lapply(int_df[col_names[1:18]] , factor)

x_df = int_df

# Enforcing Constraints
first = c(1:9)
second = c(10:18)
n = nrow(x_df)
empty_df = x_df
y_new = abs(1 - (as.numeric(x_df$Y) - 1))
for (i in 1:length(first)) {
  empty_df[, first[i]] = x_df[, second[i]]
  empty_df[, second[i]] = x_df[, first[i]]
}

empty_df$Y = y_new


#take out ethno for now
final_df = rbind(x_df, empty_df)

final_df$ppage = as.numeric(as.character(final_df$ppage))

for (i in c(21:23)) {
  final_df[,i] = factor(as.character(final_df[,i]))
}

X = model.matrix(Y~ . , final_df, contrasts.arg = lapply(final_df[, c(1:18, 21:23)], contrasts, contrasts = FALSE))[, -1]

y_var = as.numeric(final_df$Y)- 1

left_idx = grep("Mexico|White", colnames(X))[1:2]
right_idx = grep("Mexico|White", colnames(X))[3:4]

lambda = c(20, 25, 30, 40)

best_lam = get_lam(lambda, X = X, y_var = y_var)

invisible(capture.output(fit <- hierNet.logistic(as.matrix(X), y_var, lam= best_lam, diagonal = FALSE,trace = 0)))

obs_test_stat = hiernet_group(fit, left_idx = left_idx, X = X, analysis = TRUE)

write.csv(obs_test_stat, file = "data/Section5_Supplementary_Analysis/immigration_ethnocentrism/obs_test_stat.csv")




