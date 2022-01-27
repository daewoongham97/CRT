# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Generating AMCE Data to obtain original AMCE p-value in Hainmueller and Hopkins (2014) for Table 1 row 1 column 2 

library(foreign)

load("data/hh_data.RData")

x = x[!x$CaseID %in% c(757,  771, 1192 ,1412 ,1421, 1572, 1585 ,1607 ), ]

hh_data = x[!is.na(x$Chosen_Immigrant), ]

int_df = hh_data[, 4:13]

int_df$FeatCountry = as.character(int_df$FeatCountry)

int_df$FeatCountry[int_df$FeatCountry %in% c("Germany", "France", "Poland")] = "European"

factor_idx = c(1:9)
for (i in 1:length(factor_idx)) {
  int_df[, factor_idx[i]] = factor(as.character(int_df[, factor_idx[i]]))
}

int_df = within(int_df, FeatCountry <- relevel(FeatCountry, ref = "India"))
int_df = within(int_df, FeatReason <- relevel(FeatReason, ref = "Reunite with family members already in the U.S."))

int_df$cluster = hh_data$CaseID
int_df$Chosen_Immigrant = factor(int_df$Chosen_Immigrant)
write.dta(int_df, "data/int_df_full.dta")



