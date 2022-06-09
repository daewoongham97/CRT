* Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
* Producing AMCE p-value from original Hainmueller and Hopkins (2014)
* Producing Table 1 Row 1 Column 2 Result

clear all

use "data/int_df_full.dta"

global ses =  "cl(cluster)"

reg Chosen_Immigrant i.FeatCountry##i.FeatReason, $ses

test 3.FeatCountry + 0.5*(3.FeatCountry#3.FeatReason) = 5.FeatCountry + 0.5*(5.FeatCountry#3.FeatReason)
