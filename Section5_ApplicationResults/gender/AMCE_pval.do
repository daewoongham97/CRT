* Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
* Producing AMCE p-values for Table 1 Row 2 Column 2 
* written on 06/09/2022

* First p-value
clear all 

use "data/full_gender_df.dta"

global ses =  "cl(respondentIndex)"

reg selected i.Sex, $ses

test 2.Sex

* Second p-value

reg selected i.Sex##i.Party_affiliation, $ses 

test 2.Sex 2.Sex#2.Party_affiliation 





