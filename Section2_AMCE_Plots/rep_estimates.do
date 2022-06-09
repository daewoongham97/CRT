* Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
* Plotting AMCE results from Hainmueller and Hopkins (2014)
* Producing Figure 1
* written on 06/09/2022
* Originally obtained from Hainmueller and Hopkins (2014) replication file: rep_estimates.do. Minor edits were made to contain only estimates relevant for our project 

clear all
** two helper programs to compute AMCEs		   					   

* adjustmat
* gets coef estimates and ses from the model
* eliminates interaction terms needed for restrictions
capture program drop adjustmat
program def adjustmat
capture matrix drop resmat
* get coefficients and ses
mat    coef = e(b)
mat    varr = vecdiag(e(V))
matmap varr se , m(sqrt(@))
mat    coef = coef' , se'
* eliminate constant
scalar R = rowsof(coef)-1
mat    coef = coef[1..R,1..2]
* eliminate interactions terms
local namess : rownames coef
 foreach el of local namess {
   local include = regexm("`el'", "#")
   if "`include'" != "1" {
                    matrix getthis = coef["`el'",1..2]
                    matrix resmat = nullmat(resmat) \ getthis
            }
 }
end
* findit matmap

* restrictamce				   
* estimates AMCE for attributes with interactions
* and replaces them in the results matrix
capture program drop restrictamce
program def restrictamce
* education
* reference cat: no formal
* AMCE is defined over subset of commmon support 
* for the reference and comparison category 
* AMCEs are weighted averages of educ effect 
* we average over low-skilled jobs since 
* reference category (no formal) only occurs with low-skilled jobs
foreach x of numlist 2(1)7 {
qui: lincom  (`x'.FeatEd * 1/7  + (`x'.FeatEd + `x'.FeatEd#2.FeatJob)  * 1/7 + /// 
                             (`x'.FeatEd + `x'.FeatEd#3.FeatJob)  * 1/7 + ///
                             (`x'.FeatEd + `x'.FeatEd#4.FeatJob)  * 1/7 + ///
							 (`x'.FeatEd + `x'.FeatEd#5.FeatJob)  * 0 + ///
							 (`x'.FeatEd + `x'.FeatEd#6.FeatJob)  * 1/7 + ///
							 (`x'.FeatEd + `x'.FeatEd#7.FeatJob)  * 1/7 + ///
							 (`x'.FeatEd + `x'.FeatEd#8.FeatJob)  * 0 + ///
							 (`x'.FeatEd + `x'.FeatEd#9.FeatJob)  * 1/7 + ///
							 (`x'.FeatEd + `x'.FeatEd#10.FeatJob) * 0 + ///
							 (`x'.FeatEd + `x'.FeatEd#11.FeatJob) * 0)
mat resmat[rownumb(resmat,"`x'.FeatEd"),1] = r(estimate) 
mat resmat[rownumb(resmat,"`x'.FeatEd"),2] = r(se)	
}

* profession
* reference cat: janitor
* low skilled jobs 1-4,6,7,9 we average over all educ levels
foreach x of numlist 2(1)4 6 7 9 {
qui: lincom  (`x'.FeatJob *1/7    + (`x'.FeatJob + 2.FeatEd#`x'.FeatJob) *1/7   + /// 
                               (`x'.FeatJob + 3.FeatEd#`x'.FeatJob) *1/7   + ///
                               (`x'.FeatJob + 4.FeatEd#`x'.FeatJob) *1/7   + ///
							   (`x'.FeatJob + 5.FeatEd#`x'.FeatJob) *1/7   + ///
							   (`x'.FeatJob + 6.FeatEd#`x'.FeatJob) *1/7   + ///
							   (`x'.FeatJob + 7.FeatEd#`x'.FeatJob) *1/7 )								  
mat resmat[rownumb(resmat,"`x'.FeatJob"),1] = r(estimate) 
mat resmat[rownumb(resmat,"`x'.FeatJob"),2] = r(se)									  
}
* other jobs occur only with high educ (Financial analyst, Computer programmer, research scientist, Doctor)
* so effect of going from janitor to these only exists for high educ levels
foreach x of numlist 5 8 10 11 {
qui: lincom  (`x'.FeatJob *0    + (`x'.FeatJob + 2.FeatEd#`x'.FeatJob) *0   + /// 
                             (`x'.FeatJob + 3.FeatEd#`x'.FeatJob) *0   + ///
                             (`x'.FeatJob + 4.FeatEd#`x'.FeatJob) *0   + ///
							 (`x'.FeatJob + 5.FeatEd#`x'.FeatJob) *1/3   + ///
							 (`x'.FeatJob + 6.FeatEd#`x'.FeatJob) *1/3   + ///
							 (`x'.FeatJob + 7.FeatEd#`x'.FeatJob) *1/3 )	
mat resmat[rownumb(resmat,"`x'.FeatJob"),1] = r(estimate) 
mat resmat[rownumb(resmat,"`x'.FeatJob"),2] = r(se)				  
}

* origin
* reference cat: India 
* India only occurs with application reason 1 and 2, but not reason 3 (escape persecution)
* so we average over reason 1 and 2
foreach x of numlist 1(1)5 7(1)10 {
qui: lincom  (`x'.FeatCountry * 1/2  + (`x'.FeatCountry + `x'.FeatCountry#2.FeatReason)  * 1/2 + /// 
                                  (`x'.FeatCountry + `x'.FeatCountry#3.FeatReason)  * 0 ) 
							      
mat resmat[rownumb(resmat,"`x'.FeatCountry"),1] = r(estimate) 
mat resmat[rownumb(resmat,"`x'.FeatCountry"),2] = r(se)
}
* application reason
* reference: reunite with family 
* reason 1 and 2 go with all countries
foreach x of numlist 2 {
qui: lincom  (`x'.FeatReason * 1/10  + (`x'.FeatReason + 2.FeatCountry#`x'.FeatReason)  * 1/10 + /// 
                                  (`x'.FeatReason + 3.FeatCountry#`x'.FeatReason)  * 1/10 + /// 
							      (`x'.FeatReason + 4.FeatCountry#`x'.FeatReason)  * 1/10 + /// 
								  (`x'.FeatReason + 5.FeatCountry#`x'.FeatReason)  * 1/10 + /// 
								  (`x'.FeatReason + 6.FeatCountry#`x'.FeatReason)  * 1/10 + /// 
								  (`x'.FeatReason + 7.FeatCountry#`x'.FeatReason)  * 1/10 + /// 
								  (`x'.FeatReason + 8.FeatCountry#`x'.FeatReason)  * 1/10 + /// 
								  (`x'.FeatReason + 9.FeatCountry#`x'.FeatReason)  * 1/10 + /// 
								  (`x'.FeatReason + 10.FeatCountry#`x'.FeatReason) * 1/10) 							      
mat resmat[rownumb(resmat,"`x'.FeatReason"),1] = r(estimate) 
mat resmat[rownumb(resmat,"`x'.FeatReason"),2] = r(se)
}
* reason 3 (escape persecution) occurs only with 4 countries (China, Sudan, Somalia, Iraq)
foreach x of numlist 3 {
qui: lincom  (`x'.FeatReason * 0  + (`x'.FeatReason + 2.FeatCountry#`x'.FeatReason)  * 0 + /// 
                               (`x'.FeatReason + 3.FeatCountry#`x'.FeatReason)  * 0 + /// 
							   (`x'.FeatReason + 4.FeatCountry#`x'.FeatReason)  * 0 + /// 
							   (`x'.FeatReason + 5.FeatCountry#`x'.FeatReason)  * 0 + /// 
							   (`x'.FeatReason + 6.FeatCountry#`x'.FeatReason)  * 0 + /// 
							   (`x'.FeatReason + 7.FeatCountry#`x'.FeatReason)  * 1/4 + /// 
							   (`x'.FeatReason + 8.FeatCountry#`x'.FeatReason)  * 1/4 + /// 
							   (`x'.FeatReason + 9.FeatCountry#`x'.FeatReason)  * 1/4 + /// 
							   (`x'.FeatReason + 10.FeatCountry#`x'.FeatReason) * 1/4) 		
mat resmat[rownumb(resmat,"`x'.FeatReason"),1] = r(estimate) 
mat resmat[rownumb(resmat,"`x'.FeatReason"),2] = r(se)
}
end


* bechmark model with poststratification weights
global mod =  "i.FeatGender i.FeatEd##i.FeatJob i.FeatLang ib6.FeatCountry##i.FeatReason i.FeatExp ib3.FeatPlans i.FeatTrips [pweight=weight2]"
* clustered ses
global ses =  "cl(CaseID)"

* load data
use "repdata.dta", clear

** Figure 2: 
** Effects of Immigrant Attributes on Probability of Being Preferred for Admission
reg Chosen_Immigrant $mod ,  $ses

esttab, se nostar brackets, using "../data/HH_results.csv"

