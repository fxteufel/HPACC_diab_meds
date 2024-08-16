********************************************************************************
********************************************************************************
* Global patterns of diabetes medication use								   *
* By: Felix Teufel						  								   	   *
* Last updated: 15 Aug 2024									  				   *
********************************************************************************

global data "[insert_path]/Data"												// insert path to data file
global results "[insert_path]/Results"											// insert path to results folder			

use "$data/HPACC_diab_meds_simul_clean.dta", clar
cd "$results" 																	// change directory to results folders

********************************************************************************
*** 				  SUBSET DATA & IMPLEMENT WEIGHTING		   				 ***			
********************************************************************************
	
** NUMBERS FOR FLOW DIAGRAM **
count 								// 403,148
drop if age <25 & age !=. 				// 68,222 observations deleted
drop if age ==. & age_ll25 !=1 			// 1,528 individuals (0.38%) excluded
drop if pregnant == 1 					// 5,894 observations deleted

count 								// 327,504
drop if biomarker == . 					// 100,082 (30.56% of 327,504) observations deleted
drop if w3 == 0							// 11 individuals in Iran who should not have gotten biomarkers measured (add to 100,082 = 100,093)
drop if dia_therapy	== .				// 4,091 (1.25% of 327,504) observations deleted
drop if psu_num == .					// 37 (0.01% of 327,504) observations deleted

count  								// 223,283
count if clin_dia_main == 1 		// 21,715				


** DEFINE ANALYTIC SUB-POPULATION FOR SVY **														

* WHOLE POPULATION 
use "$data/HPACC_diab_meds_simul_clean.dta", clar

gen analytic_pop_all = 1
replace analytic_pop_all = 0 if age ==. & age_ll25 !=1 
replace analytic_pop_all = 0 if age <25
replace analytic_pop_all = 0 if pregnant == 1 
replace analytic_pop_all = 0 if biomarker ==. 
replace analytic_pop_all = 0 if dia_therapy ==. 
replace analytic_pop_all = 0 if w3 == 0 
replace analytic_pop_all = 0 if psu_num ==. 

count if analytic_pop_all == 1 	// should be 223,283 --> confirmed
mdesc analytic_pop_all 			// missingness should be 0 --> confirmed

* DIABETICS
gen analytic_pop_diab = 0
replace analytic_pop_diab = 1 if analytic_pop_all == 1
replace analytic_pop_diab = 0 if clin_dia_main !=1 
replace analytic_pop_diab = 0 if ((insulin_new ==. | dia_med_new ==.) & clin_dia_main == 1)	

count if analytic_pop_diab == 1 	// should be 21,715 --> confirmed
mdesc analytic_pop_diab 			// missingness should be 0 --> confirmed

* DIAGNOSED DIABETICS
gen analytic_pop_diab_diag = 0
replace analytic_pop_diab_diag = 1 if analytic_pop_diab == 1 & hbg_new == 1


** IMPLEMENT SAMPLING WEIGHTS **

* Generate weight for analytic subpop *		
generate w3_analytic_all = w3 if analytic_pop_all == 1

* By country: Impute missing values of weights 
mdesc w3_analytic_all if analytic_pop_all == 1	 						// Missing for 293 (0.13%) 

bys country: egen double w3_analytic_all_mean = mean(w3_analytic_all) if w3_analytic_all!=0 	// Note: No observation actually have w3_analytic_all==0 
replace w3_analytic_all = w3_analytic_all_mean if w3_analytic_all ==. & analytic_pop_all == 1
mdesc w3_analytic_all if analytic_pop_all == 1		// 0

* Weighting each country equally
tab w3_analytic_all if w3_analytic_all <0.000001 // no obs with weight = 0
bys country : egen double all_obs_all = sum(w3_analytic_all) if w3_analytic_all !=. 
generate weq_w3_analytic_all=.
replace weq_w3_analytic_all = w3_analytic_all / all_obs_all				

forvalues x = 1/63 { 
sum weq_w3_analytic_all if countrycat == `x'	
display r(sum)										// should sum to 1 for each country --> verified (except for 3 countries it is around 0.99999998)
}

* Weighting each country according to its population size						
generate wpop_w3_analytic_all =.
replace wpop_w3_analytic_all = w3_analytic_all*Population2015/all_obs_all 

forvalues x = 1/63 { 
sum wpop_w3_analytic_all if countrycat == `x'	
display r(sum)	
tab	Population2015 if countrycat == `x'				// r(sum) and Population2015 should have same value for each country --> verified
}

* VERIFY *
mdesc weq_w3_analytic_all if analytic_pop_all == 1					// 0

** SET SURVEY DESIGN **	

svyset psu_num[pw = weq_w3_analytic_all], strata(stratum_num) singleunit(centered)  	// "singleunit(centered)" implies that strata with a single sampling unit are centered at the grand mean instead of the stratum mean	

** SAVE **
save "$data/HPACC_diab_meds_simul_final.dta", replace
