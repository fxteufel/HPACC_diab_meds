********************************************************************************
********************************************************************************
* Global patterns of diabetes medication use								   *
* By: Felix Teufel						  								   	   *
* Last updated: 15 Aug 2024									  				   *
********************************************************************************

global data "[insert_path]/Data"												// insert path to data file
global results "[insert_path]/Results"											// insert path to results folder			

use "$data/HPACC_diab_meds_simul.dta", clear
cd "$results" 																	// change directory to results folders

********************************************************************************
*** 							DATA CLEANING				   				 ***
********************************************************************************

** CLEANING VARIABLES **

* Clean education 	
replace educat = . if educat == .c	
replace edyears = . if edyears >77

* Clean sex
replace sex = . if sex > 1			// 151 to missing

* Pregnancy status 
replace pregnant = . if pregnant > 1

* Clean diabetes biomarkers (replace placeholders = .)
replace hba1c_p = . if hba1c_p < 3 				// 0 changes
replace hba1c_p = . if hba1c_p > 17				// 17 = largest value; rest are placeholders
replace hba1c_m = . if hba1c_m < 9.3 			// 0 changes
replace hba1c_m = . if hba1c_m > 162.4			// 162.31 = largest value; rest are placeholders	
replace fbg_new = . if fbg_new < 2.2  			// 0 changes
replace fbg_new = . if fbg_new > 33.3			// 33.3 = largest value; rest are placeholders
replace dia_med_new = . if dia_med_new > 1		// set placeholder = .
replace insulin_new = . if insulin_new > 1		// set placeholder = .

* Clean diabetes cascade variables
gen dia_therapy = .
replace dia_therapy = 1 if insulin_new == 1 | dia_med_new == 1
replace dia_therapy = 0 if insulin_new == 0 & dia_med_new == 0					// Note: we restrict our analysis to individuals with complete data on both insulin_new and dia_med_new
replace dia_therapy = . if insulin_new ==. | dia_med_new == .

tab dia_therapy hbg_new						// 	Of 341,524 people who are not diagnosed, 0 are on treatment; all 14,927 on treatment are also diagnosed
count if dia_therapy !=. & hbg_new == .		//  0 individuals with data on treatment don't have data on diagnosis status 

gen biomarker = 0
replace biomarker = . if fbg_new ==. & hba1c_p ==.	

	* Define individuals missing treatment data who were never told that they had diabetes as not receiving treatment (STEPS surveys assumes that undiagnosed are untreated)		// Note: does not affect estimates of medication use, as these only focus on those diagnosed
	tab1 dia_med_new insulin_new if hbg_new == 0 				// no one who is undiagnosed takes meds
	bys country: mdesc dia_med_new insulin_new					// verified: no country misses either treatment variable completely
		
	mdesc dia_therapy if hbg_new == 0							// 	18,377 (5.31%) of all folks who were never told that they had diabetes miss either insulin use and/or oral meds use
	mdesc dia_med_new if hbg_new == 0							// 	13,967 (4.03%) of all folks who were never told that they had diabetes miss oral meds use
	mdesc insulin_new if hbg_new == 0							// 	18,352 (5.30%) of all folks who were never told that they had diabetes miss insulin use

	replace dia_therapy = 0 if dia_therapy == . & hbg_new == 0 
	replace dia_med_new = 0 if dia_med_new == . & hbg_new == 0 
	replace insulin_new = 0 if insulin_new == . & hbg_new == 0 

* Diabetes status variable: biomarkers, treatment corrected
gen clin_dia_main = 0
replace clin_dia_main = 1 if (fbg_new >= 7 & fbg_new !=. & fast_new != 0) | (fbg_new >= 11.1 & fbg_new !=.) | (hba1c_p >= 6.5 & hba1c_p !=.) | (dia_therapy == 1)			
replace clin_dia_main = 0 if (fbg_new >= 7 & fbg_new !=. & fast_new != 0 & (hba1c_p < 6.5 & hba1c_p !=.) & dia_therapy != 1) | (fbg_new >= 11.1 & fbg_new !=. & (hba1c_p < 6.5 & hba1c_p !=.) & dia_therapy != 1)		// HbA1c prioritized over FBG/RBG
replace clin_dia_main = . if biomarker == . | dia_therapy == .	

* Treatment status among all individuals with diabetes 					
gen  treat_alldia_new = 0 if clin_dia_main == 1		
replace treat_alldia_new = 4 if insulin_new == 1 & dia_med_new == 0 & clin_dia_main == 1
replace treat_alldia_new = 3 if insulin_new == 1 & dia_med_new == 1 & clin_dia_main == 1					
replace treat_alldia_new = 2 if insulin_new == 0 & dia_med_new == 1 & clin_dia_main == 1
replace treat_alldia_new = 1 if insulin_new == 0 & dia_med_new == 0 & clin_dia_main == 1 & hbg_new == 1		// Note: no diabetic not on meds misses diagnosis var		
replace treat_alldia_new = 0 if insulin_new == 0 & dia_med_new == 0 & clin_dia_main == 1 & hbg_new == 0		// Note: for diabetic sample, "insulin_new == 0 & dia_med_new == 0" is redundant, because it applies to all undiagnosed diabetics
replace treat_alldia_new = . if clin_dia_main != 1															// 0 changes
replace treat_alldia_new = . if clin_dia_main == 1 & (insulin_new ==. | dia_med_new ==.)					// 1,292 changes (= diabetics missing meds OR insulin)
label define trt_new 0 "Undiagnosed" 1 "Untreated" 2 "Orals only" 3 "Orals and insulin" 4 "Insulin only" 
label values treat_alldia_new trt_neww

* Treatment status among all individuals with DIAGNOSED diabetes 		
gen  treat_diagdia = 0 if clin_dia_main == 1 & hbg_new == 1 			
replace treat_diagdia = 3 if insulin_new == 1 & dia_med_new == 0 & clin_dia_main == 1 & hbg_new == 1 
replace treat_diagdia = 2 if insulin_new == 1 & dia_med_new == 1 & clin_dia_main == 1 & hbg_new == 1 			
replace treat_diagdia = 1 if insulin_new == 0 & dia_med_new == 1 & clin_dia_main == 1 & hbg_new == 1 	
replace treat_diagdia = 0 if insulin_new == 0 & dia_med_new == 0 & clin_dia_main == 1 & hbg_new == 1 	// 0 changes	
replace treat_diagdia = . if clin_dia_main != 1 														// 0 changes
replace treat_diagdia = . if clin_dia_main == 1 & hbg_new == .											// 0 changes 
mdesc hbg_new if clin_dia_main == 1 																	
replace treat_alldia = . if clin_dia_main == 1 & hbg_new == 1 & (insulin_new ==. | dia_med_new ==.)		// 0 changes 
label define trt 0 "No medication" 1 "Orals only" 2 "Orals and insulin" 3 "Insulin only" 
label values treat_diagdia trt

	* Inverted
	gen treat_diagdia_inv = 0 if treat_diagdia == 3
	replace treat_diagdia_inv = 1 if treat_diagdia == 2
	replace treat_diagdia_inv = 2 if treat_diagdia == 1
	replace treat_diagdia_inv = 3 if treat_diagdia == 0
	replace treat_diagdia_inv = . if treat_diagdia == .

* Binary medication variables: all individuals with diabetes 		
gen nodiag_alldia_new =.
replace nodiag_alldia_new = 1 if treat_alldia_new == 0
replace nodiag_alldia_new = 0 if treat_alldia_new !=0
replace nodiag_alldia_new = . if treat_alldia_new  == .

gen nomeds_alldia_new =.
replace nomeds_alldia_new = 1 if treat_alldia_new == 1
replace nomeds_alldia_new = 0 if treat_alldia_new != 1 
replace nomeds_alldia_new = . if treat_alldia_new == .

gen oral_alldia_new =.
replace oral_alldia_new = 1 if treat_alldia_new == 2
replace oral_alldia_new = 0 if treat_alldia_new != 2
replace oral_alldia_new = . if treat_alldia_new == .

gen orl_insl_alldia_new =.
replace orl_insl_alldia_new = 1 if treat_alldia_new == 3
replace orl_insl_alldia_new = 0 if treat_alldia_new != 3
replace orl_insl_alldia_new = . if treat_alldia_new == .

gen insul_alldia_new =.
replace insul_alldia_new = 1 if treat_alldia_new == 4
replace insul_alldia_new = 0 if treat_alldia_new != 4 
replace insul_alldia_new = . if treat_alldia_new == .

* Binary medication variables: all individuals with DIAGNOSED diabetes 
gen nomeds_diagdia =.
replace nomeds_diagdia = 1 if treat_diagdia == 0
replace nomeds_diagdia = 0 if (treat_diagdia == 1 | treat_diagdia == 2 | treat_diagdia == 3) 
replace nomeds_diagdia = . if treat_diagdia == .

gen oral_diagdia =.
replace oral_diagdia = 1 if treat_diagdia == 1
replace oral_diagdia = 0 if (treat_diagdia == 0 | treat_diagdia == 2 | treat_diagdia == 3) 
replace oral_diagdia = . if treat_diagdia == .

gen orl_insl_diagdia =.
replace orl_insl_diagdia = 1 if treat_diagdia == 2
replace orl_insl_diagdia = 0 if (treat_diagdia == 0 | treat_diagdia == 1 | treat_diagdia == 3) 
replace orl_insl_diagdia = . if treat_diagdia == .

gen insul_diagdia =.
replace insul_diagdia = 1 if treat_diagdia == 3
replace insul_diagdia = 0 if (treat_diagdia == 0 | treat_diagdia == 1 | treat_diagdia == 2) 
replace insul_diagdia = . if treat_diagdia == .

* Binary variable: undiagnosed diabetes 
gen undiag = .
replace undiag = 1 if clin_dia_main == 1 & hbg_new == 0
replace undiag = 0 if clin_dia_main == 1 & hbg_new == 1		// n diabetic sample, undiag variable has same number as treat_alldia_new = 0 

* Binary variable: diagnosed diabetes (among diabetics)
gen diag = 0 if undiag == 1
replace diag = 1 if undiag == 0
replace diag = . if undiag == .

* Generate countrycat
egen countrycat = group (country) 

* World regions					
generate worldregion6 = .						
replace worldregion6 = 1 if country == "Ecuador" | country == "Guyana" | country == "Mexico" | country == "St. Vincent & the Grenadines"  ///
| country == "Costa Rica" | country == "Chile" | country == "El Salvador"
replace worldregion6 = 2 if country == "Georgia" | country == "Romania" | country == "Azerbaijan" | country == "Mongolia" ///
| country == "Kyrgyzstan"  | country == "Moldova"  | country == "Tajikistan" | country == "Belarus" | country == "Turkmenistan" 
replace worldregion6 = 3 if country == "Bhutan" | country == "China" | country == "India" | country == "Indonesia" | country == "Nepal"  ///
| country == "Bangladesh" | country == "Cambodia" | country == "Myanmar" | country == "Laos" | country == "Vietnam" | country == "Sri Lanka" | country == "Afghanistan"
replace worldregion6 = 4 if country == "Benin" | country == "Comoros" | country == "Kenya" | country == "Liberia" | country == "Eritrea"  ///
| country == "South Africa" | country == "Eswatini" | country == "Tanzania" | country == "Togo" | country == "Sudan"  ///
| country == "Lesotho" | country == "Botswana" | country == "Zambia" | country == "Zanzibar" | country == "Burkina Faso" | country == "Ethiopia" | country == "Cabo Verde" ///
| country == "Rwanda" | country == "Seychelles" | country == "Malawi" | country == "Mozambique"
replace worldregion6 = 5 if country == "Morocco" | country == "Lebanon" | country == "Algeria" | country == "Iran" | country == "Iraq" | country == "Libya" | country == "Jordan"
replace worldregion6 = 6 if country == "Fiji" | country == "Kiribati" | country == "Samoa" | country == "Solomon Islands" | country == "Tuvalu" | country == "Vanuatu" ///
| country == "Marshall Islands" | country == "Tokelau" | country == "Palau" | country == "Nauru"
label define wr6 1 "Latin America and the Caribbean" 2 "Europe and Central Asia" 3 "South, East and Southeast Asia" 4 "Sub-Saharan Africa" 5 "Middle East and North Africa" 6 "Oceania"
label values worldregion6 wr6    

* Age categories 
gen age_cat = .
replace age_cat = 1 if age >=25 & age <35
replace age_cat = 2 if age >=35 & age <45
replace age_cat = 3 if age >=45 & age <55
replace age_cat = 4 if age >=55 & age !=.
label define agect 1 "25-34" 2 "35-44" 3 "45-54" 4 ">54" 
label values age_cat agect

gen age_cat2 = .
replace age_cat2 = 1 if age >=25 & age <35
replace age_cat2 = 2 if age >=35 & age <45
replace age_cat2 = 3 if age >=45 & age <55
replace age_cat2 = 4 if age >=55 & age <65
replace age_cat2 = 5 if age >=65 & age !=.
label define agect2 1 "25-34" 2 "35-44" 3 "45-54" 4 "55-64" 5 ">64" 
label values age_cat2 agect2

* Indicator for type of survey question
gen country_sngl = country
replace country_sngl = "Costa_Rica" if country == "Costa Rica" 
replace country_sngl = "Burkina_Faso" if country == "Burkina Faso" 
replace country_sngl = "Marshall_Islands" if country == "Marshall Islands" 		
replace country_sngl = "Solomon_Islands" if country == "Solomon Islands" 		
replace country_sngl = "Sri_Lanka" if country == "Sri Lanka" 
replace country_sngl = "South_Africa" if country == "South Africa" 	
replace country_sngl = "El_Salvador" if country == "El Salvador" 			

global group1 Afghanistan Algeria Azerbaijan Bangladesh Belarus Benin Bhutan Botswana Burkina_Faso Ecuador Eswatini Ethiopia Georgia Guyana Iraq Jordan Kenya Kiribati Kyrgyzstan Lebanon Malawi Marshall_Islands Moldova Mongolia Morocco Mozambique Myanmar Nauru Nepal Solomon_Islands Sri_Lanka Sudan Tajikistan Tokelau Turkmenistan Tuvalu Vietnam Zambia
global group2 Cambodia Comoros Costa_Rica El_Salvador Eritrea Fiji Iran Laos Lesotho Liberia Libya Palau Romania Rwanda Samoa Seychelles Tanzania Togo Vanuatu Zanzibar 
global group3 Chile China Indonesia Mexico Romania South_Africa

gen qst_type = .
foreach lname in $group1 {
replace qst_type = 1 if country_sngl == "`lname'"
}
foreach lname in $group2 {
replace qst_type = 2 if country_sngl == "`lname'"
}
foreach lname in $group3 {
replace qst_type = 3 if country_sngl == "`lname'"
}

* Indicator for ambiguous (1) vs unambiguous (2,3) surveys
gen unambig = .
foreach lname in $group1 {
replace unambig = 0 if country_sngl == "`lname'"
}
foreach lname in $group2 $group3 {
replace unambig = 1 if country_sngl == "`lname'"
}
label define una 0 "Ambiguous" 1 "Unambgiuous"
label values unambig una

* Add superscript numbers to country names
foreach lname in $group1 {
replace country_sngl = "`lname'{sup:{bf}1}" if country_sngl == "`lname'" & qst_type == 1 
}
foreach lname in $group2 {
replace country_sngl = "`lname'{sup:{bf}2}" if country_sngl == "`lname'" & qst_type == 2
}
foreach lname in $group3 {
replace country_sngl = "`lname'{sup:{bf}3}" if country_sngl == "`lname'" & qst_type == 3 
}

tab country_sngl if country == "Costa Rica" | country == "Burkina Faso" | country == "Marshall Islands" | country == "Solomon Islands" ///
| country == "Sri Lanka" | country == "South Africa" | country == "El Salvador" 

replace country_sngl = "Costa Rica{sup:{bf}2}" if country_sngl == "Costa_Rica{sup:{bf}2}" 
replace country_sngl = "Burkina Faso{sup:{bf}1}" if country_sngl == "Burkina_Faso{sup:{bf}1}" 
replace country_sngl = "Marshall Islands{sup:{bf}1}" if country_sngl == "Marshall_Islands{sup:{bf}1}" 		
replace country_sngl = "Solomon Islands{sup:{bf}1}" if country_sngl == "Solomon_Islands{sup:{bf}1}" 		
replace country_sngl = "Sri Lanka{sup:{bf}1}" if country_sngl == "Sri_Lanka{sup:{bf}1}" 
replace country_sngl = "South Africa{sup:{bf}3}" if country_sngl == "South_Africa{sup:{bf}3}" 	
replace country_sngl = "El Salvador{sup:{bf}2}" if country_sngl == "El_Salvador{sup:{bf}2}" 

* Indicator for surveys with lower age limit 25 years
gen age_ll25 = 0			
forval x = 1/63 {
*tab country if countrycat == `x' 
sum age if countrycat == `x'
replace age_ll25 = 1 if countrycat == `x' & `r(min)' >=25
*mdesc age if countrycat == `x'
}
mdesc age_ll25	// 0
tab country if age_ll25 == 1	// Burkina Faso Cambodia Comoros Eritrea Fiji Kyrgyzstan Lesotho Libya Malawi Myanmar Palau  Sychelles Tanzania Vanuatu

* LIC vs MIC
gen LIC = .
replace LIC = 1 if countryGDPclass == 1
replace LIC = 0 if countryGDPclass == 2 | countryGDPclass == 3
label define lic 0 "MIC" 1 "LIC"
label values LIC lic

* Append response rates
merge m:1 country using "/Users/felixteufel/Desktop/HPACC Diab Meds/Data/Response rates.dta"
replace response_rate = response_rate/100

* Save
save "$data/HPACC_diab_meds_simul_clean.dta", replace
