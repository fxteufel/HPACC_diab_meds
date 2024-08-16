********************************************************************************
********************************************************************************
* Global patterns of diabetes medication use								   *
* By: Felix Teufel  													   	   *
* Last updated: 15 Aug 2024									  				   *
********************************************************************************

global data "[insert_path]/Data"												// insert path to data file
global results "[insert_path]/Results"											// insert path to results folder			

use "$data/HPACC_diab_meds_simul_final.dta", clear
cd "$results" 																	// change directory to results folders

********************************************************************************
***							 MAIN ANALYSES									 ***						
********************************************************************************

*** TABLE 1: SAMPLE CHARACTERISTICS ***					

putexcel set table1.xls, modify

* Country estimates
forval x = 1/63 {
preserve
keep if countrycat == `x'	
putexcel C`=2+`x'' = year[1]
restore
sum response_rate if countrycat == `x', detail
putexcel E`=2+`x'' = matrix(r(p50)), nformat("0.0%") 
count if countrycat == `x' & analytic_pop_all == 1 
putexcel F`=2+`x'' = `r(N)'
svy, subpop(if analytic_pop_all == 1 & countrycat == `x'): mean age 
putexcel G`=2+`x'' = matrix(e(b)), nformat("0.0") 
estat sd
matrix sd = r(sd)
putexcel H`=2+`x'' = matrix(sd), nformat("0.0") 
count if countrycat == `x' & analytic_pop_all == 1 & sex == 1
putexcel I`=2+`x'' = `r(N)'
svy, subpop(if analytic_pop_all == 1 & countrycat == `x'): mean sex 
putexcel J`=2+`x'' = matrix(e(b)), nformat("0.0%") 
svy, subpop(if analytic_pop_all == 1 & countrycat == `x'): mean clin_dia_main 
putexcel K`=2+`x'' = matrix(e(b)), nformat("0.0%") 
}

* Global estimates
count if analytic_pop_all == 1 
putexcel F68 = `r(N)'
svy, subpop(analytic_pop_all): mean age 
putexcel G68 = matrix(e(b)), nformat("0.0") 
estat sd
matrix sd = r(sd)
putexcel H68 = matrix(sd), nformat("0.0") 
count if analytic_pop_all == 1 & sex == 1
putexcel I68 = `r(N)'
svy, subpop(analytic_pop_all): mean sex
putexcel J68 = matrix(e(b)), nformat("0.0%") 
svy, subpop(analytic_pop_all): mean clin_dia_main 
putexcel K68 = matrix(e(b)), nformat("0.0%") 

* Median response rate global 						// Note: No response rate reported for Afghanistan, so not included in calculation
collapse response_rate, by(country) 
drop if country == "Afghanistan"

sum response_rate, detail							// also indicates IQR
putexcel E68 = matrix(r(p50)), nformat("0.0%") 


*** FIGURE 1: GLOBAL MEDICATION USE PATTERNS  ***			

use "$data/HPACC_diab_meds_simul_final.dta", clear
cd "$results"
graph drop _all

* All			

svy, subpop(analytic_pop_diab): proportion diag		// point estimates should be identical to numbers in pie chart --> Verified
matrix est = e(b) 
gen B = .
replace B = est[1,2]
global C = 0.5*360*B
display `C'

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f)) ///
legend(position(7) ring(0) order(2 "Undiagnosed" 1 "Diagnosed") symy(2) symx(2) textw(16) size(small) col(1)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(50) fxsize(50) angle0($C) ///
title("{bf}A) All individuals with diabetes", size(medsmall) col(black) position(11)) ///
name(global_pie) 

* Diagnosed

generate proportions_diag = .
generate upper_bound_diag = .
generate lower_bound_diag = .

svy, subpop(analytic_pop_diab): proportion nomeds_diagdia
matrix est = e(b)	
matrix ci = r(table)
replace proportions_diag = est[1, 2] if treat_diagdia == 0
replace lower_bound_diag = ci[5,2] if treat_diagdia == 0
replace upper_bound_diag = ci[6,2] if treat_diagdia == 0
		
svy, subpop(analytic_pop_diab): proportion oral_diagdia
matrix est = e(b)
matrix ci = r(table)
replace proportions_diag = est[1, 2] if treat_diagdia == 1
replace lower_bound_diag = ci[5,2] if treat_diagdia == 1
replace upper_bound_diag = ci[6,2] if treat_diagdia == 1
		
svy, subpop(analytic_pop_diab): proportion orl_insl_diagdia 
matrix est = e(b)
matrix ci = r(table)
replace proportions_diag = est[1, 2] if treat_diagdia == 2
replace lower_bound_diag = ci[5,2] if treat_diagdia == 2
replace upper_bound_diag = ci[6,2] if treat_diagdia == 2
		
svy, subpop(analytic_pop_diab): proportion insul_diagdia
matrix est = e(b)
matrix ci = r(table)
replace proportions_diag = est[1, 2] if treat_diagdia == 3
replace lower_bound_diag = ci[5,2] if treat_diagdia == 3
replace upper_bound_diag = ci[6,2] if treat_diagdia == 3
						
replace proportions_diag = proportions_diag*100		
replace upper_bound_diag = upper_bound_diag*100
replace lower_bound_diag = lower_bound_diag*100

gen proportions_marker_diag = string(proportions_diag, "%9.1f")
replace proportions_marker_diag = proportions_marker_diag + "%"
					
twoway (bar proportions_diag treat_diagdia if nomeds_diagdia == 1, bcolor(eltblue) barw(0.95))  ///
(bar proportions_diag treat_diagdia if oral_diagdia == 1, bcolor(orange) barw(0.95))  ///
(bar proportions_diag treat_diagdia if orl_insl_diagdia == 1, bcolor(eltgreen) barw(0.95))  ///
(bar proportions_diag treat_diagdia if insul_diagdia == 1, bcolor(gold) barw(0.95)) ///
(rcap upper_bound_diag lower_bound_diag treat_diagdia, lcolor(gs8)) ///
(scatter proportions_diag treat_diagdia, msymbol(none) mlabel(proportions_marker_diag) mlabposition(2) mlabcolor(black) mlabsize(small)), ///			
xtitle("") xlabel(none) ytitle("Weighted proportion (%)", size(small)) yscale(range(0 100) titlegap(*1.2)) ylabel(0(10)100,labsize(small) format(%9.0f) nogrid angle(0)) ///		
legend(position(2) ring(0) order(1 "No medication" 2 "Oral only" 3 "Oral and insulin" 4 "Insulin only") col(1) symy(2) symx(2) textw(16) size(small)) ///
title("{bf}B) Individuals with diagnosed diabetes", size(medsmall) col(black) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) ///
name(global_bars_diagdia) 

svy, subpop(analytic_pop_diab): proportion treat_diagdia			// Check whether numbers in graphs are identical to these --> Verified

* Combine
graph combine global_pie global_bars_diagdia, row(1) graphregion(margin(small)color("white")) imargin(0 0 0 0)
graph save "global_bars_new", replace


*** FIGURE 2: MEDICATION USE PATTERNS AMONG ALL DIABETICS BY COUNTRY ***				

/* Note: Because we do not report CIs in graph 2, including only pweights (and not psu+stratum & no subpop option) generates the same estimates, 
because psu & stratum in svyset as well as svy,subpop only affect the calculation (clustering) of standard errors */

use "$data/HPACC_diab_meds_simul_final.dta", clear
cd "$results"

svy, subpop(analytic_pop_diab): proportion treat_alldia_new											

keep if analytic_pop_diab == 1
proportion treat_alldia_new [pw = weq_w3_analytic_all] 	// proportion estimates should be same as above --> verified

bys country: tab treat_alldia		

graph hbar nomeds_alldia_new oral_alldia_new orl_insl_alldia_new insul_alldia_new undiag [pw = weq_w3_analytic_all], over(country_sngl, sort(5) label(labsize(tiny))) stack percent xsize(4) ysize(5.5) ///
ytitle(,size(small)) graphregion(color(white)) bar(1, color(eltblue)) bar(2, color(orange)) bar(3, color(eltgreen)) bar(4, color(gold)) bar(5, color(ltbluishgray)) ///
title ("{bf}All individuals with diabetes", size(vsmall) col(black) position(11)) ///
ylabel(0(10)100,labsize(vsmall) nogrid) ytitle("Weighted proportion (%)", size(vsmall)) ytick(0(5)100, tstyle(minor_nolabel)) ///
legend(order(1 "No medication" 2 "Oral only" 3 "Oral and insulin" 4 "Insulin only" 5 "Undiagnosed") col(3) symy(2) symx(2) textw(15)  size(vsmall)) 
graph save "country_bars_alldiab", replace 


*** FIGURE 3: STRATIFICATION BY WORLD BANK INCOME GROUP ***

use "$data/HPACC_diab_meds_simul_final.dta", clear
cd "$results"
graph drop _all 

* All

forval x = 1/3 { 
svy, subpop(if analytic_pop_diab == 1 & countryGDPclass == `x'): proportion diag
matrix est = e(b) 
gen B`x' = est[1,2]
global C`x' = 0.5*360*B`x'
}

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & countryGDPclass == 1, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(50) fxsize(50) angle0($C1) legend(off) title(" ", size(medsmall) position(11)) l1title("{bf}LIC", size(medsmall) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(large)) title("{bf}A) All individuals with diabetes", size(medsmall) col(black) position(11)) 
graph save "pie1_h", replace

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & countryGDPclass == 2, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(50) fxsize(50) angle0($C2) legend(off) title(" ", size(medsmall) position(11)) l1title("{bf}Lower-MIC", size(medsmall) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(large))
graph save "pie2_h", replace

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & countryGDPclass == 3, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f)) ///
legend(position(7) ring(0) order(2 "Undiagnosed" 1 "Diagnosed") symy(2) symx(2) textw(11) size(small) col(1) keygap(*0.5)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(50) fxsize(50) angle0($C3) ///
l1title("{bf}Upper-MIC", size(medsmall) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(large)) title(" ", size(medsmall))
graph save "pie3_h", replace

svy, subpop(analytic_pop_diab) over(countryGDPclass): proportion diag	// verify numbers in graph --> check

* Diagnosed

forval x = 1/3 { 
generate proportions_diag`x' = .
generate upper_bound_diag`x' = .
generate lower_bound_diag`x' = .

svy, subpop(if analytic_pop_diab_diag == 1 & countryGDPclass == `x'): proportion nomeds_diagdia 
matrix est = e(b)	
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 3
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 3
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 3
		
svy, subpop(if analytic_pop_diab_diag == 1 & countryGDPclass == `x'): proportion oral_diagdia
matrix est = e(b)
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 2
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 2
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 2
		
svy, subpop(if analytic_pop_diab_diag == 1 & countryGDPclass == `x'): proportion orl_insl_diagdia 
matrix est = e(b)
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 1
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 1
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 1
		
svy, subpop(if analytic_pop_diab_diag == 1 & countryGDPclass == `x'): proportion insul_diagdia
matrix est = e(b)
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 0
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 0
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 0
						
replace proportions_diag`x' = proportions_diag`x'*100		
replace upper_bound_diag`x' = upper_bound_diag`x'*100
replace lower_bound_diag`x' = lower_bound_diag`x'*100

gen proportions_marker_diag`x' = string(proportions_diag`x', "%9.1f")
replace proportions_marker_diag`x' = proportions_marker_diag`x' + "%"
}	
		
twoway (bar proportions_diag1 treat_diagdia_inv if nomeds_diagdia == 1 & countryGDPclass == 1, bcolor(eltblue) barw(0.95) horizontal)  ///
(bar proportions_diag1 treat_diagdia_inv if oral_diagdia == 1 & countryGDPclass == 1, bcolor(orange) barw(0.95) horizontal)  ///
(bar proportions_diag1 treat_diagdia_inv if orl_insl_diagdia == 1 & countryGDPclass == 1, bcolor(eltgreen) barw(0.95) horizontal)  ///
(bar proportions_diag1 treat_diagdia_inv if insul_diagdia == 1 & countryGDPclass == 1, bcolor(gold) barw(0.95) horizontal) ///
(rcap upper_bound_diag1 lower_bound_diag1 treat_diagdia_inv if countryGDPclass == 1, lcolor(gs8) horizontal) ///
(scatter treat_diagdia_inv proportions_diag1 if countryGDPclass == 1, msymbol(none) mlabel(proportions_marker_diag1) mlabposition(1) mlabcolor(black) mlabsize(small)), ///			
ytitle("") ylabel(none) xtitle(" ", size(small)) xscale(range(0 100) titlegap(*1.2)) xlabel(0(10)100,labsize(small) format(%9.0f) nogrid angle(0)) ///		
legend(off) ///
title("{bf}B) Individuals with diagnosed diabetes", size(medsmall) col(black) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) 
graph save "bar1_h", replace

twoway (bar proportions_diag2 treat_diagdia_inv if nomeds_diagdia == 1 & countryGDPclass == 2, bcolor(eltblue) barw(0.95) horizontal)  ///
(bar proportions_diag2 treat_diagdia_inv if oral_diagdia == 1 & countryGDPclass == 2, bcolor(orange) barw(0.95) horizontal)  ///
(bar proportions_diag2 treat_diagdia_inv if orl_insl_diagdia == 1 & countryGDPclass == 2, bcolor(eltgreen) barw(0.95) horizontal)  ///
(bar proportions_diag2 treat_diagdia_inv if insul_diagdia == 1 & countryGDPclass == 2, bcolor(gold) barw(0.95) horizontal) ///
(rcap upper_bound_diag2 lower_bound_diag2 treat_diagdia_inv if countryGDPclass == 2, lcolor(gs8) horizontal) ///
(scatter treat_diagdia_inv proportions_diag2 if countryGDPclass == 2, msymbol(none) mlabel(proportions_marker_diag2) mlabposition(1) mlabcolor(black) mlabsize(small)), ///			
ytitle("") ylabel(none) xtitle(" ", size(small)) xscale(range(0 100) titlegap(*1.2)) xlabel(0(10)100,labsize(small) format(%9.0f) nogrid angle(0)) ///		
title(" ", size(medsmall) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) legend(off) 
graph save "bar2_h", replace

twoway (bar proportions_diag3 treat_diagdia_inv if nomeds_diagdia == 1 & countryGDPclass == 3, bcolor(eltblue) barw(0.95) horizontal)  ///
(bar proportions_diag3 treat_diagdia_inv if oral_diagdia == 1 & countryGDPclass == 3, bcolor(orange) barw(0.95) horizontal)  ///
(bar proportions_diag3 treat_diagdia_inv if orl_insl_diagdia == 1 & countryGDPclass == 3, bcolor(eltgreen) barw(0.95) horizontal)  ///
(bar proportions_diag3 treat_diagdia_inv if insul_diagdia == 1 & countryGDPclass == 3, bcolor(gold) barw(0.95) horizontal) ///
(rcap upper_bound_diag3 lower_bound_diag3 treat_diagdia_inv if countryGDPclass == 3, lcolor(gs8) horizontal) ///
(scatter treat_diagdia_inv proportions_diag3 if countryGDPclass == 3, msymbol(none) mlabel(proportions_marker_diag3) mlabposition(1) mlabcolor(black) mlabsize(small)), ///			
ytitle("") ylabel(none) xtitle("Weighted proportion (%)", size(small)) xscale(range(0 100) titlegap(*1.2)) xlabel(0(10)100,labsize(small) format(%9.0f) nogrid angle(0)) ///		
title(" ", size(medsmall) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) ///
legend(position(5) ring(0) order(1 "No medication" 2 "Oral only" 3 "Oral and insulin" 4 "Insulin only") row(2) symy(2) symx(2) textw(11) size(small) rowgap(*0.5) colgap(*0.7) keygap(*0.5)) 
graph save "bar3_h", replace

svy, subpop(analytic_pop_diab_diag) over(countryGDPclass): proportion treat_diagdia 	// compare to numbers in graph --> verified

* Combine
graph combine "pie1_h" "bar1_h" "pie2_h" "bar2_h" "pie3_h" "bar3_h", row(4) graphregion(margin(small)color("white")) imargin(0 0 0 0)
graph save "WB_income", replace 


********************************************************************************
***						NUMBERS FOR PAPER TEXT								 ***						
********************************************************************************

** CIs female proportion ** 
use "$data/HPACC_diab_meds_simul_final.dta", clear
svy, subpop(analytic_pop_all): proportion sex 

** Distribution of countries by World Bank income group **
use "$data/HPACC_diab_meds_simul_final.dta", clear
tab countryGDPclass if country == "Zanzibar" | country == "Tanzania"		// LIC
drop if country == "Zanzibar"												// Zanzibar is part of Tanzania, but has separate health system and STEPS survey
collapse countryGDPclass, by(country)
tab countryGDPclass

** CIs diabetes prevalence **
use "$data/HPACC_diab_meds_simul_final.dta", clear
svy, subpop(analytic_pop_all): proportion clin_dia_main 

** CIs diagnosis prevalence among diabetics **
use "$data/HPACC_diab_meds_simul_final.dta", clear
svy, subpop(analytic_pop_diab): proportion hbg_new 

** CIs medication use among diagnosed diabetics **
use "$data/HPACC_diab_meds_simul_final.dta", clear
svy, subpop(analytic_pop_diab_diag): proportion nomeds_diagdia
svy, subpop(analytic_pop_diab_diag): proportion oral_diagdia
svy, subpop(analytic_pop_diab_diag): proportion orl_insl_diagdia
svy, subpop(analytic_pop_diab_diag): proportion insul_diagdia
