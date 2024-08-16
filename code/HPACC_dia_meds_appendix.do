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
***							 APPENDIX										 ***						
********************************************************************************

*** Table S2: Insulin use LIC vs MIC (diagnosed vs all) ***			// Need to incorporate putexcel code

use "$data/HPACC_diab_meds_simul_final.dta", clear
cd "$results" 

* All Diabetes *
count if analytic_pop_diab == 1 & LIC == 1

count if analytic_pop_diab == 1 & LIC == 0 & insulin_new == 1
svy, subpop(analytic_pop_diab if LIC == 1) : proportion insulin_new

count if analytic_pop_diab == 1 & LIC == 1 & oral_alldia_new == 1
svy, subpop(analytic_pop_diab if LIC == 1) : proportion oral_alldia_new

svy, subpop(analytic_pop_diab) over(LIC): proportion insulin_new				// more accurate SE
lincom _b[Yes:LIC] - _b[Yes:MIC]

svy, subpop(analytic_pop_diab) over(LIC): proportion oral_alldia_new,			// more accurate SE
lincom _b[_prop_2:LIC] - _b[_prop_2:MIC]

svy, subpop(analytic_pop_diab): tab insulin_new LIC				// p=0.6271
svy, subpop(analytic_pop_diab_diag): tab insulin_new LIC		// p=0.0000

* Diagnosed diabetes *
count if analytic_pop_diab_diag == 1 & LIC == 1

count if analytic_pop_diab_diag == 1 & LIC == 1 & insulin_new == 1
svy, subpop(analytic_pop_diab_diag if LIC == 1) : proportion insulin_new

count if analytic_pop_diab_diag == 1 & LIC == 1 & oral_alldia_new == 1
svy, subpop(analytic_pop_diab_diag if LIC == 1) : proportion oral_alldia_new

svy, subpop(analytic_pop_diab_diag) over(LIC): proportion insulin_new			// more accurate SE
lincom _b[Yes:LIC] - _b[Yes:MIC]

svy, subpop(analytic_pop_diab_diag) over(LIC): proportion oral_alldia_new		// more accurate SE
lincom _b[_prop_2:LIC] - _b[_prop_2:MIC]

* p-values 
svy, subpop(analytic_pop_diab): tab oral_alldia_new LIC			// p=0.0000
svy, subpop(analytic_pop_diab_diag): tab oral_alldia_new LIC	// p=0.001


*** TABLE S3:  MEDICATION USE PATTERNS AMONG DIABETICS BY COUNTRY ***	(table corresponding to figure 2)															
	
use "$data/HPACC_diab_meds_simul_final.dta", clear
cd "$results" 

putexcel set table_s3.xls, modify

foreach x of num 1/35 37/45 47/48 50/63{  																				
tab undiag if undiag == 1 & countrycat == `x' & analytic_pop_diab == 1
putexcel C`=4+`x'' = `r(N)'
capture svy, subpop(if analytic_pop_diab == 1 & countrycat == `x'): proportion undiag 
capture matrix estimate_undiag`x' = e(b) 
capture matrix est_undiag`x' = estimate_undiag`x'[1...,2..2]' 
capture putexcel D`=4+`x'' = matrix(est_undiag`x'), nformat("0.0%")
capture matrix ci_undiag`x' = r(table)
capture matrix cix_undiag`x' = ci_undiag`x'[5..6,2...]' 											
capture putexcel E`=4+`x'' = matrix(cix_undiag`x'), nformat("0.0%")

tab nomeds_alldia_new if nomeds_alldia_new == 1 & countrycat == `x' & analytic_pop_diab == 1
putexcel G`=4+`x'' = `r(N)'
capture svy, subpop(if analytic_pop_diab == 1 & countrycat == `x'): proportion nomeds_alldia_new 
capture matrix estimate_nomeds`x' = e(b) 
capture matrix est_nomeds`x' = estimate_nomeds`x'[1...,2..2]' 
capture putexcel H`=4+`x'' = matrix(est_nomeds`x'), nformat("0.0%")
capture matrix ci_nomeds`x' = r(table)
capture matrix cix_nomeds`x' = ci_nomeds`x'[5..6,2...]' 											
capture putexcel I`=4+`x'' = matrix(cix_nomeds`x'), nformat("0.0%")

tab oral_alldia_new if oral_alldia_new == 1 & countrycat == `x' & analytic_pop_diab == 1
putexcel K`=4+`x'' = `r(N)'
capture svy, subpop(if analytic_pop_diab == 1 & countrycat == `x'): proportion oral_alldia
capture matrix estimate_oral`x' = e(b) 
capture matrix est_oral`x' = estimate_oral`x'[1...,2..2]' 
capture putexcel L`=4+`x'' = matrix(est_oral`x'), nformat("0.0%")
capture matrix ci_oral`x' = r(table)
capture matrix cix_oral`x' = ci_oral`x'[5..6,2...]' 											
capture putexcel M`=4+`x'' = matrix(cix_oral`x'), nformat("0.0%")

tab orl_insl_alldia_new if orl_insl_alldia_new == 1 & countrycat == `x' & analytic_pop_diab == 1
putexcel O`=4+`x'' = `r(N)'
capture svy, subpop(if analytic_pop_diab == 1 & countrycat == `x'): proportion orl_insl_alldia
capture matrix estimate_orl_insl`x' = e(b) 
capture matrix est_orl_insl`x' = estimate_orl_insl`x'[1...,2..2]' 
capture putexcel P`=4+`x'' = matrix(est_orl_insl`x'), nformat("0.0%")
capture matrix ci_orl_insl`x' = r(table)
capture matrix cix_orl_insl`x' = ci_orl_insl`x'[5..6,2...]' 											
capture putexcel Q`=4+`x'' = matrix(cix_orl_insl`x'), nformat("0.0%")

tab insul_alldia_new if insul_alldia_new == 1 & countrycat == `x' & analytic_pop_diab == 1
putexcel S`=4+`x'' = `r(N)'
capture svy, subpop(if analytic_pop_diab == 1 & countrycat == `x'): proportion insul_alldia_new 
capture matrix estimate_insul`x' = e(b) 
capture matrix est_insul`x' = estimate_insul`x'[1...,2..2]' 
capture putexcel T`=4+`x'' = matrix(est_insul`x'), nformat("0.0%")
capture matrix ci_insul`x' = r(table)
capture matrix cix_insul`x' = ci_insul`x'[5..6,2...]' 											
capture putexcel U`=4+`x'' = matrix(cix_insul`x'), nformat("0.0%")
}

svyset [pw = weq_w3_analytic_all], strata(stratum_num) singleunit(centered)		// 3 countries only have single stratum with single PSU (only relevant when estimating SEs for these countries individually)

tab countrycat if country == "Marshall Islands" | country == "Romania" | country == "Seychelles"

foreach x of num 36 46 49 {  											
tab undiag if undiag == 1 & countrycat == `x' & analytic_pop_diab == 1
putexcel C`=4+`x'' = `r(N)'
capture svy, subpop(if analytic_pop_diab == 1 & countrycat == `x'): proportion undiag 	
capture matrix estimate_undiag`x' = e(b) 
capture matrix est_undiag`x' = estimate_undiag`x'[1...,2..2]' 
capture putexcel D`=4+`x'' = matrix(est_undiag`x'), nformat("0.0%")
capture matrix ci_undiag`x' = r(table)
capture matrix cix_undiag`x' = ci_undiag`x'[5..6,2...]' 											
capture putexcel E`=4+`x'' = matrix(cix_undiag`x'), nformat("0.0%")

tab nomeds_alldia_new if nomeds_alldia_new == 1 & countrycat == `x' & analytic_pop_diab == 1
putexcel G`=4+`x'' = `r(N)'
capture svy, subpop(if analytic_pop_diab == 1 & countrycat == `x'): proportion nomeds_alldia_new 
capture matrix estimate_nomeds`x' = e(b) 
capture matrix est_nomeds`x' = estimate_nomeds`x'[1...,2..2]' 
capture putexcel H`=4+`x'' = matrix(est_nomeds`x'), nformat("0.0%")
capture matrix ci_nomeds`x' = r(table)
capture matrix cix_nomeds`x' = ci_nomeds`x'[5..6,2...]' 											
capture putexcel I`=4+`x'' = matrix(cix_nomeds`x'), nformat("0.0%")

tab oral_alldia_new if oral_alldia_new == 1 & countrycat == `x' & analytic_pop_diab == 1
putexcel K`=4+`x'' = `r(N)'
capture svy, subpop(if analytic_pop_diab == 1 & countrycat == `x'): proportion oral_alldia
capture matrix estimate_oral`x' = e(b) 
capture matrix est_oral`x' = estimate_oral`x'[1...,2..2]' 
capture putexcel L`=4+`x'' = matrix(est_oral`x'), nformat("0.0%")
capture matrix ci_oral`x' = r(table)
capture matrix cix_oral`x' = ci_oral`x'[5..6,2...]' 											
capture putexcel M`=4+`x'' = matrix(cix_oral`x'), nformat("0.0%")

tab orl_insl_alldia_new if orl_insl_alldia_new == 1 & countrycat == `x' & analytic_pop_diab == 1
putexcel O`=4+`x'' = `r(N)'
capture svy, subpop(if analytic_pop_diab == 1 & countrycat == `x'): proportion orl_insl_alldia_new 
capture matrix estimate_orl_insl`x' = e(b) 
capture matrix est_orl_insl`x' = estimate_orl_insl`x'[1...,2..2]' 
capture putexcel P`=4+`x'' = matrix(est_orl_insl`x'), nformat("0.0%")
capture matrix ci_orl_insl`x' = r(table)
capture matrix cix_orl_insl`x' = ci_orl_insl`x'[5..6,2...]' 											
capture putexcel Q`=4+`x'' = matrix(cix_orl_insl`x'), nformat("0.0%")

tab insul_alldia_new if insul_alldia_new == 1 & countrycat == `x' & analytic_pop_diab == 1
putexcel S`=4+`x'' = `r(N)'
capture svy, subpop(if analytic_pop_diab == 1 & countrycat == `x'): proportion insul_alldia_new 
capture matrix estimate_insul`x' = e(b) 
capture matrix est_insul`x' = estimate_insul`x'[1...,2..2]' 
capture putexcel T`=4+`x'' = matrix(est_insul`x'), nformat("0.0%")
capture matrix ci_insul`x' = r(table)
capture matrix cix_insul`x' = ci_insul`x'[5..6,2...]' 											
capture putexcel U`=4+`x'' = matrix(cix_insul`x'), nformat("0.0%")
}


*** FIGURE S2: STRATIFICATION BY WORLD-REGION ***

use "$data/HPACC_diab_meds_simul_final.dta", clear
cd "$results" 

* All

forval x = 1/6 { 
svy, subpop(if analytic_pop_diab == 1 & worldregion6 == `x'): proportion diag	
matrix est = e(b) 
gen B`x' = est[1,2]
global C`x' = 0.5*360*B`x'
}

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & worldregion6 == 1, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f) gap(42)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(33) fxsize(33) angle0($C1) legend(off) title(" ", size(medsmall) position(11)) l1title("{bf}LAC", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(small)) title("{bf}A) All individuals with diabetes", size(small) col(black) position(11)) 
graph save "pie1_h", replace

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & worldregion6 == 2, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f) gap(42)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(33) fxsize(33) angle0($C2) legend(off) title(" ", size(small) position(11)) l1title("{bf}ECA", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(small))
graph save "pie2_h", replace

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & worldregion6 == 3, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f) gap(42)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(33) fxsize(33) angle0($C3) legend(off) title(" ", size(small) position(11)) l1title("{bf}SEA", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(small))
graph save "pie3_h", replace

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & worldregion6 == 4, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f) gap(42)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(33) fxsize(33) angle0($C4) legend(off) title(" ", size(small) position(11)) l1title("{bf}SSA", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(small))
graph save "pie4_h", replace

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & worldregion6 == 5, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f) gap(42)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(33) fxsize(33) angle0($C5) legend(off) title(" ", size(small) position(11)) l1title("MENA", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(small))
graph save "pie5_h", replace

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & worldregion6 == 6, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f) gap(42)) ///
legend(position(7) ring(0) order(2 "Undiagnosed" 1 "Diagnosed") symy(2) symx(2) textw(6) size(vsmall) col(1) keygap(*0.3) rowgap(*0.5)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(33) fxsize(33) angle0($C6) ///
l1title("OCN", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(small)) title(" ", size(small))
graph save "pie6_h", replace

svy, subpop(analytic_pop_diab) over(worldregion6): proportion diag		// verify numbers in graph --> check

* Diagnosed

forval x = 1/6 { 
generate proportions_diag`x' = .
generate upper_bound_diag`x' = .
generate lower_bound_diag`x' = .

svy, subpop(if analytic_pop_diab_diag == 1 & worldregion6 == `x'): proportion nomeds_diagdia  
matrix est = e(b)	
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 3
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 3
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 3
		
svy, subpop(if analytic_pop_diab_diag == 1 & worldregion6 == `x'): proportion oral_diagdia 
matrix est = e(b)
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 2
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 2
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 2
		
svy, subpop(if analytic_pop_diab_diag == 1 & worldregion6 == `x'): proportion orl_insl_diagdia
matrix est = e(b)
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 1
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 1
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 1
		
svy, subpop(if analytic_pop_diab_diag == 1 & worldregion6 == `x'): proportion insul_diagdia 
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

twoway (bar proportions_diag1 treat_diagdia_inv if nomeds_diagdia == 1 & worldregion6 == 1, bcolor(eltblue) barw(0.95) horizontal)  ///
(bar proportions_diag1 treat_diagdia_inv if oral_diagdia == 1 & worldregion6 == 1, bcolor(orange) barw(0.95) horizontal)  ///
(bar proportions_diag1 treat_diagdia_inv if orl_insl_diagdia == 1 & worldregion6 == 1, bcolor(eltgreen) barw(0.95) horizontal)  ///
(bar proportions_diag1 treat_diagdia_inv if insul_diagdia == 1 & worldregion6 == 1, bcolor(gold) barw(0.95) horizontal) ///
(rcap upper_bound_diag1 lower_bound_diag1 treat_diagdia_inv if worldregion6 == 1, lcolor(gs8) horizontal) ///
(scatter treat_diagdia_inv proportions_diag1 if worldregion6 == 1, msymbol(none) mlabel(proportions_marker_diag1) mlabposition(1) mlabcolor(black) mlabsize(small)), ///			
ytitle("") ylabel(none) xtitle(" ", size(small)) xscale(range(0 100) titlegap(*1.2)) xlabel(0(10)100,labsize(vsmall) format(%9.0f) nogrid angle(0)) ///		
legend(off) ///
title("{bf}B) Individuals with diagnosed diabetes", size(small) col(black) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) 
graph save "bar1_h", replace

forval x = 2/5 {
twoway (bar proportions_diag`x' treat_diagdia_inv if nomeds_diagdia == 1 & worldregion6 == `x', bcolor(eltblue) barw(0.95) horizontal)  ///
(bar proportions_diag`x' treat_diagdia_inv if oral_diagdia == 1 & worldregion6 == `x', bcolor(orange) barw(0.95) horizontal)  ///
(bar proportions_diag`x' treat_diagdia_inv if orl_insl_diagdia == 1 & worldregion6 == `x', bcolor(eltgreen) barw(0.95) horizontal)  ///
(bar proportions_diag`x' treat_diagdia_inv if insul_diagdia == 1 & worldregion6 == `x', bcolor(gold) barw(0.95) horizontal) ///
(rcap upper_bound_diag`x' lower_bound_diag`x' treat_diagdia_inv if worldregion6 == `x', lcolor(gs8) horizontal) ///
(scatter treat_diagdia_inv proportions_diag`x' if worldregion6 == `x', msymbol(none) mlabel(proportions_marker_diag`x') mlabposition(1) mlabcolor(black) mlabsize(small)), ///			
ytitle("") ylabel(none) xtitle(" ", size(small)) xscale(range(0 100) titlegap(*1.2)) xlabel(0(10)100,labsize(vsmall) format(%9.0f) nogrid angle(0)) ///		
title(" ", size(medsmall) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) legend(off) 
graph save bar`x'_h, replace
}

twoway (bar proportions_diag6 treat_diagdia_inv if nomeds_diagdia == 1 & worldregion6 == 6, bcolor(eltblue) barw(0.95) horizontal)  ///
(bar proportions_diag6 treat_diagdia_inv if oral_diagdia == 1 & worldregion6 == 6, bcolor(orange) barw(0.95) horizontal)  ///
(bar proportions_diag6 treat_diagdia_inv if orl_insl_diagdia == 1 & worldregion6 == 6, bcolor(eltgreen) barw(0.95) horizontal)  ///
(bar proportions_diag6 treat_diagdia_inv if insul_diagdia == 1 & worldregion6 == 6, bcolor(gold) barw(0.95) horizontal) ///
(rcap upper_bound_diag6 lower_bound_diag6 treat_diagdia_inv if worldregion6 == 6, lcolor(gs8) horizontal) ///
(scatter treat_diagdia_inv proportions_diag6 if worldregion6 == 6, msymbol(none) mlabel(proportions_marker_diag6) mlabposition(1) mlabcolor(black) mlabsize(small)), ///			
ytitle("") ylabel(none) xtitle("Weighted proportion (%)", size(vsmall)) xscale(range(0 100) titlegap(*1.2)) xlabel(0(10)100,labsize(vsmall) format(%9.0f) nogrid angle(0)) ///		
title(" ", size(medsmall) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) ///
legend(position(5) ring(0) order(1 "No medication" 2 "Oral only" 3 "Oral and insulin" 4 "Insulin only") row(1) symy(2) symx(2) textw(5) size(vsmall) keygap(*0.3) colgap(*0.8)) 
graph save "bar6_h", replace

graph combine "pie1_h" "bar1_h" "pie2_h" "bar2_h" "pie3_h" "bar3_h" "pie4_h" "bar4_h" "pie5_h" "bar5_h" "pie6_h" "bar6_h", row(6) graphregion(margin(small)color("white")) imargin(0 0 0 0)
graph save "World region", replace 

svy, subpop(analytic_pop_diab) over(worldregion6): proportion treat_diagdia		// verify numbers in graph --> check


*** FIGURE S3: STRATIFYING BY INDIVIDUAL-LEVEL WEALTH ***								

use "$data/HPACC_diab_meds_simul_final.dta", clear
cd "$results" 

*bys country: mdesc wealth_quintile if analytic_pop_diab == 1
count if analytic_pop_all == 1 // 223,283

drop if country == "Bangladesh" | country == "Belarus" | country == "Burkina Faso" | country == "Chile" | country == "Costa Rica" | country == "El Salvador" | country == "Iraq"  ///
| country == "Libya" | country == "Turkmenistan" | country == "Vietnam"| country == "Tokelau" | country == "Nepal"

count if analytic_pop_all == 1 // 183,972
mdesc wealth_quintile if analytic_pop_all == 1		// 11.8%

drop countrycat
egen countrycat = group(country) 	// 53 countries remaining

* All

forval x = 1/5 { 
svy, subpop(if analytic_pop_diab == 1 & wealth_quintile == `x'): proportion diag
matrix est = e(b) 
gen B`x' = est[1,2]
global C`x' = 0.5*360*B`x'
}

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & wealth_quintile == 1, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f) gap(35)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(40) fxsize(40) angle0($C1) legend(off) title(" ", size(medsmall) position(11)) l1title("{bf}Poorest", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(small)) title("{bf}A) All individuals with diabetes", size(small) col(black) position(11)) 
graph save "pie1_h", replace

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & wealth_quintile == 2, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f) gap(35)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(40) fxsize(40) angle0($C2) legend(off) title(" ", size(small) position(11)) l1title("{bf}2", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
l1title("{bf}Poorer", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(small))
graph save "pie2_h", replace

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & wealth_quintile == 3, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f) gap(35)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(40) fxsize(40) angle0($C3) legend(off) title(" ", size(small) position(11)) l1title("{bf}3", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
l1title("{bf}Middle", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(small))
graph save "pie3_h", replace

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & wealth_quintile == 4, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f) gap(35)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(40) fxsize(40) angle0($C4) legend(off) title(" ", size(small) position(11)) l1title("{bf}4", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
l1title("{bf}Wealthier", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(small))
graph save "pie4_h", replace

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & wealth_quintile == 5, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f) gap(35)) ///
legend(position(7) ring(0) order(2 "Undiagnosed" 1 "Diagnosed") symy(2) symx(2) textw(8) size(vsmall) col(1) keygap(*0.3)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(40) fxsize(40) angle0($C5) ///
l1title("{bf}Wealthiest", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(small)) title(" ", size(small))
graph save "pie5_h", replace

svy, subpop(analytic_pop_diab) over(wealth_quintile): proportion diag		// verify numbers in graph --> check

* Diagnosed

forval x = 1/5 { 
generate proportions_diag`x' = .
generate upper_bound_diag`x' = .
generate lower_bound_diag`x' = .

svy, subpop(if analytic_pop_diab_diag == 1 & wealth_quintile == `x'): proportion nomeds_diagdia 
matrix est = e(b)	
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 3
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 3
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 3
		
svy, subpop(if analytic_pop_diab_diag == 1 & wealth_quintile == `x'): proportion oral_diagdia 
matrix est = e(b)
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 2
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 2
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 2
		
svy, subpop(if analytic_pop_diab_diag == 1 & wealth_quintile == `x'): proportion orl_insl_diagdia 
matrix est = e(b)
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 1
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 1
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 1
		
svy, subpop(if analytic_pop_diab_diag == 1 & wealth_quintile == `x'): proportion insul_diagdia 
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

twoway (bar proportions_diag1 treat_diagdia_inv if nomeds_diagdia == 1 & wealth_quintile == 1, bcolor(eltblue) barw(0.95) horizontal)  ///
(bar proportions_diag1 treat_diagdia_inv if oral_diagdia == 1 & wealth_quintile == 1, bcolor(orange) barw(0.95) horizontal)  ///
(bar proportions_diag1 treat_diagdia_inv if orl_insl_diagdia == 1 & wealth_quintile == 1, bcolor(eltgreen) barw(0.95) horizontal)  ///
(bar proportions_diag1 treat_diagdia_inv if insul_diagdia == 1 & wealth_quintile == 1, bcolor(gold) barw(0.95) horizontal) ///
(rcap upper_bound_diag1 lower_bound_diag1 treat_diagdia_inv if wealth_quintile == 1, lcolor(gs8) horizontal) ///
(scatter treat_diagdia_inv proportions_diag1 if wealth_quintile == 1, msymbol(none) mlabel(proportions_marker_diag1) mlabposition(1) mlabcolor(black) mlabsize(small)), ///			
ytitle("") ylabel(none) xtitle(" ", size(small)) xscale(range(0 100) titlegap(*1.2)) xlabel(0(10)100,labsize(vsmall) format(%9.0f) nogrid angle(0)) ///		
title("{bf}B) Individuals with diagnosed diabetes", size(small) col(black) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) ///
legend(position(1) ring(0) order(1 "No medication" 2 "Oral only" 3 "Oral and insulin" 4 "Insulin only") row(3) symy(2) symx(2) textw(7) size(vsmall) keygap(*0.3) colgap(*0.8))
graph save "bar1_h", replace

forval x = 2/4 {
twoway (bar proportions_diag`x' treat_diagdia_inv if nomeds_diagdia == 1 & wealth_quintile == `x', bcolor(eltblue) barw(0.95) horizontal)  ///
(bar proportions_diag`x' treat_diagdia_inv if oral_diagdia == 1 & wealth_quintile == `x', bcolor(orange) barw(0.95) horizontal)  ///
(bar proportions_diag`x' treat_diagdia_inv if orl_insl_diagdia == 1 & wealth_quintile == `x', bcolor(eltgreen) barw(0.95) horizontal)  ///
(bar proportions_diag`x' treat_diagdia_inv if insul_diagdia == 1 & wealth_quintile == `x', bcolor(gold) barw(0.95) horizontal) ///
(rcap upper_bound_diag`x' lower_bound_diag`x' treat_diagdia_inv if wealth_quintile == `x', lcolor(gs8) horizontal) ///
(scatter treat_diagdia_inv proportions_diag`x' if wealth_quintile == `x', msymbol(none) mlabel(proportions_marker_diag`x') mlabposition(1) mlabcolor(black) mlabsize(small)), ///			
ytitle("") ylabel(none) xtitle(" ", size(small)) xscale(range(0 100) titlegap(*1.2)) xlabel(0(10)100,labsize(vsmall) format(%9.0f) nogrid angle(0)) ///		
title(" ", size(medsmall) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) legend(off) 
graph save bar`x'_h, replace
}

twoway (bar proportions_diag5 treat_diagdia_inv if nomeds_diagdia == 1 & wealth_quintile == 5, bcolor(eltblue) barw(0.95) horizontal)  ///
(bar proportions_diag5 treat_diagdia_inv if oral_diagdia == 1 & wealth_quintile == 5, bcolor(orange) barw(0.95) horizontal)  ///
(bar proportions_diag5 treat_diagdia_inv if orl_insl_diagdia == 1 & wealth_quintile == 5, bcolor(eltgreen) barw(0.95) horizontal)  ///
(bar proportions_diag5 treat_diagdia_inv if insul_diagdia == 1 & wealth_quintile == 5, bcolor(gold) barw(0.95) horizontal) ///
(rcap upper_bound_diag5 lower_bound_diag5 treat_diagdia_inv if wealth_quintile == 5, lcolor(gs8) horizontal) ///
(scatter treat_diagdia_inv proportions_diag5 if wealth_quintile == 5, msymbol(none) mlabel(proportions_marker_diag5) mlabposition(1) mlabcolor(black) mlabsize(small)), ///			
ytitle("") ylabel(none) xtitle("Weighted proportion (%)", size(small)) xscale(range(0 100) titlegap(*1.2)) xlabel(0(10)100,labsize(vsmall) format(%9.0f) nogrid angle(0)) ///		
title(" ", size(medsmall) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) ///
legend(off) /*legend(position(1) ring(0) order(1 "No medication" 2 "Oral only" 3 "Oral and insulin" 4 "Insulin only") row(1) symy(2) symx(2) textw(7) size(vsmall) keygap(*0.3) colgap(*0.8))*/ 
graph save "bar5_h", replace

graph combine "pie1_h" "bar1_h" "pie2_h" "bar2_h" "pie3_h" "bar3_h" "pie4_h" "bar4_h" "pie5_h" "bar5_h", row(5) graphregion(margin(small)color("white")) imargin(0 0 0 0)
graph save "wealth_bars", replace

svy, subpop(analytic_pop_diab) over(wealth_quintile): proportion treat_diagdia	// verify numbers in graph --> check


*** Figure S4: Stratify by sex ***			

use "$data/HPACC_diab_meds_simul_final.dta", clear
cd "$results" 
graph drop _all 

* All

forval x = 0/1 { 
svy, subpop(if analytic_pop_diab == 1 & sex == `x'): proportion diag
matrix est = e(b) 
gen B`x' = est[1,2]
global C`x' = 0.5*360*B`x'
}

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & sex == 0, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(50) fxsize(50) angle0($C0) legend(off) title(" ", size(medsmall) position(11)) l1title("{bf}Men", size(medsmall) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(large)) title("{bf}A) All individuals with diabetes", size(medsmall) col(black) position(11)) 
graph save "pie1_h", replace

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & sex == 1, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f)) ///
legend(position(7) ring(0) order(2 "Undiagnosed" 1 "Diagnosed") symy(2) symx(2) textw(11) size(small) col(1) keygap(*0.5)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(50) fxsize(50) angle0($C1) ///
l1title("{bf}Women", size(medsmall) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(large)) title(" ", size(medsmall))
graph save "pie2_h", replace

svy, subpop(analytic_pop_diab) over(sex): proportion diag	// verify numbers in graph --> check

* Diagnosed

forval x = 0/1 { 
generate proportions_diag`x' = .
generate upper_bound_diag`x' = .
generate lower_bound_diag`x' = .

svy, subpop(if analytic_pop_diab_diag == 1 & sex == `x'): proportion nomeds_diagdia 
matrix est = e(b)	
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 3
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 3
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 3
		
svy, subpop(if analytic_pop_diab_diag == 1 & sex == `x'): proportion oral_diagdia
matrix est = e(b)
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 2
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 2
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 2
		
svy, subpop(if analytic_pop_diab_diag == 1 & sex == `x'): proportion orl_insl_diagdia 
matrix est = e(b)
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 1
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 1
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 1
		
svy, subpop(if analytic_pop_diab_diag == 1 & sex == `x'): proportion insul_diagdia
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
		
twoway (bar proportions_diag0 treat_diagdia_inv if nomeds_diagdia == 1 & sex == 0, bcolor(eltblue) barw(0.95) horizontal)  ///
(bar proportions_diag0 treat_diagdia_inv if oral_diagdia == 1 & sex == 0, bcolor(orange) barw(0.95) horizontal)  ///
(bar proportions_diag0 treat_diagdia_inv if orl_insl_diagdia == 1 & sex == 0, bcolor(eltgreen) barw(0.95) horizontal)  ///
(bar proportions_diag0 treat_diagdia_inv if insul_diagdia == 1 & sex == 0, bcolor(gold) barw(0.95) horizontal) ///
(rcap upper_bound_diag0 lower_bound_diag0 treat_diagdia_inv if sex == 0, lcolor(gs8) horizontal) ///
(scatter treat_diagdia_inv proportions_diag0 if sex == 0, msymbol(none) mlabel(proportions_marker_diag0) mlabposition(1) mlabcolor(black) mlabsize(small)), ///			
ytitle("") ylabel(none) xtitle(" ", size(small)) xscale(range(0 100) titlegap(*1.2)) xlabel(0(10)100,labsize(small) format(%9.0f) nogrid angle(0)) ///		
legend(off) ///
title("{bf}B) Individuals with diagnosed diabetes", size(medsmall) col(black) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) 
graph save "bar1_h", replace

twoway (bar proportions_diag1 treat_diagdia_inv if nomeds_diagdia == 1 & sex == 1, bcolor(eltblue) barw(0.95) horizontal)  ///
(bar proportions_diag1 treat_diagdia_inv if oral_diagdia == 1 & sex == 1, bcolor(orange) barw(0.95) horizontal)  ///
(bar proportions_diag1 treat_diagdia_inv if orl_insl_diagdia == 1 & sex == 1, bcolor(eltgreen) barw(0.95) horizontal)  ///
(bar proportions_diag1 treat_diagdia_inv if insul_diagdia == 1 & sex == 1, bcolor(gold) barw(0.95) horizontal) ///
(rcap upper_bound_diag1 lower_bound_diag1 treat_diagdia_inv if sex == 1, lcolor(gs8) horizontal) ///
(scatter treat_diagdia_inv proportions_diag1 if sex == 1, msymbol(none) mlabel(proportions_marker_diag1) mlabposition(1) mlabcolor(black) mlabsize(small)), ///			
ytitle("") ylabel(none) xtitle("Weighted proportion (%)", size(small)) xscale(range(0 100) titlegap(*1.2)) xlabel(0(10)100,labsize(small) format(%9.0f) nogrid angle(0)) ///		
title(" ", size(medsmall) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) ///
legend(position(5) ring(0) order(1 "No medication" 2 "Oral only" 3 "Oral and insulin" 4 "Insulin only") row(2) symy(2) symx(2) textw(11) size(small) rowgap(*0.5) colgap(*0.7) keygap(*0.5)) 
graph save "bar2_h", replace

svy, subpop(analytic_pop_diab_diag) over(sex): proportion treat_diagdia 	// verify numbers in graph --> check

* Combine
graph combine "pie1_h" "bar1_h" "pie2_h" "bar2_h", row(2) graphregion(margin(small)color("white")) imargin(0 0 0 0)
graph save "Sex", replace 


*** FIGURE S5: STRATIFICATION BY AGE CATEGORY ***		// Note: Data on age is missing for 12 people

use "$data/HPACC_diab_meds_simul_final.dta", clear
cd "$results" 

* All

forval x = 1/4 { 
svy, subpop(if analytic_pop_diab == 1 & age_cat == `x'): proportion diag
matrix est = e(b) 
gen B`x' = est[1,2]
global C`x' = 0.5*360*B`x'
}

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & age_cat == 1, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f) gap(35)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(40) fxsize(40) angle0($C1) legend(off) title(" ", size(medsmall) position(11)) l1title("{bf}25-34", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(small)) title("{bf}A) All individuals with diabetes", size(small) col(black) position(11)) 
graph save "pie1_h", replace

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & age_cat == 2, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f) gap(35)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(40) fxsize(40) angle0($C2) legend(off) title(" ", size(small) position(11)) l1title("{bf}35-44", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(small))
graph save "pie2_h", replace

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & age_cat == 3, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f) gap(35)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(40) fxsize(40) angle0($C3) legend(off) title(" ", size(small) position(11)) l1title("{bf}45-54", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(small))
graph save "pie3_h", replace

graph pie diag undiag [pw = weq_w3_analytic_all] if analytic_pop_diab == 1 & age_cat == 4, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f) gap(35)) ///
legend(position(7) ring(0) order(2 "Undiagnosed" 1 "Diagnosed") symy(2) symx(2) textw(8) size(vsmall) col(1) keygap(*0.3)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(40) fxsize(40) angle0($C4) ///
l1title("{bf}>54", size(small) col(black) ring(0) /*orientation(horizontal)*/) ///
b1title(" ", size(small)) title(" ", size(small))
graph save "pie4_h", replace

svy, subpop(analytic_pop_diab) over(age_cat): proportion diag		// verify numbers in graph --> check

* Diagnosed

forval x = 1/4 { 
generate proportions_diag`x' = .
generate upper_bound_diag`x' = .
generate lower_bound_diag`x' = .

svy, subpop(if analytic_pop_diab_diag == 1 & age_cat == `x'): proportion nomeds_diagdia 
matrix est = e(b)	
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 3
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 3
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 3
		
svy, subpop(if analytic_pop_diab_diag == 1 & age_cat == `x'): proportion oral_diagdia 
matrix est = e(b)
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 2
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 2
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 2
		
svy, subpop(if analytic_pop_diab_diag == 1 & age_cat == `x'): proportion orl_insl_diagdia 
matrix est = e(b)
matrix ci = r(table)
replace proportions_diag`x' = est[1, 2] if treat_diagdia_inv == 1
replace lower_bound_diag`x' = ci[5,2] if treat_diagdia_inv == 1
replace upper_bound_diag`x' = ci[6,2] if treat_diagdia_inv == 1
		
svy, subpop(if analytic_pop_diab_diag == 1 & age_cat == `x'): proportion insul_diagdia 
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

twoway (bar proportions_diag1 treat_diagdia_inv if nomeds_diagdia == 1 & age_cat == 1, bcolor(eltblue) barw(0.95) horizontal)  ///
(bar proportions_diag1 treat_diagdia_inv if oral_diagdia == 1 & age_cat == 1, bcolor(orange) barw(0.95) horizontal)  ///
(bar proportions_diag1 treat_diagdia_inv if orl_insl_diagdia == 1 & age_cat == 1, bcolor(eltgreen) barw(0.95) horizontal)  ///
(bar proportions_diag1 treat_diagdia_inv if insul_diagdia == 1 & age_cat == 1, bcolor(gold) barw(0.95) horizontal) ///
(rcap upper_bound_diag1 lower_bound_diag1 treat_diagdia_inv if age_cat == 1, lcolor(gs8) horizontal) ///
(scatter treat_diagdia_inv proportions_diag1 if age_cat == 1, msymbol(none) mlabel(proportions_marker_diag1) mlabposition(1) mlabcolor(black) mlabsize(small)), ///			
ytitle("") ylabel(none) xtitle(" ", size(small)) xscale(range(0 100) titlegap(*1.2)) xlabel(0(10)100,labsize(vsmall) format(%9.0f) nogrid angle(0)) ///		
legend(off) ///
title("{bf}B) Individuals with diagnosed diabetes", size(small) col(black) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) 
graph save "bar1_h", replace

forval x = 2/3 {
twoway (bar proportions_diag`x' treat_diagdia_inv if nomeds_diagdia == 1 & age_cat == `x', bcolor(eltblue) barw(0.95) horizontal)  ///
(bar proportions_diag`x' treat_diagdia_inv if oral_diagdia == 1 & age_cat == `x', bcolor(orange) barw(0.95) horizontal)  ///
(bar proportions_diag`x' treat_diagdia_inv if orl_insl_diagdia == 1 & age_cat == `x', bcolor(eltgreen) barw(0.95) horizontal)  ///
(bar proportions_diag`x' treat_diagdia_inv if insul_diagdia == 1 & age_cat == `x', bcolor(gold) barw(0.95) horizontal) ///
(rcap upper_bound_diag`x' lower_bound_diag`x' treat_diagdia_inv if age_cat == `x', lcolor(gs8) horizontal) ///
(scatter treat_diagdia_inv proportions_diag`x' if age_cat == `x', msymbol(none) mlabel(proportions_marker_diag`x') mlabposition(1) mlabcolor(black) mlabsize(small)), ///			
ytitle("") ylabel(none) xtitle(" ", size(small)) xscale(range(0 100) titlegap(*1.2)) xlabel(0(10)100,labsize(vsmall) format(%9.0f) nogrid angle(0)) ///		
title(" ", size(medsmall) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) legend(off) 
graph save bar`x'_h, replace
}

twoway (bar proportions_diag4 treat_diagdia_inv if nomeds_diagdia == 1 & age_cat == 4, bcolor(eltblue) barw(0.95) horizontal)  ///
(bar proportions_diag4 treat_diagdia_inv if oral_diagdia == 1 & age_cat == 4, bcolor(orange) barw(0.95) horizontal)  ///
(bar proportions_diag4 treat_diagdia_inv if orl_insl_diagdia == 1 & age_cat == 4, bcolor(eltgreen) barw(0.95) horizontal)  ///
(bar proportions_diag4 treat_diagdia_inv if insul_diagdia == 1 & age_cat == 4, bcolor(gold) barw(0.95) horizontal) ///
(rcap upper_bound_diag4 lower_bound_diag4 treat_diagdia_inv if age_cat == 4, lcolor(gs8) horizontal) ///
(scatter treat_diagdia_inv proportions_diag4 if age_cat == 4, msymbol(none) mlabel(proportions_marker_diag4) mlabposition(1) mlabcolor(black) mlabsize(small)), ///			
ytitle("") ylabel(none) xtitle("Weighted proportion (%)", size(vsmall)) xscale(range(0 100) titlegap(*1.2)) xlabel(0(10)100,labsize(vsmall) format(%9.0f) nogrid angle(0)) ///		
title(" ", size(medsmall) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) ///
legend(position(5) ring(0) order(1 "No medication" 2 "Oral only" 3 "Oral and insulin" 4 "Insulin only") row(1) symy(2) symx(2) textw(7) size(vsmall) keygap(*0.3) colgap(*0.8)) 
graph save "bar4_h", replace

graph combine "pie1_h" "bar1_h" "pie2_h" "bar2_h" "pie3_h" "bar3_h" "pie4_h" "bar4_h", row(4) graphregion(margin(small)color("white")) imargin(0 0 0 0)
graph save "agecat_bars", replace

svy, subpop(analytic_pop_diab) over(age_cat): proportion treat_diagdia	// verify numbers in graph --> check


*** FIGURE S6: WPOP INSTEAD OF WEQ ***

use "$data/HPACC_diab_meds_simul_final.dta", clear
cd "$results" 
graph drop _all

* All
svyset psu_num[pw = wpop_w3_analytic_all], strata(stratum_num) singleunit(centered)  		

svy, subpop(analytic_pop_diab): proportion diag		// point estimates should be identical to numbers in pie chart --> verify
matrix est = e(b) 
gen B = .
replace B = est[1,2]
global C = 0.5*360*B
display `C'

graph pie diag undiag [pw = wpop_w3_analytic_all] if analytic_pop_diab == 1, graphregion(color(white)) ysize(4) xsize(4) plabel(_all percent, size(small) format(%9.1f)) ///
legend(position(7) ring(0) order(2 "Undiagnosed" 1 "Diagnosed") symy(2) symx(2) textw(16) size(small) col(1)) ///
pie(1, explode(2) color(lavender)) pie(2, color(edkblue)) fysize(50) fxsize(50) angle0($C) ///
title("{bf}A) All individuals with diabetes", size(medsmall) col(black) position(11)) ///
name(global_pie_wpop) 

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
(scatter proportions_diag treat_diagdia, msymbol(none) mlabel(proportions_marker_diag) mlabposition(2) mlabcolor(black)), ///			
xtitle("") xlabel(none) ytitle("Weighted proportion (%)", size(small)) yscale(range(0 100) titlegap(*1.2)) ylabel(0(10)100,labsize(small) format(%9.0f) nogrid angle(0)) ///		
legend(position(2) ring(0) order(1 "No medication" 2 "Oral only" 3 "Oral and insulin" 4 "Insulin only") col(1) symy(2) symx(2) textw(16) size(small)) ///
title("{bf}B) Individuals with diagnosed diabetes", size(medsmall) col(black) position(11)) graphregion(color(white)) subtitle(" ", size(minuscule)) ///
name(global_bars_diagdia_wpop) 

svy, subpop(analytic_pop_diab): proportion treat_diagdia			// Check whether numbers in graphs are identical to these --> Verified

* Combine
graph combine global_pie_wpop global_bars_diagdia_wpop, row(1) graphregion(margin(small)color("white")) imargin(0 0 0 0)
graph save "global_bars_wpop", replace
