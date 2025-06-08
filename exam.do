*******************************************
* Question 1
*******************************************

* Read data
use "homeexamdata.dta", clear

* summarize
summarize 

* collapse by mean
collapse (mean) y, by(year treated)

* visualize the plot of treated and not treated
twoway ///
    (line y year if treated == 1, lcolor(blue) lpattern(solid)) ///
    (line y year if treated == 0, lcolor(red) lpattern(dash)) ///
    , ///
    legend(label(1 "Treated") label(2 "Control")) ///
    title("Average Outcome Over Time by Treatment Status") ///
    ytitle("Average Outcome") xtitle("Year")


*******************************************
* Question 2
*******************************************

use "homeexamdata.dta", clear

gen post_treat = post*treated


* alternativly 
* reg y i.year i.id post_treat base, vce(cluster id)

* two way fixed effect absorbing both id and year 

reg post_treat base shock

esttab using results0.rtf, replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    keep(base shock _cons) ///
    title("Regression Table: Post_treat = α + β · base + β1 · shock") ///
    label ///
    b(3) se(3) ///
    nogaps nomtitles
	

* by checking we see that base is a significant predictor of y therefore will be used in our analysis, by looking at the vif we can test for multicollinearity
reg y post_treat shock base i.year i.id, vce(cluster id)
vif

reghdfe y base shock post_treat, absorb(id year) vce(cluster id)

esttab using results1.rtf, replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    keep(base shock post_treat _cons) ///
    title("Two-Way Fixed Effects Regression") ///
    label ///
    b(3) se(3) ///
    nogaps nomtitles

* unused 
* Collapse the data
* collapse (mean) y, by(year treated)

* Reshape to wide format 
*reshape wide y, i(year) j(treated)

* generate delta
* gen diff_y = y1-y0

* Scatterplot of differens over time
* scatter diff_y year, c(l) xline(2008.5) title("δ of treated and average control group")

* dis 0.75*_N^(1/3)
* tsset year 
* newey diff_y post, lag(3)

*******************************************
* Question 3 
*******************************************

*******************************************
* Question 3:1 
*******************************************

use homeexamdata.dta, clear

forvalues z=0/4 {
	
	gen pre_`z'=treated*(year==2009-`z')
	gen post_`z'=treated*(year==2009+`z')
	
}

gen pre_5=treated*(year<=2009-5) // Binned end point -4
gen post_5=treated*(year>=2009+5) // Binned end point +4

reghdfe y pre_4 pre_3 pre_2 post_0 post_1 post_2 post_3  post_4 base shock, absorb(id year) vce(cluster id) // No robusts according to Peters code. 

* or reg y pre_4 pre_3 pre_2 post_0 post_1 post_2 post_3  post_4 base shock i.year i.id, vce(cluster id)

*******************************************
* Question 3.4 | Test Parallel Trends
*******************************************

**F-test
reghdfe y pre_4 pre_3 pre_2 post_0 post_1 post_2 post_3  post_4 base shock, vce(cluster id)  absorb(id year) // No robusts according to Peters code. 

test pre_4 pre_3 pre_2 

**Straight line test. 
******* Är osäker ifall denna borde ersättas av XTEVENT? 
	coefplot, keep(pre_4 pre_3 pre_2 post_0 post_1 post_2 post_3  post_4 base shock) /// 
 order(pre_4 pre_3 pre_2 post_0 post_1 post_2 post_3  post_4 ) vertical  ///
	saving(dynamic_twfe.gph, replace) t2("Effects on treated") ///
	xlabel(1 "-5+" 2 "-4" 3 "-3" 4 "-2" 5 "0" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5+", angle(70)) ///
	mcolor("black") ciopts(lcolor("black")) ms("d") ///
	yline(0, lpattern(dash) lcolor(black) lwidth(thin)) ///
	xline(4.5, lpattern(dash) lcolor(black) lwidth(vthin)) 
	
xtevent y, panelvar(id) ///
timevar(year) policyvar(policy) window(-4 4) impute(stag) reghdfe plot 

* event study plot // Chanelles graf
marginsplot, recast(connected) ciopts(recast(rline)) /// 
yline(0, lpattern(dash)) /// 
title("Event Study: Dynamic Treatment Effects") ///
xtitle("Years from Treatment") ytitle("Effect on y")


*******************************************
* Question 4 
*******************************************

