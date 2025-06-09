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

* Reshape to wide format 
reshape wide y, i(year) j(treated)

* generate delta
gen diff_y = y1-y0

* Scatterplot of differens over time
scatter diff_y year, c(l) xline(2008.5) title("δ of treated and average control group")

*******************************************
* Question 2
*******************************************

* Read data
use "homeexamdata.dta", clear

* Generate post_trest
gen post_treat = post*treated

* Testing pre-treatment effect.
reg post_treat base shock

* Extra tests, confindence intervall contains 0 
reg base post_treat  
reg shock post_treat

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

* regression 
reghdfe y base shock post_treat, absorb(id year) vce(cluster id)

esttab using results1.rtf, replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    keep(base shock post_treat _cons) ///
    title("Two-Way Fixed Effects Regression") ///
    label ///
    b(3) se(3) ///
    nogaps nomtitles

*******************************************
* Question 3 
*******************************************

* Read data
use "homeexamdata.dta", clear

forvalues z=0/3 {
	
	gen pre_`z'=treated*(year==2009-`z')
	gen post_`z'=treated*(year==2009+`z')
	
}

gen pre_4=treated*(year<=2009-4) // Binned end point -4
gen post_4=treated*(year>=2009+4) // Binned end point +4

* regression
reghdfe y pre_4 pre_3 pre_2 post_0 post_1 post_2 post_3  post_4 base shock, absorb(id year) vce(cluster id) // cluster erros

esttab using resultsevent.rtf, replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    keep(pre_4 pre_3 pre_2 post_0 post_1 post_2 post_3 post_4 base shock _cons) ///
    title("Event study") ///
    label ///
    b(3) se(3) ///
    nogaps nomtitles rtf

*******************************************
* Question 3:4 | Test Parallel Trends
*******************************************

** F-test
reghdfe y pre_4 pre_3 pre_2 post_0 post_1 post_2 post_3  post_4 base shock, absorb(id year)  vce(cluster id) // cluster erros

test pre_4 pre_3 pre_2 
	
** Straight line test, line is added post render.
xtevent y, panelvar(id) ///
timevar(year) policyvar(policy) window(-3 3) impute(stag) reghdfe plot 

*******************************************
* Question 4 
*******************************************
* read data
use homeexamdata.dta, clear

foreach year in 1990 1994 1998 2000 2002 2007 2008 {
    gen y_`year' = y if year == `year'
    replace y_`year' = 0 if missing(y_`year')
} // Loop that creates a variable for Y-output for each of the years that are requested in the task. 

tsset id year // Transform data into T-series in order for Synth to run correctly. 

synth y y_1990 y_1994 y_1998 y_2000 y_2002 y_2007 y_2008 , trunit(1) trperiod(2009) fig keep(synth_reg, replace) // *Fig 4.1*  Creates a synthetic control based on the variables Y_xxxx. Saves it into "synt_reg"

use synth_reg, clear // Use the dataset we created in previous chunk. 

gen diff= _Y_treated-_Y_synthetic // Generate new var for the difference in output between synth control and treated group. Labeling below is optional. 

lab var _Y_treated "Treated"
lab var _Y_synthetic "Synthetic control"

twoway line diff _time, sort  xtitle(Year) ytitle("Gap in y") title("Yearly gap between treated and control output") xline(2009) note("fig 4.2") // Plotted line to display the difference between the two lines previously generated in Fig4.1 


********************************* ATT Estimation *********************************

gen post = (_time >= 2009) // Post var, binary. = 1 if _Time >= year 2009. 

sum diff if post == 1  // Mean diff across all of the post treatment years. 
display "Average ATT (2009–2020) = " %6.3f r(mean)  // Summary in average. 


********************************* Placebo *********************************

* read data
use homeexamdata.dta, clear

forvalues z=1/39 {

	use homeexamdata.dta, clear
	tsset id year
	
    synth y y(1990) y(1994) y(1998) y(2000) y(2002) y(2007) y(2008), trunit(`z') trperiod(2009) fig keep(results`z', replace)

	use results`z', clear

	rename _time year
	
	gen gap`z'=_Y_treated-_Y_synthetic
	sort year
	keep if year!=.
	save t`z', replace // Creates gaps data for all placebos and T

	* Prepare p-values
	gen post=year>=2009
	gen gap_2=gap`z'*gap`z'
	collapse gap_2, by(post) 
	gen ratio=sqrt(gap_2)/sqrt(gap_2[_n-1]) // Creates MSPE ratio from MSP generated in the code just above. 
	keep ratio 
	gen id=`z'
	drop if ratio==.
	save pvalue`z', replace
	
	} // Loop that creates a placebo for each of the chosen years, similar to what we did previously but without any actual treatment. Saves everything in var gapZ
	
use t1, clear
forvalues z=2/39 {

	merge 1:1 year using t`z'
	drop _merge

	} // Merging our new gapZ var into our data so that we can plot them in the next step. 
	
*TREATMENT VS PLACEBO CONTROL PLOT*	
{ // RUN THIS CODE!  
	twoway ///
(line gap1 year, lp(solid) lw(thick) lcolor(black)) /// * Highlighting gap1 thicker. 
(line gap2 year, lcolor(gs10) lwidth(thin)) ///
(line gap3 year, lcolor(gs10) lwidth(thin)) ///
(line gap4 year, lcolor(gs10) lwidth(thin)) ///
(line gap5 year, lcolor(gs10) lwidth(thin)) ///
(line gap6 year, lcolor(gs10) lwidth(thin)) ///
(line gap7 year, lcolor(gs10) lwidth(thin)) ///
(line gap8 year, lcolor(gs10) lwidth(thin)) ///
(line gap9 year, lcolor(gs10) lwidth(thin)) ///
(line gap10 year, lcolor(gs10) lwidth(thin)) ///
(line gap11 year, lcolor(gs10) lwidth(thin)) ///
(line gap12 year, lcolor(gs10) lwidth(thin)) ///
(line gap13 year, lcolor(gs10) lwidth(thin)) ///
(line gap15 year, lcolor(gs10) lwidth(thin)) ///
(line gap16 year, lcolor(gs10) lwidth(thin)) ///
(line gap17 year, lcolor(gs10) lwidth(thin)) ///
(line gap18 year, lcolor(gs10) lwidth(thin)) ///
(line gap19 year, lcolor(gs10) lwidth(thin)) ///
(line gap20 year, lcolor(gs10) lwidth(thin)) ///
(line gap21 year, lcolor(gs10) lwidth(thin)) ///
(line gap22 year, lcolor(gs10) lwidth(thin)) ///
(line gap23 year, lcolor(gs10) lwidth(thin)) ///
(line gap24 year, lcolor(gs10) lwidth(thin)) ///
(line gap25 year, lcolor(gs10) lwidth(thin)) ///
(line gap26 year, lcolor(gs10) lwidth(thin)) ///
(line gap27 year, lcolor(gs10) lwidth(thin)) ///
(line gap28 year, lcolor(gs10) lwidth(thin)) ///
(line gap29 year, lcolor(gs10) lwidth(thin)) ///
(line gap30 year, lcolor(gs10) lwidth(thin)) ///
(line gap31 year, lcolor(gs10) lwidth(thin)) ///
(line gap32 year, lcolor(gs10) lwidth(thin)) ///
(line gap33 year, lcolor(gs10) lwidth(thin)) ///
(line gap34 year, lcolor(gs10) lwidth(thin)) ///
(line gap35 year, lcolor(gs10) lwidth(thin)) ///
(line gap36 year, lcolor(gs10) lwidth(thin)) ///
(line gap37 year, lcolor(gs10) lwidth(thin)) ///
(line gap38 year, lcolor(gs10) lwidth(thin)) ///
(line gap39 year, lcolor(gs10) lwidth(thin)), ///
yline(0, lpattern(shortdash) lcolor(black)) xline(2008.5, lpattern(shortdash) lcolor(black)) ///
xtitle("", si(small)) xlabel(#10) ytitle("Gap in prediction error", size(small)) ///
legend(off) xlabel(1990(2)2020, angle(70)) ylabel(, angle(0)) graphregion(fcolor(white) color(white))

}
	

use pvalue1, clear // Includes MSPE ratio for ID 1. 
forvalues z=2/39 {

	append using pvalue`z'

	}

save pvalue, replace

gen bratio_T=ratio if id==1 // Ratio for treated, used as upper limit/ bar. 
egen ratio_T=mean(bratio_T) // Mean treatment MSPE ratio
gen pvalue=ratio>=ratio_T // Dummy taking value 1 if MSPE-ratio at least as large as T's
sum pvalue // Fraction (probability) of obtaining a MSPE-ratio, as large as T's

********************************* P-VALUE ********************************* 
use pvalue, clear

histogram ratio, bin(50) frequency fcolor(gs13) lcolor(black) ylabel(0(2)6) ///
xtitle(Post/pre RMSPE ratio. P-value=0.0769) xlabel(0(1)10) text(2 4.824051 "↓", size(large)) text(2.28 4.824051 "Treatment", size(small)) text(1.30 5.824051 "⏞", size(large)) text(1.24 5.824051 "Controls", size(small)) saving("pvalue.gph", replace) note(fig 4.5)

********************************* END OF DO-FILE *********************************


