**************************************************************************************************************************************************************************************************************************************************************
* Question 1 
**************************************************************************************************************************************************************************************************************************************************************

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


**************************************************************************************************************************************************************************************************************************************************************
* Question 2
**************************************************************************************************************************************************************************************************************************************************************

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

**************************************************************************************************************************************************************************************************************************************************************
* Question 3
**************************************************************************************************************************************************************************************************************************************************************

**************************************************************************************************************************************************************************************************************************************************************
* Question 4
**************************************************************************************************************************************************************************************************************************************************************
