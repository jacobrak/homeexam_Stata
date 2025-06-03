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

	
	
*
use "homeexamdata.dta", clear

gen post_treat = post*treated
reg y base i.year i.id post_treat, vce(cluster id)

xtset id year
xtreg y absorb(i.year) base post_treat, fe vce(cluster id)

reghdfe y base post_treat, absorb(id year) vce(cluster id)
outreg2 using results.doc, replace                            ///
    keep(base post_treat)                                      ///
    bdec(3) sdec(3)                                            ///
    ctitle("Two‐Way FE")                                       ///
    noobs nor2


outreg2 using results.doc, replace ///
    keep(base post_treat) ///
    bdec(3) sdec(3) ///
    ctitle("Two‐Way FE")

collapse (mean) y post, by(year treated)

reshape wide y, i(year) j(treated)

gen diff_y = y1-y0


dis 0.75*_N^(1/3)
tsset year 
newey diff_y post, lag(3)