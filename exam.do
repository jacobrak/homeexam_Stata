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

dis 0.75*_N^(1/3)
tsset year 
newey y base, lag(8)