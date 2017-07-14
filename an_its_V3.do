
** do-file: an_its_V3
** Interrupted time series analysis - July 2017
** This version is a complete change from period versions where I was estimating
** multiplicative effects from the negative binomial model
** Here I am looking at additive effects and attempting to estimate the rate 
** difference and then looking at the expected vs observed number of deaths 
** and bootstrapping the difference



use "timeseries.dta", clear

* set up a matrix to collect results - 1 row, 5 columns all containing -99
matrix define table = J(1,5,-99)

* find best fitting splines in the pre-policy period
mvrs poisson bothsexes year if adaa == 0, robust offset(lpop) nolog all

* save best fitting spline as global macro

global rhs `e(fp_fvl)'
	
* temporary for observed and expected 
tempvar pxbs pnums omenums


* predict number of events and observed minus expected

qui predict double `pxbs', xb
label var `pxbs' "linear prediction"
gen pnums = exp(`pxbs')
label var pnum "exponentiated linear predictions"

gen `omenums' = bothsexes - pnums
label var `omenums' "obs - expected number"

* sum observed and expected in recession period observed (in memory)

sum bothsexes if adaa==1 
scalar oev = `r(sum)' // observed events
disp as text "Observed events: " as result r(sum)
matrix table[1,1] = oev
	
* expected (predicted)
qui sum pnums if adaa==1 
scalar eev= `r(sum)' // expected events
disp as text "Expected events: " as result r(sum)
matrix table[1,2] = eev

* obs minus expected
qui sum `omenums' if adaa==1 
scalar ome = `r(sum)' // obs - expected
disp as text "Obs - expected: " as result r(sum)
matrix table[1,3] = ome
	
matrix oer = [oev, eev, ome]
matrix list oer



**************************************************************************
// program to bootstrap the process to get SE for observed minus expected
**************************************************************************


capture program drop bootall
program define bootall, rclass
version 13.1
 preserve 
 
  bsample	// take a bootstrap sample with replacement
  
  qui sum year if adaa==0, detail

	
	tempvar pnums omenums
	gen `pnums' = .
	gen `omenums' = .
	
	* use best fitting restricted cubic spline model
	
	
poisson bothsexes ${rhs} if adaa==0, offset(lpop) robust nolog
	
	tempvar pxbs pnums omenums
	
	* predict expected number of events
predict double `pxbs', xb 	// predicted log events
gen `pnums' = exp(`pxbs')	// predicted events

qui sum `pnums' if adaa==1 	// pred events after policy change
return scalar eev = r(sum)
disp as text "Expected events: " as result r(sum)


* observed number of events
qui sum bothsexes if adaa==1  // obs events in recession
return scalar oev = r(sum)
disp as text "Observed events: " as result r(sum)
	
* observed minus expected
gen `omenums' = bothsexes - `pnums`g'' // obs - exp events after recession
	qui sum `omenums' if adaa==1 
	return scalar ome = r(sum)
	disp as text "Obs minus expected: " as result r(sum)
	
	
matrix oer = [oev, eev, ome]


  restore
end



// now use the program to simulate this process multiple times

* run once to make sure all is well
bootall

* now simulate distribution of outcomes with 500 replications
preserve
simulate oev=r(oev) eev=r(eev) ome=r(ome) ///
		, reps(500) seed(1234567): bootall

* bootstrapped standard errors - what is the 19??

bstat, stat(oer) n(19)
estat bootstrap, percentile
matrix totci = e(ci_percentile)

* Put CI's of OME in the final table
matrix table[1,4] = totci[1,3]
matrix table[1,5] = totci[2,3]

restore


* export results to excel 
putexcel B3 = matrix(table) using results, replace



