** do-file: an_its_V4
** Interrupted time series analysis - August 2017
** This version is a complete change from period versions where I was estimating
** multiplicative effects from the negative binomial model
** Here I am looking at additive effects and attempting to estimate the rate 
** difference and then looking at the expected vs observed number of deaths 
** and bootstrapping the difference

** Addition to V4: trying to bootstrap the annual rate difference rather than 
** just the overall 

** Difference between V3 & V4: 
* added 1968-1978 data 
* stratified by age, race & sex 


use "drugdeaths.dta", clear


* set up a matrix to collect results - 1 row, 5 columns all containing -99
matrix define table = J(1,5,-99)

* find best fitting splines in the pre-policy period
mvrs poisson bothsexes year if adaa == 0, robust offset(lpop) nolog all

*mvrs poisson female year if adaa == 0, robust offset(lpop) nolog all
* for men
*mvrs poisson male year if adaa == 0, robust offset(lpop) nolog all


* for women


* save best fitting spline as global macro

global rhs `e(fp_fvl)'
	
* temporary for observed and expected (these are the log number)
tempvar pxbs pnums omenums

* predict number of events and observed minus expected

qui predict double `pxbs', xb
label var `pxbs' "linear prediction"

* Exponentiate the log of the predicted number
gen pnums = exp(`pxbs')
label var pnum "exponentiated linear predictions"


* generate observed minus expected
gen `omenums' = bothsexes - pnums
label var `omenums' "obs - expected number"

* sum observed and expected after ADAA (in memory)

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


