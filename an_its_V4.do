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


use "drugdeaths-race-sex.dta", clear


* set up a matrix to collect results:  1 row, 5 columns all containing -99
* 5 columns: observer, expected, difference, LL, UL. 
* 4 rows: black (men, women) white (mean, women)

drop year_0 year_1 year_2

matrix define table = J(5,5,-99)

* temporary variables to store observed and predicted suicides
tempvar pnums omenums
gen `pnums'= .
gen `omenums'= .

qui levelsof ga, local(levels)
foreach g of local levels {
	di " " 
	di " " 
	di as result "-> Group = `: label (ga) `g''"

	* find best fitting splines in the pre-policy period
	mvrs poisson deaths year if adaa == 0 & ga == `g', robust offset(lpop) nolog all

	* save best fitting spline as global macro
	global rhs`g' `e(fp_fvl)'
	
	* temporary for observed and expected 
	tempvar pxbs`g' pnums`g' omenums`g'

	* predict number of events and observed minus expected
	qui predict double `pxbs`g'' if ga==`g', xb
	label var `pxbs`g'' "linear prediction"

	* Exponentiate the log of the predicted number
	qui gen `pnums`g'' = exp(`pxbs`g'') if ga==`g'
	replace `pnums' = `pnums`g'' if ga==`g'
	*label var `pnums`g'' "exponentiated linear predictions of `g'"

	* generate observed minus expected
	gen `omenums`g'' = deaths - `pnums`g''
	qui replace `omenums' = `omenums`g'' if ga==`g'
	*label var `omenums`g'' "obs - expected number `g'"

	* sum observed and expected after ADAA (in memory)
	sum deaths if adaa==1 & ga==`g'
	scalar oev`g' = `r(sum)' // observed events
	disp as text "Observed events: " as result r(sum)
	matrix table[`g',1] = oev`g'
	
	* expected (predicted)
	qui sum `pnums`g'' if adaa==1 & ga == `g'
	scalar eev`g' = `r(sum)' // expected events
	disp as text "Expected events: " as result r(sum)
	matrix table[`g',2] = eev`g'

	* obs minus expected
	qui sum `omenums`g'' if adaa==1 & ga == `g'
	scalar ome`g' = `r(sum)' // obs - expected
	disp as text "Obs - expected: " as result r(sum)
	matrix table[`g',3] = ome`g'
	
	matrix oer`g' = [oev`g', eev`g', ome`g']
	matrix list oer`g'

}

* totals across all age and gender groups
qui sum deaths if adaa==1 
scalar toev = r(sum) // observed

qui sum `pnums' if adaa == 1
scalar teev = r(sum) // expected

qui sum `omenums' if adaa == 1
scalar tome = r(sum) // obs - exp
	
* write to matrices	
mat table[5,1]=toev
mat table[5,2]=teev
mat table[5,3]=tome

mat oer = [oer1,oer2,oer3,oer4,oer5,oer6,oer7,oer8,toev,teev,tome]	
mat list table




