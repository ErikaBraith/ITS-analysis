** do-file: an_its_V5
** Interrupted time series analysis - August 2017
** This version is a complete change from period versions where I was estimating
** multiplicative effects from the negative binomial model
** Here I am looking at additive effects and attempting to estimate the rate 
** difference and then looking at the expected vs observed number of deaths 
** and bootstrapping the difference

** Addition to V4: trying to bootstrap the annual rate difference rather than 
** just the overall
** Addition to V5: ageXraceXgender

** Difference between V3 & V4: 
* added 1968-1978 data 
* stratified by age, race & sex 


use "drugdeaths-age-race-sex.dta", clear


* set up a matrix to collect results:  13 row, 5 columns all containing -99
* 5 columns: observer, expected, difference, LL, UL. 
* 13 rows: black (men, women) white (mean, women)


matrix define table = J(13,5,-99)

* temporary variables to store observed and predicted suicides
tempvar pnums omenums
gen `pnums'= .
gen `omenums'= .

qui levelsof gar, local(levels)
foreach g of local levels {
	di " " 
	di " " 
	di as result "-> Group = `: label (gar) `g''"

	* find best fitting splines in the pre-policy period
	mvrs poisson deaths year if adaa == 0 & gar == `g', robust offset(lpop) nolog all

	* save best fitting spline as global macro
	global rhs`g' `e(fp_fvl)'
	
	* temporary for observed and expected 
	tempvar pxbs`g' pnums`g' omenums`g'

	* predict number of events and observed minus expected
	qui predict double `pxbs`g'' if ga==`g', xb
	label var `pxbs`g'' "linear prediction"

	* Exponentiate the log of the predicted number
	qui gen `pnums`g'' = exp(`pxbs`g'') if gar ==`g'
	replace `pnums' = `pnums`g'' if gar ==`g'
	*label var `pnums`g'' "exponentiated linear predictions of `g'"

	* generate observed minus expected
	gen `omenums`g'' = deaths - `pnums`g''
	qui replace `omenums' = `omenums`g'' if gar ==`g'
	*label var `omenums`g'' "obs - expected number `g'"

	* sum observed and expected after ADAA (in memory)
	sum deaths if adaa==1 & gar ==`g'
	scalar oev`g' = `r(sum)' // observed events
	disp as text "Observed events: " as result r(sum)
	matrix table[`g',1] = oev`g'
	
	* expected (predicted)
	qui sum `pnums`g'' if adaa==1 & gar == `g'
	scalar eev`g' = `r(sum)' // expected events
	disp as text "Expected events: " as result r(sum)
	matrix table[`g',2] = eev`g'

	* obs minus expected
	qui sum `omenums`g'' if adaa==1 & gar  == `g'
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
mat table[13,1]=toev
mat table[13,2]=teev
mat table[13,3]=tome

mat oer = [oer1,oer2,oer3,oer4,toev,teev,tome]	
mat list table



// #2
// program to bootstrap the process to get SE for observed minus expected

capture program drop bootall
program define bootall, rclass
version 13.1
 preserve 
 
  bsample	// take a bootstrap sample with replacement
  
  qui sum year if adaa==0, detail
  // create splines using quantiles
  qui splinegen year `r(p25)' `r(p50)' `r(p75)', ///
		bknots(`r(min)' `r(max)') orthog  
	
	tempvar pnums omenums
	gen `pnums' = .
	gen `omenums' = .
	
	* use best fitting restricted cubic spline model
	qui levelsof gar, local(levels)
	foreach g of local levels {
	
	poisson deaths ${rhs`g'} if adaa ==0 & gar==`g', offset(lpop) robust nolog
	
	tempvar pxbs`g' pnums`g' omenums`g'
	
	* predict expected number of events
	predict double `pxbs`g'', xb 	// predicted log events
	gen `pnums`g'' = exp(`pxbs`g'')	// predicted events
	qui sum `pnums`g'' if adaa == 1 & ga==`g'	// pred events post policy
	return scalar eev`g' = r(sum)
	disp as text "Expected events: " as result r(sum)
	qui replace `pnums' = `pnums`g'' if gar ==`g'
	
	* observed number of events
	qui sum deaths if adaa == 1 & gar ==`g' // obs events post policy
	return scalar oev`g' = r(sum)
	disp as text "Observed events: " as result r(sum)
	
	* observed minus expected
	gen `omenums`g'' = deaths - `pnums`g'' // obs - exp events post policy
	qui sum `omenums`g'' if adaa == 1 & gar ==`g'
	return scalar ome`g' = r(sum)
	disp as text "Obs minus expected: " as result r(sum)
	qui replace `omenums' = `omenums`g'' if gar ==`g'
	
	matrix oer`g' = [oev`g', eev`g', ome`g']
	}
	
	* totals across all age and gender groups
	qui sum deaths if adaa == 1
	return scalar toev = r(sum)

	qui sum `pnums' if adaa == 1
	return scalar teev = r(sum)

	qui sum `omenums' if adaa == 1
	return scalar tome = r(sum)
  restore
end



// #3
// now use the program to simulate this process multiple times

* run once to make sure all is well
bootall

* now simulate distribution of outcomes with 500 replications
set trace on 

preserve
simulate oev1=r(oev1) eev1=r(eev1) ome1=r(ome1) ///
		 oev2=r(oev2) eev2=r(eev2) ome2=r(ome2) ///
		 oev3=r(oev3) eev3=r(eev3) ome3=r(ome3) ///
		 oev4=r(oev4) eev4=r(eev4) ome4=r(ome4) ///
		 oev5=r(oev5) eev5=r(eev5) ome5=r(ome5) ///
		 oev6=r(oev6) eev6=r(eev6) ome6=r(ome6) ///
		 oev7=r(oev7) eev7=r(eev7) ome7=r(ome7) ///
		 oev8=r(oev8) eev8=r(eev8) ome8=r(ome8) ///
		 oev9=r(oev9) eev9=r(eev9) ome9=r(ome9) ///
		 oev10=r(oev10) eev10=r(eev10) ome10=r(ome10) ///
		 oev11=r(oev11) eev11=r(eev11) ome11=r(ome11) ///
		 oev12=r(oev12) eev12=r(eev12) ome12=r(ome12) ///
		 toev=r(toev) teev=r(teev) tome=r(tome) /// 
		, reps(500) seed(123456): bootall

* bootstrapped standard errors
bstat, stat(oer) n(10)
estat bootstrap, percentile
matrix totci = e(ci_percentile)
forvalues i=1/13 {
	matrix table[`i',4] = totci[1,`i'*3]
	matrix table[`i',5] = totci[2,`i'*3]
}
restore

* export results to excel 
putexcel B3 = matrix(table) using results_V5, replace

drop year_0 year_1 year_2

* Plots 
lowess deaths year, by(gar)




