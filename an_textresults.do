
 ** an_textresults.do
 ** Goal:  Interrupted Time Series data set 
 ** Author: Erika Braithwaite
 ** Date: 16.7.2016
 ** Results and graphs in the body of the text
 
 
* Results in text

sum bothsexes
egen float tot = total(bothsexes) if year < 1999
sum  both_drug if year < 1986
sum  both_drug if year > 1986 & year < 1999

* Figure 1. Crude rates with smoothed line

twoway scatter both_drug year if year < 1999 || lfit both_drug year if year < 1999, sort clstyle(solid) ///
	 xline(1986, lwidth(vthin)) ytitle("Drug related deaths, per 100,000") graphregion(color(white)) bgcolor(white) ///
	 title("Rate of drug related deaths in the United States") note("source: National Center for Health Statistics") ///
	 text(6 1986.5 "Anti-Drug Abuse Act", place(e)) 

* no extra changes were made to graph iteratively 
graph save Graph "/Users/erikabraithwaite/Dropbox/Nandi/Mass Incarceration/Paper 3/Graphs/rate.gph", replace



* Linear time trend - no change in level 


glm bothsexes time adaa   if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 5) eform 
lincom time + adaa, eform 

* linear rate
predict linear, nooffset 
replace linear = linear * 100000




** Main results ** 
* To get the cubic splines


mvrs glm bothsexes time if adaa==0 & year < 2000, family(poisson) link(log) offset(lpop) scale(x2) eform all
  
* the model 
glm bothsexes time_0 time_1 adaa_L1 adaasince if year < 2000, family(poisson) link(log) offset(lpop) eform 
lincom time_0  + time_1 + adaasince , eform 


ac bothsexes 
graph save Graph "/Users/erikabraithwaite/Dropbox/Nandi/Mass Incarceration/Paper 3/Graphs/autocorrelation.gph"


* predicted number of deaths
predict predglm,  mu 
label var predglm "predicted count cublic spline"

* predicted rate
predict predglm_r, nooffset 
replace predglm_r = predglm_r*100000
label var predglm_r "predicted rate cubic spline"

 
* null model * 
*glm bothsexes time if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 5) eform 
*predict null, nooffset
*replace null = null * 10000
*label var null "no intervention"


*Generate the counterfactual rate by removing the effect of the intervention (_b[smokban]) for the post-intervention period

* not sure about this bit - drop for the time being
/*
drop counter_rate
gen counter_rate = predglm_r/(exp(_b[adaasince]) + exp(_b[time_0]) +  exp(_b[time_1])) if adaa==1 
label var counter "counterfactual rate"

drop counter_rate
gen counter_rate = predglm_r/(exp(_b[adaa_L1]) + exp(_b[adaasince])) if adaa == 1
label var counter "counterfactual rate"
*/ 


* Table 1. Marginal effects 
* based somewhat sam and jay's code from that email


* Predicted differences (I get the same numbers - but with no confidence intervals* 
*glm bothsexes time_0 time_1 i.adaa_L1 adaasince if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 5) eform 

* predicted margins at a given time point for both treatment variables. 
* [youÕll need to see what the values of your spline variables are at that 
* time point, which should be simple) In the first year after the intervention
* when time_0 and time_1 have values of ÒxxxÓ I think something more like: 

list time adaasince adaa time_0 time_1

margins, at(time_0 = -0.73 time_1=-0.96  adaa_L1=0 adaasince=0) at(time_0 = -0.73 time_1=-0.96  adaa_L1=1 adaasince=1)


margins adaa_L1, at(adaasince=(1(1)13)) post coeflegend
	
* this command gets the confidence intervals 

doesn't make sense - this is just the value at 1986
* forvalues i = 1(1)13 {
 * 	lincom _b[`i'._at#1.adaa_L1] - _b[`i'._at#0.adaa_L1]
           } 
*
		   
* Rate difference ?? I don't think so
*glm bothsexes time_1 i.adaa_L1 adaasince if year < 2000, family(poisson) link(log) offset(lpop) eform vce(hac nwest 5)

*margins, dydx(*)


** Sensitivity analysis - adjusting for crack epidemic ** 
glm bothsexes time_0 time_1 i.adaa_L1 adaasince crack_index if year < 2000, family(poisson) link(log) offset(lpop) eform vce(hac nwest 5)
lincom time_0 + time_1 + adaasince, eform 

* predicted count (after adjustment) -- we lose two years of observation (1978-1979)
predict predcount_adj, mu
label var predcount_adj "predicted count, crack adj, cubic spline"

* predicted rate
predict predglm_r_adj, nooffset 
replace predglm_r_adj = predglm_r_adj*100000
label var predglm_r_adj "predicted rate GLM - after crack adjustment"





** sam's additional code --> couldn't figure it out - to revisit later, I'm pretty sure it gives similar results
*list time_0 time_1 if adaa_L1==1 &  adaasince==1
*list time_0 time_1 if adaa_L1==0 & adaasince==0


* year of implementation
*margins, at(time_0=-.63012604 time_1=.89011488  adaa_L1=0 adaasince=0) at(time_0=-.63012604 time_1=.89011488  adaa_L1=1 adaasince=1)

* 
*margins, at(time_0=-.63012604 time_1=.89011488  adaa_L1=0 adaasince=1) at(time_0=-.63012604 time_1=.89011488  adaa_L1=1 adaasince=1)


* I can't seem to bootstrap
*set seed 1
*bootstrap _b, saving(margins, replace) reps(100) bca: margins r.adaa_L1, at(adaasince=(1(1)24)) coeflegend


*estat bootstrap, all
	

* This is the difference in the counts at the moment the intervention was implemented
*list time_0 time_0 if adaa_L1 == 1 & adaasince =0

*margins, at(time_0=-.73514704 time_1=.96327948  adaa_L1=0 adaasince=0) at(time_0=-.73514704 time_1=.96327948  adaa_L1=1 adaasince=1) post
*lincom 2._at - 1._at





** Figure 2.  Graph the linear and cubic model together 

* this is the count (observed [bothsexes], predicted [predglm], counterfactual)
twoway (scatter bothsexes year if year < 1999)  (line predglm year if year < 1999), ///
	title("Predicted versus observed number of drug deaths by year") ytitle("Number of drug deaths") ///
	graphregion(color(white)) bgcolor(white) xline(1986, lwidth(vthin)) ///
	xline(1986, lwidth(vthin)) text(15000 1986.5 "Anti-Drug Abuse Act", place(e)) ///
	ylabel(, labsize(small)) xlabel(1978(2)1999) legend(lab(1 "observed") lab(2 "predicted"))
	
	
legend(pos(3) col(1)
lab(1 "Males") lab(2 "Females") stack)

* plotting the rate	(observed [rate1], predicted [predglm_r], counterfactual [predcount_adj])

twoway (scatter rate1 year if year < 2000)  (line predglm_r year if year < 2000) ///
		(line linear year if year < 2000) (line predglm_r_adj year if year < 2000), ///
		title("Predicted versus observed rate of drug deaths by year") ytitle("Rate of drug deaths, per 100,000") ///
		graphregion(color(white)) bgcolor(white) xline(1986, lwidth(vthin)) ///
		xline(1986, lwidth(vthin)) text(15000 1986.5 "Anti-Drug Abuse Act", place(e)) ///
		ylabel(, labsize(small))
	

* generate fake data that will be the example line 


	
graph save Graph "/Users/erikabraithwaite/Dropbox/Nandi/Mass Incarceration/Paper 3/Graphs/predictedvobserved.gph"


* WAY BATTER GRAPH - COMMAND IS CALLING ON LINCOM TO GRAPH THE SPLINE RESULTS * 
* reference is 1986 - which 
* rate ratio
drop pa or lb ub 

xblc time_0 time_1 , covname(year) at(1978(1)1999) reference(1978) eform scatter	/// 
	ytitle("Rate ratio") xtitle("year") ///
	xline(1986, lwidth(vthin)) text(2.5 1986.5 "Anti-Drug Abuse Act", place(e)) ///
	title("Annual comparisons of rates versus 1978") ///
	xlabel(1978(2)1999) generate(pa or lb ub)

label var pa "year"
label var or "incidence rate ration"
label var lb "lower bound"
label var ub "upper bound"

drop pa or lb ub

graph save Graph "/Users/erikabraithwaite/Dropbox/Nandi/Mass Incarceration/Paper 3/stata/new/rateratios.gph", replace

* reference is 1988 -- counts

* I don't know if this is telling me anything interesting - since it has nothing to do with the intervention

xblc time_0 time_1 , covname(year) at(1978(1)1999) reference(1978) eform scatter	/// 
	ytitle("Drug related deaths, per 100,000") xtitle("year") ///
	xline(1986, lwidth(vthin)) text(1.8 1986.5 "Anti-Drug Abuse Act", place(e)) ///
	title("Drug related deaths in the United States, 1978-1999") ///
	xlabel(1978(2)1999) 


* Post-implementation trend B1 + B3
glm bothsexes time_0 time_1 adaa_L1 adaasince if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 1) eform 
lincom	_b[time_0] + _b[time_1]	+	_b[adaasince],	eform

** lagged effects 
glm bothsexes time_0 time_1 adaa_L2 adaasince2 if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 1) eform 
lincom	_b[time_0] + _b[time_1]	+	_b[adaasince2],	eform

glm bothsexes time_0 time_1 adaa_L3 adaasince3 if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 1) eform 
lincom	_b[time_0] + _b[time_1]	+	_b[adaasince3],	eform

glm bothsexes time_0 time_1 adaa_L4 adaasince4 if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 1) eform 
lincom	_b[time_0] + _b[time_1]	+	_b[adaasince4],	eform

glm bothsexes time_0 time_1 adaa_L5 adaasince5 if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 1) eform 
lincom	_b[time_0] + _b[time_1]	+	_b[adaasince5],	eform



** absolute change  absolute change after the intervention 
** (difference in the treatment scenario vs. the counterfactual), 
** which is what I would call the effect of the intervention
* Results from linear time trend 

glm bothsexes time adaa_L1 adaasince if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 5) eform 

lincom _b[adaa_L1] + _b[adaasince], eform
lincom _b[adaa_L1] + _b[adaasince]*5, eform
lincom _b[adaa_L1] + _b[adaasince]*10, eform
lincom _b[adaa_L1] + _b[adaasince]*15, eform

/* Didn't work
program boo, rclass
	glm bothsexes time adaa_L1 adaasince if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 1) eform 
	lincom _b[time] 	+	_b[adaasince],	eform
	return scalar bootse = r(se)
end
bootstrap diffse=r(bootse), reps(100) : boo
program drop 

program boo, rclass
	glm bothsexes time adaa_L1 adaasince if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 1) eform 
	lincom _b[time] 	+	_b[adaasince],	eform
	return scalar bootse = r(se)
end
bootstrap diffse=r(bootse), reps(100) : boo
*/


** lagged effects 
glm bothsexes time adaa_L2 adaasince2 if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 1) eform 
lincom	_b[time] 	+	_b[adaasince2],	eform

glm bothsexes time adaa_L3 adaasince3 if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 1) eform 
lincom	_b[time] + 	_b[adaasince3],	eform

glm bothsexes time adaa_L4 adaasince4 if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 1) eform 
lincom	_b[time]	+	_b[adaasince4],	eform

glm bothsexes time adaa_L5 adaasince5 if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 1) eform 
lincom	_b[time] + 	_b[adaasince5],	eform


* after adjusting for the crack epidemic 
*glm bothsexes time_0 time_1 adaa_L1 adaasince crack_index if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 5) eform 
*lincom	_b[time_0] + _b[time_1]	+	_b[adaasince],	eform



** Make this graph nicer ** 
twoway line crack_index year if year < 2000 || line ageadjustedrate year if year < 2000


* Linear model (no interaction term * 
glm bothsexes time adaa  if year < 2000, family(poisson) link(log) offset(lpop) vce(hac nwest 5) eform 
 

predict linear, nooffset 
replace linear = linear*100000

twoway scatter rate1 year || line linear year, sort ///
	graphregion(color(white)) bgcolor(white) xline(1986, lwidth(vthin)) ///
	xline(1986, lwidth(vthin)) text(5.5 1986.5 "Anti-Drug Abuse Act", place(e)) ///
	ylabel(, labsize(small)) title("Interrupted time series with linear time trend") ///
	xtitle("year") ytitle("Drug deaths, per 100,000") xlabel(1978(2)1999) 

graph save Graph "/Users/erikabraithwaite/Dropbox/Nandi/Mass Incarceration/Paper 3/stata/new/ITSlinear.gph"



***** GRAPHS FOR PRESENTATION FOR DEFENCE **** 

ssc install blindschemes, replace all

set scheme plotplainblind

** ITS graph with data plotted ** 

twoway scatter rate1 year  || function y = x - 1985, range(1978 1986) ///
	|| function y= x - 1975, range(1986 2000) graphregion(color(white)) bgcolor(white) xline(1986, lwidth(vthin)) ///
	xline(1986, lwidth(vthin)) text(15.5 1986.5 "Anti-Drug Abuse Act", place(e)) ///
	ylabel(, labsize(small)) title("Interrupted time series with linear time trend") ///
	xtitle("year") ytitle("Drug deaths, per 100,000") xlabel(1978(2)2000) legend(off) 
	
 graph save Graph "/Users/erikabraithwaite/Dropbox/Nandi/Mass Incarceration/Thesis/figure
> s/ITSwithresults.gph", replace
	
	
* just data and counterfactual line 
twoway scatter rate1 year  || line linear year, sort graphregion(color(white)) bgcolor(white) xline(1986, lwidth(vthin)) ///
	xline(1986, lwidth(vthin)) text(5.5 1986.5 "Anti-Drug Abuse Act", place(e)) ///
	ylabel(, labsize(small)) title("Interrupted time series with linear time trend") ///
	xtitle("year") ytitle("Drug deaths, per 100,000") xlabel(1978(2)2000) legend(off)
	

	
** Blank ITS to demonstrate model ** 
	
	
twoway function y = x - 1978, range(1978 2000) ///
	|| function y = x*0.8 - 1970, range(1986 2000) graphregion(color(white)) bgcolor(white) xline(1986, lwidth(vthin)) ///
	xline(1986, lwidth(vthin)) text(15.5 1986.5 "Anti-Drug Abuse Act", place(e)) ///
	ylabel(, labsize(small)) title("Interrupted time series with linear time trend") ///
	xtitle("year") ytitle("Drug deaths, per 100,000") xlabel(1978(2)2000) legend(off)
	
graph save Graph "/Users/erikabraithwaite/Dropbox/Nandi/Mass Incarceration/Thesis/figures/blankITS.gph", replace
	
twoway function y = x - 1978, range(1978 2000) ///
	|| function y = x - 1970, range(1986 2000) graphregion(color(white)) bgcolor(white) xline(1986, lwidth(vthin)) ///
	xline(1986, lwidth(vthin)) text(15.5 1986.5 "Anti-Drug Abuse Act", place(e)) ///
	ylabel(, labsize(small)) title("Interrupted time series with linear time trend") ///
	xtitle("year") ytitle("Drug deaths, per 100,000") xlabel(1978(2)2000) legend(off)


