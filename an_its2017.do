
** an_its2017.do
** Goal: Redo analysis June 2017
** Author: Erika Braithwaite

use "/Users/erikabraithwaite/Dropbox/Nandi/Mass Incarceration/Paper 3/ITS re-work-2017/ITS/ITS-analysis/timeseries.dta"


* Add cubic spline over entire time period to capture national trends
* Add cubic spline over time period after policy 

mkspline2 sp_year = year, cubic nknots(3)
mkspline2 sp_adaa = adaasince, cubic nknots(3)


* Kaufman's code * 
* http://www.citymatch.org/sites/default/files/documents/MCHEPITraining/Hirai_Risk%20Differences_Risk%20Ratios_5_20.pdf


* Model 1: Level and slope change
*  models the rate ratio
glm bothsexes sp_year* i.adaa_L1 sp_adaa*, family(poisson) link(log) offset(lpop) eform nolog
est store model1 
predict predm1, nooffset

* what's the difference - totally different results
mvrs glm bothsexes year, family(poisson) link(log) offset(lpop) eform all
fracplot 

*glm bothsexes year_0 year_1 year_2 adaa_L1 sp_adaa*, family(poisson) link(log) offset(lpop) eform
*est store model4

* Model 2: Slope change

glm bothsexes sp_year* sp_adaa*, family(poisson) link(log) offset(lpop) eform
est store model2

* Model 3: Level change

glm bothsexes sp_year* adaa_L1 , family(poisson) link(log) offset(lpop) eform
est store model3

* Table of estimates
estimates table *, drop(_cons) eform 

* if time were linear 

margins i.adaa_L1, at(sp_year1=(1978(1)2000))


* Getting the predicted counts *
* because it's not linear 

glm bothsexes sp_year* i.adaa_L1 adaasince, family(poisson) link(log) offset(lpop) eform nolog

list sp_year1 sp_year2 if adaa_L1 == 0 & adaasince == 0
*

* year of implementation
list sp_year1 sp_year2 if adaa_L1 == 1 & adaasince == 1

margins, at(adaa_L1 == 1 adaasince ==1  sp_year1 == 1987 sp_year2 == .64 )





* Model 2: Level change 
*glm bothsexes sp_year* adaa_L1 , family(poisson) link(log) offset(lpop) eform

* Model 3: Slope change 



* compute marginal effects 




* raw data 
twoway (scatter bothsexes year), name(a)
adjustrcspline, name(b, replace) 
mfxrcspline, name(c, replace) link(identity)

graph combine a b c 

*for each value of the exposure 

mfxrcspline, name(c)
