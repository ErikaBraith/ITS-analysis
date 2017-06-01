
* Do-file: an_timeseries_V3.do
* Analysis for the time series analysis 


use "timeseries.dta"

fracpoly: regress mpg weight foreign
marginscontplot weight (Iweig__1 Iweig__2) foreign, var1(20) ci

mvrs glm bothsexes time if adaa==0 & year < 2000, family(poisson) link(log) offset(lpop) scale(x2) eform all

* fp won't work with glm --> only with poisson 
  
fp :  glm bothsexes time if adaa==0 & year < 2000, family(poisson) link(log) offset(lpop) scale(x2) eform all
fp <time>: poisson bothsexes time if adaa==0 & year < 2000, irr

fp <time>: reg bothsexes time if adaa==0 & year < 2000
