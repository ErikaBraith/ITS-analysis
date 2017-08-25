
* cr_drugdeaths.do
* Creating data for updated ITS
* stratified by age-race-age (restricting to ages 15+) - otherwise 55% of estimates
* restricting to Black vs White
* were unreliable (versus 13%)
 
 
 
import delimited "Compressed Mortality, 1968-1978-age-race-sex.txt", clear 
save "deaths-1968-1978.dta", replace 


 ** 1978-1999
import delimited "Compressed Mortality, 1979-1998-age-race-sex.txt", clear 
append using "deaths-1968-1978.dta"

drop notes

* generate crude that includes the unreliable estimates
destring cruderate, generate(crude) ignore((Unreliable) Not Applicable)
label var crude "crude rate, includes unreliable estimates"

destring cruderate, generate(crude1) force
label var crude1 "crude rates, excludes unreliable estimates"
 
* create indicator for unreliable estimates
gen unrel = strmatch(cruderate, "*rel*")
label var unrel "indidicator for unreliable estimate"


drop racecode yearcode agegroup gendercode
drop if year == . 
order year, first

* race 
encode race, gen(racebw) 
label drop racebw
recode racebw (1=0) (2 = 1)
label define racebw 0 "black" 1 "white"
label values racebw racebw 
label var racebw "0 white 1 black"

drop race

* gender 
encode gender, gen(gendermf) label(gend)
drop gender
rename gendermf gender
recode gender (1 = 0) (2 = 1)
label define gender 0 "female" 1 "male"
label values gender gender
label var gender "0 female 1 male"

* 7-cat age group 
encode agegroupcode, gen(agegr)
drop agegroupcode
label var agegr "7-cat age group"

* create age-race-gender groups * 

egen gar=group(gender agegr racebw), label
tab gar 
tab gar , sum(crude1)
 
* save and export

save "drugdeaths.dta", replace

export delimited "drugdeaths.csv", replace



