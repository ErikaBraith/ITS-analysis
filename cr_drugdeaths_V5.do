* do-file: cr_drugsdeaths_V5
* date: Aug. 30, 2017
* Author: Erika Braithwaite
* Create drug deaths files - with age stratified rates (3 categories)



import delimited "Compressed Mortality, 1968-1978-age-race-sex.txt", clear 
save "deaths-1968-1978-arg.dta", replace 


 ** 1978-1999
import delimited "Compressed Mortality, 1979-1998-age-race-sex.txt", clear 
append using "deaths-1968-1978-arg.dta"
-
drop notes
drop if year == . 


drop gendercode racecode yearcode agegroupcode 

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


* collapse into smaller groups
* create race-gender groups * 

* create new age groups
encode agegroup, gen(agecat) 
drop agegroup
rename agecat agegroup

recode agegroup (1 2 = 0) (3 4 = 1) (5 6 7 = 2), gen(agecat3)
label define agecat3 0 "15-24" 1 "25-44" 2 "45+"
label values agecat3 agecat3


egen gar=group(gender racebw agecat3), label
tab gar

gen lpop = log(population)
label var lpop "log of population

* generate policy variable

gen adaa = 0 if year < 1986
replace adaa = 1 if year >= 1986
label var adaa "Anti-Drug Abuse Act of 1986"
label define adaa 0 "pre-policy" 1 "post-policy"
label values adaa adaa 


* save and export

save "drugdeaths-age-race-sex.dta", replace

export delimited "drugdeaths-age-race-sex.csv", replace


