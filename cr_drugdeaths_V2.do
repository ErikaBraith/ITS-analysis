* do-file: cr_drugsdeaths_V2
* date: Aug. 24, 2017
* Author: Erika Braithwaite
* Create drug deaths files - with age adjusted rates 
* So no more stratifying by age categories


 
import delimited "Compressed Mortality, 1968-1978-race-gender.txt", clear 
save "deaths-1968-1978-rg.dta", replace 


 ** 1978-1999
import delimited "Compressed Mortality, 1979-1998-race-gender.txt", clear 
append using "deaths-1968-1978-rg.dta"

drop notes
drop if year == . 

* none of the rates (adjusted or crude are unreliable)


drop gendercode racecode yearcode

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

* create age-race-gender groups * 

egen ga=group(gender racebw), label
tab ga

gen lpop = log(population)

* generate policy variable

gen adaa = 0 if year < 1986
replace adaa = 1 if year >= 1986
label var adaa "Anti-Drug Abuse Act of 1986"
label define adaa 0 "pre-policy" 1 "post-policy"
label values adaa adaa 


 
* save and export

save "drugdeaths-race-sex.dta", replace



export delimited "drugdeaths-race-sex.csv", replace

