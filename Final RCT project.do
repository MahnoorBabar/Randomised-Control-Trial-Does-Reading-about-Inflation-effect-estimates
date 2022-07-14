use "final.dta"

reg inf_ann_22 inf_know_21
*not significant

*countries:
gen namer = 0
replace namer = 1 if countries == "Canada" 
replace namer = 1 if countries == "USA, Canada" 

*surveyed "0" = pre
*surveyed "1" = post
gen post = 0
replace post = 1 if surveyed==1 
*post "0" = pre
*post "1" = post

save "final.dta", replace

//OLS
reg inf_ann_22 inf_know_21 age gender namer fin_econ
outreg2 using "OLS.tex", label bdec(2) se sdec(2) ctitle("Simple OLS") 



//Balance test
use "randomized.dta"

ssc install orth_out

orth_out inf_ann_21 inf_ann_22 age yearsofeducation gender state_of_world inf_think monthly_spending inf_interest inf_know_21, by(treatment) pcompare
*manual table 

use "randomized.dta", clear
orth_out inf_ann_21 inf_ann_22 age yearsofeducation gender state_of_world inf_think monthly_spending inf_interest inf_know_21 inf_read, by(treatment) pcompare


//Table1: DID: the actual thing 

gen txpost = post*treatment

save "final.dta", replace

*Main regression + with controls
reg inf_ann_22 treatment post txpost, robust 
outreg2 using "DID.tex", label bdec(2) se sdec(2) ctitle("DID") 

reg inf_ann_22 treatment post txpost inf_know_21 fin_econ, robust 
outreg2 using "DID.tex", label bdec(2) se sdec(2) ctitle("DID with controls") append

xi: reg inf_ann_22 i.mcgillstudentid post txpost, robust 
outreg2 using "iFE.tex", label bdec(2) se sdec(2) ctitle("DID iFE") 


****** i think inf_know_21 makes sense: may change with treatment
xi: reg inf_ann_22 i.mcgillstudentid post txpost inf_know_21 fin_econ, robust 
outreg2 using "iFE.tex", label bdec(2) se sdec(2) ctitle("DID iFE with controls") append 
****** using emails_read or articles_read is highly multicollinear so gives no results.

sum inf_ann_22 if treatment==0&post==0
*mean=3.41 st.dev=1.68

sum inf_ann_21 if treatment==0&post==0
*mean=3.42 st.dev=1.72



xi: reg inf_ann_21 i.mcgillstudentid post txpost inf_know_21 fin_econ, robust 
outreg2 using "iFE-.tex", label bdec(2) se sdec(2) ctitle("DID'21 iFE with controls") append 



replace emails_read = 0 if emails_read==. 
replace articles_read = 0 if articles_read==. 
xi: reg inf_ann_22 i.mcgillstudentid post txpost emails_read, robust 


xi: reg inf_ann_21 i.mcgillstudentid post txpost, robust 
outreg2 using "infcontrols.tex", label bdec(2) se sdec(2) ctitle("DID'21") 

xi: reg inf_ann_22 i.mcgillstudentid post txpost, robust 
outreg2 using "infcontrols.tex", label bdec(2) se sdec(2) ctitle("DID'22") append

xi: reg inf_ann_21 i.mcgillstudentid post txpost inf_read inf_think state_of_world monthly_spending, robust 
outreg2 using "infcontrols.tex", label bdec(2) se sdec(2) ctitle("DID'21 with controls") append

xi: reg inf_ann_22 i.mcgillstudentid post txpost inf_read inf_think state_of_world monthly_spending, robust 
outreg2 using "infcontrols.tex", label bdec(2) se sdec(2) ctitle("DID'22 with controls") append


*t-observed
ttest inf_ann_22, by(treatment)
*t =  -1.3227

gen id=_n
gen numbernew=runiform()
sort numbernew
gen new_treatment=treatment[id] 

forvalues i=1(1)1000 {
    gen id`i'=_n
	gen numbernew`i'=runiform()
    sort numbernew`i' 
    gen new_treatment`i'=treatment[id] 
	ttest inf_ann_22, by(new_treatment`i')
	di r(t)
}

*t observed -1.32 is smaller than the t stats which range between -0.7 and 3.1. Since all these are greater than -1.32, this means Fischer p-value is 0. 

ttest inf_ann_21, by(treatment)
*t = -0.6933

drop id* numbernew* new_treatment* n_number* 

gen id=_n
gen numbernew=runiform()
sort numbernew
gen new_treatment=treatment[id] 

forvalues i=1(1)1000 {
    gen id`i'=_n
	set seed 3
	gen numbernew`i'=runiform()
    sort numbernew`i' 
    gen new_treatment`i'=treatment[id] 
	ttest inf_ann_21, by(new_treatment`i')
}

******Actual
*t = -1.32
ttest inf_ann_22, by(treatment)
loc og r(t)

set seed 3 

gen id=_n
gen n_number=runiform()
sort n_number
gen new_treatment=treatment[id]


forvalues i=1(1)1000 {
	gen id`i' = _n
	gen n_number`i'  = runiform()
	sort n_number`i' 
	gen new_treatment`i'  = treatment[id]
	qui ttest inf_ann_22, by(new_treatment`i' )
	di r(t)
	mat a = r(t)
	if `i' ==1 mat b = a
	else mat b = (b \ a)
	drop n_number`i' new_treatment`i' id`i'
}
mat list b

svmat b

hist b

count if abs(b1) > abs(`og')
*Fisher statistic = 0 


*t = -0.6933
ttest inf_ann_21, by(treatment)
loc og r(t)

set seed 3 

gen id=_n
gen n_number=runiform()
sort n_number
gen new_treatment=treatment[id]


forvalues i=1(1)1000 {
	gen id`i' = _n
	gen n_number`i'  = runiform()
	sort n_number`i' 
	gen new_treatment`i'  = treatment[id]
	qui ttest inf_ann_21, by(new_treatment`i' )
	di r(t)
	mat a = r(t)
	if `i' ==1 mat b = a
	else mat b = (b \ a)
	drop n_number`i' new_treatment`i' id`i'
}
mat list b

svmat b

hist b

count if b1 > `og'

count if abs(b1) > abs(`og')

****

ttest inf_ann_22, by(treatment)
loc og r(t)

set seed 3 

gen id=_n
gen n_number=runiform()
sort n_number
gen new_treatment=treatment[id]


forvalues i=1(1)1000 {
	gen id`i' = _n
	gen n_number`i'  = runiform()
	sort n_number`i' 
	gen new_treatment`i'  = treatment[id]
	qui ttest inf_ann_22, by(new_treatment`i' )
	di r(t)
	mat a = r(t)
	if `i' ==1 mat b = a
	else mat b = (b \ a)
	drop n_number`i' new_treatment`i' id`i'
}
mat list b

svmat b

hist b

count if b1 > `og'

count if abs(b1) > abs(`og')




*t observed -0.6933 is smaller than the t stats which range between -1.73 and 1.71. 
*Since all these are greater than -1.32, this means Fischer p-value is 0. 

drop id* new_treatment* n_number* numbernew*


******???
reg inf_ann_22 post i.mcgillstudentid txpost inf_concern, r

mat m = e(b)
mat j = e(V)
local a0 = m[1,31]
local b0 = j[31,31]
local c0 = a0'/sqrt(b0')
global c0 = `c0'
mat drop m j 

set seed 3

egen id2= group(mcgillstudentid)

forvalues i = 1(1)1000{
disp(`i')
drop number 
gen number = uniform()
sort number
gen new_treatment=treatment[id2]
gen did2=post*new_treatment
reg inf_ann_22 post i.mcgillstudentid txpost inf_concern, r
mat m = e(b)
mat list m
mat j = e(V)
mat list j
local a`i' = m[1,31]
disp ai''
local b`i' = j[31,31]
disp bi''
local c`i' = ai''/sqrt(bi'')
disp ci''
global c`i' = ci''
disp ${c`i'}
reg inf_ann_21 post i.mcgillstudentid txpost inf_concern, r
mat m = e(b)
mat list m
mat j = e(V)
mat list j
local a`i' = m[1,31]
disp ai''
local b`i' = j[31,31]
disp bi''
local d`i' = ai''/sqrt(bi'')
disp di''
global d`i' = di''
disp ${d`i'}
mat drop m j 
drop new_treatment
drop did2
}


preserve

clear all

set obs 1001

gen n = _n
gen t_value_21 = .
gen t_value_22 = .

forvalues i = 0(1)1000 {
	disp ${c`i'}
	replace t_value_21 = ${c`i'} if n == 1 + `i'
	disp ${d`i'}
	replace t_value_22 = ${d`i'} if n == 1 + `i'
}

sort t_value_21
gen rank = _n
tab rank if n == 1

sort t_value_22
drop rank
gen rank = _n
tab rank if n == 1
restore



*shahzan
replace mcgillstudentid = 260777777 in 57

ssc install orth_out

orth_out inf_ann_21 age yearsofeducation gender state_of_world inf_think monthly_spending inf_interest, by(treatment) compare

orthout using "balance.tex", label bdec(2) se sdec(2) ctitle("Balance Test") 

sort post

*peter
replace treatment = 1 in 1
replace treatment = 1 in 2
replace treatment = 1 in 3
replace treatment = 1 in 4
replace treatment = 0 in 9
replace inf_50_21 = "2, 2.5" in 10
replace inf_95_21 = "2, 2.5" in 10
replace inf_50_21_high = 2.5 in 10
replace inf_95_21_high = 2.5 in 10
replace inf_50_22 = "1.50, 2" in 10
replace inf_95_22 = "1.50, 2" in 10
replace inf_50_22_high = 2 in 10
replace inf_95_22_high = 2 in 10
replace treatment = 1 in 10
replace treatment = 1 in 26
replace treatment = 1 in 28
replace treatment = 1 in 18
replace treatment = 1 in 15
replace treatment = 1 in 7

save "final.dta", replace

sort treatment mcgillstudentid


//Multiple hypothesis testing
*The Bonferroni is -1.32/2 = -0.66 for primary outcomes (2022)
*-0.6933/2= -0.3466 (for 2021)

//Create attrition
*2021
gsort -post treatment mcgillstudentid
replace inf_ann_21=. in 1/2
*post=1, treatment=0 on top
*two observations dropped from control endline

gsort -post -treatment mcgillstudentid
replace inf_ann_21=. in 1
*one observation dropped from treatment endline

*2022
gsort -post treatment mcgillstudentid
replace inf_ann_22=. in 1/2
*post=1, treatment=0 on top
*two observations dropped from control endline

gsort -post -treatment mcgillstudentid
replace inf_ann_22=. in 1

save "attrition.dta", replace

**worst-case scenario/Manski bounds
//test for differentiated attrition


use "attrition.dta", clear
preserve

sum inf_ann_21 if treatment == 1 & post == 1
replace inf_ann_21 = `r(max)' if inf_ann_21 == . & treatment == 1 & post == 1
sum inf_ann_21 if treatment == 0 & time == 1
replace inf_ann_21 = `r(min)' if inf_ann_21 == . & treatment == 0 & post == 1


reg inf_ann_21 post i.mcgillstudentid txpost, r 
outreg2 using "Manski.tex", label bdec(2) se sdec(2) ctitle("DID'21 Manski bounds") 

restore
preserve

sum inf_ann_22

sum inf_ann_22 if treatment == 1 & post == 1
replace inf_ann_22 = `r(max)' if inf_ann_22 == . & treatment == 1 & post == 1
sum inf_ann_22 if treatment == 0 & time == 1
replace inf_ann_22 = `r(min)' if inf_ann_22 == . & treatment == 0 & post == 1

reg inf_ann_22 post i.mcgillstudentid txpost, r 
outreg2 using "Manski.tex", label bdec(2) se sdec(2) ctitle("DID'22 Manski bounds") append



*****txpost (both coefficients) = lower and upper bounds

**Lee bounds
*2021
restore
xtset mcgillstudentid post

gen inf_ann_21_diff = .
replace inf_ann_21_diff = inf_ann_21 - L1.inf_ann_21
leebounds inf_ann_21_diff treatment

*2022
restore
xtset mcgillstudentid post

gen inf_ann_22_diff = .
replace inf_ann_22_diff = inf_ann_22 - L1.inf_ann_22
leebounds inf_ann_22_diff treatment

/////////////////
******Graham 14/3
use attrition.dta, clear

xtset mcgillstudentid post 
gen inf_ann_21_diff = .
replace inf_ann_21_diff = inf_ann_21 - L1.inf_ann_21
leebounds inf_ann_21_diff treatment, tight()
*trying this per ceci, still can't add age in tight()
qui reg inf_ann_21 post txpost i.mcgillstudentid, robust 
di "Point estimate is "_b[txpost]

*Manski upper, treatment gets max, control gets min
preserve
	qui sum inf_ann_21 if post==1 & treatment==1
		replace inf_ann_21 = r(max) if inf_ann_21==. & treatment==1
	qui sum inf_ann_21 if post==1 & treatment==0
		replace inf_ann_21 = r(min) if inf_ann_21==. & treatment==0
	qui reg inf_ann_21 post txpost i.mcgillstudentid, robust 
	di "Upper Manski Bound is "_b[txpost]
restore
*Manski lower, treatment gets mind, control gets max
preserve
	qui sum inf_ann_21 if post==1 & treatment==1
		replace inf_ann_21 = r(min) if inf_ann_21==. & treatment==1
	qui sum inf_ann_21 if post==1 & treatment==0
		replace inf_ann_21 = r(max) if inf_ann_21==. & treatment==0
	qui reg inf_ann_21 post txpost i.mcgillstudentid, robust 
	di "Lower Manski Bound is "_b[txpost]
restore


****
gen inf_ann_22_diff = .
replace inf_ann_22_diff = inf_ann_22 - L1.inf_ann_22
leebounds inf_ann_22_diff treatment, tight()
*trying this per ceci, still can't add age in tight()
qui reg inf_ann_22 post txpost i.mcgillstudentid, robust 
di "Point estimate is "_b[txpost]

*Manski upper, treatment gets max, control gets min
preserve
	qui sum inf_ann_22 if post==1 & treatment==1
		replace inf_ann_22 = r(max) if inf_ann_22==. & treatment==1
	qui sum inf_ann_22 if post==1 & treatment==0
		replace inf_ann_22 = r(min) if inf_ann_22==. & treatment==0
	qui reg inf_ann_22 post txpost i.mcgillstudentid, robust 
	di "Upper Manski Bound is "_b[txpost]
restore
*Manski lower, treatment gets mind, control gets max
preserve
	qui sum inf_ann_22 if post==1 & treatment==1
		replace inf_ann_22 = r(min) if inf_ann_22==. & treatment==1
	qui sum inf_ann_22 if post==1 & treatment==0
		replace inf_ann_22 = r(max) if inf_ann_22==. & treatment==0
	qui reg inf_ann_22 post txpost i.mcgillstudentid, robust 
	di "Lower Manski Bound is "_b[txpost]
restore

//Table2
use "final.dta", clear

**2a
xi: reg inf_ann_21 i.mcgillstudentid post txpost inf_read state_of_world inf_think monthly_spending if gender==1, robust  
outreg2 using "by gender.tex", label bdec(2) se sdec(2) ctitle("DID'21 for males") 

xi: reg inf_ann_22 i.mcgillstudentid post txpost inf_read state_of_world inf_think monthly_spending if gender==1, robust  
outreg2 using "by gender.tex", label bdec(2) se sdec(2) ctitle("DID'22 for males") append

xi: reg inf_ann_21 i.mcgillstudentid post txpost inf_read state_of_world inf_think monthly_spending if gender==2, robust  
outreg2 using "by gender.tex", label bdec(2) se sdec(2) ctitle("DID'21 for females") append

xi: reg inf_ann_22 i.mcgillstudentid post txpost inf_read state_of_world inf_think monthly_spending if gender==2, robust  
outreg2 using "by gender.tex", label bdec(2) se sdec(2) ctitle("DID'22 for females") append

ttest inf_ann_21, by(gender)
ttest inf_ann_22, by(gender)

**2b
gen age_s = 0
replace age_s==1 if age>24


xi: reg inf_ann_21 i.mcgillstudentid post txpost inf_read state_of_world inf_think monthly_spending age_s, robust  
outreg2 using "by age_s.tex", label bdec(2) se sdec(2) ctitle("DID'21 for age") 

xi: reg inf_ann_22 i.mcgillstudentid post txpost inf_read state_of_world inf_think monthly_spending age_s, robust  
outreg2 using "by age_s.tex", label bdec(2) se sdec(2) ctitle("DID'22 for age") append



xi: reg inf_ann_22 i.mcgillstudentid post txpost inf_read state_of_world inf_think monthly_spending if age<=24, robust  
outreg2 using "by age.tex", label bdec(2) se sdec(2) ctitle("DID'22 for age<=24") append

xi: reg inf_ann_21 i.mcgillstudentid post txpost inf_read state_of_world inf_think monthly_spending if age>24, robust  
outreg2 using "by age.tex", label bdec(2) se sdec(2) ctitle("DID'21 for age>24") append

xi: reg inf_ann_22 i.mcgillstudentid post txpost inf_read state_of_world inf_think monthly_spending if age>24, robust  
outreg2 using "by gender.tex", label bdec(2) se sdec(2) ctitle("DID'22 for age<=24") append

ttest inf_ann_21, by(age_s)
ttest inf_ann_22, by(age_s)


//Table3
*maybe only those read who had an interest
*x(inf_read=articles_read)
*x(inf_read=emails_read)
*x(inf_read=articles_read)
*x(inf_read=articles_read)
*x(inf_concern=articles_read) 
*x(inf_think=articles_read) 
*x(inf_concern=inf_read) 


**
xi: reg inf_concern i.mcgillstudentid post txpost, r 
outreg2 using "secondary.tex", label bdec(2) se sdec(2) ctitle("Secondary outcomes") 

xi: reg inf_secure i.mcgillstudentid post txpost, r 
outreg2 using "secondary.tex", label bdec(2) se sdec(2) ctitle("Feel secure") append

xi: reg inf_affect i.mcgillstudentid post txpost, r 
outreg2 using "secondary.tex", label bdec(2) se sdec(2) ctitle("Affected") append

xi: reg state_of_world i.mcgillstudentid post txpost, r 
outreg2 using "secondary.tex", label bdec(2) se sdec(2) ctitle("Worry") append


**IV for Appendix
xi: ivreg inf_ann_21 i.mcgillstudentid post txpost (inf_concern=inf_read), first
*very bad p>0.99
xi: ivreg 2slsl inf_ann_21 i.mcgillstudentid post txpost (state_of_world=inf_read), first
*marginally better
outreg2 using "IV.tex", label bdec(2) se sdec(2) ctitle("IV: Worrying about the state of the world") 
estat firststage
estat endog


//Table 4
*Robustness checks

*Drop Peter, Rounan

*Drop housing market controls

drop if mcgillstudentid == 260782874 
drop if mcgillstudentid == 261034560
drop if mcgillstudentid == 260777777
drop if mcgillstudentid == 26077777


replace articles_read=0 if articles_read==.
replace emails_read=0 if emails_read==.

xi: reg inf_ann_21 i.mcgillstudentid post txpost inf_read state_of_world inf_think monthly_spending emails_read articles_read, robust  

outreg2 using "by gender.tex", label bdec(2) se sdec(2) ctitle("DID'21 for males") 


drop mcgillstudentid ==260782874 261034560 260777777


////
xi: reg inf_ann_21 i.mcgillstudentid post txpost inf_read emails_read articles_read, robust
outreg2 using "table4.tex", label bdec(2) se sdec(2) ctitle("Reading about Inflation") 

xi: reg inf_ann_22 i.mcgillstudentid post txpost inf_read emails_read articles_read, robust
outreg2 using "table4.tex", label bdec(2) se sdec(2) ctitle("Reading about Inflation") append
