// Article title: "EU rules beyond its borders: 
// the policy-specific effects of transgovernmental networks and EU agencies in the European neighbourhood"
// JCMS
//Authors: Karina Shyrokykh and Dovilė Rimkutė

xtset ccode year

twoway (scatter pop year), ytitle(population) xtitle(year)
twoway (scatter logpop year), ytitle(log_population) xtitle(year)
sum log_pop

twoway (scatter gdp_pp year), ytitle(population) xtitle(year)
twoway (scatter log_gdp_pp year), ytitle(log_gdp_pp) xtitle(year)
sum log_gdp_pp
sum gow_eff

twoway (scatter NumberEvents year), ytitle(NumberEvents) xtitle(year)
twoway (scatter d.NumberEvents year), ytitle(NumberEvents) xtitle(year)
twoway (scatter logNumberEvents year), ytitle(NumberEvents) xtitle(year)

*******************************************************************
// summary stat IVs
*******************************************************************
 summarize OtherSector Health Asylum Environment Energy Transport Aviation /// 
 FinancialServices FreeGoodsMovement FoodSafety BorderControl Police LAN JHA INTMARKT INFRA ///
 INDSTUD INDEXP ETT AGR NumberEvents

 *******************************************************************
 //Table 1
 *******************************************************************
sum police_dv foodsafety_dv aviation_dv border_dv environment_dv health_dv ///
Police Health Environment Aviation FoodSafety BorderControl ///
 OtherSector gow_eff polity_2 gdp_pp pop ///
taex_request rule_law pol_stab
 
// autocorrelation test
xtserial foodsafety_dv
xtserial asylum_dv
xtserial aviation_dv
xtserial border_dv 
xtserial police_dv 
xtserial environment_dv
xtserial health_dv
xtserial maritime_dv

// frequency plots
sum foodsafety_dv
sum aviation_dv
sum border_dv 
sum police_dv
sum environment_dv
sum border_dv

// Stationarity
xtunitroot llc foodsafety_dv
xtunitroot llc border_dv 
xtunitroot llc d.border_dv
xtunitroot llc polity 
xtunitroot llc gow_eff
xtunitroot llc aviation_dv

*******************************************************************
//comparing two means (Table 4 and 5)
*******************************************************************
sum Police if police_iv_cepol == 1 | police_adhoc == 1 
sum Police if police_iv_cepol == 0 & police_adhoc == 0  
xtreg Police i.police_any_cooperation i.year, fe 
testparm i.police_any_cooperation
test 1.police_any_cooperation
est sto d_1

sum Asylum if asylum_adhoc == 1 
sum Asylum if asylum_adhoc == 0   
xtreg Asylum i.asylum_adhoc i.year, fe 
testparm 1.asylum_adhoc
est sto d_2

sum BorderControl if border_iv_frontex == 1 | border_adhoc_frontex == 1 
sum BorderControl if border_iv_frontex == 0 & border_adhoc_frontex == 0   
xtreg BorderControl i.border_any_cooperation i.year, fe 
testparm 1.border_any_cooperation
est sto d_3

sum Health if health_iv_ecdc_ema == 1 
sum Health if health_iv_ecdc_ema == 0   
xtreg Health 1.health_iv_ecdc_ema i.year, fe 
testparm i.health_iv_ecdc_ema
est sto d_4

sum Aviation if aviation_iv_easa == 1 | aviation_adhoc_easa == 1 
sum Aviation if aviation_iv_easa == 0 & aviation_adhoc_easa == 0   
xtreg Aviation i.aviation_any_cooperation i.year, fe 
test 1.aviation_any_cooperation
est sto d_5

sum Environment if environment_adhoc == 1 
sum Environment if environment_adhoc == 0 
xtreg Environment i.environment_adhoc i.year, fe 
test 1.environment_adhoc
est sto d_6

sum FoodSafety if foodsafety_adhoc_efsa == 1 
sum FoodSafety if foodsafety_adhoc_efsa == 0 
xtreg FoodSafety i.foodsafety_adhoc_efsa i.year, fe
test 1.foodsafety_adhoc_efsa
est sto d_7

esttab d_1 d_2 d_3 d_4 d_5 d_6 d_7 using ///
"C:/Users/Admin/Desktop/Table4.rtf", ///
cells(b(star fmt(3)) se(par fmt(2))) ///
legend label title(Table 4. Difference in the number of cooperation events between instittutionalised and non-institutionalised framework) ///
nonumbers mtitles("Police" "Asylum" "Border control" "Health" "Aviation" "Environment" "FoodSafety") ///
star(* 0.10 ** 0.05 *** 0.01) scalars(F bic aic) nogaps replace

*********************hypothesis 1**********************************
//endogeniety test
reg Police police_dv  
predict Police_res, res
xtoprobit police_dv l.Police ll.police_dv l.police_dv lll.police_dv l.gow_eff l.polity_2 ///
year l.OtherSector year l.log_gdp_pp l.log_pop l.taex_request l.rule_law l.pol_stab Police_res, nolog
test Police_res 
test 

//Model 1 - FE
xtoprobit police_dv l.Police ll.police_dv lll.police_dv l.police_dv l.gow_eff l.polity_2 ///
l.OtherSector i.year l.log_gdp_pp l.log_pop l.taex_request l.rule_law l.pol_stab ///
 l.police_iv_cepol l.police_adhoc, nolog
*testparm i.ccode 
*testparm i.year 
estat ic
est sto m1

xtoprobit police_dv l.d.Police ll.police_dv lll.police_dv l.police_dv l.gow_eff l.polity_2 ///
l.OtherSector i.year l.log_gdp_pp l.log_pop l.taex_request l.rule_law l.pol_stab ///
 l.police_iv_cepol l.police_adhoc, nolog
 
** reported short model
xtoprobit police_dv l.Police ll.police_dv lll.police_dv l.police_dv l.gow_eff ///
l.OtherSector year l.log_gdp_pp l.log_pop  l.police_iv_cepol l.police_adhoc, nolog
*testparm i.ccode 
*testparm i.year 
estat ic
est sto m1a

//Model 1 - RE
xtoprobit police_dv l.Police ll.police_dv lll.police_dv l.police_dv l.gow_eff l.polity_2 ///
l.OtherSector year l.log_gdp_pp l.log_pop l.taex_request l.rule_law l.pol_stab ///
l.police_iv_cepol l.police_adhoc, nolog
est sto m1re

set more off
xtoprobit police_dv l.Police ll.police_dv lll.police_dv l.police_dv l.gow_eff l.polity_2 ///
 i.ccode l.OtherSector i.year l.log_gdp_pp l.log_pop l.taex_request l.rule_law l.pol_stab ///
 l.police_iv_cepol l.police_adhoc, nolog
//margins, dydx(l.Police) predict(pu0) over (year)
//marginsplot, recast (line) recastci(rarea) name(margins1a, replace)

xtoprobit police_dv l.Police ll.police_dv lll.police_dv l.police_dv l.gow_eff l.polity_2 ///
 i.ccode l.OtherSector i.year l.log_gdp_pp l.log_pop l.taex_request l.rule_law l.pol_stab ///
 l.police_iv_cepol l.police_adhoc, nolog
estat ic
est sto lag1

// Robustness check with xthtaylor (fits panel-data random-effects models in which some of the covariates are correlated
*with the unobserved individual-level random effect). The estimators, originally proposed by Hausman
*and Taylor (1981) and by Amemiya and MaCurdy (1986), are based on instrumental variables.
xthtaylor police_dv l.Police l.police_dv ll.police_dv l.police_iv_cepol l.police_adhoc l.gow_eff ///
l.OtherSector year l.log_gdp_pp l.log_pop ccode , ///
endog(l.Police l.police_dv ll.police_dv l.police_iv_cepol l.police_adhoc) constant(ccode) vce(robust)

* Instrumental variables and two-stage least squares for panel-data models
xtivreg  police_dv l.gow_eff l.OtherSector year l.log_gdp_pp l.log_pop ///
(l.Police = l.police_dv ll.police_dv l.police_iv_cepol l.police_adhoc ), re vce(robust)

xtivreg police_dv  l.gow_eff l.polity l.OtherSector ///
year l.log_gdp_pp l.log_pop l.taex_request l.rule_law l.pol_stab ///
(l.Police = l.police_dv ll.police_dv l.police_iv_cepol l.police_adhoc ), re vce(robust)
 
*******************************************************************
xtunitroot llc asylum_dv
xtserial asylum_dv 

reg  Asylum asylum_dv  
predict Asylum_res, res
xtoprobit asylum_dv l.Asylum l.gow_eff l.polity_2 ///
year l.OtherSector year l.gdp_pp l.pop l.taex_request l.rule_law l.pol_stab Asylum_res, nolog
test Asylum_res 
test 

xtoprobit asylum_dv l.Asylum l.asylum_dv ll.asylum_dv lll.asylum_dv l.asylum_adhoc l.gow_eff l.polity_2 ///
l.OtherSector year l.log_gdp_pp l.log_pop l.taex_request l.rule_law l.pol_stab, nolog
estat ic
est sto m2
*testparm i.year 
*testparm i.ccode 

//reported shorter model
xtoprobit asylum_dv l.Asylum l.asylum_dv ll.asylum_dv lll.asylum_dv l.asylum_adhoc l.gow_eff ///
l.OtherSector year l.log_gdp_pp l.log_pop, nolog
estat ic
est sto m2a
*testparm i.year 
*testparm i.ccode 

xtoprobit asylum_dv ll.Asylum ll.asylum_dv lll.asylum_dv llll.asylum_dv ll.asylum_adhoc ll.gow_eff ll.polity_2 ///
ll.OtherSector year ll.log_gdp_pp ll.log_pop ll.taex_request ll.rule_law ll.pol_stab, nolog

// Robustness check with xthtaylor 
xthtaylor asylum_dv l.Asylum l.asylum_dv ll.asylum_dv lll.asylum_dv l.asylum_adhoc l.gow_eff ///
l.OtherSector year l.log_gdp_pp l.log_pop ccode, ///
endog(l.Asylum l.asylum_dv ll.asylum_dv lll.asylum_dv l.asylum_adhoc) constant(ccode) vce(robust)

* Instrumental variables and two-stage least squares for panel-data models
xtivreg  asylum_dv l.gow_eff ///
l.OtherSector year l.log_gdp_pp l.log_pop ///
(l.Asylum = l.asylum_dv ll.asylum_dv lll.asylum_dv l.asylum_adhoc), re vce(robust)

xtivreg  asylum_dv l.gow_eff l.polity l.taex_request l.rule_law l.pol_stab ///
l.OtherSector year l.log_gdp_pp l.log_pop ///
(l.Asylum = l.asylum_dv ll.asylum_dv lll.asylum_dv l.asylum_adhoc), re vce(robust)

*******************************************************************
xtunitroot llc border_dv 
xtserial border_dv 

reg  BorderControl border_dv  
predict BorderControl_res, res

xtoprobit border_dv l.BorderControl l.border_dv ll.border_dv lll.border_dv ll.gow_eff ll.polity_2 ///
ll.OtherSector year ll.log_gdp_pp ll.log_pop ll.taex_request ll.rule_law ll.pol_stab /// 
l.border_iv_frontex l.border_adhoc_frontex BorderControl_res, nolog
test BorderControl_res 
test 

xtoprobit border_dv l.BorderControl l.border_dv ll.border_dv lll.border_dv l.gow_eff l.polity_2 ///
l.OtherSector i.year l.log_gdp_pp l.log_pop l.taex_request l.rule_law ll.pol_stab /// 
l.border_iv_frontex l.border_adhoc_frontex , nolog
*testparm i.year 
*testparm i.ccode 
estat ic
est sto m3

//reported shorter model
xtoprobit border_dv l.BorderControl l.border_dv ll.border_dv lll.border_dv l.gow_eff  ///
l.OtherSector i.year l.log_gdp_pp l.log_pop l.border_iv_frontex l.border_adhoc_frontex , nolog
est sto m3a
estat ic

// Robustness check 
xthtaylor border_dv l.BorderControl l.border_dv ll.border_dv lll.border_dv l.gow_eff  ///
l.OtherSector year l.log_gdp_pp l.log_pop l.border_iv_frontex l.border_adhoc_frontex ccode, ///
endog(l.BorderControl l.border_dv ll.border_dv lll.border_dv l.border_iv_frontex ///
l.border_adhoc_frontex) constant(ccode) vce(robust)

* Instrumental variables and two-stage least squares for panel-data models
xtivreg  border_dv  l.gow_eff  ///
l.OtherSector year l.log_gdp_pp l.log_pop  ///
(l.BorderControl = l.border_dv ll.border_dv lll.border_dv l.border_iv_frontex l.border_adhoc_frontex), re vce(robust)

xtivreg  border_dv  l.gow_eff  l.polity l.taex_request l.rule_law l.pol_stab ///
l.OtherSector year l.log_gdp_pp l.log_pop  ///
(l.BorderControl = l.border_dv ll.border_dv lll.border_dv l.border_iv_frontex l.border_adhoc_frontex), re vce(robust)

*******************************************************************

xtunitroot llc health_dv 
xtserial health_dv 

reg  Health health_dv  
predict Health_res, res

xtoprobit health_dv l.health_dv ll.health_dv lll.health_dv l.Health l.gow_eff l.polity_2 ///
 l.OtherSector i.year l.log_gdp_pp l.log_pop l.taex_request l.rule_law ll.pol_stab l.health_iv_ecdc_ema health_adhoc ///
Health_res, nolog
test Health_res 
test 

xtoprobit health_dv l.Health l.health_dv ll.health_dv lll.health_dv  l.gow_eff l.polity_2 ///
 l.OtherSector year l.log_gdp_pp l.log_pop l.taex_request l.rule_law ll.pol_stab l.health_iv_ecdc_ema /// 
 l.health_adhoc, nolog
* testparm i.year 
* testparm i.ccode 
estat ic
est  sto m4

// shorter model
xtoprobit health_dv l.Health l.health_dv ll.health_dv lll.health_dv  l.gow_eff  ///
 l.OtherSector year l.log_gdp_pp l.log_pop l.health_iv_ecdc_ema /// 
 l.health_adhoc, nolog
estat ic
est  sto m4a

*testparm i.year 
*testparm i.ccode 
 
 // Robustness check with xthtaylor 
xthtaylor health_dv l.Health l.health_dv ll.health_dv lll.health_dv l.gow_eff  ///
l.OtherSector year l.log_gdp_pp l.log_pop l.health_iv_ecdc_ema ccode, ///
endog(l.Health l.health_dv ll.health_dv lll.health_dv l.health_iv_ecdc_ema) constant(ccode) vce(robust)

* Instrumental variables and two-stage least squares for panel-data models
xtivreg  health_dv  l.gow_eff  ///
l.OtherSector year l.log_gdp_pp l.log_pop   ///
(l.Health = l.health_dv ll.health_dv lll.health_dv l.health_iv_ecdc_ema), re vce(robust)

xtivreg  health_dv  l.gow_eff  l.polity l.taex_request l.rule_law l.pol_stab ///
l.OtherSector year l.log_gdp_pp l.log_pop   ///
(l.Health = l.health_dv ll.health_dv lll.health_dv l.health_iv_ecdc_ema), re vce(robust)

*******************************************************************
xtunitroot llc foodsafety_dv 
xtserial foodsafety_dv 

reg  FoodSafety foodsafety_dv  
predict FoodSafety_res, res

xtoprobit foodsafety_dv l.FoodSafety l.foodsafety_dv ll.foodsafety_dv lll.foodsafety_dv l.gow_eff l.polity_2 ///
 l.OtherSector year l.log_gdp_pp l.log_pop l.taex_request l.rule_law ll.pol_stab foodsafety_adhoc_efsa ///
FoodSafety_res, nolog
test FoodSafety_res 
test

xtoprobit  foodsafety_dv l.FoodSafety l.foodsafety_dv ll.foodsafety_dv lll.foodsafety_dv l.gow_eff l.polity_2 ///
 l.OtherSector year l.log_gdp_pp l.log_pop l.taex_request l.rule_law ll.pol_stab foodsafety_adhoc_efsa ///
, nolog
*testparm i.year 
* testparm i.ccode 
estat ic
est  sto m5

//reported shorter model
xtoprobit  foodsafety_dv l.FoodSafety l.foodsafety_dv ll.foodsafety_dv lll.foodsafety_dv l.gow_eff  ///
 l.OtherSector year l.log_gdp_pp l.log_pop foodsafety_adhoc_efsa, nolog
*testparm i.year 
* testparm i.ccode 
estat ic
est  sto m5a
 
*******************************************************************
xtunitroot llc aviation_dv 
xtserial aviation_dv 

reg  Aviation aviation_dv  
predict Aviation_res, res

xtoprobit aviation_dv l.Aviation l.aviation_dv ll.aviation_dv lll.aviation_dv l.gow_eff l.polity_2 ///
 l.OtherSector year l.log_gdp_pp l.log_pop l.taex_request l.rule_law l.pol_stab ///
Aviation_res, nolog
test Aviation_res 
test

xtoprobit aviation_dv l.Aviation l.aviation_dv ll.aviation_dv lll.aviation_dv l.gow_eff ///
 l.OtherSector i.year l.log_gdp_pp l.log_pop l.aviation_iv_easa l.aviation_adhoc_easa, nolog
*testparm i.year 
* testparm i.ccode 
estat ic
est  sto m6a

// test with xthtaylor 
xthtaylor aviation_dv l.Aviation l.aviation_dv ll.aviation_dv lll.aviation_dv l.gow_eff ///
 l.OtherSector year l.log_gdp_pp l.log_pop l.aviation_iv_easa l.aviation_adhoc_easa ccode, ///
endog(l.Aviation l.aviation_dv ll.aviation_dv lll.aviation_dv l.aviation_adhoc_easa) constant(ccode) vce(robust)

* Instrumental variables and two-stage least squares for panel-data models
xtivreg  aviation_dv  l.gow_eff ///
 l.OtherSector i.year l.log_gdp_pp l.log_pop   ///
(l.Aviation = l.aviation_dv ll.aviation_dv lll.aviation_dv l.aviation_iv_easa l.aviation_adhoc_easa), re vce(robust)

xtivreg  aviation_dv  l.gow_eff l.polity l.taex_request l.rule_law l.pol_stab ///
 l.OtherSector i.year l.log_gdp_pp l.log_pop   ///
(l.Aviation = l.aviation_dv ll.aviation_dv lll.aviation_dv l.aviation_iv_easa l.aviation_adhoc_easa), re vce(robust)

*******************************************************************
xtunitroot llc environment_dv 
xtserial environment_dv 

reg Environment environment_dv  
predict Environment_res, res

xtoprobit environment_dv l.Environment l.environment_dv ll.environment_dv lll.environment_dv l.gow_eff l.polity_2 ///
 l.OtherSector year l.log_gdp_pp l.log_pop l.taex_request l.rule_law ll.pol_stab l.environment_adhoc ///
Environment_res, nolog
test Environment_res 
test

xtoprobit environment_dv l.Environment l.environment_dv ll.environment_dv lll.environment_dv l.gow_eff l.polity_2 ///
 l.OtherSector year l.log_gdp_pp l.log_pop l.taex_request l.rule_law ll.pol_stab l.environment_adhoc, nolog
* testparm i.year
* testparm i.ccode 
estat ic
est sto m7

** short model reported in Table 2
xtoprobit environment_dv l.Environment l.environment_dv ll.environment_dv lll.environment_dv l.gow_eff ///
 l.OtherSector year l.log_gdp_pp l.log_pop l.environment_adhoc , nolog
* testparm i.year 
* testparm i.ccode 
estat ic
est sto m7a

xthtaylor environment_dv l.Environment l.environment_dv ll.environment_dv lll.environment_dv l.gow_eff ///
 l.OtherSector year l.log_gdp_pp l.log_pop l.environment_adhoc ccode, ///
endog(l.Environment l.environment_dv ll.environment_dv lll.environment_dv l.environment_adhoc) ///
constant(ccode) vce(robust)

* Instrumental variables and two-stage least squares for panel-data models
xtivreg  environment_dv l.gow_eff ///
 l.OtherSector year l.log_gdp_pp l.log_pop ///
(l.Environment = l.environment_dv ll.environment_dv lll.environment_dv l.environment_adhoc), re vce(robust)

xtivreg  environment_dv l.gow_eff l.polity l.taex_request l.rule_law l.pol_stab ///
 l.OtherSector year l.log_gdp_pp l.log_pop ///
(l.Environment = l.environment_dv ll.environment_dv lll.environment_dv l.environment_adhoc), re vce(robust)

*********************************************************************
esttab m1a m2a m3a m4a m5a m6a m7a using ///
"C:/Users/Admin/Desktop/Table2.rtf", ///
cells(b(star fmt(3)) se(par fmt(2))) ///
legend label title(Table 2. Regression analysis: The effects of transgovernmental cooperation, 2006-2016) ///
nonumbers mtitles("Model 1." "Model 2." "Model 3." "Model 4." "Model 5." "Model 6." "Model 7.") ///
star(* 0.10 ** 0.05 *** 0.01) scalars(F bic aic) nogaps replace

esttab m1 m2 m3 m4 m5 m6 m7 using ///
"C:/Users/Admin/Desktop/Table1_robustness.rtf", ///
cells(b(star fmt(3)) se(par fmt(2))) ///
legend label title(Table 2a. Robustness check: The effects of transgovernmental cooperation, 2006-2016) ///
nonumbers mtitles("Model 1." "Model 2." "Model 3." "Model 4." "Model 5." "Model 6." "Model 7.") ///
star(* 0.10 ** 0.05 *** 0.01) scalars(F bic aic) nogaps replace

*********************hypothesis 2**********************************
xtoprobit police_dv c.LPolice##i.Lpolice_any_cooperation ll.police_dv lll.police_dv l.police_dv l.gow_eff ///
l.OtherSector year l.log_gdp_pp l.log_pop, nolog
est sto m_pol

set more off
xtoprobit Dpolice_dv l.c.Police##l.police_any_cooperation ll.police_dv ///
l.gow_eff l.polity_2 year l.OtherSector l.gdp_pp l.pop ///
l.taex_request l.rule_law l.pol_stab i.ccode, nolog
margins l.police_any_cooperation , at(l.Police=(0(100)1000))
marginsplot, xdimension(at(l.Police))
marginsplot, recast (line) recastci(rarea) name(margins3b, replace)

*******************************************************************
xtoprobit asylum_dv c.LAsylum##i.Lasylum_adhoc l.asylum_dv ll.asylum_dv lll.asylum_dv  l.gow_eff ///
l.OtherSector year l.log_gdp_pp l.log_pop, nolog
est sto m_asyl
estat ic

set more off
xtoprobit asylum_dv c.LAsylum##i.Lasylum_iv_cooperation l.Asylum l.asylum_iv_cooperation ll.asylum_dv l.gow_eff l.polity_2 i.ccode ///
year l.OtherSector l.gdp_pp l.pop l.taex_request l.rule_law l.pol_stab, nolog
margins l.asylum_iv_anyCooperation , at(l.Asylum=(0(10)100))
marginsplot, xdimension(at(l.Asylum))

// Robustness check with
* Instrumental variables and two-stage least squares for panel-data models
xtivreg  asylum_dv c.LAsylum##i.Lasylum_adhoc l.asylum_dv ll.asylum_dv  l.gow_eff ///
l.OtherSector year l.log_gdp_pp l.log_pop  ///
(LAsylum = l.asylum_dv ll.asylum_dv Lasylum_adhoc), re vce(robust)

*******************************************************************
xtoprobit border_dv c.LBorderControl##i.l.border_iv_frontex l.border_dv ll.border_dv lll.border_dv l.gow_eff  ///
l.OtherSector i.year l.log_gdp_pp l.log_pop /// 
Lborder_adhoc_frontex , nolog
est sto m_bord
estat ic

xtoprobit border_dv c.LBorderControl##i.Lborder_adhoc_frontex l.border_dv ll.border_dv lll.border_dv l.gow_eff l.polity_2 ///
l.OtherSector i.year l.log_gdp_pp l.log_pop l.taex_request l.rule_law l.pol_stab /// 
l.border_iv_frontex , nolog

xtoprobit border_dv c.LBorderControl##i.Lborder_iv_frontex  l.border_dv ll.border_dv lll.border_dv l.gow_eff l.polity_2 ///
l.OtherSector i.year l.log_gdp_pp l.log_pop l.taex_request l.rule_law l.pol_stab /// 
l.border_adhoc_frontex , nolog

set more off
xtoprobit Dborder_dv l.c.BorderControl##l.i.frontex_iv_adhoc l.border_dv ll.border_dv l.gow_eff l.polity_2 ///
year i.ccode l.OtherSector year l.gdp_pp l.pop l.taex_request l.rule_law l.pol_stab, nolog
margins l.frontex_iv_adhoc , at(l.BorderControl=(0(10)100))
marginsplot, xdimension(at(l.BorderControl))

// Robustness check with
* Instrumental variables and two-stage least squares for panel-data models
xtivreg  border_dv c.LBorderControl##i.Lborder_iv_frontex  l.border_dv ll.border_dv lll.border_dv l.gow_eff l.polity_2 ///
l.OtherSector i.year l.log_gdp_pp l.log_pop l.taex_request l.rule_law l.pol_stab /// 
l.border_adhoc_frontex  ///
(LBorderControl = Lborder_iv_frontex  l.border_dv ll.border_dv lll.border_dv), re vce(robust)

set more off
xtoprobit Dborder_dv l.c.BorderControl##l.i.frontex_iv_anyCooperation l.border_dv ll.border_dv l.gow_eff l.polity_2 ///
year i.ccode l.OtherSector year l.gdp_pp l.pop l.taex_request l.rule_law l.pol_stab, nolog
margins l.frontex_iv_anyCooperation, at(l.BorderControl=(0(10)100))
marginsplot, xdimension(at(l.BorderControl))

********************************************************
xtoprobit health_dv c.LHealth##i.Lhealth_any_cooperation l.health_dv ll.health_dv lll.health_dv  l.gow_eff  ///
l.OtherSector i.year l.log_gdp_pp l.log_pop, nolog
est  sto m_heal

set more off
xtoprobit Dhealth_dv l.c.Health##l.i.health_iv_anyCooperation l.health_dv ll.health_dv l.gow_eff l.polity_2 ///
year i.ccode l.OtherSector year l.gdp_pp l.pop l.taex_request l.rule_law l.pol_stab, nolog
margins l.health_iv_anyCooperation, at(l.Health=(0(10)100))
marginsplot, xdimension(at(l.Health))

********************************************************
xtoprobit foodsafety_dv c.LFoodSafety#i.Lfoodsafety_adhoc_efsa l.foodsafety_dv ll.foodsafety_dv lll.foodsafety_dv l.gow_eff ///
 l.OtherSector year l.log_gdp_pp l.log_pop ll.foodsafety_adhoc_efsa ll.FoodSafety, nolog
est  sto m_food

set more off
xtoprobit Dfoodsafety_dv l.c.FoodSafety##l.i.foodsafety_iv_anyCooperation ll.foodsafety_dv l.gow_eff l.polity_2 ///
year i.ccode l.OtherSector year l.gdp_pp l.pop l.taex_request l.rule_law l.pol_stab, nolog
estat ic
margins l.foodsafety_iv_anyCooperation, at(l.FoodSafety=(0(100)1000))
marginsplot, xdimension(at(l.FoodSafety))

 // Robustness check with
* Instrumental variables and two-stage least squares for panel-data models
xtivreg  foodsafety_dv c.LFoodSafety#i.Lfoodsafety_adhoc_efsa l.gow_eff ///
 l.OtherSector year l.log_gdp_pp l.log_pop   ///
(FoodSafety = l.foodsafety_dv ll.foodsafety_dv foodsafety_adhoc_efsa), re vce(robust)

********************************************************
xtoprobit aviation_dv c.LAviation##i.Laviation_any_cooperation l.aviation_dv ll.aviation_dv lll.aviation_dv l.gow_eff  ///
l.OtherSector year l.log_gdp_pp l.log_pop , nolog 
est  sto m_avi
estat ic

 // Robustness check with
* Instrumental variables and two-stage least squares for panel-data models
xtivreg  aviation_dv c.LAviation##i.Laviation_iv_easa l.aviation_dv ll.aviation_dv lll.aviation_dv l.gow_eff  ///
l.OtherSector year i.ccode l.log_gdp_pp l.log_pop Laviation_adhoc_easa  ///
(LAviation = Laviation_iv_easa l.aviation_dv ll.aviation_dv lll.aviation_dv), re vce(robust)

********************************************************
xtoprobit environment_dv c.LEnvironment##i.Lenvironment_adhoc  l.environment_dv ll.environment_dv lll.environment_dv l.gow_eff  ///
l.OtherSector year l.log_gdp_pp l.log_pop, nolog
estat ic
est  sto m_env
 
*** Results tabble 2 
esttab m_pol m_asyl m_bord m_heal m_food m_avi m_env using ///
"C:/Users/Admin/Desktop//Table3.rtf", ///
cells(b(star fmt(3)) se(par fmt(2))) ///
legend label title(Table 3. Regression analysis: the joint effects of transgovernmental cooperation and EU agencies' involvement, 2006-2016) ///
nonumbers mtitles("Model 8." "Model 9." "Model 10." "Model 11." "Model 12." "Model 13.") ///
star(* 0.10 ** 0.05 *** 0.01) scalars(F bic aic) nogaps replace


