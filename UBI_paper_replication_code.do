cd "insert directory"
clear
use ESS8

*** Clean dataset

encode cntry, gen(country)
rename cntry iso2
drop if iso2=="IL" | iso2=="RU"
encode region, gen(region2)
replace region2=. if region2==1  // the region (in Norway) with value 1 is named 99999. I drop it because I cannot associate to any real region

**** rename variables
rename (agea hinctnta gndr inwyye)  (age income sex year)

****
gen agesq= age^2

****
rename mnactic emplstatus
replace emplstatus=3 if emplstatus==4
replace emplstatus=9  if emplstatus==7

******
rename maritalb married
replace married=1 if married==2
replace married=3 if married==4

*****
gen educ=.
replace educ=1 if eisced==1
replace educ=2 if eisced ==2
replace educ=3 if eisced >=3 & eisced<=4
replace educ=4 if eisced ==5
replace educ=5 if eisced >=6 & eisced<=7

*****
replace ctzcntr=0 if ctzcntr==2

*****
replace sex=0 if sex==2

*****

global mistrust ppltrst pplfair pplhlp trstprl trstprt trstplt 
revrs $mistrust, replace
*maintain the 0-10 scale

foreach v of global mistrust {
    gen `v'_= `v'-1
  } 
drop  ppltrst pplfair pplhlp trstprl trstprt trstplt 
rename ( ppltrst_ pplfair_ pplhlp_ trstprl_ trstprt_ trstplt_ ) (ppltrst pplfair pplhlp trstprl trstprt trstplt)
  
  
*** normalise trust

global t ppltrst pplfair pplhlp
pca $t
estat loadings
predict trust2, score
estat kmo
egen sum_2= rowtotal($t ) 
pwcorr $t trust2 sum_2, star(0.01) sig


global trust trstprl trstprt trstplt
pca $trust 
estat loadings
predict institutions, score
estat kmo
egen sum_= rowtotal($trust ) 
pwcorr $trust institutions sum_, star(0.01) sig

*** standardization for interepretation
rename trust2 trustT
egen mtrust= mean(trustT)
egen sdtrust=sd(trustT)
gen trust2= (trustT-mtrust)/sdtrust

rename institutions political
egen mpolitical= mean(political)
egen sdpolitical=sd(political)
gen institutions= (political-mpolitical)/sdpolitical


* save a copy of nonstandardized basinc variable
gen NS_basinc=basinc
* standardise basinc
rename basinc UBIincome
egen mbasinc= mean(UBIincome)
egen sdbasinc=sd(UBIincome)
gen basinc= (UBIincome-mbasinc)/sdbasinc

*** Rename variables
rename (ctzcntr emplstatus married sex rlgdgr) (citizen employment maritalstatus male religion)

*** iso08 by submajor groups
tostring isco08, gen(my_string_variable) format(%07.0f)
gen first_two_digits2 =  substr(my_string_variable, 4, .) 
gen first_two_digits =  substr(first_two_digits2, 1, strlen(first_two_digits2) - 2) 
destring first_two_digits, generate(isco)

**** variables for mechanisms (welfare retrenchment)
rename wrkprbf UBIwrkprbf
egen mwrkprbf= mean(UBIwrkprbf)
egen sdwrkprbf=sd(UBIwrkprbf)
gen wrkprbf= (UBIwrkprbf-mwrkprbf)/sdwrkprbf

rename eduunmp UBIeduunmp
egen meduunmp= mean(UBIeduunmp)
egen sdeduunmp=sd(UBIeduunmp)
gen eduunmp= (UBIeduunmp-meduunmp)/sdeduunmp

revrs inctxff, replace
rename inctxff UBIinctxff
egen minctxff= mean(UBIinctxff)
egen sdinctxff=sd(UBIinctxff)
gen inctxff= (UBIinctxff-minctxff)/sdinctxff

**** variables for mechanisms (COST)
gen cost=1 if sbstrec==1 | sbstrec==2
replace cost=0 if sbstrec==3 | sbstrec==4 | sbstrec==5

*** generate cheating variable
gen cheat1= 1 if  bennent==1 | bennent==2 
replace cheat1=0 if bennent==3 | bennent==4| bennent==5
gen cheat2= 1 if  uentrjb==1 | uentrjb==2 
replace cheat2=0 if uentrjb==3 | uentrjb==4| uentrjb==5

*** cheating variable below
gen cheat=1 if cheat1==1 & cheat2==1
replace cheat=0 if (cheat1==0 & cheat2==0) | (cheat1==1 & cheat2==0) | (cheat1==0 & cheat2==1)

**** label variables 

label variable age "Age"
label variable agesq "Age (squared)"
label variable eduyrs "Years of education"
label variable male "Male"
label variable citizen "Citizen"
label define citizen  1 "Citizen" 0 "Noncitizen" 
label variable hhmmb "Household members"
label variable religion "Religiosity"
label variable lrscale "Left-Right scale"
label variable isco "Isco (2-digits)"
label define domicil1  1 "A big city" 2 "Suburbs or outskirts of big city" 3 "Town or small city" 4 "Country village" 5 "Farm or home in countryside"
label values domicil domicil1
label define marital 1 "Married" 3 "Separated" 5 "Widowed" 6 "None of these" 
label values maritalstatus marital
label define empl 1 "Paid work" 2 "Education" 3 "Unemployed" 5 "Permanently sick or disabled"  6 "Retired" 8 "Housework" 9 "Other"
label values employment empl
label define quintile  1 "Income Decile 1" 2 "Income Decile 2" 3 "Income Decile 3" 4 "Income Decile 4" 5 "Income Decile 5" 6 "Income Decile 6" 7 "Income Decile 7" 8 "Income Decile 8" 9 "Income Decile 9" 10 "Income Decile 10"
label values income quintile
label variable trstplt "Mistrust in politicians"
label variable trstprt "Mistrust in political parties"
label variable trstprl "Mistrust in parliament"
label variable ppltrst "Mistrust in people"
label variable pplfair "Unfairness"
label variable pplhlp "Unhelpfulness"
label variable trust2 "Generalised mistrust index"
label variable institutions "Political mistrust index"
label variable income "Income"
label variable eduunmp "Education investment vs UI"
label define leveleduc 1 "Less than lower secondary education (ISCED 0-1) " 2 "Lower secondary education completed (ISCED 2) " 3 "Upper secondary education completed (ISCED 3)" 4 "Post-secondary non-tertiary education completed (ISCED 4)" 5 "Tertiary education completed (ISCED 5-6)"
label values educ leveleduc
tabulate  maritalstatus, generate(Maritalstatus)
tabulate domicil, generate(Domicil)
tabulate employment, generate(Employment) 
tabulate educ, generate(Educ)
label variable Employment1 "Paid work"
label variable Employment2 "Education"
label variable Employment3 "Unemployed"
label variable Employment4 "Permanently sick or disabled"
label variable Employment5 "Retired"
label variable Employment6 "Housework"
label variable Employment7 "Other"
label variable Domicil1 "A big city"
label variable Domicil2 "Suburbs or outskirts of big city"
label variable Domicil3 "Town or small city"
label variable Domicil4 "Country village"
label variable Domicil5 "Farm or home in countryside"
label variable Maritalstatus1 "Married"
label variable Maritalstatus2 "Separated"
label variable Maritalstatus3 "Widowed"
label variable Maritalstatus4 "None of these"
label variable Educ1 "Less than lower secondary education (ISCED 0-1) "
label variable Educ2 "Lower secondary education completed (ISCED 2) "
label variable Educ3 "Upper secondary education completed (ISCED 3)"
label variable Educ4 "Post-secondary non-tertiary education completed (ISCED 4)"
label variable Educ5 "Tertiary education completed (ISCED 5-6)"
label variable cost "Welfare cost"
label variable cheat "Misbehaviour"
label variable wrkprbf "Parental Benefits"
label variable eduunmp "Training vs UI "
label variable inctxff "Climate Change Policies"
label variable bennent "Misbehaviour benefits"
label variable uentrjb "Misbehaviour unemployed"

 
******
global socioeconomic age agesq i.educ i.male i.citizen i.maritalstatus ib4.domicil hhmmb 
global incomevar income i.employment i.isco 
global ideology lrscale religion 


***descriptive statics

sum basinc institutions trstprl trstprt trstplt trust2 ppltrst pplfair pplhlp Employment* Domicil* Maritalstatus* hhmmb age agesq Educ* income isco lrscale religion wrkprbf eduunmp inctxff cost cheat [aw=anweight]
outreg2 using Table_B1, tex label replace sum(log) keep(basinc institutions trstprl trstprt trstplt trust2 ppltrst pplfair pplhlp Employment* Domicil* Maritalstatus* hhmmb age agesq Educ* income isco lrscale religion wrkprbf eduunmp inctxff cost cheat) sortvar(basinc institutions trstprl trstprt trstplt trust2 ppltrst pplfair pplhlp Employment* Domicil* Maritalstatus* hhmmb age agesq Educ* income isco lrscale religion wrkprbf eduunmp inctxff cost cheat) 


**** graphs

graph bar (mean) institutions [aw=anweight], over(iso2, sort(1) descending label(angle(0) labsize(small))) graphregion(color(white)) plotregion(color(white) margin(medsmall)) ytitle("Political Mistrust Index", size(medium))  yscale(titlegap(*5))
graph save institutions.gph, replace
graph bar (mean) trust2 [aw=anweight], over(iso2, sort(1) descending label(angle(0) labsize(small))) graphregion(color(white)) plotregion(color(white) margin(medsmall))  ytitle("Generalised Mistrust", size(medium)) yscale(titlegap(*5)) 
graph save trust2.gph, replace
graph combine institutions.gph trust2.gph, graphregion(color(white)) plotregion(color(white) margin(small)) xsize(8)
graph export "Figure_C31.pdf", replace
erase institutions.gph 
erase trust2.gph
*****
revrs basinc
tabulate revbasinc, generate(B)
*setting colours
graph hbar B1 B2 B3 B4 [aw=anweight], vert over(iso2, sort(4) descending label(angle(0) labsize(small))) stack percent legend(order(1 "Strongly against" 2 "Against" 3 "In favor" 4 "Strongly in favor" )) graphregion(color(white)) plotregion(color(white)) ytitle("UBI preferences % population", size(medium)) yscale(titlegap(*5)) bar(1, color(eltblue%40)) ///
    bar(2, color(ebblue%60)) ///
    bar(3, color(navy%80)) ///
    bar(4, color(dknavy%100))
graph export Figure_4.pdf, replace


*** regressions with region fixed effects

areg ppltrst pplfair, absorb(region2) vce(cluster region2)
outreg2 using Table_C11, tex label addtext(Region FE, YES,) nocons adjr2  drop(i.region2) dec(3) replace ctitle((1)) 
areg ppltrst pplhlp, absorb(region2) vce(cluster region2)
outreg2 using Table_C11, tex label addtext(Region FE, YES,) nocons adjr2  drop(i.region2) dec(3) append ctitle((2)) 
areg pplhlp pplfair, absorb(region2)  vce(cluster region2)
outreg2 using Table_C11, tex label addtext(Region FE, YES,) nocons adjr2  drop(i.region2) dec(3) append ctitle((3)) 
areg trstplt trstprt, absorb(region2)  vce(cluster region2)
outreg2 using Table_C11, tex label addtext(Region FE, YES,) nocons adjr2  drop(i.region2) dec(3) append ctitle((4)) 
areg trstplt trstprl, absorb(region2)  vce(cluster region2)
outreg2 using Table_C11, tex label addtext(Region FE, YES,) nocons adjr2  drop(i.region2) dec(3) append ctitle((5)) 
areg trstprl trstprt, absorb(region2)  vce(cluster region2)
outreg2 using Table_C11, tex label addtext(Region FE, YES,) nocons adjr2  drop(i.region2) dec(3) append ctitle((6)) 


**** binscatters

// Run binscatter
binscatter ppltrst pplfair, absorb(region2) xtitle("Residualised Unfairness", size(medium)) ytitle("Residualised Mistrust in people", size(medium)) title("(A) Mistrust in people" "on Unfairness" , size(medium)) yscale(titlegap(5)) graphregion(color(white)) plotregion(color(white) margin(medium))
graph save bin1.gph, replace

binscatter ppltrst pplhlp, absorb(region2) xtitle("Residualised Unhelpfulness", size(medium)) ytitle("Residualised Mistrust in people", size(medium)) title("(B) Mistrust in people" "on Unhelpfulness" , size(medium)) yscale(titlegap(5)) graphregion(color(white)) plotregion(color(white) margin(medium))
graph save bin2.gph, replace

// Run binscatter
binscatter pplhlp pplfair, absorb(region2) xtitle("Residualised Unfairness", size(medium)) ytitle("Residualised Unhelpfulness", size(medium)) title("(C) Unhelpfulness" "on Unfairness" , size(medium)) yscale(titlegap(5)) graphregion(color(white)) plotregion(color(white) margin(medium))
graph save bin3.gph, replace

**** political mistrust

// Run regression
binscatter trstplt trstprt, absorb(region2) xtitle("Residualised" "Mistrust in politicians", size(medium)) ytitle("Residualised Mistrust in political parties", size(medium)) title("(D) Mistrust in political parties" "on Mistrust in politicians" , size(medium)) yscale(titlegap(5)) graphregion(color(white)) plotregion(color(white) margin(medium))
graph save bin4.gph, replace

// Run binscatter
binscatter trstplt trstprl, absorb(region2)  xtitle("Residualised" "Mistrust in politicians", size(medium)) ytitle("Residualised Mistrust in parliament", size(medium)) title("(E) Mistrust in parliament" "on Mistrust in politicians" , size(medium)) yscale(titlegap(5)) graphregion(color(white)) plotregion(color(white) margin(medium))
graph save bin5.gph, replace
 
// Run binscatter
binscatter trstprl trstprt, absorb(region2) xtitle("Residualised" "Mistrust in parliament", size(medium)) ytitle("Residualised Mistrust in political parties", size(medium)) title("(F) Mistrust in political parties" "on Mistrust in parliament" , size(medium)) yscale(titlegap(5)) graphregion(color(white)) plotregion(color(white) margin(medium)) graphregion(color(white)) plotregion(color(white) margin(medium))
graph save bin6.gph, replace

graph combine bin1.gph bin2.gph bin3.gph bin4.gph bin5.gph bin6.gph, graphregion(color(white)) plotregion(color(white) margin(small)) //xsize(8)
graph export Figure_C11.pdf, replace
erase bin1.gph 
erase bin2.gph 
erase bin3.gph 
erase bin4.gph 
erase bin5.gph 
erase bin6.gph



***pairwise correlation
estpost correlate $trust,  matrix 
est sto storagename
esttab * using Table_C21.tex, label replace unstack not noobs compress 

estpost correlate $t,  matrix 
est sto storagename
esttab * using Table_C22.tex, label replace unstack not noobs compress 


***** TABLE 1
areg basinc institutions,absorb(region2) vce(cluster region2)
outreg2 using TABLE1.tex, label addtext(Region FE, YES, ) nocons adjr2  drop(i.region2) sortvar(institutions trust2) dec(3) replace ctitle((1))
areg basinc trust2,absorb(region2) vce(cluster region2)
outreg2 using TABLE1.tex, label addtext(Region FE, YES, ) nocons adjr2  drop(i.region2) sortvar(institutions trust2) dec(3) append ctitle((2))
areg basinc institutions trust2 ,absorb(region2) vce(cluster region2)
outreg2 using TABLE1.tex, label addtext(Region FE, YES, ) nocons adjr2  drop(i.region2) sortvar(institutions trust2) dec(3) append ctitle((3)) 
areg basinc institutions trust2 $incomevar ,absorb(region2) vce(cluster region2)
outreg2 using TABLE1.tex, label addtext(Region FE, YES, Income Controls, YES) nocons adjr2  drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(institutions trust2 income) dec(3) append ctitle((4))
areg basinc institutions trust2 $incomevar $socioeconomic ,absorb(region2) vce(cluster region2)
outreg2 using TABLE1.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES,) nocons adjr2  drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(institutions trust2 income) dec(3) append ctitle((5))  
areg basinc institutions c.trust2 $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using TABLE1.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(institutions trust2 income) dec(3) append ctitle((6)) 

******
*** MECHANISMS: welfare-retrenchment/taxes
**** welfare retrenchment mechanism 

areg eduunmp institutions c.trust2 $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table2.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 income) sortvar(institutions trust2) dec(3) replace ctitle((1))
areg wrkprbf institutions c.trust2 $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table2.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 income) sortvar(institutions trust2 income) dec(3) append ctitle((2)) 
areg inctxff institutions c.trust2 $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table2.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 income) sortvar(institutions trust2) dec(3) append ctitle((3)) 

**** Mechanism cost

areg basinc c.institutions c.trust2 $incomevar $socioeconomic $ideology if cost!=. & cheat!=.,absorb(region2) vce(cluster region2)
outreg2 using Table3.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2) sortvar(institutions trust2) dec(3) replace ctitle((1))
areg basinc c.institutions c.trust2 i.cost i.cheat $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table3.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 i.cost i.cheat) sortvar(institutions trust2 i.cost i.cheat) dec(3) append ctitle((2))
areg basinc c.institutions##i.cost c.trust2 i.cheat  $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table3.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 i.cost i.cheat c.institutions#i.cost c.trust2#i.cheat) sortvar(institutions trust2 i.cost i.cheat c.institutions#i.cost) dec(3) append ctitle((3))
margins, dydx(institutions) at(cost=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(vsmall)) xlab(1 "Do not agree" 2 "Agree") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Social benefits/services place too great strain" "on the economy ") title("Average Marginal Effect of Political Mistrust with 95% CIs" "(Single interaction)", size(small)) yline(0) graphregion(color(white)) plotregion(color(white))
graph save cost1.gph, replace  
areg basinc c.institutions c.trust2##i.cheat i.cost $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table3.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 i.cost i.cheat c.institutions#i.cost c.trust2#i.cheat) sortvar(institutions trust2 i.cost i.cheat c.trust2#i.cheat) dec(3) append ctitle((4))
margins, dydx(trust2) at(cheat=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(vsmall))  xlab(1 "Do not agree" 2 "Agree") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Citizens likely to cheat") title( "Average Marginal Effect of Generalised Mistrust with 95% CIs" "(Single interaction)" , size(small)) yline(0) graphregion(color(white)) plotregion(color(white))
graph save cheat1.gph, replace
areg basinc c.institutions##i.cost c.trust2##i.cheat $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table3.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 i.cost i.cheat c.institutions#i.cost c.trust2#i.cheat) sortvar(institutions trust2 i.cost i.cheat c.trust2#i.cheat) dec(3) append ctitle((5))
margins, dydx(institutions) at(cost=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(vsmall)) xlab(1 "Do not agree" 2 "Agree") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Social benefits/services place too great strain" "on the economy") title( "Average Marginal Effect of Political Mistrust with 95% CIs" "(Double interaction)", size(small)) yline(0) graphregion(color(white)) plotregion(color(white))
graph save cost2.gph, replace  
quietly: areg basinc c.institutions##i.cost c.trust2##i.cheat $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(cheat=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(vsmall)) xlab(1 "Do not agree" 2 "Agree") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Citizens likely to cheat") title( "Average Marginal Effect of Generalised Mistrust with 95% CIs" "(Double interaction)" , size(small)) yline(0) graphregion(color(white)) plotregion(color(white))
graph save cheat2.gph, replace

graph combine cost1.gph cheat1.gph cost2.gph cheat2.gph, rows(2) cols(2) graphregion(color(white)) plotregion(color(white) margin(small)) 
graph export Figure_E1.pdf, replace
erase cost1.gph 
erase cheat1.gph 
erase cost2.gph 
erase cheat2.gph


**** Heterogeneous Analysis main body of the paper
preserve

** gen HigherEducation
gen HigherEduc=1 if educ==5
replace HigherEduc=0 if educ<5
label define H1 1 "Higher Education" 0 "No Higher Education"  
label values HigherEduc H1
label variable HigherEduc "Education"

**gen quantile TopBotom 20-80
gen Top2080=2 if income<=2
replace Top2080=3 if income>=8
replace Top2080=1 if Top2080==.
label define top1 1 "19-79%" 2 "Bottom 20%" 3 "Top 80%" 
label values Top2080 top1
label variable Top2080 "Income Quintiles"


*** Higher Education Heterogeneity 
** top2080
global socioeconomic age agesq i.educ i.male i.citizen i.maritalstatus ib4.domicil hhmmb 
global incomevar i.employment i.isco 
global ideology lrscale religion 
areg basinc c.institutions c.trust2 i.Top2080 c.institutions#i.Top2080 c.trust2#i.Top2080  $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_F11.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(c.institutions c.trust2 i.Top2080 c.institutions#i.Top2080 c.trust2#i.Top2080) sortvar(c.institutions c.trust2 i.Top2080 c.institutions#i.Top2080 c.trust2#i.Top2080) dec(3) replace ctitle((1))
margins, dydx(institutions) at(Top2080=(2 1 3)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(small)) yscale(titlegap(*5)) xlab(1 "Bottom 20%" 2 "Range 21-79%" 3 "Top 80%") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Income Groups") title("Average Marginal Effect with 95% CIs" "Political Mistrust", size(medsmall)) yline(0) graphregion(color(white)) plotregion(color(white))
graph save Top2080P.gph, replace  
quietly: areg basinc c.institutions c.trust2 i.Top2080 c.institutions#i.Top2080 c.trust2#i.Top2080  $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(Top2080=(2 1 3)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(small)) yscale(titlegap(*5)) xlab(1 "Bottom 20%" 2 "Range 21-79%" 3 "Top 80%") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Income") title("Average Marginal Effect with 95% CIs" "Generalised Mistrust", size(medsmall)) yline(0)  graphregion(color(white)) plotregion(color(white))
graph save Top2080T.gph, replace  


*** Higher Education
global socioeconomic age agesq i.male i.citizen i.maritalstatus ib4.domicil hhmmb 
global incomevar income i.employment i.isco 
global ideology lrscale religion 
areg basinc c.institutions c.trust2 i.HigherEduc c.institutions#i.HigherEduc c.trust2#i.HigherEduc $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_F11.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(c.institutions c.trust2 i.HigherEduc c.institutions#i.HigherEduc c.trust2#i.HigherEduc) sortvar(c.institutions c.trust2 i.HigherEduc c.institutions#i.HigherEduc c.trust2#i.HigherEduc) dec(3) append ctitle((3))
margins, dydx(institutions) at(HigherEduc=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(small)) yscale(titlegap(*5)) xlab(1 "No Higher Education" 2 "Higher Education") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Education") title("Average Marginal Effect with 95% CIs" "Political Mistrust", size(medsmall)) graphregion(color(white)) plotregion(color(white))
graph save HigherEducP.gph, replace  
quietly: areg basinc c.institutions c.trust2 i.HigherEduc c.institutions#i.HigherEduc c.trust2#i.HigherEduc $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(HigherEduc=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(small)) yscale(titlegap(*5)) xlab(1 "No Higher Education" 2 "Higher Education") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Education") title("Average Marginal Effect with 95% CIs" "Generalised Mistrust", size(medsmall)) yline(0) graphregion(color(white)) plotregion(color(white))
graph save HigherEducT.gph, replace  


graph combine Top2080P.gph  Top2080T.gph HigherEducP.gph HigherEducT.gph, rows(2) cols(2) graphregion(color(white)) plotregion(color(white) margin(small))
graph export Figure_F11.pdf, replace
erase Top2080P.gph  
erase Top2080T.gph 
erase HigherEducP.gph 
erase HigherEducT.gph
restore

**** Heterogeneity

preserve
** gen religious
gen NotRel=0 if rlgblg==2
replace NotRel=1 if rlgblg==1
label define H4 0 "Not religious at all" 1 "Religious"  
label values NotRel H4
label variable NotRel "Religion"
tab NotRel

*** Religious
global socioeconomic age agesq i.educ i.male i.citizen i.maritalstatus ib4.domicil hhmmb 
global incomevar income i.employment i.isco 
global ideology lrscale 
areg basinc c.institutions c.trust2 i.NotRel c.institutions#i.NotRel c.trust2#i.NotRel $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_F21.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(c.institutions c.trust2 i.NotRel c.institutions#i.NotRel c.trust2#i.NotRel) sortvar(c.institutions c.trust2 i.NotRel c.institutions#i.NotRel c.trust2#i.NotRel) dec(3) append ctitle((1))
margins, dydx(institutions) at(NotRel=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(small)) yscale(titlegap(*5)) xlab(2 "Religious" 1 "Not Religious") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Religiosity", size(large)) title("Average Marginal Effect with 95% CIs" "Political Mistrust", size(large)) graphregion(color(white)) plotregion(color(white))
graph save ReligiousP.gph, replace  
quietly:areg basinc c.institutions c.trust2 i.NotRel c.institutions#i.NotRel c.trust2#i.NotRel $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(NotRel=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(small)) yscale(titlegap(*5))  xlab(2 "Religious" 1 "Not Religious") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Religiosity", size(large)) title("Average Marginal Effect with 95% CIs" "Generalised Mistrust", size(large)) yline(0)  graphregion(color(white)) plotregion(color(white))
graph save ReligiousT.gph, replace 

graph combine ReligiousP.gph ReligiousT.gph, rows(2) cols(1) graphregion(color(white)) plotregion(color(white) margin(small))  
graph export Figure_F21.pdf, replace
erase ReligiousP.gph 
erase ReligiousT.gph
restore

***** HETEROGENEOUS ANALYSIS Appendix 
preserve


* gen variable for interaction (Urban heterogeneity)
gen city=1 if domicil==1 | domicil==2
replace city=0 if domicil==3 | domicil==4 | domicil==5
label define H0 1 "City"  
label values city H0
label variable city "City"

** gen over 65 (people close to retirement)
gen Over65=1 if age>65
replace Over65=0 if age<=65
label define H2 1 "Older than 65" 0 "Younger than 65"  
label values Over65 H2
label variable Over65 "Over 65"


**** Table 5

*Urban heterogeneity
global socioeconomic age agesq i.educ i.male i.citizen i.maritalstatus hhmmb 
global incomevar income i.employment i.isco 
global ideology lrscale religion 
areg basinc c.institutions c.trust2 i.city c.institutions#i.city c.trust2#i.city $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_F31.tex,  label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(c.institutions c.trust2 i.city c.institutions#i.city c.trust2#i.city) sortvar(c.institutions c.trust2 i.city c.institutions#i.city c.trust2#i.city) dec(3) replace ctitle((1))
margins, dydx(institutions) at(city=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(small)) yscale(titlegap(*5)) xlab(1 "Rural or Small City" 2 "Big City") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Residential Area") title("Average Marginal Effect with 95% CIs" "Political Mistrust", size(medsmall)) graphregion(color(white)) plotregion(color(white))
graph save UrbanP.gph, replace  
quietly: areg basinc c.institutions c.trust2 i.city c.institutions#i.city c.trust2#i.city $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(city=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(small)) yscale(titlegap(*5))  xlab(1 "Rural or Small City" 2 "Big City") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Residential Area") title("Average Marginal Effect with 95% CIs" "Generalised Mistrust", size(medsmall)) yline(0) graphregion(color(white)) plotregion(color(white))
graph save UrbanT.gph, replace  

*** Over65 Heterogeneity
global socioeconomic i.educ i.male i.citizen i.maritalstatus ib4.domicil hhmmb 
global incomevar income i.employment i.isco 
global ideology lrscale religion 
areg basinc c.institutions c.trust2 i.Over65 c.institutions#i.Over65 c.trust2#i.Over65 $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_F31.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(c.institutions c.trust2 i.Over65 c.institutions#i.Over65 c.trust2#i.Over65) sortvar(c.institutions c.trust2 i.Over65 c.institutions#i.Over65 c.trust2#i.Over65) dec(3) append ctitle((2))
margins, dydx(institutions) at(Over65=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(small)) yscale(titlegap(*5)) xlab(1 "Under 65" 2 "Over 65") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Age") title("Average Marginal Effect with 95% CIs" "Political Mistrust", size(medsmall)) graphregion(color(white)) plotregion(color(white))
graph save Over65P.gph, replace  
quietly: areg basinc c.institutions c.trust2 i.Over65 c.institutions#i.Over65 c.trust2#i.Over65 $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(Over65=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(small)) yscale(titlegap(*5))  xlab(1 "Under 65" 2 "Over 65") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Age") title("Average Marginal Effect with 95% CIs" "Generalised Mistrust", size(medsmall)) yline(0)  graphregion(color(white)) plotregion(color(white))
graph save Over65T.gph, replace 



*** citizen
global socioeconomic age agesq i.educ i.male i.maritalstatus ib4.domicil hhmmb 
global incomevar income i.employment i.isco 
global ideology lrscale religion 
areg basinc  c.institutions c.trust2 i.citizen c.institutions#i.citizen c.trust2#i.citizen $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_F31.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(c.institutions c.trust2 i.citizen c.institutions#i.citizen c.trust2#i.citizen) sortvar(c.institutions c.trust2 i.citizen c.institutions#i.citizen c.trust2#i.citizen) dec(3) append ctitle((3))
margins, dydx(institutions) at(citizen=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(small)) yscale(titlegap(*5)) xlab(1 "Noncitizen" 2 "Citizen") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Citizenship") title("Average Marginal Effect with 95% CIs" "Political Mistrust", size(medsmall)) yline(0) order(2.citizen 1.citizen) graphregion(color(white)) plotregion(color(white))
graph save citizenP.gph, replace  
quietly:areg basinc  c.institutions c.trust2 i.citizen c.institutions#i.citizen c.trust2#i.citizen $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(citizen=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(small)) yscale(titlegap(*5))  xlab(1 "Noncitizen" 2 "Citizen") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Citizenship") title("Average Marginal Effect with 95% CIs" "Generalised Mistrust", size(medsmall)) yline(0) order(2.citizen 1.citizen) graphregion(color(white)) plotregion(color(white))
graph save citizenT.gph, replace 

*** male
global socioeconomic age agesq i.educ i.citizen i.maritalstatus ib4.domicil hhmmb 
global incomevar income i.employment i.isco 
global ideology lrscale religion 
areg basinc  c.institutions c.trust2 i.male c.institutions#i.male c.trust2#i.male $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_F31.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(c.institutions c.trust2 i.male c.institutions#i.male c.trust2#i.male) sortvar(c.institutions c.trust2 i.male c.institutions#i.male c.trust2#i.male) dec(3) append ctitle((4))
margins, dydx(institutions) at(male=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(small)) yscale(titlegap(*5)) xlab(1 "Female" 2 "Male") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Gender") title("Average Marginal Effect with 95% CIs" "Political Mistrust", size(medsmall)) graphregion(color(white)) plotregion(color(white))
graph save GenderP.gph, replace  
quietly:areg basinc  c.institutions c.trust2 i.male c.institutions#i.male c.trust2#i.male $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(male=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(small)) yscale(titlegap(*5))  xlab(1 "Female" 2 "Male") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Gender") title("Average Marginal Effect with 95% CIs" "Generalised Mistrust", size(medsmall)) yline(0) graphregion(color(white)) plotregion(color(white))
graph save GenderT.gph, replace 

graph combine UrbanP.gph  UrbanT.gph Over65P.gph Over65T.gph, rows(2) cols(2) graphregion(color(white)) plotregion(color(white) margin(small)) 
graph export Figure_F31.pdf, replace

graph combine citizenP.gph citizenT.gph GenderP.gph GenderT.gph, rows(2) cols(2) graphregion(color(white)) plotregion(color(white) margin(small)) 
graph export Figure_F32.pdf, replace
erase UrbanP.gph  
erase UrbanT.gph 
erase Over65P.gph 
erase Over65T.gph
erase citizenP.gph 
erase citizenT.gph 
erase GenderP.gph 
erase GenderT.gph
restore


**** OTHER ROBUSTNESS CHECKS
*** robustness political trusts

global socioeconomic age agesq i.educ i.male i.citizen i.maritalstatus ib4.domicil hhmmb 
global incomevar income i.employment i.isco 
global ideology lrscale religion 


egen polgroup= group(trstplt trstprt trstprl)


areg basinc trstplt trust2 $incomevar $socioeconomic $ideology if polgroup!=.,absorb(region2) vce(cluster region2)
outreg2 using Table_G22.tex,  label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2  keep(trstplt trstprt trstprl trust2 income) sortvar(trstplt trstprt trstprl trust2 income) dec(3) replace ctitle((1)) 
areg basinc trstprt trust2 $incomevar $socioeconomic $ideology if polgroup!=.,absorb(region2) vce(cluster region2)
outreg2 using Table_G22.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2  keep(trstplt trstprt trstprl trust2 income) sortvar(trstplt trstprt trstprl trust2 income) dec(3) append ctitle((2)) 
areg basinc trstprl trust2 $incomevar $socioeconomic $ideology if polgroup!=.,absorb(region2) vce(cluster region2)
outreg2 using Table_G22.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2  keep(trstplt trstprt trstprl trust2 income) sortvar(trstplt trstprt trstprl trust2 income) dec(3) append ctitle((3))



**** robustnes social trust
egen trustgroup= group(ppltrst pplfair pplhlp)
areg basinc ppltrst institutions  $incomevar $socioeconomic $ideology if trustgroup!=.,absorb(region2) vce(cluster region2)
outreg2 using Table_G21.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2  keep(ppltrst pplfair pplhlp institutions income) sortvar(ppltrst pplfair pplhlp institutions income) dec(3) replace ctitle((1)) 
areg basinc pplfair institutions  $incomevar $socioeconomic $ideology if trustgroup!=.,absorb(region2) vce(cluster region2)
outreg2 using Table_G21.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2  keep(ppltrst pplfair pplhlp institutions income) sortvar(ppltrst pplfair pplhlp institutions income) dec(3) append ctitle((2)) 
areg basinc pplhlp institutions  $incomevar $socioeconomic $ideology if trustgroup!=.,absorb(region2) vce(cluster region2)
outreg2 using Table_G21.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2  keep(ppltrst pplfair pplhlp institutions income) sortvar(ppltrst pplfair pplhlp institutions income) dec(3) append ctitle((3)) 

**** robustness UBI 1-4 scale, probit
oprobit basinc institutions i.region2, vce(cluster region2)
outreg2 using Table_G41.tex, label addtext(Region FE, YES, ) nocons e(ll)  drop(i.region2) sortvar(institutions trust2) dec(3) replace ctitle((1))
oprobit basinc trust2 i.region2, vce(cluster region2)
outreg2 using Table_G41.tex, label addtext(Region FE, YES, ) nocons e(ll)  drop(i.region2) sortvar(institutions trust2) dec(3) append ctitle((2))
oprobit basinc institutions trust2 i.region2, vce(cluster region2)
outreg2 using Table_G41.tex, label addtext(Region FE, YES, ) nocons  e(ll)  drop(i.region2) sortvar(institutions trust2) dec(3) append ctitle((3)) 
oprobit basinc institutions trust2 $incomevar i.region2, vce(cluster region2)
outreg2 using Table_G41.tex, label addtext(Region FE, YES, Income Controls, YES) nocons  e(ll) drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(institutions trust2 income) dec(3) append ctitle((4))
oprobit basinc institutions trust2 $incomevar $socioeconomic i.region2, vce(cluster region2)
outreg2 using Table_G41.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES,) nocons  e(ll) drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(institutions trust2 income) dec(3) append ctitle((5))  
oprobit basinc institutions c.trust2 $incomevar $socioeconomic $ideology i.region2, vce(cluster region2)
outreg2 using Table_G41.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons e(ll) drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(institutions trust2 income) dec(3) append ctitle((6)) 


*** robustness country
areg basinc institutions,absorb(country) vce(cluster country)
outreg2 using Table_D11.tex, label addtext(Country FE, YES, ) nocons adjr2  drop(i.country) sortvar(institutions trust2) dec(3) replace ctitle((1)) 
areg basinc trust2,absorb(country) vce(cluster country)
outreg2 using Table_D11.tex, label addtext(Country FE, YES, ) nocons adjr2  drop(i.country) sortvar(institutions trust2) dec(3) append ctitle((2)) 
areg basinc institutions trust2 ,absorb(country) vce(cluster country)
outreg2 using Table_D11.tex, label addtext(Country FE, YES, ) nocons adjr2  drop(i.country) sortvar(institutions trust2) dec(3) append ctitle((3))  
areg basinc institutions trust2  $incomevar ,absorb(country) vce(cluster country)
outreg2 using Table_D11.tex, label addtext(Country FE, YES, Income Controls, YES) nocons adjr2  drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(institutions trust2 income) dec(3) append ctitle((4))
areg basinc institutions trust2 $incomevar $socioeconomic ,absorb(country) vce(cluster country)
outreg2 using Table_D11.tex, label addtext(Country FE, YES, Income Controls, YES, Individual Controls, YES,) nocons adjr2  drop($socioeconomic $ideology i.employment i.isco i.country) sortvar(institutions trust2 income) dec(3) append ctitle((5))  
areg basinc institutions c.trust2 $incomevar $socioeconomic $ideology ,absorb(country) vce(cluster country)
outreg2 using Table_D11.tex, label addtext(Country FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2  drop($socioeconomic $ideology i.employment i.isco i.country) sortvar(institutions trust2 income) dec(3) append ctitle((6))

*********************************
***** Income categorical variable
*********************************
global socioeconomic age agesq i.educ i.male i.citizen i.maritalstatus ib4.domicil hhmmb 
global incomevar i.income i.employment i.isco 
global ideology lrscale religion 

areg basinc institutions,absorb(region2) vce(cluster region2)
outreg2 using TABLE_G43.tex, label addtext(Region FE, YES, ) nocons adjr2  drop(i.region2) sortvar(institutions trust2) dec(3) replace ctitle((1)) 
areg basinc institutions trust2 ,absorb(region2) vce(cluster region2)
outreg2 using TABLE_G43.tex, label addtext(Region FE, YES, ) nocons adjr2  drop(i.region2) sortvar(institutions trust2) dec(3) append ctitle((2)) 
areg basinc institutions trust2 $incomevar ,absorb(region2) vce(cluster region2)
outreg2 using TABLE_G43.tex, label addtext(Region FE, YES, Income Controls, YES) nocons adjr2  drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(institutions trust2 i.income) dec(3) append ctitle((3))
areg basinc institutions trust2 $incomevar $socioeconomic ,absorb(region2) vce(cluster region2)
outreg2 using TABLE_G43.tex, tex label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES,) nocons adjr2  drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(institutions trust2 i.income) dec(3) append ctitle((4))  
areg basinc institutions c.trust2 $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using TABLE_G43.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(institutions trust2 i.income) dec(3) append ctitle((5)) 


*** Generate cheating variable using PCA (APPENDIX ROBUSTNESS CHECKS)

**** GENERATE THE PCA VARIABLE
global misb bennent uentrjb
pca $misb
predict misb2, score
estat kmo
egen sum_misb= rowtotal($misb )


*** label the new variable
label variable misb2 "Misbehaviour (PCA)"


* Generate correlation table
estpost correlate $misb,  matrix 
est sto storagename
esttab * using Table_E23.tex, label replace unstack not noobs compress 

**** Generate the pca table

pca $misb , components(2)

matrix ev = e(Ev)'
matrix roweq ev = ""
matrix colnames ev = "Eigenvalue"

matrix d = ev - ( ev[2...,1] \ . )
matrix colnames d = "Difference"

matrix p = ev[1...,1] / e(trace)
matrix colnames p = "Proportion"


matrix c = J(e(trace),1,0)
matrix c[1,1] = p[1,1]
forvalues i=2/`e(trace)' {
    matrix c[`i',1] = c[`=`i'-1',1] + p[`i',1]
    }
matrix colnames c = "Cumulative"

matrix t = ( ev , d , p , c )
matrix list t

estadd matrix table = t

esttab, ///
    cells("table[Eigenvalue](t) table[Difference](t) table[Proportion](t fmt(4)) table[Cumulative](t fmt(4))") ///
    nogap noobs nonumber nomtitle
esttab using Table_E21_E22.tex, tex label replace ///
    cells("table[Eigenvalue](t) table[Difference](t) table[Proportion](t fmt(4)) table[Cumulative](t fmt(4))") ///
    nogap noobs nonumber nomtitle

esttab, ///
    cells("L[Comp1](t) L[Comp2](t) Psi[Unexplained]") ///
    nogap noobs nonumber nomtitle
esttab using Table_E21_E22.tex, tex label append ///
    cells("L[Comp1](t) L[Comp2](t) Psi[Unexplained]") ///
    nogap noobs nonumber nomtitle

screeplot, yline(1) ci(het) graphregion(color(white)) lcolor(edkblue) mcolor(edkblue)
graph export Figure_E21.pdf, replace 


***************************
global socioeconomic age agesq i.educ i.male i.citizen i.maritalstatus ib4.domicil hhmmb 
global incomevar income i.employment i.isco 
global ideology lrscale religion 

* pca for costs

areg basinc c.institutions c.trust2 i.cost misb2 $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_E24.tex,  label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 i.cost c.misb2) sortvar(institutions trust2 i.cost i.cheat) dec(3) replace ctitle((1))
areg basinc c.institutions c.trust2##c.misb2 i.cost $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_E24.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 i.cost misb2 c.institutions#i.cost c.trust2#c.misb2) sortvar(institutions trust2 i.cost misb2 c.trust2#c.misb2) dec(3) append ctitle((2))
areg basinc c.institutions##i.cost c.trust2##c.misb2 $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_E24.tex, label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 i.cost c.misb2 c.institutions#i.cost c.trust2#c.misb2) sortvar(institutions trust2 i.cost misb2 c.trust2#c.misb2) dec(3) append ctitle((3))

*** plot the graph of the interactions ************


margins, dydx(trust2) at(misb2=(-2.319832(0.5430993)3.111161))
marginsplot , xlabel(-2.319832 `"-2.32"' -1.2336334 `" -1.23 "' -0.1474348 `" -0.15 "'  0.9387638 `" -0.94 "'  2.0249624 `" 2.02 "' 3.111161 `" 3.11 "', labsize(small)) ylabel(, labsize(vsmall)) yline(0) ciopt(fcolor(%10) lpattern(dash) lcolor(black)) recast(line) recastci(rarea) title("Average Marginal Effect with 95% Cl",size(medsmall)) ytitle("Effects on Linear Prediction", size(medsmall)) graphregion(color(white)) plotregion(color(white))
graph export Figure_E22.pdf, replace





**** UNIQUE PCA TO SHOW THAT TWO COMPONENTS ARE EXTRACTED

*export pca table for 
******

pca $t $trust , components(6)

matrix ev = e(Ev)'
matrix roweq ev = ""
matrix colnames ev = "Eigenvalue"

matrix d = ev - ( ev[2...,1] \ . )
matrix colnames d = "Difference"

matrix p = ev[1...,1] / e(trace)
matrix colnames p = "Proportion"

matrix c = J(e(trace),1,0)
matrix c[1,1] = p[1,1]
forvalues i=2/`e(trace)' {
    matrix c[`i',1] = c[`=`i'-1',1] + p[`i',1]
    }
matrix colnames c = "Cumulative"

matrix t = ( ev , d , p , c )
matrix list t

estadd matrix table = t

esttab, ///
    cells("table[Eigenvalue](t) table[Difference](t) table[Proportion](t fmt(4)) table[Cumulative](t fmt(4))") ///
    nogap noobs nonumber nomtitle
esttab using Table_G11_G22.tex, tex replace ///
    cells("table[Eigenvalue](t) table[Difference](t) table[Proportion](t fmt(4)) table[Cumulative](t fmt(4))") ///
    nogap noobs nonumber nomtitle

esttab, ///
    cells("L[Comp1](t) L[Comp2](t) L[Comp3](t) L[Comp4](t) L[Comp5](t) L[Comp6](t) Psi[Unexplained]") ///
    nogap noobs nonumber nomtitle
esttab using Table_G11_G22.tex, tex label append ///
    cells("L[Comp1](t) L[Comp2](t) L[Comp3](t) L[Comp4](t) L[Comp5](t) L[Comp6](t) Psi[Unexplained]") ///
    nogap noobs nonumber nomtitle

screeplot, yline(1) ci(het) graphregion(color(white)) lcolor(edkblue) mcolor(edkblue)
graph export Figure_G11.pdf, replace 

***** create table pwcorr

estpost correlate $trust,  matrix 
est sto storagename
esttab * using Table_G13.tex, label replace unstack not noobs compress 

***** only mistrust

****** correlation and multicollinearity
pwcorr  trust2 institutions, star(0.0001) sig
estpost correlate trust2 institutions,  matrix 
est sto storagename
esttab * using correlationgen.tex, label replace unstack not noobs compress 

qui: areg basinc institutions trust2 ,absorb(region2) vce(cluster region2)
vif, uncentered
qui: areg basinc institutions trust2 $incomevar,absorb(region2) vce(cluster region2)
vif, uncentered
qui: areg basinc institutions trust2 $incomevar $socioeconomic,absorb(region2) vce(cluster region2)
vif, uncentered
qui: areg basinc institutions c.trust2 $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
vif, uncentered

***** TABLE 1
areg basinc trust2,absorb(region2) vce(cluster region2)
outreg2 using Table_D12.tex, tex label addtext(Region FE, YES, ) nocons adjr2  drop(i.region2) sortvar(trust2) dec(3) replace ctitle((1))
areg basinc trust2 $incomevar ,absorb(region2) vce(cluster region2)
outreg2 using Table_D12.tex, tex label addtext(Region FE, YES, Income Controls, YES) nocons adjr2  drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(trust2 income) dec(3) append ctitle((2))
areg basinc trust2 $incomevar $socioeconomic ,absorb(region2) vce(cluster region2)
outreg2 using Table_D12.tex, tex label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES,) nocons adjr2  drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(trust2 income) dec(3) append ctitle((3))  
areg basinc trust2 $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_D12.tex, tex label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(trust2 income) dec(3) append ctitle((4)) 


**** Generalised Mistrust x Welfare cost

areg basinc c.institutions c.trust2##i.cost $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_E31.tex, tex label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 i.cost c.trust2#i.cost) sortvar(institutions trust2 i.cost i.cheat) dec(3) replace ctitle((1))
areg basinc c.institutions##i.cost c.trust2##i.cost $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_E31.tex, tex label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 i.cost c.trust2#i.cost c.institutions#i.cost) sortvar(institutions trust2 i.cost i.cheat) dec(3) append ctitle((2))
areg basinc c.institutions c.trust2##i.cost i.cheat $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_E31.tex, tex label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 i.cost c.trust2#i.cost i.cheat) sortvar(institutions trust2 i.cost i.cheat) dec(3) append ctitle((3))
areg basinc c.institutions##i.cost c.trust2##i.cost i.cheat $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_E31.tex, tex label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 i.cost c.trust2#i.cost c.institutions#i.cost i.cheat) sortvar(institutions trust2 i.cost i.cheat) dec(3) append ctitle((4))


******** plot the marginal effects

areg basinc c.institutions c.trust2##i.cost $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(cost=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(vsmall)) xlab(1 "Do not agree" 2 "Agree") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Social benefits/services place too great strain" "on the economy") title( "Average Marginal Effect of Generalised Mistrust with 95% CIs" "Column. (1)", size(small)) yline(0) graphregion(color(white)) plotregion(color(white))
graph save mistrustxcost1.gph, replace  

areg basinc c.institutions##i.cost c.trust2##i.cost $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(cost=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(vsmall)) xlab(1 "Do not agree" 2 "Agree") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Social benefits/services place too great strain" "on the economy") title( "Average Marginal Effect of Generalised Mistrust with 95% CIs" "Column. (2)", size(small)) yline(0) graphregion(color(white)) plotregion(color(white))
graph save mistrustxcost2.gph, replace  

areg basinc c.institutions c.trust2##i.cost i.cheat $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(cost=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(vsmall)) xlab(1 "Do not agree" 2 "Agree") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Social benefits/services place too great strain" "on the economy") title( "Average Marginal Effect of Generalised Mistrust with 95% CIs" "Column. (3)", size(small)) yline(0) graphregion(color(white)) plotregion(color(white))
graph save mistrustxcost3.gph, replace 

areg basinc c.institutions##i.cost c.trust2##i.cost i.cheat $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(cost=(0 1)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(vsmall)) xlab(1 "Do not agree" 2 "Agree") xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Social benefits/services place too great strain" "on the economy") title( "Average Marginal Effect of Generalised Mistrust with 95% CIs" "Column. (4)", size(small)) yline(0) graphregion(color(white)) plotregion(color(white))
graph save mistrustxcost4.gph, replace  

graph combine mistrustxcost1.gph  mistrustxcost2.gph mistrustxcost3.gph mistrustxcost4.gph, rows(2) cols(2) graphregion(color(white)) plotregion(color(white) margin(small)) 
graph export Figure_E31.pdf, replace
erase mistrustxcost1.gph
erase mistrustxcost2.gph 
erase mistrustxcost3.gph 
erase mistrustxcost4.gph

**** Non-standardized basinc

areg NS_basinc institutions,absorb(region2) vce(cluster region2)
outreg2 using Table_G42.tex, tex label addtext(Region FE, YES, ) nocons adjr2  drop(i.region2) sortvar(institutions trust2) dec(3) replace ctitle((1))
areg NS_basinc trust2,absorb(region2) vce(cluster region2)
outreg2 using Table_G42.tex, tex label addtext(Region FE, YES, ) nocons adjr2  drop(i.region2) sortvar(institutions trust2) dec(3) append ctitle((2))
areg NS_basinc institutions trust2 ,absorb(region2) vce(cluster region2)
outreg2 using Table_G42.tex, tex label addtext(Region FE, YES, ) nocons adjr2  drop(i.region2) sortvar(institutions trust2) dec(3) append ctitle((3)) 
areg NS_basinc institutions trust2 $incomevar ,absorb(region2) vce(cluster region2)
outreg2 using Table_G42.tex, tex label addtext(Region FE, YES, Income Controls, YES) nocons adjr2  drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(institutions trust2 income) dec(3) append ctitle((4))
areg NS_basinc institutions trust2 $incomevar $socioeconomic ,absorb(region2) vce(cluster region2)
outreg2 using Table_G42.tex, tex label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES,) nocons adjr2  drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(institutions trust2 income) dec(3) append ctitle((5))  
areg NS_basinc institutions c.trust2 $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_G42.tex, tex label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 drop($socioeconomic $ideology i.employment i.isco i.region2) sortvar(institutions trust2 income) dec(3) append ctitle((6)) 




******************************************
*********FIND OUTLIERS********************
****************************************** 

**** Remove one country per time

*** Run the loops once per time, otherwise it shows the coefficients for trust twice in the second graph


**** political mistrust

levelsof country, local(levels)             
foreach i of local levels {                
quietly eststo: areg basinc institutions c.trust2 $incomevar $socioeconomic $ideology if country !=`i',absorb(region2) vce(cluster region2)

    est sto y`i'                
        *build macro for coefplot                
        loc cc `"`cc' (y`i', mlabpos(9) mlabangle(vertical) mlabsize(vsmall) pstyle(p1) msize(small) mlabcolor(black) mlabgap(3pt))"'        
    loc `++i'                
    }        
                
**from macro in loop                
      di `"`cc'"'                
      coefplot `cc', keep(institutions) graphregion(color(white)) bgcolor(white) label vertical title(" ", size(large)) xtitle("Political Mistrust", size(large)) ytitle("Coefficients with s.e.")  legend(off) coeflabels(institutions= " ")    
graph save outlier_country_polimistrust.gph, replace 

drop _est_y* _est_est*

* Stop and run the second loop otherwise it shows the coefficients for trust twice in the second graph
STOP

***** generalised mistrust
levelsof country, local(levels)             
foreach v of local levels {                
quietly eststo: areg basinc institutions trust2 $incomevar $socioeconomic $ideology if country !=`v',absorb(region2) vce(cluster region2)

    est sto y`v'                
        *build macro for coefplot                
        loc cc `"`cc' (y`v', mlabpos(9) mlabangle(vertical) mlabsize(vsmall) pstyle(p1) msize(small) mlabcolor(black) mlabgap(3pt))"'        
    loc `++v'                
    }   
      di `"`cc'"'                
      coefplot `cc', yline(0) keep(trust2) graphregion(color(white)) bgcolor(white) label vertical title(" ", size(large)) xtitle("Generalised Mistrust", size(large)) ytitle("Coefficients with s.e.")  legend(off) coeflabels(trust2= " ")    
graph save outlier_country_genmitrust.gph, replace 

drop _est_y* _est_est*


graph combine outlier_country_polimistrust.gph outlier_country_genmitrust.gph, rows(2) cols(1) graphregion(color(white)) plotregion(color(white) margin(small)) title("Estimation with one country left out", size(large)) 
graph export Figure_G31.pdf, replace 
erase outlier_country_polimistrust.gph
erase outlier_country_genmitrust.gph

****** POPULISM APPENDIX
**** ANTI-IMMIGRANTS
global antiimmigrants imbgeco imueclt imwbcnt
revrs $antiimmigrants , replace

pca $antiimmigrants
estat loadings
predict anti, score
estat kmo
egen sum_anti= rowtotal($antiimmigrants ) 
pwcorr $antiimmigrants anti sum_anti, star(0.01) sig
label variable anti "Populism (Anti-immigrants)"

*standardization
rename anti antiT
egen mantiT= mean(antiT)
egen sdantiT=sd(antiT)
replace anti= (antiT-mantiT)/sdantiT

**** anti eu
revrs trstep , replace
label variable trstep "Populism (Euroscetiscism)"


*** authoritarism

revrs ipstrgv, replace
label variable ipstrgv "Populism (Authoritarianism)"

***** centrist, left, right

gen centrist=1 if lrscale==5
replace centrist=2 if lrscale>=6 //right
replace centrist=3 if lrscale<=4  //leftist
label define H3 1 "Centrist" 2 "Rightist" 3 "Leftist" 
label values centrist H3
label variable centrist "Political View"

***** regressions and tables

*** create marginsplot for political ideology
global socioeconomic age agesq i.educ i.male i.citizen i.maritalstatus ib4.domicil hhmmb 
global incomevar income i.employment i.isco 
global ideology religion 
areg basinc c.institutions c.trust2 i.centrist c.institutions#i.centrist c.trust2#i.centrist $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(institutions) at(centrist=(3 1 2)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(medsmall)) yscale(titlegap(*5)) xlab(1 "Leftist" 2 "Centrist" 3 "Rightist", labsize(large)) xscale(titlegap(*5)) ylab(, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Political spectrum", size(large)) title("Average Marginal Effect with 95% CIs" "Political Mistrust", size(large)) yline(0) graphregion(color(white)) plotregion(color(white))
graph save PoliticalP.gph, replace  
quietly:areg basinc c.institutions c.trust2 i.centrist c.institutions#i.centrist c.trust2#i.centrist $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(centrist=(3 1 2)) post
coefplot, vert ytitle("Effect on Linear Prediction", size(medsmall)) yscale(titlegap(*5))  xlab(1 "Leftist" 2 "Centrist" 3 "Rightist", labsize(large)) xscale(titlegap(*5)) ylab(-0.08(0.02)0.04, labsize(vsmall)) ciopts(recast(rcap)) xtitle("Political spectrum", size(large)) title("Average Marginal Effect with 95% CIs" "Generalised Mistrust", size(large)) yline(0) graphregion(color(white)) plotregion(color(white))
graph save PoliticalT.gph, replace 

graph combine PoliticalP.gph PoliticalT.gph , rows(2) cols(1) graphregion(color(white)) plotregion(color(white) margin(small)) 
graph export Figure_5.pdf, replace
erase PoliticalP.gph 
erase PoliticalT.gph

*** regression table

global socioeconomic age agesq i.educ i.male i.citizen i.maritalstatus ib4.domicil hhmmb 
global incomevar income i.employment i.isco 
global ideology religion 
quietly:areg basinc c.institutions c.trust2 i.centrist c.institutions#i.centrist c.trust2#i.centrist $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_4.tex, tex label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(c.institutions c.trust2 i.centrist c.institutions#i.centrist c.trust2#i.centrist) sortvar(c.institutions c.trust2 i.centrist c.institutions#i.centrist c.trust2#i.centrist c.trust2#c.anti c.institutions#c.anti c.trust2#c.trstep c.institutions#c.trstep c.trust2#c.ipstrgv c.institutions#c.ipstrgv) dec(3) replace ctitle((1))

global socioeconomic age agesq i.educ i.male i.citizen i.maritalstatus ib4.domicil hhmmb 
global incomevar income i.employment i.isco 
global ideology lrscale religion 
quietly:areg basinc c.institutions c.trust2 anti c.institutions#c.anti c.trust2#c.anti $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_4.tex, tex label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 c.trust2#c.anti c.institutions#c.anti) sortvar(c.institutions c.trust2 i.centrist c.institutions#i.centrist c.trust2#i.centrist c.trust2#c.anti c.institutions#c.anti c.trust2#c.trstep c.institutions#c.trstep c.trust2#c.ipstrgv c.institutions#c.ipstrgv)  dec(3) append ctitle((2)) 

quietly:areg basinc c.institutions c.trust2 trstep c.institutions#c.trstep c.trust2#c.trstep $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_4.tex, tex label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 c.trust2#c.trstep c.institutions#c.trstep) sortvar(c.institutions c.trust2 i.centrist c.institutions#i.centrist c.trust2#i.centrist c.trust2#c.anti c.institutions#c.anti c.trust2#c.trstep c.institutions#c.trstep c.trust2#c.ipstrgv c.institutions#c.ipstrgv) dec(3) append ctitle((3)) 

quietly:areg basinc c.institutions c.trust2 ipstrgv c.institutions#c.ipstrgv c.trust2#c.ipstrgv $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
outreg2 using Table_4.tex, tex label addtext(Region FE, YES, Income Controls, YES, Individual Controls, YES, Ideology Controls, YES) nocons adjr2 keep(institutions c.trust2 c.trust2#c.ipstrgv c.institutions#c.ipstrgv) sortvar(c.institutions c.trust2 i.centrist c.institutions#i.centrist c.trust2#i.centrist c.trust2#c.anti c.institutions#c.anti c.trust2#c.trstep c.institutions#c.trstep c.trust2#c.ipstrgv c.institutions#c.ipstrgv) dec(3) append ctitle((4))

*** margins plot anti immigrants

quietly:areg basinc c.institutions c.trust2 anti c.institutions#c.anti c.trust2#c.anti $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(anti=(-2.159575 (1.12410025)2.336826))
marginsplot , xlabel(-2.159575  `"-2.16"' -1.03547475 `" -1.04 "'  0.0886255 `" -0.09 "' 1.21272575 `" 1.21 "' 2.336826 `" 2.33 "', labsize(small)) ylabel(, labsize(vsmall)) yline(0) ciopt(fcolor(%10) lpattern(dash) lcolor(black)) recast(line) recastci(rarea) title("Average Marginal Effect with 95% Cl" "Generalised mistrust",size(medium)) ytitle("Effects on Linear Prediction", size(small)) graphregion(color(white)) plotregion(color(white))
graph save antiimmigrantstrust.gph, replace  

margins, dydx(institutions) at(anti=(-3.343795 (1.3924074)3.618242))
marginsplot , xlabel(-3.343795 `"-3.34"' -1.6901196 `" -1.69 "'  -0.036444 `" -0.04 "' 1.6172314 `" 1.62 "'  3.62 `" 3.62 "', labsize(small)) ylabel(, labsize(vsmall)) yline(0) ciopt(fcolor(%10) lpattern(dash) lcolor(black)) recast(line) recastci(rarea) title("Average Marginal Effect with 95% Cl" "Political mistrust",size(medium)) ytitle("Effects on Linear Prediction", size(small)) graphregion(color(white)) plotregion(color(white))
graph save antiimmigrantspoli.gph, replace  


*** TRUST IN EUROPEAN PARLIAMENT

quietly:areg basinc c.institutions c.trust2 trstep c.institutions#c.trstep c.trust2#c.trstep $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(trstep=(0(1)10))
marginsplot , xlabel(0 `"0"' 2 `" 2 "'  4 `" 4 "' 6 `" 6 "'  8 `" 8 "' 10 `" 10 "', labsize(small)) ylabel(, labsize(vsmall)) yline(0) ciopt(fcolor(%10) lpattern(dash) lcolor(black)) recast(line) recastci(rarea) title("Average Marginal Effect with 95% Cl" "Generalised mistrust",size(medium)) ytitle("Effects on Linear Prediction", size(small)) graphregion(color(white)) plotregion(color(white))
graph save eutrust.gph, replace  

margins, dydx(institutions) at(trstep=(0(1)10))
marginsplot , xlabel(0 `"0"' 2 `" 2 "'  4 `" 4 "' 6 `" 6 "'  8 `" 8 "' 10 `" 10 "', labsize(small)) ylabel(, labsize(vsmall)) yline(0) ciopt(fcolor(%10) lpattern(dash) lcolor(black)) recast(line) recastci(rarea) title("Average Marginal Effect with 95% Cl" "Political mistrust",size(medium)) ytitle("Effects on Linear Prediction", size(small)) graphregion(color(white)) plotregion(color(white))
graph save eupoli.gph, replace  


**** authoritarism

quietly:areg basinc c.institutions c.trust2 ipstrgv c.institutions#c.ipstrgv c.trust2#c.ipstrgv $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(trust2) at(ipstrgv=(1(1)6)) 
marginsplot , xlabel(1 `" 1 "' 2 `" 2 "'  3 `" 3 "' 4`" 4 "' 5 `" 5 "' 6 `" 6 "' , labsize(small)) ylabel(, labsize(vsmall)) yline(0) ciopt(fcolor(%10) lpattern(dash) lcolor(black)) recast(line) recastci(rarea) title("Average Marginal Effect with 95% Cl" "Generalised mistrust",size(medium)) ytitle("Effects on Linear Prediction", size(small)) graphregion(color(white) ) plotregion(color(white))
graph save stronggovatrust.gph, replace  
areg basinc c.institutions c.trust2 c.ipstrgv c.institutions#c.ipstrgv c.trust2#c.ipstrgv $incomevar $socioeconomic $ideology ,absorb(region2) vce(cluster region2)
margins, dydx(institutions) at(ipstrgv=(1(1)6)) 
marginsplot , xlabel(1 `" 1 "' 2 `" 2 "'  3 `" 3 "' 4`" 4 "' 5 `" 5 "' 6 `" 6 "', labsize(small)) ylabel(, labsize(vsmall)) yline(0) ciopt(fcolor(%10) lpattern(dash) lcolor(black)) recast(line) recastci(rarea) title("Average Marginal Effect with 95% Cl" "Political mistrust",size(medium)) ytitle("Effects on Linear Prediction", size(small)) graphregion(color(white)) plotregion(color(white))
graph save stronggovapoli.gph, replace  

graph combine antiimmigrantstrust.gph antiimmigrantspoli.gph eutrust.gph eupoli.gph stronggovatrust.gph stronggovapoli.gph, rows(3) cols(2) graphregion(color(white)) plotregion(color(white) margin(small)) 
graph export Figure_6.pdf, replace
erase antiimmigrantstrust.gph 
erase antiimmigrantspoli.gph 
erase eutrust.gph 
erase eupoli.gph 
erase stronggovatrust.gph 
erase stronggovapoli.gph

*********** end ********************

