
******************************************************************
*
* PANEL-ANALYSIS (using an artificial dataset)
*
* JOSEF BR‹DERL, UNIVERSITY OF MANNHEIM, March 2015
*
******************************************************************

* This DO-File contains the STATA commands for the
* intuitive introduction via an example

* These are the variables in the dataset:
* id    person identifier
* time  # panel wave
* wage  monthly wage in Euro
* marr  marriage dummy (1=married)

* The further variables are variants of these, used for special problems

*************************************
** Preliminaries    *****************
*************************************
clear 
set more off

* Load data
cd  "K:\Vorlesung PDA\Stata Beispiele\Artificial Data\"    //Adapt this path!
use "Wage Premium.dta", clear

***** Declare data to be panel data *****
xtset id time


*****************************************
* Descriptives
*****************************************
* Listing the data
list id time wage marr, separator(6)

* Describing the data
xtdes
xtsum wage
xttab marr
xtline wage, overlay


******************************************
* Plotting the data
******************************************
twoway  (scatter wage time, ylabel(0(1000)5000, grid angle(0))           ///
                 ymtick(500(1000)4500, grid) c(L))                       ///
        (scatter wage time if marr==1, c(L)),                            ///
        legend(label(1 "before marriage") label(2 "after marriage"))

* Including the group means		
twoway  (scatter wage time, ylabel(0(1000)5000, grid angle(0))          ///
                 ymtick(500(1000)4500, grid) c(L)                       ///
				 yline(1000 2000 3250 4250, lc(blue) lw(thick))         ///
				 yline(2625, lc(brown) lw(thick)))                      ///
        (scatter wage time if marr==1, c(L)),                           ///
		legend(off)





*******************************************
* Plot: Mechanics of a FE-Regression
*******************************************
* Within transformation by hand
egen      mwage = mean(wage), by(id)
egen      mmarr = mean(marr), by(id)
generate  wwage = wage - mwage
generate  wmarr = marr - mmarr
regress   wwage wmarr
twoway  (scatter wwage wmarr if marr==0, jitter(2) msymbol(O) msize(large))     ///
        (scatter wwage wmarr if marr==1, jitter(2) msymbol(O) msize(large))     ///
        (lfit wwage wmarr, lwidth(thick)),                                      ///
        legend(off) ylabel(-400(100)400, grid angle(0))                         ///
		xtitle(demeaned(marr)) ytitle(demeaned(wage))

		
* Within transformation by STATA
* xtdata wage marr, fe clear
* scatter wage marr
* regress wage marr


********************************************
* Equivalent FE-Estimators
********************************************
* Dummy Variable Regression(LSDV)
regress wage marr ibn.id, noconstant

*Individual Slope Regressions
by id: regress wage marr
twoway (scatter wage marr if id>2 & marr==0, jitter(2) msize(large))   ///
       (scatter wage marr if id>2 & marr==1, jitter(2) msize(large))   ///
       (lfit wage marr if id==3, lwidth(medthick) lcolor(blue))        ///
       (lfit wage marr if id==4, lwidth(medthick) lcolor(blue)),       ///
	   ylabel(0(1000)5000, grid angle(0)) ymtick(500(1000)4500, grid)  ///
       legend(off) xlabel(0 1) ytitle("EURO per month")

* The same with a "spaghetti plot" (net search spagplot)
*   Spagplot has a problem with persons with Var(X)=0, therefore plot only for id>2
*   The red line is POLS for all observations!
spagplot wage marr if id>2, id(id) ytitle("EURO per month") xlabel(0 1) ///
                    ylabel(0(1000)5000, grid) ymtick(500(1000)4500, grid) note("")


* Difference-in-Differences Estimator
gen   treat     = id   >= 3
gen   post      = time >= 4
gen   posttreat = post*treat
regr  wage post treat posttreat


*****************************************************
* Period effects in the data that correlate with marr
* Effect of marr is 0
*****************************************************
twoway                                                                            ///
 (scatter wage3 time, ylabel(0(1000)5000, grid) ymtick(500(1000)4500, grid) c(L)) ///
 (scatter wage3 time if marr==1, c(L)),                                           ///
 legend(label(1 "before marriage") label(2 "after marriage")) ytitle("EURO per month")
	
* Fixed-Effects Regression (within estimator)
xtreg wage3 marr, fe
* Solution: Add Fixed Period-Effects (twoway FE-Model)
xtreg wage3 marr i.time, fe


* Alternatively
regress wage3 marr ibn.id i.time, noconstant


*******************************************
* Random-Effects Regression
*******************************************
xtreg wage marr, re theta

* Hausman Test
xtreg wage marr, re
estimates store randeff
xtreg wage marr, fe
estimates store fixdeff
hausman fixdeff randeff, sigmamore


******************************************
* Measurement error in the X-variable (marr)
******************************************
* Effect of marr is 500
twoway                                                                           ///  
 (scatter wage time, ylabel(0(1000)5000, grid) ymtick(500(1000)4500, grid) c(L)) ///
 (scatter wage time if marr1==1, c(L)),                                          ///
    legend(label(1 "before marriage") label(2 "after marriage"))                 ///
	ytitle("EURO per month")
* Pooled OLS
regress wage marr1
* Fixed-Effects Regression (within estimator)
xtreg   wage marr1, fe


**************************************************
* Causality runs the other way: Reverse Causality
* Effect of marr is 0
**************************************************
twoway (scatter wage2 time, ylabel(0(1000)5000, grid) ymtick(500(1000)4500, grid) c(L))  ///
 (scatter wage2 time if marr==1, c(L)),                                            ///
 legend(label(1 "before marriage") label(2 "after marriage"))  ytitle("EURO per month")

* Fixed-Effects Regression (within estimator)
xtreg wage2 marr, fe

* First-Differences Estimator
regress D.(wage2 marr), noconstant


* Instrumental Variables Regression
* Lag(marr) is used as an instrument
xtivreg wage2 (marr=L.marr), fe



********************************************************************
********************************************************************
*  Further variants not discussed on the slides
********************************************************************
********************************************************************

* FE-estimator with AR(1) disturbance
xtregar wage marr, fe


******************************************
*  FE-estimator also works
*  with a time trend in wages
******************************************
* Effect of marr is +500, time trend is +100 per wave
twoway                                                                              ///
   (scatter wage1 time, ylabel(0(1000)5000, grid) ymtick(500(1000)4500, grid) c(L)) ///
   (scatter wage1 time if marr==1, c(L)),                                           ///
    legend(label(1 "before marriage") label(2 "after marriage"))
* DID
regr  wage1 post treat posttreat
* Pooled OLS
regress  wage1 marr time
* Fixed-Effects Regression (within estimator)
xtreg wage1 marr time, fe



******************************************
* Dynamic Models (seems to be nonsense)
******************************************
regress   wage L.wage marr
generate  l1wage = L.wage
xtreg     wage l1wage marr, re
xtreg     wage l1wage marr, fe
