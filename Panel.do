************************************************************************
*Clase 5. Datos de Panel
************************************************************************
clear all
capture log close 
log using "log/DATOS DE PANEL.txt", replace
************************************************************************

use "Wage Premium.dta", clear
list id t wage casado, separator(6)
xtset id t

**********************************
*Describir los Datos
************************************************************************

xtdes
xtsum wage
xttab casado
xtline wage, overlay


******************************************
* Plotting los datos
******************************************
twoway  (scatter wage t, ylabel(0(1000)5000, grid angle(0))           ///
                 ymtick(500(1000)4500, grid) c(L))                       ///
        (scatter wage t if casado ==1, c(L)),                            ///
        legend(label(1 "solteros") label(2 "casados"))

* Con los promedios de las variables	
twoway  (scatter wage t, ylabel(0(1000)5000, grid angle(0))          ///
                 ymtick(500(1000)4500, grid) c(L)                       ///
				 yline(1000 2000 3250 4250, lc(blue) lw(thick))         ///
				 yline(2625, lc(brown) lw(thick)))                      ///
        (scatter wage t if casado==1, c(L)),                           ///
		legend(off)

		
******************************************
* DID
******************************************
gen D = [ id == 3 | id == 4]	
gen t2 = 1 if t==4
replace t2 = 0 if t == 3
	
sum wage if D == 1 & t== 4
sum wage if D == 1 & t== 3

sum wage if D == 0 & t== 4
sum wage if D == 0 & t== 3

reg wage D##t2  

******************************************
* Cross-sectional OLS  at T=4
******************************************
regress  wage casado if t==4


******************************************
* Pooled OLS
******************************************
regress  wage casado                    //incorrectas default S.E.
regress  wage casado, vce(cluster id)   //correctas panel-robust S.E.


******************************************
* Dummy Variable Regression(LSDV)
regress wage casado ibn.id, noconstant

*Individual Slope Regressions
bysort id: regress wage casado
twoway (scatter wage casado if id>2 & casado==0, jitter(2) msize(large))   ///
       (scatter wage casado if id>2 & casado==1, jitter(2) msize(large))   ///
       (lfit wage casado if id==3, lwidth(medthick) lcolor(blue))        ///
       (lfit wage casado if id==4, lwidth(medthick) lcolor(blue)),       ///
	   ylabel(0(1000)5000, grid angle(0)) ymtick(500(1000)4500, grid)  ///
       legend(off) xlabel(0 1) ytitle("Millones de Pesos")


net search spagplot
spagplot wage casado if id>2, id(id) ytitle("Millones de Pesos") xlabel(0 1) ///
                    ylabel(0(1000)5000, grid) ymtick(500(1000)4500, grid) note("")

*******************************************
* Fixed-Effects Regression (within estimator)
*******************************************
xtreg wage casado, fe                   //incorrectas default S.E.
xtreg wage casado, fe vce(cluster id)   //does not work with these data!	
		
		
*******************************************
* Plot: Mechanics of a FE-Regression
*******************************************
* Within transformation by hand
egen      mwage = mean(wage), by(id)
egen      mcasado = mean(casado), by(id)
generate  wwage = wage - mwage
generate  wcasado = casado - mcasado
regress   wwage wcasado
twoway  (scatter wwage wcasado if casado==0, jitter(2) msymbol(O) msize(large))     ///
        (scatter wwage wcasado if casado==1, jitter(2) msymbol(O) msize(large))     ///
        (lfit wwage wcasado, lwidth(thick)),                                      ///
        legend(off) ylabel(-400(100)400, grid angle(0))                         ///
		xtitle(demeaned(casado)) ytitle(demeaned(wage))

*******************************************
		
twoway                                                                            ///
 (scatter wage3 t, ylabel(0(1000)5000, grid) ymtick(500(1000)4500, grid) c(L)) ///
 (scatter wage3 t if casado==1, c(L)),                                           ///
 legend(label(1 "Solteros") label(2 "Casados")) ytitle("Pesos")
	
* Fixed-Effects
xtreg wage3 casado, fe
* Solución: incluir efectos fijos de tiempo
xtreg wage3 casado i.t, fe


* O
regress wage3 casado ibn.id i.t, noconstant		

******************************************
* First-differences estimator
******************************************
sort id t

regress D.(wage casado), noconstant

* Plot "Mechanics"
generate  dwage = wage - L.wage                    // L. is the lag-operator
generate  dcasado = casado - L.casado
twoway  (scatter dwage dcasado, jitter(2))                ///
        (lfit dwage dcasado, estopts(noconstant)),        ///
        legend(off) ylabel(-200(100)600, grid) xtitle(delta(casado)) ytitle(delta(wage))



		
************************************************************************
use "datafiles/base4.dta"

xtset id t


xtdescribe
xtsum

xttab D

xtline ha_nchs if id <= 10, overlay


sort id t


correlate ha_nchs L.ha_nchs

reg ha_nchs D

reg ha_nchs D
reg ha_nchs D, cluster(id)
avplots

*Caso 1. Cov(D,alpha)=0 

reg ha_nchs D_t, cluster(id)

*Caso 2. Cov(D,alpha)!= 0. Efectos Fijos
reg ha_nchs D_t i.id, cluster(id)
xtreg ha_nchs D_t , vce(cluster id) fe

*No olvidar incluir los efectos fijos de tiempo
xtreg ha_nchs D_t t, vce(cluster id) fe

*Evitar este tipo de ejercicios
xtreg ha_nchs t, vce(cluster id) fe
xtreg ha_nchs D, vce(cluster id) fe

* Caso 3. Cov(D,alpha)!= 0. Primeras Diferencias
reg D.ha_nchs D.D_t, cluster(id) 

* Caso 4.Cov(D,alpha)=0 
correlate ha_nchs L.ha_nchs
xtreg ha_nchs D_t t, cluster(id) re

*Efectos fijos vs Aleatorios
xtreg ha_nchs D_t t, fe
estimates store fijos
xtreg ha_nchs D_t t, re
hausman fijos ., sigmamore



