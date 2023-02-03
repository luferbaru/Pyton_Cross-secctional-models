**DO trabajo final cortes 
**Helena Hernández y Luisa Ballén

**esta base es una base procesada de la GEIH que uso para mi trabajo (ya están agrupados los módulos, los meses y creadas algunas variabes)
use "D:\helen\Documents\Economía del cuidado\Bases de datos GEIH\Proc_HH\workdata_2019_2020.dta", clear


***********Estadísticas descriptivas*************	
*se pone bys month y year porque a veces se repite id de los hogares entre meses para familias distintas. 
*Hijos menores de 10 años 
bys month year hh_id1 hh_id2: gen hijos10=(rel_head ==3 & age<=10)
*el hogar tiene hijos menores de 10 años 
bys month year hh_id1 hh_id2: egen hijosh10=max(hijos10)
*cuántos hijos menores de 10 años tiene
bys month year hh_id1 hh_id2: egen hijost10= total(hijos10)

save "D:\helen\Documents\Economía del cuidado\Bases de datos GEIH\Proc_HH\workdata_2019_2020.dta", replace

use "D:\helen\Documents\Economía del cuidado\Bases de datos GEIH\Proc_HH\workdata_2019_2020.dta", clear

*no hay info de rel_head en marzo, abril de 2020

*TGP para mujeres con hijos y sin hijos

table year_m if male == 0 & (rel_head == 1 | rel_head == 2), c(mean active) by (hijosh10) replace	
reshape wide table1 , i(year_m) j(hijosh10)
rename (table10 table11) (tgp_muj_sin_h tgp_muj_con_h)
save "D:\helen\Documents\Maestría en Economía\1 sem 2020 II\Secciones transversales\Trabajo final\Outputs Stata\tgp_muj_hij.dta", replace

*TGP hombres con hijos vs mujerse con hijos
table year_m if hijosh10 == 1 & (rel_head == 1 | rel_head == 2), c(mean active) by (gender) replace	
reshape wide table1 , i(year_m) j(gender)
rename (table10 table11) (tgp_muj_sin_h tgp_muj_con_h)
save "D:\helen\Documents\Maestría en Economía\1 sem 2020 II\Secciones transversales\Trabajo final\Outputs Stata\tgp_muj_hij.dta", replace

*ttest mayo de 20200
use "D:\helen\Documents\Economía del cuidado\Bases de datos GEIH\Proc_HH\workdata_2019_2020.dta", clear
gen y = active 
gen t = .
replace t = 1 if year == 2020 & month == 5
replace t = 0 if year == 2019 & month == 5

gen D = .
replace D = 1 if male == 0 & (rel_head == 1 | rel_head == 2) & workage == 1 & hijosh10==1
replace D = 0 if male == 0 & (rel_head == 1 | rel_head == 2) & workage == 1 & hijosh10==0


ttest y if month==5 & year==2020 , by(D)
ttest y if month==5 & year==2019 , by(D)

*pruebas de hipótesis estadísticas descriptivas
table D t [w=int(wgt)], c(mean y sd y)

save "D:\helen\Documents\Maestría en Economía\1 sem 2020 II\Secciones transversales\Trabajo final\process_cortes.dta", replace


use "D:\helen\Documents\Maestría en Economía\1 sem 2020 II\Secciones transversales\Trabajo final\process_cortes.dta", clear
*hacer todo el procesamiento con mayo de 2020 
keep if year==2020 & month==5
save "D:\helen\Documents\Maestría en Economía\1 sem 2020 II\Secciones transversales\Trabajo final\base_PSM.dta"

*Controles

*1 si vive en área rural
gen rural = (area==2)
replace rural =. if area ==.

*1 si es jefe de hogar
gen jefe_hog = (rel_head==1)
replace jefe_hog =. if rel_head==. 

*ingresos del hogar
gen inglab_mensual = w_m_gross 
replace inglab_mensual=profit if w_m_gross ==.
egen inglab_hogar= total(inglab_mensual), by(hh_id1 hh_id2) missing
label var inglab_hogar "Ingreso laboral mensual del hogar por ocupación principal"
gen inghog_percap= inglab_hogar/hh_size

*edad al cuadrado y al cubo 
gen age2=age^2 
gen age3=age^3

**1 si nivel de educacacion tecnico o mas 
gen ed_sup=(edu_level==2| edu_level==3 | edu_level==4)
replace ed_sup =. if edu_years==. 

*sustitución cuidado: 1 si en el hogar hay personas mayores de 15 años y menores de 70 que puedan sustituir cuidado
bys month year hh_id1 hh_id2: gen sust_cuid=(rel_head != 1 & rel_head!=2 & age>=15 & age <=70)
*en el hogar hay alguien que puede sustituir cuidado
bys month year hh_id1 hh_id2: egen sust_cuidh=max(sust_cuid)

*Hogares en los que hay una pareja 
bys hh_id1 hh_id2: gen conyugue =(rel_head ==2)
bys hh_id1 hh_id2: egen conyugueh=max(conyugue)

**1 si cónyugue trabaja 
gen conyu_trab =.
replace conyu_trab = 1 if conyugueh==1 & D==. & (rel_head==2 | rel_head==1) & employed ==1
replace conyu_trab = 0 if conyugueh==1 & D==. & (rel_head==2 | rel_head==1) & (unemployed==1 | active==0)
replace conyu_trab = 0 if conyugueh==0
bys hh_id1 hh_id2: egen conyu_trab_h=max(conyu_trab)

*1 si cónyuge informal 
gen conyu_inf =.
replace conyu_inf = 1 if conyugueh==1 & D==. & (rel_head==2 | rel_head==1) & informal_ss ==1
replace conyu_inf = 0 if conyugueh==1 & D==. & (rel_head==2 | rel_head==1) & informal_ss ==0
replace conyu_inf = 0 if conyugueh==0
bys hh_id1 hh_id2: egen conyu_inf_h=max(conyu_inf)

*1 si conyuge tiene educación terciaria o más 
gen conyu_edsup =.
replace conyu_edsup = 1 if conyugueh==1 & D==. & (rel_head==2 | rel_head==1) & ed_sup ==1
replace conyu_edsup = 0 if conyugueh==1 & D==. & (rel_head==2 | rel_head==1) & ed_sup ==0
replace conyu_edsup = 0 if conyugueh==0
bys hh_id1 hh_id2: egen conyu_edsup_h=max(conyu_edsup)

*caribe
gen caribe=(region==1)


**edad del cónyuge 
bys hh_id1 hh_id2: gen edad_conyu= age if  conyugueh==1 & D==. & (rel_head==2 | rel_head==1)
replace edad_conyu = 0 if conyugueh==0
bys hh_id1 hh_id2: egen edad_conyu_h= max(edad_conyu)

*tratamiento alterno
gen D2 = .
replace D2 = 1 if male == 0 & (rel_head == 1 | rel_head == 2) & workage == 1 & hijosh10==1 & age<=54 & age>=20
replace D2 = 0 if male == 0 & (rel_head == 1 | rel_head == 2) & workage == 1 & hijosh10==0 & age<=54 & age>=20

*ingresos del conyugue
bys hh_id1 hh_id2: gen conyu_ingreso= inglab_mensual if conyugueh==1 & D==. & (rel_head==2 | rel_head==1)
replace conyu_ingreso = 0 if conyugueh==0
bys hh_id1 hh_id2: egen conyu_ingreso_h= max(conyu_ingreso)

** tabla estadísticas descriptivas 
gen period=0
diff y, treated(D) period(period) cov(age edu_years inglab_hogar rural caribe sust_cuidh conyugueh jefe_hog)  test

**********************PSM*********************
*calcular el propensity 
global X "age age2 age3 rural edu_years jefe_hog caribe inghog_percap sust_cuidh conyugueh edad_conyu_h conyu_trab_h"
dprobit D $X
predict ps
sum ps
bysort D: sum ps
dprobit D ps $X
twoway (kdensity ps if D==1) (kdensity ps if D == 0),title(PS) yt(densidad) xt(prob) legend(order(1 "Tratado" 2 "Control"))

set seed 50
drawnorm aleatorio
sort aleatorio
*vecino más cercano 
psmatch2 D $X, outcome(y) n(1) com ai(1)
psgraph 
pstest $X, treated(D)
*caliper
psmatch2 D $X, outcome(y) caliper(0.05) com ai(1)
psgraph 
pstest $X
*radius
psmatch2 D $X, outcome(y) radius caliper(0.05) com	ai(1)
pstest $X

**********DID*************


cd"C:\Users\Luisa Ballen\Downloads\8vo semestre\Cortes\Trabajo final"
use "C:\Users\Luisa Ballen\Downloads\8vo semestre\Cortes\Trabajo final\workdata_2019_2020.dta", clear

*Por bys para month y year porque a veces se repite id entre meses para familias distintas. 
*Hijos menores de 10 años 
bys month year hh_id1 hh_id2: gen hijos10=(rel_head ==3 & age<=10)
*el hogar tiene hijos menores de 10 años 
bys month year hh_id1 hh_id2: egen hijosh10=max(hijos10)
*cuántos hijos menores de 10 años tiene
bys month year hh_id1 hh_id2: egen hijost10= total(hijos10)

*NOOOO hay info de rel_head en marzo, abril de 2020

*Participación laboral
gen y=active 

*Mujer con hijos menores a 10 años
gen D=.
replace D=1 if male == 0 & (rel_head == 1 | rel_head == 2) & workage == 1 & hijosh10==1
replace D=0 if male == 0 & (rel_head == 1 | rel_head == 2) & workage == 1 & hijosh10==0
tab D

*1 si vive en área rural
gen rural = (area==2)
replace rural =. if area ==.
tab rural

*1 si es jefe de hogar
gen jefe_hog = (rel_head==1)
replace jefe_hog =. if rel_head==. 
tab jefe_hog

*ingresos del hogar per capita
gen inglab_mensual = w_m_gross 
replace inglab_mensual=profit if w_m_gross ==.
egen inglab_hogar= total(inglab_mensual), by(hh_id1 hh_id2) missing
label var inglab_hogar "Ingreso laboral mensual del hogar por ocupación principal"
gen inghog_percap= inglab_hogar/hh_size

*edad al cuadrado
gen age2=age^2 

*sustitución cuidado: 1 si en el hogar hay personas mayores de 15 años y menores de 75 que puedan sustituir cuidado
bys month year hh_id1 hh_id2: gen sust_cuid=(rel_head != 1 & rel_head!=2 & age>=15 & age <=70)
*en el hogar hay alguien que puede sustituir cuidado
bys month year hh_id1 hh_id2: egen sust_cuidh=max(sust_cuid)

*Hogares en los que hay una pareja 
bys hh_id1 hh_id2: gen conyugue =(rel_head ==2)
bys hh_id1 hh_id2: egen conyugueh=max(conyugue)

**1 si cónyugue trabaja 
gen conyu_trab =.
replace conyu_trab = 1 if conyugueh==1 & (rel_head==2 | rel_head==1) & employed ==1
replace conyu_trab = 0 if conyugueh==1 & (rel_head==2 | rel_head==1) & (unemployed==1 | active==0)
replace conyu_trab = 0 if conyugueh==0
bys hh_id1 hh_id2: egen conyu_trab_h=max(conyu_trab)
tab conyu_trab_h


* 1 region caribe*
gen caribe=.
replace caribe=1 if region==1
replace caribe=0 if region==2|region==3|region==4|region==5
tab caribe

* 1 region oriental*
gen oriental=.
replace oriental=1 if region==2
replace oriental=0 if region==1|region==3|region==4|region==5
tab oriental

* 1 region central*
gen central=.
replace central=1 if region==3
replace central=0 if region==1|region==2|region==4|region==5
tab central

* 1 region pacífica*
gen pacifica=.
replace pacifica=1 if region==4
replace pacifica=0 if region==1|region==2|region==3|region==5
tab pacifica

* 1 region bogotá*
gen bogota=.
replace bogota=1 if region==5
replace bogota=0 if region==1|region==2|region==3|region==4
tab bogota


***********************Test de tendencias paralelas***********************************
gen trend=.
replace trend=1 if year==2019 & month==1
replace trend=2 if year==2019 & month==2
replace trend=3 if year==2019 & month==3
replace trend=4 if year==2019 & month==4
replace trend=5 if year==2019 & month==5
replace trend=6 if year==2019 & month==6
replace trend=7 if year==2019 & month==7
replace trend=8 if year==2019 & month==8
replace trend=9 if year==2019 & month==9
replace trend=10 if year==2019 & month==10
replace trend=11 if year==2019 & month==11
replace trend=12 if year==2019 & month==12
replace trend=13 if year==2020 & month==1
replace trend=14 if year==2020 & month==2
replace trend=17 if year==2020 & month==5
replace trend=18 if year==2020 & month==6
replace trend=19 if year==2020 & month==7
replace trend=20 if year==2020 & month==8
tab trend
sort trend


*Se omiten lo marzo,abril de 2020 debido a que no se poseen datos para la variable tratamiento*



*********** Prueba grafica de tendecias paralelas********************************************
//  FITTED TRENDS COMPARISON


global Y edu_years jefe_hog age age2 conyugueh rural caribe oriental central pacifica

regress y i.D##i.trend $Y
margins D,at(trend= (2(1)5)) 
marginsplot, name(marginsplot, replace)


//  SUBSETPLOT METHOD (-ssc install subsetplot-, by Nick Cox)
*subsetplot scatter y trend, by(D)
*graph rename subsetplot, replace

//  PLOT OF GROUP MEANS OVER YEARS
collapse (mean) y, by(trend D)
reshape wide y, i(trend) j(D)
graph twoway connect y* trend, sort name(group_means, replace)

*************Prueba del test de tendencias paralelas**************************************

gen w=D
tfdiff y w $Y, t(5) tvar(trend) datatype(cross-section) model(ols)test_pt graph ci(5)

*global Y jefe_hog inghog_percap age age2 sust_cuidh conyugueh conyu_trab_h rural caribe oriental central pacifica 

global Y edu_years jefe_hog age age2 conyugueh rural caribe oriental central pacifica
reg y i.D##i.trend $Y
outreg2 using test_tendencia.doc : reg y i.D##i.trend $Y

************Medir el impacto del 'programa'***********************************************

gen time=.
replace time=1 if trend==17
replace time=0 if trend==5
tab time

*DID*
diff y, t(D) p(time) 
est store diff_SinControles11

diff y, t(D) p(time) cov(edu_years jefe_hog age age2 conyugueh rural caribe oriental central pacifica)
*diff y, t(D) p(time) cov(edu_years jefe_hog age age2 conyugueh rural caribe oriental central pacifica) kernel rcs support
 
est store diff_ConControles11

reg y i.D##i.time edu_years jefe_hog age age2 conyugueh rural caribe oriental central pacifica

*MCO*
reg y D
est store MCO_SinControles11

reg y D edu_years jefe_hog age age2 conyugueh rural caribe oriental central pacifica 
est store MCO_ConControles11

outreg2 [MCO_SinControles11 MCO_ConControles11 diff_SinControles11 diff_ConControles11] using MCO_diffthird11.doc 




