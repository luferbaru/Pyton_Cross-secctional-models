***********************************************************************
* PSM
************************************************************************
clear all
set mem 150m
capture log close 
*cd "C:\Users\a.diaze\Dropbox\Eval de Impacto\"
cd "/Users/adiazescobar/Dropbox/Eval de Impacto/"
use "datafiles/base5.dta"
log using "log/clase5.txt", replace
************************************************************************
tab D
table D, c( mean y1 mean y2)

global x ingresos_hogar_jefe personas educa_jefe ocupado_jefe hombre orden_n
diff y2, treated(D) period(t) cov($x)  test
reg D $x

reg y1 D
reg y1 D educa_jefe
scatter y2 educa_jefe if D == 1,  mcolor(blue)|| scatter y2 educa_jefe if D == 0,  mcolor(red)

/*
levelsof educa_jefe, local(educ)
foreach l of local educ{
qui reg y2 D if educa_jefe == `l'
scalar b`l' = _b[D]
di b`l'
}
*/


reg y2 D $x, nohead

dprobit D ingresos_hogar_jefe
dprobit D ingresos_hogar_jefe personas
dprobit D ingresos_hogar_jefe personas orden_n
dprobit D ingresos_hogar_jefe personas orden_n educa_jefe
dprobit D ingresos_hogar_jefe personas orden_n educa_jefe ocupado_jefe
dprobit D ingresos_hogar_jefe personas orden_n educa_jefe ocupado_jefe hombre

sw, pe(.05): probit D $x

global X "personas orden_n ocupado_jefe educa_jefe ingresos_hogar_jefe"
dprobit D $X
predict ps1
sum ps1
bysort D: sum ps1
histogram ps1, by(D)
twoway (kdensity ps1 if D==1) (kdensity ps1 if D == 0),title(PS) yt(densidad) xt(prob) legend(order(1 "Tratado" 2 "Control"))

*-----------------*
*2. Soporte com˙n *
*-----------------*

**** Gr·ficamente vemos que las probabilidades predichas son similares. Sin embargo, resulta evidente que hay probabilidades de participaciÛn
**** en el grupo de tratamiento superiores a la m·xima probabilidad del grupo de control y, de la misma manera, probabilidades
**** en el grupo de control inferiores a la mÌnima probabilidad del grupo de tratamiento. Para solucionar este problema imponemos el 
**** soporte com˙n mediante el m·ximo y el mÌnimo. 

gen ps1_sc=ps1

**** Encontramos la m·xima probabilidad predicha para el grupo de control:
sum ps1_sc if D==0
scalar max_c=r(max)

**** Encontramos la mÌnima probabilidad predicha para el grupo de tratamiento:
sum ps1_sc if D==1
scalar min_t=r(min)

di "Soporte Común [ " min_t ", " max_c "]"
**** Ahora, no ser·n tenidas en cuenta probabilidades del grupo de tratamiento que superen la m·xima probabilidad del grupo de control
**** ni probabilidades del grupo de control inferiores a la mÌnima probabilidad del grupo de tratamiento:
replace ps1_sc=. if D==1 & ps1_sc>max_c
replace ps1_sc=. if D==0 & ps1_sc<min_t

**** Vemos cuantas observaciones perdemos al imponer esta restricciÛn:
count if ps1!=. & ps1_sc==.

**** Veamos el resultado gr·ficamente:

twoway (kdensity ps1_sc if D==1) (kdensity ps1_sc if D == 0),title(PS) yt(densidad) xt(prob) legend(order(1 "Tratado" 2 "Control"))


*------------------------------*
*3. Calidad del emparejamiento *
*------------------------------*

**** Otra posibilidad para estimar la probabilidad de participaciÛn es utilizar directamente el comando "pscore".
**** El comando "pscore", primero, determina la probabilidad de participaciÛn para cada individuo de acuerdo con el modelo que 
**** se especifique. Posterior a esto, se dividen las observaciones en un n˙mero Ûptimo de bloques de manera que dentro de Èstos
**** la probabilidad media del grupo de control no sea estadÌsticamente diferente de la probabilidad media del grupo de tratamiento. 
**** Este es el primer paso para balancear la probabilidad de participaciÛn. Si se encuentra que dentro de un mismo bloque la probabilidad de 
**** participaciÛn es estadÌsticamente diferente, se divide el bloque en dos. Una vez se determina el n˙mero de bloques mediante este procedimiento,
**** el programa prueba, bloque por bloque, que no existan diferencias estadÌsticamente significativas entre los individuos de tratamiento
**** y control para las variables incluidas para predecir la probabilidad de participaciÛn. Luego de esto, impone el soporte com˙n.

pscore D $X, pscore(ps2)
pscore D $X, pscore(ps3) comsup
tab comsup
twoway (kdensity ps3 if D==1) (kdensity ps3 if D == 0),title(PS) yt(densidad) xt(prob) legend(order(1 "Tratado" 2 "Control"))

sum ps3 if D==1 & comsup==1
sum ps1_sc if D==1
sum ps3 if D==0 & comsup==1
sum ps1_sc if D==0 

**** Otro procedimiento para garantizar la calidad del emparejamiento consiste en estimar el modelo probit con las caracterÌsticas
**** especificadas controlando por la probabilidad predicha. En teorÌa, los coeficientes asociados a las caracterÌsticas de los individuos
**** no deben ser estadÌsticamente significativos:

dprobit D ps1 $X
*predict ps4
*twoway (kdensity ps4 if D==1) (kdensity ps4 if D == 0),title(PS) yt(densidad) xt(prob) legend(order(1 "Tratado" 2 "Control"))

**** Entonces, dado que ning˙n coeficiente es significativo, podemos estar seguros de que el 
**** emparejamiento es adecuado. 

*-------------------------------------------------------------------*
*4. SelecciÛn de un algoritmo de emparejamiento (talla para la edad)*
*-------------------------------------------------------------------*

**** 4.1 Estimador PSM por vecino m·s cercano.  Al utilizar el emparejamiento por vecino m·s cercano, 
****     el programa "psmatch2" empareja a cada individuo del grupo de tratamiento con el individuo del 
****     grupo de control que tiene una probabilidad m·s cercana. Sin embargo, cuando se presentan casos 
****     en los que hay varios individuos en el grupo de control a la misma distancia de un solo individuo 
****     de tratamiento debemos asegurarnos de que el orden en el que se presentan los datos en nuestra base 
****     sea aleatorio.  Para garantizar la aleatoriedad definimos la semilla en 10, de manera arbitraria.

set seed 50
drawnorm orden
sort orden

**** 4.1 Estimador PSM por vecino m·s cercano. El comando "psmatch2" calcula el propensity score
**** y tiene en cuenta ˙nicamente a quienes est·n dentro del soporte com˙n, sin embargo, nosotros 
**** podemos indicar quÈ variable utilizar como propensity score:

psmatch2 D $X, outcome(y2) n(1) com
psgraph 
pstest $X,both

psmatch2 D, outcome(y2) n(1) pscore(ps1_sc) com
psgraph 
pstest $X

psmatch2 D, outcome(y2) n(1) pscore(ps1_sc) com
psgraph 
pstest $X

**** Ahora imponemos el soporte com˙n mediante trimming:

psmatch2 D $X, outcome(y2) n(1) trim(20) 
psgraph 

psmatch2 D $X, outcome(y2) n(1) com ate
psmatch2 D $X, outcome(y2) n(1) com ate noreplace

**** 4.2 Matching con 5 vecinos


**** 4.2.1 Imponiendo el soporte com˙n del comando.

psmatch2 D $X, outcome(y2) n(5) com
psmatch2 D $X, outcome(y2) n(5) trim(20)
psmatch2 D $X, outcome(y2) n(10) com
psmatch2 D $X, outcome(y2) n(10) trim(20)

**** 4.4.1 Emparejamiento de distancia m·xima, soporte com˙n com

psmatch2 D $X, outcome(y2) radius caliper(0.001) com

psmatch2 D $X, outcome(y2) radius caliper(0.005) com		

**** 4.4.2 Emparejamiento de distancia m·xima, soporte com˙n trimming

psmatch2 D $X, outcome(y2) radius caliper(0.001) trim(20)

psmatch2 D $X, outcome(y2) radius caliper(0.005) trim(20)	

**** 4.5.1 Emparejamiento por kernel, soporte com˙n com

psmatch2 D $X, outcome(ha_nchs2) com kernel 

bootstrap r(att) : psmatch2 D $X, out(ha_nchs2) com kernel 

**** 4.5.2 Emparejamiento por kernel, soporte com˙n trimming

psmatch2 D $X, outcome(ha_nchs2) trim(20) kernel

bootstrap r(att) : psmatch2 D $X, out(ha_nchs2) trim(20) kernel

***** 4.6.1 Estimador por lineal local, soporte com˙n com

psmatch2 D $X, llr outcome(ha_nchs2) common

bootstrap r(att) : psmatch2 D $X, llr outcome(ha_nchs2) common

***** 4.6.2 Estimador por lineal local, soporte com˙n trimming(20)

psmatch2 D $X, llr outcome(ha_nchs2) trim(20)

bootstrap r(att) : psmatch2 D $X, llr outcome(ha_nchs2) trim(20)

*---------------------------------------------------------------------*
*5. SelecciÛn de un algoritmo de emparejamiento (desnutriciÛn crÛnica)*
*---------------------------------------------------------------------*

**** 5.1 Estimador PSM por vecino m·s cercano.

**** 5.1.1 Imponiendo el soporte com˙n mediante el comando:

psmatch2 D $X, outcome(desn_cr) n(1) com

**** 5.1.2 Ahora imponemos el soporte com˙n mediante trimming:

psmatch2 D $X, outcome(desn_cr) n(1) trim(20)

**** 5.2 Matching con 5 vecinos

**** 5.2.1 Imponiendo el soporte com˙n mediante el comando:

psmatch2 D $X, outcome(desn_cr) n(5) com

**** 5.2.2 Ahora imponemos el soporte com˙n mediante trimming:

psmatch2 D $X, outcome(desn_cr) n(5) trim(20)


**** 5.3 Matching con 10 vecinos

**** 5.3.1 Imponiendo el soporte com˙n mediante el comando:

psmatch2 D $X, outcome(desn_cr) n(10) com

**** 5.3.2 Ahora imponemos el soporte com˙n mediante trimming:

psmatch2 D $X, outcome(desn_cr) n(10) trim(20)

**** 5.4.1 Emparejamiento de distancia m·xima, soporte com˙n com

psmatch2 D $X, outcome(desn_cr) radius caliper(0.001) com

psmatch2 D $X, outcome(desn_cr) radius caliper(0.005) com		

**** 5.4.2 Emparejamiento de distancia m·xima, soporte com˙n trimming

psmatch2 D $X, outcome(desn_cr) radius caliper(0.001) trim(20)

psmatch2 D $X, outcome(desn_cr) radius caliper(0.005) trim(20)	

**** 5.5.1 Emparejamiento por kernel, soporte com˙n com

psmatch2 D $X, outcome(desn_cr) com kernel

bootstrap r(att) : psmatch2 D $X, out(desn_cr) com kernel

**** 5.5.2 Emparejamiento por kernel, soporte com˙n trimming

psmatch2 D $X, outcome(desn_cr) trim(20) kernel

bootstrap r(att) : psmatch2 D $X, out(desn_cr) trim(20) kernel

***** 5.6.1 Estimador por lineal local, soporte com˙n com

psmatch2 D $X, llr outcome(desn_cr) common

bootstrap r(att) : psmatch2 D $X, llr outcome(desn_cr) common

***** 5.6.2 Estimador por lineal local, soporte com˙n trimming

psmatch2 D $X, llr outcome(desn_cr) trim(20)

bootstrap r(att) : psmatch2 D $X, llr outcome(desn_cr) trim(20)


*-----------------------------------* 
* 6. Dobles diferencias emparejadas *
*-----------------------------------*

**** En este capÌtulo utilizaremos la metodologÌa de dobles diferencias emparejadas para evaluar
**** la evoluciÛn de la variable "talla para la edad" de los individuos en dos periodos.

**** Lo primero que debemos hacer es generar la variable de diferencia entre las dos observaciones:

gen delta_ha=ha_nchs2-ha_nchs1

**** Ahora utilizamos propensity score matching para evaluar la diferencia en la evoluciÛn de la talla para
**** la edad entre individuos tratados y no tratados:


**** 6.1 Estimador PSM por vecino m·s cercano.

**** 6.1.1 Imponiendo el soporte com˙n mediante el comando:

psmatch2 D $X, outcome(delta_ha) n(1) com //Esta estimaciÛn replica el Resultado 6.7 del Libro

**** 6.1.2 Ahora imponemos el soporte com˙n mediante trimming:

psmatch2 D $X, outcome(delta_ha) n(1) trim(20)

**** 6.2 Matching con 5 vecinos

**** 6.2.1 Imponiendo el soporte com˙n mediante el comando:

psmatch2 D $X, outcome(delta_ha) n(5) com

**** 6.2.2 Ahora imponemos el soporte com˙n mediante trimming:

psmatch2 D $X, outcome(delta_ha) n(5) trim(20)


**** 6.3 Matching con 10 vecinos

**** 6.3.1 Imponiendo el soporte com˙n mediante el comando:

psmatch2 D $X, outcome(delta_ha) n(10) com

**** 6.3.2 Ahora imponemos el soporte com˙n mediante trimming:

psmatch2 D $X, outcome(delta_ha) n(10) trim(20)

**** 6.4.1 Emparejamiento de distancia m·xima, soporte com˙n com

psmatch2 D $X, outcome(delta_ha) radius caliper(0.001) com

psmatch2 D $X, outcome(delta_ha) radius caliper(0.005) com		

**** 6.4.2 Emparejamiento de distancia m·xima, soporte com˙n trimming

psmatch2 D $X, outcome(delta_ha) radius caliper(0.001) trim(20)

psmatch2 D $X, outcome(delta_ha) radius caliper(0.005) trim(20)	


**** 6.5.1 Emparejamiento por kernel, soporte com˙n com

psmatch2 D $X, outcome(delta_ha) com kernel

bootstrap r(att) : psmatch2 D $X, out(delta_ha) com kernel

**** 6.5.2 Emparejamiento por kernel, soporte com˙n trimming

psmatch2 D $X, outcome(delta_ha) trim(20) kernel

bootstrap r(att) : psmatch2 D $X, out(delta_ha) trim(20) kernel

***** 6.6.1 Estimador por lineal local, soporte com˙n com

psmatch2 D $X, llr outcome(delta_ha) common

bootstrap r(att) : psmatch2 D $X, llr outcome(delta_ha) common

***** 6.6.2 Estimador por lineal local, soporte com˙n trimming

psmatch2 D $X, llr outcome(delta_ha) trim(20)

bootstrap r(att) : psmatch2 D $X, llr outcome(delta_ha) trim(20)


**** 7. UtilizaciÛn de pesos muestrales en matching.

**** La metodologÌa de propensity score matching tambiÈn permite incluir pesos muestrales en sus estimaciones. 
**** Con el comando presentado en este archivo (psmatch2) se puede dar la opciÛn de utilizar pesos muestrales
**** mediante el siguiente comando:

**** psmatch2 t x, outcome(y) w(Z)  n(1) com

**** En este caso, Z serÌa la matriz de pesos a utilizar. Otra posibilidad es utilizar el comando
**** "attnd". Su instalaciÛn tambiÈn se hace en la pestaÒa de b˙squeda de Stata. Una vez instalado,
**** el comando para hacer matching con pesos es el siguiente:

**** attnd y t x, [pweight=z]

**** En este caso z serÌa la variable que indica el peso muestral otorgado a cada observaciÛn.



