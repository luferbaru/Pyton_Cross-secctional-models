
***** Sharp RDD *****
*** Crear datos artificiales
clear
matrix C = (1, 0, 0 \ 0, 1, 0 \ 0, 0, 1) /* matrix of correlations: 'q', 'v' and 'epsilon' */
matrix sd = (0.1,0.2,0.3) /* vector of standard deviations: 'q', 'v' and 'epsilon' */
matrix m = (0,0,0) /* vector of means: 'q', 'v' and 'epsilon' */
set seed 124569
drawnorm q v epsilon, n(1000) means(m) corr(C) sds(sd) cstorage(full) clear /* create the artificial dataset with the above parameters and 1000 observations using the normal distribution */

*Crear la variable de asignación
generate z = 5 + 8*q + epsilon
summarize z, detail
generate c = 5
generate z_centrado = z - c 

*Crear la variable de tratamiento 
generate D = 1 if z >= c
replace D = 0 if z < c
tabulate D

** Crear la variable dependiente siguiendo el modelo estructural: 'y = beta_0 + beta_1*x + ro*D + gamma*q + v' **
generate y = 1 + 2*z + 4*D + 5*q + v 
* PILAS: EL AVERAGE TREATMENT EFFECT ES IGUAL A 4

*** GRaficas ***

twoway (scatter y z) (lfit y z if z < c) (lfit y z if z >= c), ytitle(y) xtitle(z) xline(5, lcolor(black)) legend(off) name(rdd_1_linear, replace)

rdplot y z, c(5) graph_options(ytitle(Average y) xtitle(z) name(rdplot_1_linear, replace))
rdplot y z, c(5) binselect(es) graph_options(ytitle(Average y) xtitle(z) name(rdplot_2_linear, replace))
rdplot y z_centrado, graph_options(ytitle(y promedio) xtitle(z_centrado) name(rdplot_3_linear, replace))
rdplot y z_centrado, binselect(es) graph_options(ytitle(y promedio) xtitle(z_centrado) name(rdplot_4_linear, replace))

*** REGRESION ***
** Estimar los parámetros correctos con OLS **

regress y z D q 

** Nosotros queremos identificar el efecto del tratamiento 
* entonces la variable de interes es D
* Si estimamos la regresion sin controles el estimador de D es incosistente y sesgado 
regress y D

regress y z_centrado if z_centrado >= 0
scalar a_der = _b[_cons]

regress y z_centrado if z_centrado < 0
scalar a_izq = _b[_cons]
display a_der - a_izq

* Para encontrar el mismo estimador, debemos estimar:
regress y c.z_centrado##c.D, robust

regress y z D, robust
regress y z_centrado D, robust

* Usando el comando rdrobust
rdrobust y z_centrado, kernel(uniform) h(10)
rdrobust y z_centrado, kernel(uniform) h(10) all
rdrobust y z_centrado, kernel(uniform)  all


***** Fuzzy RDD *****


*Crear los datos
clear
matrix C = (1, 0, 0 \ 0, 1, 0 \ 0, 0, 1) /* matrix of correlations: q, v and epsilon */
matrix sd = (1,0.2,0.3) /* vector of standard deviations: q, v and epsilon */
matrix m = (0,0,0) /* vector of means: q, v and epsilon */
set seed 1243497
drawnorm q v epsilon, n(10000) means(m) corr(C) sds(sd) cstorage(full) clear /* create the artificial dataset with the above parameters and 10000 observations using the normal distribution */

generate z = 5 + epsilon
summarize z, detail
generate c = 5
generate z_centrado = z - c 

** Crear la variable de asignación binaria (Nuestro instrumento)
generate T = 1 if z >= c
replace T = 0 if z < c
tabulate T

** Crear la variable D
generate w = 1 + 8*T - 5*q /* 'w' es una función de  'T' ay "otros factores no observados" ('q') */
summarize w, detail
generate w0 = 5 /* create an arbitrary threshold for 'w' so that treatment ('D = 1') only occurs above it */

generate D = 1 if w >= w0
replace D = 0 if w < w0

tabulate D

tabulate D T

** Crear la variable dependiente
** 'y = beta_0 + beta_1*x + ro*D + gamma*q + v' **

generate y = 1 + 2*z + 5*D + 3*q + v /* therefore, the (average) "treatment effect" is equal to 5 */


*** Grafica ***
rdplot y z_centrado, graph_options(ytitle(Average y) xtitle(z_centrado) name(rdplot_fuzzy_3_linear, replace))
rdplot y z_centrado, binselect(es) graph_options(ytitle(Average y) xtitle(z_centrado) name(rdplot_fuzzy_4_linear, replace))

*** Regresiones ***
regress y z D q /* en este cado, MCO estima consistentemente los parametros de interes */
regress y D
regress y z D, robust
regress y z_centrado D, robust

*Variables intrumentales
ivreg2 y z_centrado (D = T)

*Rdrobust
rdrobust y z, c(5) fuzzy(D) all
rdrobust y z_centrado, fuzzy(D) all
rdrobust y z, c(5) fuzzy(D) kernel(uniform) h(7)

*Ventanas
regress y z_centrado if T == 1
local intercept_right_numerator = _b[_cons]

regress y z_centrado if T == 0
local intercept_left_numerator = _b[_cons]

regress D z_centrado if T == 1
local intercept_right_denominator = _b[_cons]

regress D z_centrado if T == 0
local intercept_left_denominator = _b[_cons]

display (`intercept_right_numerator' - `intercept_left_numerator')/(`intercept_right_denominator' - `intercept_left_denominator')

*Una forma equivalnte es estimar lo siguiente:
gen z_centradoD = z_centrado*D
gen z_centradoT = z_centrado*T

ivreg2 y z_centrado (D z_centradoD = T z_centradoT), first
