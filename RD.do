**** Taller RDN en clase ****
*VAmos a analizar un caso más complejo que el caso que acabamos de ver
clear
** Cree la variable de asignación Z
set obs 1000
set seed 1234570
generate z = rnormal(100, 50)
summarize z, detail
generate c = 100
generate z_centrado = z - c 

** Cree la variable D

generate D = 1 if z >= c
replace D = 0 if z < c

* 1. Analice la distribución de D
tabulate D

** 2. Cree la variable dependiente y con el siguiente proceso generador de datos
**  'y = beta_0 + beta_1*z + beta_2*z^2 + ro*D + delta*D*x + v' **
** seleccione los números de beta usando los de su CC.
generate v = rnormal(0, 0.5)
generate y = 

** 3. Realice una gráfica usando el comando rdplot para ver la relación entre y y la variable de asignación z

** 4. Realice una regresión de y contra D e interprete

** 5. Realice una regresión de y contra D e incluya z como variable de control e interprete

** 6. Realice una regresión de y contra D e incluya z_centrado como variable de control e interprete 

** 7. Incluya terminos al cuadrado e interacciones para encontrar el ATE 

** 8. Realice la regresión hacía la derecha del umbral y Realice la regresión hacia la izquierda del umbral y encuentre 
***.  el efecto (debe incluir términos al cuadrado del z_centrado)

** 9. Encuetre el efecto usando rdrobust: e interprete 
rdrobust y x, p(2) c(100) all /* setting p(2): local quadratic regression */
rdrobust y x_tilde, p(2) all
rdrobust y x, p(1) c(100) all /* setting p(1) (the default): local linear regression */
rdrobust y x_tilde, p(1) all
rdrobust y x, p(2) c(100) kernel(uniform) h(250) /* setting p(2): local quadratic regression */



