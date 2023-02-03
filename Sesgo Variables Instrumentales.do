*Propiedades del Estimador de Variables Instrumentales en muestras finitas



clear
set obs 1000
gen z = rnormal()
gen w = rnormal()
gen x = z*.3 + rnormal() + w
gen u = rnormal()
gen y = x + w + u*5
reg y x
ivreg y (x=z)
reg x z

*Montecarlo 
cap program drop monteiv
program monteiv, rclass
  clear
  set obs `1' 
  * El primer argumento de¡l comando monteiv es el numero de observaciones 
  gen z = rnormal()
  gen w = rnormal()
  gen x = z*.2 + rnormal() + w
  gen u = rnormal()
  gen y = x + w + u*5
  reg y x
  return scalar b_mco = _b[x]
  return scalar se_mco = _se[x]
  ivreg y (x=z)
  return scalar b_iv = _b[x]
  return scalar se_iv = _se[x]
end 

* Solo 30 observaciones
simulate b_mco=r(b_mco)     se_mco=r(se_mco)  /// 
         b_iv=r(b_iv) se_iv=r(se_iv)    /// 
   , rep(1000): monteiv 30
sum

*El sesgo del IV es ENORME. En este caso es mejor usar MCO

*Aumentar la muestra a 100
simulate b_mco=r(b_mco)     se_mco=r(se_mco)  /// 
         b_iv=r(b_iv) se_iv=r(se_iv)    /// 
   , rep(1000): monteiv 100
sum

*Aumentar la muestra a 300
simulate b_mco=r(b_mco)     se_mco=r(se_mco)  /// 
         b_iv=r(b_iv) se_iv=r(se_iv)    /// 
   , rep(1000): monteiv 300
sum

*Aumentar la muestra a 750

simulate b_mco=r(b_mco)     se_mco=r(se_mco)  /// 
         b_iv=r(b_iv) se_iv=r(se_iv)    /// 
   , rep(1000): monteiv 750
sum



*Aumentar la muestra a 10000
simulate b_mco=r(b_mco)     se_mco=r(se_mco)  /// 
         b_iv=r(b_iv) se_iv=r(se_iv)    /// 
   , rep(1000): monteiv 10000
sum
