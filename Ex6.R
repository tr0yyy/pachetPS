##Calculul mediei si dispersiei unei variabile aleatoare g(X), unde X are o repartitie continua cunoscuta iar g este o functie continua precizata de utilizator.

calcul_medie<- function(g,functie_repartitie,lower,upper){
  integrala_medie <- function(x){g(x)*functie_repartitie(x)} 
  medie <- integrate(Vectorize(integrala_medie),lower,upper) $ value
  return(medie)
}
calcul_dispersie <- function(g,functie_repartitie,lower,upper){
  integrala_dispersie <- function(x){(g(x)-calcul_medie(g,functie_repartitie,lower,upper))^2*functie_repartitie(x)}
  dispersie <- integrate(Vectorize(integrala_dispersie),lower,upper) $ value
  return(dispersie)
}

f1 <- function(x)(x^2)
f2 <- function(x) (1 * exp(1)^(-1 * x))
calcul_medie(f1,f2,0,Inf)
calcul_dispersie(f1,f2,0,Inf)


#Functie de repartitie
#x1<x2=>f(x1)<f(x2)
#f(x-0)=f(x)
#lim f(x)=0 cand x - > -Inf
#lim f(x)=1 cand x - > Inf
