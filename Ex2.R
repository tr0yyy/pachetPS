## Verificarea daca o functie introdusa de utilizator este densitate de probabilitate.

densitate <- function(f,lower,upper,pas){
  val <- seq(lower,upper,pas);
  #Verificare functie pozitiva
  if(all(f(val)>=0)=='TRUE'){
    #Verificare integrabilitate
    if(typeof(integrate(Vectorize(f),-Inf,Inf))=='logical' && integrate(Vectorize(f),-Inf,Inf)==FALSE || integrate(Vectorize(f),-Inf,Inf) $ message!="OK"){
      return(FALSE)
    }
    else{
      i = integrate(Vectorize(f),-Inf,Inf) $ value
      if (round(i)==1){
        print("Functia este densitate de probabilitate.")
      }
      else{
        print("Functia nu este densitate de probabilitate.")
      }
    }
  }
  else{
    print("Functia este negativa pe intervalul ales.")
  }
}

##TESTARE
# f <- function(x){
# if (x > 0 && x < 2){
#      3/8 * (4*x-2*x^2)
#   }else{
#      0
#   }
# }
# test <- integrate(Vectorize(f),-Inf,Inf) $ value
# print(test)
# 
# densitate(f,-10000,10000,1)
