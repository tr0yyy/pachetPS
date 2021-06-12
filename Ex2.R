## Verificarea daca o functie introdusa de utilizator este densitate de probabilitate.

densitate <- function(f,lower,upper,pas){
  val <- seq(lower,upper,pas);
  if(all(f(val)>=0)=='TRUE'){
    if(typeof(integrate(Vectorize(f),-Inf,Inf))=='logical' && integrate(Vectorize(f),-Inf,Inf)==FALSE){
      print("Functia nu este integrabila")
      return(FALSE)
    }
    else{
      i = integrate(Vectorize(f),-Inf,Inf) $ value
      if (round(i)==1 && all(f(val)>=0)){
        print("Functia este densitate de probabilitate.")
        return(TRUE)
      }
      else{
        print("Functia nu este densitate de probabilitate.")
        return(FALSE)
      }
    }
  }
  else{
    print("Functia este negativa pe intervalul ales.")
    return(FALSE)
  }
}

##TESTARE
f <- function(x){
if (x > 0 && x < 2){
      3/8 * (4*x-2*x^2)
   }else{
      0
   }
}


function(x){produs <- x*f(x)}
function(x){x*f(x)}


densitate(f,-10000,10000,10)

