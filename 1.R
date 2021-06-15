ConstantaDeNormalizareK <- function(functie)
{
  integrala <- integrate(Vectorize(functie), -Inf, Inf)$value;
  if (integrala != 0){
    return(1 / integrala);
  }
  else{
    print("Aceasta functie nu admite constante de normalizare");
  }
}

