integrarePDF<-function(f, x){
  tryCatch({
    return(integrate(Vectorize(f), 0, x)$value);
    }, eroare = function(e){
    print(e);
    return(0);
  });
}

generareInversa<-function(f, y, stanga, dreapta){
  uniroot((function (x) f(x) - y), lower=stanga, upper=dreapta)[1]
}

generareNValori<-function(f, n, stanga, dreapta){
  valori <- runif(n, stanga, dreapta)
  export <- c();
  for(y in valori){
    export <- append(export, generareInversa(function(x) (integrarePDF(f, x)), y, stanga, dreapta)$root)
  }
  print(export);
}