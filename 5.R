media<-function(f){
  result=tryCatch({
    return(integrate(function(x){
      produs<-x*f(x);
      return(produs);
    },-Inf,Inf)$value);
  }, eroare = function(e){
    print("Nu se poate calcula media");
    return(0);
  });
}

dispersia<-function(f){
  medie<-media(f)
  if(medie==0){
    print("Nu se poate calcula dispersia fara medie");
    return(0);
  }
  result=tryCatch({
    return(integrate(function(x){
      produs<-((x-medie)^2)*f(x);
      return(produs);
    },-Inf,Inf)$value);
  }, eroare = function(e){
    print("Nu se poate calcula dispersia");
    return(0);
  });
}

momentCentrat<-function(f,ordin){
  medie<-media(f)
  if(medie==0){
    warning(c("Nu se poate calcula momentul centrat fara medie"));
    return(0);
  }
  result=tryCatch({
    return(integrate(function(x){
      produs<-((x-medie)^ordin)*f(x);
      return(produs);
    },-Inf,Inf)$value);
  }, eroare = function(e){
    print("Nu se poate calcula momentul centrat");
    return(0);
  });
}

momentInitial<-function(f,ordin){
  result=tryCatch({
    return(integrate(function(x){
      produs<-(x^ordin)*f(x);
      return(produs);
    },-Inf,Inf)$value);
  }, eroare = function(e){
    print("Nu se poate calcula momentul initial");
    return(0);
  });
}

patruMomente <- function(f)
{
  print('Primele 4 momente initiale sunt:')
  print(momentInitial(f, 1))
  print(momentInitial(f, 2))
  print(momentInitial(f, 3))
  print(momentInitial(f, 4))
  print('Primele 4 momente centrate sunt:')
  print(momentCentrat(f, 1))
  print(momentCentrat(f, 2))
  print(momentCentrat(f, 3))
  print(momentCentrat(f, 4))
}