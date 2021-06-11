media<-function(f){
  result=tryCatch({
    return(integrate(function(x){
      produs<-x*f(x);
      return(produs);
    },-Inf,Inf)$value);
  }, eroare = function(e){
    warning(c("Nu se poate calcula media"));
    return(0);
  });
}

dispersia<-function(f){
  medie<-media(f)
  if(medie==0){
    warning(c("Nu se poate calcula dispersia fara medie"));
    return(0);
  }
  result=tryCatch({
    return(integrate(function(x){
      produs<-((x-medie)^2)*f(x);
      return(produs);
    },-Inf,Inf)$value);
  }, eroare = function(e){
    warning(c("Nu se poate calcula dispersia"));
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
    warning(c("Nu se poate calcula momentul centrat"));
    return(0);
  });
}

momentInitial<-funtion(f,ordin){
  result=tryCatch({
  return(integrate(function(x){
      produs<-(x^r)*f(x);
      return(produs);
    },-Inf,Inf)$value);
  }, eroare = function(e){
    warning(c("Nu se poate calcula momentul initial"));
    return(0);
  });
}

patruMomente <- function(f)
{
  print('Primele 4 momente initiale sunt:')
  print(moment_initial(f, 1))
  print(moment_initial(f, 2))
  print(moment_initial(f, 3))
  print(moment_initial(f, 4))
  print('Primele 4 momente centrate sunt:')
  print(moment_centrat(f, 1))
  print(moment_centrat(f, 2))
  print(moment_centrat(f, 3))
  print(moment_centrat(f, 4))
}