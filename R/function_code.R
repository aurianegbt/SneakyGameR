code_selection <- function(){
  return(sample(1:8,4,replace=TRUE))
}

correct <- function(reponse,code){
  return(sum(reponse==code))
}

almost <- function(reponse,code){
  if(any(reponse==code)){
    rep <- reponse[-which(reponse==code)]
    cd <- code[-which(reponse==code)]
  }else{
    rep <- reponse
    cd <- code
  }
  res <- 0
  for(r in rep){
    if(r %in% cd){
      res <- res + 1
      cd <- cd[-which(cd==r)[1]]
    }
  }
  return(res)
}

tour_codemind <- function(code){
  reponse <- readline(prompt = "Saisir un code : \n")
  test.help_mind(reponse,code)
  reponse <- as.numeric(stringr::str_split(reponse,"-")[[1]])
  while(length(reponse)!=4 & !all(reponse %in% 1:8)){
    reponse <- readline(prompt = "[Saisie Invalide] Saisir un code : \n")
    test.help_mind(reponse,code)
    reponse <- as.numeric(stringr::str_split(reponse,"-")[[1]])
  }
  cat(paste0(c(rep("\u26AA",correct(reponse,code)),rep("\u25EF",almost(reponse,code)),rep("\u00B7",4-correct(reponse,code)-almost(reponse,code))),collapse=""),"\n")

  return(reponse)
}


# PLAY --------------------------------------------------------------------


help_mind <- function(){
  cat("Informations :\n")
  cat("\t- Le code est composé de 4 chiffres entre 1 et 8 ;\n")
  cat("\t- Le joueur a 12/10/8 tours pour le deviner selon le niveau (Difficile/Moyen/Facile);\n")
  cat("\t- Pour saisir un code, saisir 5 chiffres espacés de '-';\n")
  cat("\t- \u00B7 indique un chiffre non inclu dans le code ;\n")
  cat("\t- \u25EF indique un chiffre inclu dans le code mais mal placé;\n")
  cat("\t- \u26AA  indique un chiffre inclu dans le code et bien placé;\n")
  cat("\t- Pour quitter à tout moment, saisir 'Q';\n")
  cat("\t- Pour ravoir ces règles, saisir '?'.\n")
}

test.help_mind <- function(reponse,code){
  if(identical(tolower(reponse),"q")){
    cat("Le code était :",code,"\n")
    stop()
  }else if(identical(reponse,"?")){
    help_mind()
  }
}

