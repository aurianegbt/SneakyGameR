help_421 <- function(){
  cat("Informations :\n")
  cat("\t- Pour saisir les dés à conserver, veuillez indiquer leur rang séparés de virgule;\n")
  cat("\t- Pour relancer tous les dés, '0' peut être saisi;\n")
  cat("\t- Pour quitter à tout moment, saisir 'Q';\n")
  cat("\t- Pour ravoir ces règles, saisir '?'.\n")
}

test.help_421 <- function(reponse){
  if(identical(reponse,"Q")){
    stop()
  }else if(identical(reponse,"?")){
    help_421()
  }
}
