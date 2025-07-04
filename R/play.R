#' Play games !
#'
#' @returns launch a game of Yams, Pendu or mind and return the sheets of score.
#' @export
#'
#' @examples
#' \dontrun{
#' play("Yams")
#' }
play <- function(game="Yams"){
  if(tolower(game)=="yams"){
    sheet <- play.yams()
    return(invisible(sheet))
  }else if(tolower(game)=="mind"){
    play.mind()
  }else if(tolower(game)=="pendu"){
    play.pendu()
  }else{
    stop("Jeu inconnue, jeux disponibles : Yams, Mind")
  }
}


# PENDU  ------------------------------------------------------------------

play.pendu <- function(){
  cat("--------- Début de la partie ---------\n")
  help_pendu()
  mot <- sample(databasePendu,size=1)
  print <- readline(prompt = "Souhaitez-vous désactiver l'affichage du pendu ? [Y/N] ")
  test.help_pendu(print,mot)
  while(!(tolower(print) %in% c("y","n"))){
    print <- readline(prompt = "Saisie invalide, souhaitez-vous désactiver l'affichage du pendu ? [Y/N] ")
    test.help_pendu(print,mot)
  }
  print <- tolower(print)!="y"

  win <- FALSE
  nb_err = 0
  lettresProposees <- c()
  etat <- rep("_",nchar(mot))
  cat("--------------------------------------\n")

  while(nb_err <= 10 && !win){
    cat("\n>>>",etat,"<<<\n")
    if(length(lettresProposees)!=0){
      cat("\tLettre(s) déjà proposée(s) :",toupper(abc[abc %in% tolower(lettresProposees)]))
    }
    lettre <- readline("Proposez une lettre ou un mot : ")
    test.help_pendu(lettre,mot)
    while(((nchar(lettre)==1 && !(tolower(lettre) %in% abc)) ||  lettre %in% lettresProposees) && nchar(lettre)!=nchar(mot)){
      lettre <- readline("Saisie invalide, Proposez une lettre : ")
      test.help_pendu(lettre,mot)
    }
    lettresProposees <- c(lettresProposees,lettre)
    if(nchar(lettre)==nchar(mot)){
      if(tolower(lettre)==mot){
        win <- TRUE
        break
      }else{
        nb_err <- nb_err + 1
        if(print){
          cat(penduState[nb_err])
        }else{
          cat("\n Erreur ",nb_err,"/11")
        }
      }
    }else{
      if(tolower(lettre) %in% stringr::str_split(mot,"")[[1]]){
        etat[which(stringr::str_split(mot,"")[[1]]==tolower(lettre))] <- toupper(lettre)
        if(!("_" %in% etat)){
          win <- TRUE
          break
        }
      }else{
        nb_err <- nb_err + 1
        if(print){
          cat(penduState[nb_err])
        }else{
          cat("\n Erreur ",nb_err,"/9")
        }
      }
    }
  }
  cat("Le mot était :",mot,"\n")
  if(win){
    cat("YOU HAVE WON !!")
  }else{
    cat("LOOSER...")
  }
  return(invisible(mot))
}


# Mastermind --------------------------------------------------------------

play.mind <- function(){
  cat("--------- Début de la partie ---------\n")
  help_mind()
  code <- code_selection()
  level <- readline(prompt = "Veuillez saisir la difficulté [D/M/F] :")
  test.help_mind(level,code)
  while(!(tolower(level) %in% c("d","m","f"))){
    level <- readline(prompt = "Saisie invalide, veuillez saisir la difficulté [D/M/F] :")
    test.help_mind(level,code)
  }
  nb_tour <- ifelse(tolower(level)=="d",8,ifelse(tolower(level)=="m",10,12))

  win <- FALSE
  cat("--------------------------------------\n")

  for(i in 1:nb_tour){
    cat("Tour n°",i,"/",nb_tour,":\n")
    reponse <- tour_mastermind(code)
    if(all(reponse==code)){
      win <- TRUE
      break
    }
    cat("\n----------------\n")
  }
  cat("Le code était :",code,"\n")
  if(win){
    cat("YOU HAVE WON !!")
  }else{
    cat("LOOSER...")
  }
  return(invisible(code))
}





# Yams --------------------------------------------------------------------
play.yams <- function(){
  cat("--------- Début de la partie ---------\n")
  help_yams()
  joueurs <- readline(prompt = "Veuillez saisir le noms des joeurs :")
  test.help(sheet,joueurs)
  joueurs <- stringr::str_split(joueurs,",")[[1]]
  while(length(joueurs)==0){
    joueurs <- readline(prompt = "Saisie invalide, veuillez saisir le noms des joeurs :")
    test.help_yams(sheet,joueurs)
    joueurs <- stringr::str_split(joueurs,",")[[1]]
  }

  sheet <- init_sheet(joueurs)
  cat("Feuille des scores :\n")
  print.sheet(sheet)

  yams_save <- setNames(rep(FALSE,length(joueurs)),joueurs)

  cat("--------------------------------------\n")

  for(i in 1:13){
    for(joueur in joueurs){
      if(length(joueurs)!=1){
        cat("C'est au tour de",joueur," pour le tour n°",i,"!\n")
      }else{
        cat("Tour n°",i,":\n")
      }
      res <- tour_yams(sheet,joueur)
      yams <- is.yams(res)
      if(yams & !identical(sheet[sheet$Nom=="Yams",joueur],0)){
        cat(" YAMS !!")
      }
      sheet <- update_sheet(res,sheet,joueur,yams_save[joueur])
      if(yams & !is.na(sheet[sheet$Nom=="Yams",joueur]) ){
        yams_save[joueur] <- TRUE
      }
      cat("Feuille des scores :\n")
      print.sheet(sheet)
      cat("--------------------------------------\n")
    }
  }
  if(length(joueurs)>1){
    cat(toupper(names(which.max(colSums(sheet[,-1]))))," WON !!!\n")
  }

  return(invisible(sheet))
}
