#' Play games ! # can't stop
#'
#' @returns Lance une partie d'un jeu ! Jeux disponibles : Yams, code, pendu, 421, farkle.
#' @export
#'
#' @examples
#' \dontrun{
#' rules("Yams")
#' play("Yams")
#' }
play <- function(game="Yams"){
  if(tolower(game)=="yams"){
    sheet <- play.yams()
    return(invisible(sheet))
  }else if(tolower(game)=="code"){
    play.mind()
  }else if(tolower(game)=="pendu"){
    play.pendu()
  }else if(tolower(game)=="421"){
    play.421()
  }else if(tolower(game)=="farkle"){
    play.farkle()
  }else{
    stop("Jeu inconnu, jeux disponibles : Yams, code, pendu, 421, farkle.")
  }
}


# farkle ------------------------------------------------------------------

play.farkle <- function(){
  cat("--------- Début de la partie ---------\n")
  help_farkle()
  joueurs <- readline(prompt = "Veuillez saisir le noms des joeurs :")
  test.help_farkle(joueurs)
  joueurs <- stringr::str_split(joueurs,",")[[1]]
  while(length(joueurs)==0){
    joueurs <- readline(prompt = "Saisie invalide, veuillez saisir le noms des joeurs :")
    test.help_farkle(joueurs)
    joueurs <- stringr::str_split(joueurs,",")[[1]]
  }

  scoreTOT <- setNames(rep(0,length(joueurs)),joueurs)
  stop <- FALSE
  tour = 1

  while(!stop && (length(joueurs)!=1 || tour <=10)){
    for(joueur in joueurs){
      if(length(joueurs)==1){
        cat("Score actuel :",unname(scoreTOT),"\n")
      }else{
        cat("Scores actuels :\n")
        print(scoreTOT)
      }
      if(length(joueurs)!=1){
        cat("\nC'est au tour de",joueur," !\n")
      }else{
        if(tour==10){
          cat("Dernier tour ! ")
        }
        cat("\nTour n°",tour,"/10:\n")
      }
      tour = tour +1
      scoreTOT[joueur] <- scoreTOT[joueur] + tour_farkle(joueur)

      if(scoreTOT[joueur]>5000){
        stop <- TRUE
        if(length(joueurs)!=1){
          cat("Dernier tour de jeu !")
          cat("--------------------------------------\n")
          if(length(joueurs)==1){
            cat("Score actuel :",unname(scoreTOT),"\n")
          }else{
            cat("Scores actuels :\n")
            print(scoreTOT)
          }
          break
        }
      }
      cat("--------------------------------------\n")
    }
  }
  if(length(joueurs)>1){
    last <- which.max(scoreTOT)
    for(j in 1:(length(joueurs)-1)){
      joueur <- rep(joueurs,2)[last+j]
      if(length(joueurs)!=1){
        cat("C'est au tour de",joueur," pour son dernier tour !\n")
      }
      scoreTOT[joueur] <- scoreTOT[joueur] + tour_farkle(joueur)
      cat("--------------------------------------\n")
    }
    cat("\n Score finaux:\n")
    print(scoreTOT)
    cat("\n")
    cat(toupper(names(which.max(scoreTOT))),"WON !!!\n")
  }else{
    if(scoreTOT<5000){
      cat("LOOSER...\n")
    }else{
      cat("YOU WON !\n")
    }
  }
  return(invisible(scoreTOT))
}


# 421 ---------------------------------------------------------------------

play.421 <- function(){
  cat("--------- Début de la partie ---------\n")
  help_421()
  joueurs <- readline(prompt = "Veuillez saisir le noms des joeurs :")
  test.help_421(joueurs)
  joueurs <- stringr::str_split(joueurs,",")[[1]]
  while(length(joueurs)==0){
    joueurs <- readline(prompt = "Saisie invalide, veuillez saisir le noms des joeurs :")
    test.help_421(joueurs)
    joueurs <- stringr::str_split(joueurs,",")[[1]]
  }

  score <- setNames(rep(NA,length(joueurs)),joueurs)

  for(joueur in joueurs){
    cat("--------------------------------------\n")
    if(length(joueurs)!=1){
      cat("C'est au tour de",joueur," !\n")
    }
    des <- lancer_des(n=3)
    score[joueur] <- 1

    while(!setequal(c(4,2,1),as.numeric(des))){
      cat("Dés :", des, "\n")
      reponse <- readline(prompt = "\tQuels dés veux-tu garder ? ")
      test.help_421(reponse)
      reponse <- as.numeric(stringr::str_split(reponse,",")[[1]])
      while(!identical(reponse,0)  & any(!(reponse %in% 1:length(des))) & setequal(c(1,2,3),reponse)){
        reponse <- readline(prompt = "\t[Dés invalides] Quels dés veux-tu garder ? ")
        test.help_421(reponse)
        reponse <- as.numeric(stringr::str_split(reponse,",")[[1]])
      }
      if(identical(reponse,0)) {
        des <- lancer_des(n=3)
      } else {
        des[setdiff(1:3,reponse)] <- lancer_des(3 - length(reponse))
        score[joueur] <- score[joueur] + 1
      }
    }
    cat("Dés :", des, "\n")
  }
  cat("Score Finaux :")
  print(score)
  if(length(joueurs)>1){
    cat(toupper(names(which.min(score))),"WON !!!\n")
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


# codemind --------------------------------------------------------------

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
    reponse <- tour_codemind(code)
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
  test.help_yams(sheet,joueurs)
  joueurs <- stringr::str_split(joueurs,",")[[1]]
  while(length(joueurs)==0){
    joueurs <- readline(prompt = "Saisie invalide, veuillez saisir le noms des joeurs :")
    test.help_yams(sheet,joueurs)
    joueurs <- stringr::str_split(joueurs,",")[[1]]
  }

  sheet <- init_sheet(joueurs)
  cat("Feuille des scores :\n")
  print.sheetYams(sheet)

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
      print.sheetYams(sheet)
      cat("--------------------------------------\n")
    }
  }
  if(length(joueurs)>1){
    cat(toupper(names(which.max(colSums(sheet[,-1]))))," WON !!!\n")
  }

  return(invisible(sheet))
}
