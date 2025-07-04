#' Play games !
#'
#' @returns launch a game of Yams or mind and return the sheets of score.
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
  }else{
    stop("Jeu inconnue, jeux disponibles : Yams, Mind")
  }
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
