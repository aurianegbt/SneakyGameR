#' Play Yams !
#'
#' @returns launch a game of Yams.
#' @export
#'
#' @examples
#' play()
play <- function(){
  cat("--------- Début de la partie ---------\n")
  help()
  joueurs <- readline(prompt = "Veuillez saisir le noms des joeurs :")
  test.help(sheet,joueurs)
  joueurs <- stringr::str_split(joueurs,",")[[1]]
  while(length(joueurs)==0){
    joueurs <- readline(prompt = "Saisie invalide, veuillez saisir le noms des joeurs :")
    test.help(sheet,joueurs)
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
}
