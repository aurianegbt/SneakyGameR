# Lancer de dés -----------------------------------------------------------
lancer_des <- function(n=5) {
  sample(1:6, n, replace = TRUE)
}

# Tests -------------------------------------------------------------------
is.brelan <- function(des){
  return(any(table(des)>=3))
}

is.carre <- function(des){
  return(any(table(des)>=4))
}

is.full <- function(des){
  return(setequal(c(2,3),table(des)))
}

is.small <- function(des){
  return(all(c(1,2,3,4) %in% des) |
           all(c(2,3,4,5) %in% des) |
           all(c(3,4,5,6) %in% des))
}

is.large <- function(des){
  return(all(c(1,2,3,4,5) %in% des) |
           all(c(2,3,4,5,6) %in% des))
}

is.yams <- function(des){
  return(sd(des)==0)
}


# Score -------------------------------------------------------------------

init_sheet <- function(joueurs){
  sheet <- data.frame(Nom=c("1","2","3","4","5","6","Brelan","Carré","Small","Large","Full","Chance","Yams"))
  for(j in joueurs){
    sheet <- cbind(sheet,rep(NA,13))
  }
  colnames(sheet)[-1] <- joueurs
  return(sheet)
}

update_sheet <- function(des,sheet,joueur,yams=FALSE){
  if(yams & is.yams(des)){
    sheet[sheet$Nom=="Yams",joueur] <- sheet[sheet$Nom=="Yams",joueur] + 100
  }

  warning <- TRUE
  while(warning){
    reponse <- readline(prompt = "Quelle case souhaites-tu remplir ?")
    test.help(sheet,reponse)
    while(!(reponse %in% sheet$Nom) || !is.na(sheet[sheet$Nom==reponse,joueur])){
      reponse <- readline(prompt = "[Saisie invalide] Quelle case souhaites-tu remplir ?")
      test.help(sheet,reponse)
    }
    if(reponse %in% as.character(1:6)){
      score <- sum(des==as.numeric(reponse))*as.numeric(reponse)
    }else if(tolower(reponse)=="brelan"){
      score <- ifelse(is.brelan(des),sum(des),0)
    }else if(tolower(reponse)=="carré" | tolower(reponse)=="carre"){
      score <- ifelse(is.carre(des),sum(des),0)
    }else if(tolower(reponse)=="full"){
      score <- ifelse(is.full(des),25,0)
    }else if(tolower(reponse)=="small"){
      score <- ifelse(is.small(des),30,0)
    }else if(tolower(reponse)=="large"){
      score <- ifelse(is.large(des),40,0)
    }else if(tolower(reponse)=="chance"){
      score <- sum(des)
    }else if(tolower(reponse)=="yams"){
      score <- ifelse(is.yams(des),50,0)
    }
    if(score==0){
      write <- readline(paste0("Tu t'apprêtes à inscrire un 0 dans la case",reponse,", souhaites-tu confirmer ? [Y/N]"))
      test.help(sheet,write)
      if(identical(write,"Y")){
        warning <- FALSE
      }
    }else{
      warning <- FALSE
    }
  }
  sheet[sheet$Nom==reponse,joueur] <- score

  return(sheet)
}

#' @export
print.sheet <- function(sheet){
  part1 <- sheet[sheet$Nom %in% as.character(1:6),]
  part1[is.na(part1)] <- "-"
  part1 <- rbind(part1,c(Nom="Sous-Total",colSums(sheet[sheet$Nom %in% as.character(1:6),-1,drop=FALSE],na.rm = TRUE)))
  part1 <- rbind(part1,c(Nom="Ecart",colSums(sheet[sheet$Nom %in% as.character(1:6),-1,drop=FALSE],na.rm = TRUE)-63))
  part1 <- rbind(part1, c(Nom="Bonus",ifelse(as.numeric(part1[7,-1])>=63,35,0)))

  part2 <- sheet[!(sheet$Nom %in% as.character(1:6)),]
  part2[is.na(part2)] <- "-"
  part2 <- rbind(part2,rep(" ",3))
  part2 <- rbind(part2,c(Nom="Total",colSums(sheet[,-1,drop=FALSE],na.rm = TRUE)+ifelse(as.numeric(part1[7,-1])>=63,35,0)))

  blank = data.frame(Nom=rep(" ",9))

  to.print <- as.data.frame(cbind(part1,cbind(blank,part2)))
  colnames(to.print)[which(colnames(to.print)=="Nom")] <- "  "

  print(to.print,row.names = FALSE)
}


# Tour de jeu  ------------------------------------------------------------

tour_yams <- function(sheet,joueur) {
  des <- lancer_des()
  cat("\tPremier lancer :", des, "\n")

  for (i in 2:3) {
    reponse <- readline(prompt = "\tQuels dés veux-tu garder ? ")
    test.help(sheet,reponse)
    if(!identical(reponse,"all")){
      reponse <- as.numeric(stringr::str_split(reponse,",")[[1]])
    }
    while(!identical(reponse,0) & !identical(reponse,"all") & any(!(reponse %in% 1:length(des)))){
      reponse <- readline(prompt = "\t[Dés invalides] Quels dés veux-tu garder ? ")
      test.help(sheet,reponse)
      if(!identical(reponse,"all")){
        reponse <- as.numeric(stringr::str_split(reponse,",")[[1]])
      }
    }
    if(identical(reponse,"all")){
      reponse <- 1:5
    }
    if (identical(reponse,0)) {
      des <- lancer_des()
    } else {
      if(length(reponse)==5){
        break
      }
      des[setdiff(1:5,reponse)] <- lancer_des(5 - length(reponse))
    }

    cat(paste0(i, "e lancer : "), des, "\n")
  }

  return(des)
}


# Play --------------------------------------------------------------------

help <- function(){
  cat("Informations :\n")
  cat("\t- Pour saisir les joeurs veuillez espacer leur nom d'une virgule ;\n")
  cat("\t- Pour saisir les dès à conserver, veuillez indiquer leur rang séparés de virgule;\n")
  cat("\t- La feuille des scores sera réimprimée au début de chaque tour;\n")
  cat("\t- Le nom de la case saisie doit correspondre à celle de la feuille de score;\n")
  cat("\t- Pour quitter à tout moment, saisir Q;\n")
  cat("\t- Pour ravoir la feuille de score à tout moment, saisir S;\n")
  cat("\t- Pour ravoir ces règles, saisir ?.\n")
}

test.help <- function(sheet,reponse){
  if(identical(reponse,"Q")){
    stop()
  }else if(identical(reponse,"S")){
    print.sheet(sheet)
  }else if(identical(reponse,"?")){
    help()
  }
}
