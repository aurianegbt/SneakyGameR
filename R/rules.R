# to add : can't stop express, Knister, 99

#' Games rules
#'
#' @param game name of the game.
#'
#' @returns print les règles du jeu dans la console.
#' @export
#'
#' @examples
rules <- function(game="Yams"){
  if(game=="421"){
    cat("But du jeu :\n")
    cat("\tAvoir un dé 4, un 2 et un 1, en le moins de tour possible.\n")
    cat("Déroulement d'un tour:\n")
    cat("\tChaque joueur lance 3 dés et décide d'en conserver un certain nombre. Les dés restants sont relancés, etc. ")
  }else if(game=="pendu"){
    cat("But du jeu :\n")
    cat("\tDeviner le mot en proposant des lettres (accents ignorés).\n")
    cat("Déroulement d'un tour:\n")
    cat("\tLe joueur propose une lettre ou tente de deviner le mot cacher. Au bout de 11 erreurs, le joueur a perdu.\n")
  }else if(game=="farkle"){
    cat("But du jeu:\n")
    cat("\tAtteindre 5000 points (en moins de 10 tour si seul).\n")
    cat("Déroulement d'un tour:\n")
    cat("\tAu début de chaque tour, le joueur lance tous les dés à la fois. Après chaque lancer, un ou plusieurs dés de score doivent 'marqué', permettant d'ajouter des points à une réserve. Le joueur peut décider de terminer son tour et valider le score accumulé dans la réserve jusqu'à présent, ou continuer à lancer les dés restants, au risque de perdre la réserve. Si le joueur peut marqué les six dés et donc plus de dés à relancer, il a des 'dés chauds' et peut continuer son tour en relançant les six dés et continuer d'accumuler le score du tour dans sa réserve. Il n'y a pas de limite au nombre de 'dés chauds' qu'un joueur peut lancer en un tour. Si le joueur lance des dés et n'a aucun marquage possible, alors il perd sa réserve de points et passe au tour suivant.\n")
    cat("\t Les marquages possibles sont:\n")
    cat("\t\t- un dé 1 vaut 100pts;\n")
    cat("\t\t- un dé 5 vaut 50pts;\n")
    cat("\t\t- Trois dés 1/2/3/4/5/6 valent 1000/200/300/400/500/600 pts;\n")
    cat("\t\t- Quatre dés 1/2/3/4/5/6 valent 2000/400/600/800/1000/1200 pts;\n")
    cat("\t\t- Cinq dés 1/2/3/4/5/6 valent 4000/800/1200/1600/2000/2400 pts;\n")
    cat("\t\t- Six dés 1/2/3/4/5/6 valent 8000/1600/2400/3200/4000/4800 pts;\n")
    cat("\t\t- Trois paires vaut 1000pts;\n")
    cat("\t\t- Suite vaut 1500pts;\n")
    cat("\t\t- Six dés sans marquage possible vaut 500pts.\n")
  }else if(game=="mastermind"){
    cat("But du jeu:\n")
    cat("\tDeviner la combinaison secrète avant la fin des tours.\n")
    cat("Déroulement d'un tour:\n")
    cat("\tA chaque tour, proposer un code. Le nombre de chiffre bien placé, mal placé et non présent dans le code seront alors indiquer.")
  }
}
