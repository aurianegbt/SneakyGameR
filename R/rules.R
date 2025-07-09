#' Games rules
#'
#' @param game name of the game.
#'
#' @returns print les règles du jeu dans la console.
#' @export
#'
#' @examples
rules <- function(game="Yams"){
  if(tolower(game)=="421"){
    cat("But du jeu :\n")
    trait(12)
    cat("Avoir un dé 4, un 2 et un 1, en le moins de tour possible.\n")
    cat("\nDéroulement d'un tour:\n")
    trait(22)
    cat(strwrap("Chaque joueur lance 3 dés et décide d'en conserver un certain nombre. Les dés restants sont relancés, etc. ",width = 70),sep="\n")
  }else if(tolower(game)=="pendu"){
    cat("But du jeu :\n")
    trait(12)
    cat("Deviner le mot en proposant des lettres (accents ignorés).\n")
    cat("\nDéroulement d'un tour:\n")
    trait(22)
    cat(strwrap("Le joueur propose une lettre ou tente de deviner le mot cacher. Au bout de 11 erreurs, le joueur a perdu.",width = 70),sep="\n")
  }else if(tolower(game)=="farkle"){
    cat("But du jeu :\n")
    trait(12)
    cat("Atteindre 5000 points (en moins de 10 tour si seul).\n")
    cat("\nDéroulement d'un tour:\n")
    trait(22)
    cat(strwrap("Au début de chaque tour, le joueur lance tous les dés à la fois. Après chaque lancer, un ou plusieurs dés de score doivent 'marqué', permettant d'ajouter des points à une réserve. Le joueur peut décider de terminer son tour et valider le score accumulé dans la réserve jusqu'à présent, ou continuer à lancer les dés restants, au risque de perdre la réserve. Si le joueur peut marqué les six dés et donc plus de dés à relancer, il a des 'dés chauds' et peut continuer son tour en relançant les six dés et continuer d'accumuler le score du tour dans sa réserve. Il n'y a pas de limite au nombre de 'dés chauds' qu'un joueur peut lancer en un tour. Si le joueur lance des dés et n'a aucun marquage possible, alors il perd sa réserve de points et passe au tour suivant.",width=70),sep="\n")
    cat("Les marquages possibles sont:\n")
    cat("\t- un dé 1 vaut 100pts;\n")
    cat("\t- un dé 5 vaut 50pts;\n")
    cat("\t- Trois dés 1/2/3/4/5/6 valent 1000/200/300/400/500/600 pts;\n")
    cat("\t- Quatre dés 1/2/3/4/5/6 valent 2000/400/600/800/1000/1200 pts;\n")
    cat("\t- Cinq dés 1/2/3/4/5/6 valent 4000/800/1200/1600/2000/2400 pts;\n")
    cat("\t- Six dés 1/2/3/4/5/6 valent 8000/1600/2400/3200/4000/4800 pts;\n")
    cat("\t- Trois paires vaut 1000pts;\n")
    cat("\t- Suite vaut 1500pts;\n")
    cat("\t- Six dés sans marquage possible vaut 500pts.\n")
  }else if(tolower(game)=="mastermind"){
    cat("But du jeu :\n")
    trait(12)
    cat("Deviner la combinaison secrète de 4 chiffres entre 1 et 8 avant la fin des tours.\n")
    cat("\nDéroulement d'un tour:\n")
    trait(22)
    cat(strwrap("A chaque tour, proposer un code. Le nombre de chiffre bien placé, mal placé et non présent dans le code seront alors indiquer.",width = 70),sep="\n")
  }else if(tolower(game)=="yams"){
    cat("But du jeu :\n")
    trait(12)
    cat("Enchaîner les combinaisons à l'aide de cinq dés pour remporter un maximum de points.\n")
    cat("\nDéroulement d'un tour:\n")
    trait(22)
    cat(strwrap("Le jeu se déroule tour à tour. À son tour, chaque joueur lance les cinq dés dans le but d'obtenir une figure (c'est-à-dire, un arrangement particulier des dés) permettant de marquer des points. Le joueur a le droit à trois lancers de dés par tour. Après chaque lancer, il peut choisir de mettre de côté certains de ces dés, et relancer le reste.\n Après un maximum de trois lancers, le joueur doit choisir entre marquer une figure dont il remplit les conditions, ou barrer une figure. Marquer une figure permet d'inscrire le score dans la ligne et donc d'obtenir les points correspondants. S'il n'a aucune combinaison de dés, ou s'il ne peut pas marquer de figure, il doit barrer une figure et inscrire 0 dans la ligne de son choix : il ne pourra pas marquer les points de cet item dans la partie.",width = 70),sep="\n")
    cat("Le calcul des points se fait comme suit :\n")
    cat("\t- Pour les figures de 1 à 6, on marque la somme des dés correspondants;\n")
    cat("\t- Pour le brelan, il faut obtenir 3 dés identiques et on marque la somme des dés.;\n")
    cat("\t- Pour le carré, il faut obtenir 4 dés identiques et on marque la somme des dés.;\n")
    cat("\t- Pour le full, il faut obtenir 3 dés identiques et une paire, et on marque 25pts.;\n")
    cat("\t- Pour la petite suite (small), il faut obtenir 4 dés qui se suivent, et on marque 30pts.;\n")
    cat("\t- Pour la grande suite (large), il faut obtenir 5 dés qui se suivent, et on marque 40pts.;\n")
    cat("\t- Pour le Yams, il faut obtenir 5 dés identique, et on marque 50pts.\n\t\tSi un Yams est obtenu après avoir marqué déjà 50pts dans la case, un bonus de 100 pts est ajouté;\n")
    cat("\t - Pour la chance, on marque la somme des dés.\n")
  }
}

trait <- function(n){
  cat(rep("\u203E",n),sep = "","\n")
}
