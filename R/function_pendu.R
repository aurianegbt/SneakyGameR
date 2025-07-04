# PLAY --------------------------------------------------------------------


help_pendu <- function(){
  cat("Informations :\n")
  cat("\t- Le jeu du pendu autorise au maximum 11 erreurs;\n")
  cat("\t- Les accents sont ignorés ici;\n")
  cat("\t -Les réponses acceptées sont \n\t\t- les lettres seules sans accent,\n\t\t- soit un mot de la même taille que le mot recherché;\n")
  cat("\t- Pour quitter à tout moment, saisir '!';\n")
  cat("\t- Pour ravoir ces règles, saisir '?'.\n")
}

test.help_pendu <- function(reponse,mot){
  if(identical(reponse,"!")){
    cat("Le mot était :",mot,"\n")
    stop()
  }else if(identical(reponse,"?")){
    help_pendu()
  }
}

# penduState <- c("\n   \n        \n       \n      \n        \n         \n____\n",
#                 "\n   \n  |      \n  |     \n  |    \n  |      \n  |       \n__|__\n",
#                 "\n   _______\n  |      \n  |     \n  |    \n  |      \n  |       \n__|__\n",
#                 "\n   _______\n  |/      \n  |     \n  |    \n  |      \n  |       \n__|__\n",
#                 "\n   _______\n  |/      |\n  |     \n  |    \n  |      \n  |       \n__|__\n",
#                 "\n   _______\n  |/      |\n  |      ( )\n  |    \n  |      \n  |       \n__|__\n",
#                 "\n   _______\n  |/      |\n  |      ( )\n  |       | \n  |      \n  |       \n__|__\n",
#                 "\n   _______\n  |/      |\n  |      ( )\n  |      /|\n  |      \n  |       \n__|__\n",
#                 "\n   _______\n  |/      |\n  |      ( )\n  |      /|\\\n  |      \n  |       \n__|__\n",
#                 "\n   _______\n  |/      |\n  |      ( )\n  |      /|\\\n  |      / \n  |       \n__|__\n",
#                 "\n   _______\n  |/      |\n  |      ( )\n  |      /|\\\n  |      / \\\n  |       \n__|__\n")
