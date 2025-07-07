char_marq <- function(x){
  return(paste0("c(",paste0(x,collapse=","),")"))
}

marquage_possible <- function(des){
  fData <- data.frame(Des=rep(1:6,4),
                      Occ=rep(3:6,each=6),
                      Score=c(1000,200,300,400,500,600)*rep(c(1,2,4,8),each=6))
  possible=data.frame()
  ## MARQUE FORCEMENT
  if(length(des)==0){
    return(possible)
  }
  if(length(des)==6 && all(table(des)==2)){
    return(data.frame(Type = "Trois paires",
                      Marque="1:6",
                      Relance = 0,
                      Score=1000))
  }
  if(6 %in% table(des)){
    return(data.frame(Type = "Six identiques",
                      Marque="1:6",
                      Relance= 0 ,
                      Score=ifelse(des[1]==1,10,des[1])*100*8))
  }
  if(setequal(c(1,2,3,4,5,6),des)){
    return(data.frame(Type="Suite",
                      Marque="1:6",
                      Relance=0,
                      Score = 1500))
  }
  if(!(1%in% des) && !(5 %in% des) && all(table(des))<=2 && length(des)==6){
    return(data.frame(Type="Sans marquage",
                      Marque="1:6",
                      Relance=0,
                      Score=500))
  }
  ## MARQUE PAS FORCEMENT
  if(5 %in% table(des)){
    # Marque forcement si 5 des identiques et un 1 ou un 5
    if(length(des)==5){
      return(data.frame(Type="Cinq identiques",
                        Marque=1:5,
                        Relance=0,
                        Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==5,"Score"]))
    }
    if(names(which.min(table(des))) %in% c(1,5)){
      return(data.frame(Type=paste("Cinq identiques & un",names(which.min(table(des)))),
                        Marque=1:6,
                        Relance=0,
                        Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==5,"Score"]+ifelse(names(which.min(table(des)))=="1",100,50)))
    }
    possible <- rbind(possible,
                      data.frame(Type=paste(c("Trois","Quatre","Cinq"),"identiques"),
                                 Marque=c(char_marq(which(des==names(which.max(table(des))))[c(1,2,3)]),
                                          char_marq(which(des==names(which.max(table(des))))[c(1,2,3,4)]),
                                          char_marq(which(des==names(which.max(table(des))))[c(1,2,3,4,5)])),
                                 Relance=length(des)-c(3,4,5),
                                 Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ %in% c(3,4,5),"Score"]))

    if(names(which.max(table(des))) %in% c(1,5)){
      possible <- rbind(possible,
                        data.frame(Type=paste(c("Un","Deux"),names(which.max(table(des)))),
                                   Marque=c(which(des==names(which.max(table(des))))[1],
                                            char_marq(which(des==names(which.max(table(des))))[c(1,2)])),
                                   Relance=length(des)-c(1,2),
                                   Score=ifelse(names(which.max(table(des)))=="1",100,50)*c(1,2)))
    }
    return(possible)
  }
  if(4 %in% table(des)){
    if(length(des)==4){
      return(data.frame(Type="Quatre identiques",
                        Marque="1:4",
                        Relance=0,
                        Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==4,"Score"]))
    }
    if(length(des)==5){
      if(names(which.min(table(des))) %in% c(1,5)){
        return(data.frame(Type=paste("Quatre identiques & un",names(which.min(table(des)))),
                          Marque="1:5",
                          Relance=0,
                          Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==5,"Score"]+ifelse(names(which.min(table(des)))=="1",100,50)))
      }
      possible <- rbind(possible,
                        data.frame(Type=paste(c("Trois","Quatre"),"identiques"),
                                   Marque=c(char_marq(which(des==names(which.max(table(des))))[c(1,2,3)]),
                                            char_marq(which(des==names(which.max(table(des))))[c(1,2,3,4)])),
                                   Relance=c(2,1),
                                   Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ %in% c(3,4),"Score"]))

      if(names(which.max(table(des))) %in% c(1,5)){
        possible <- rbind(possible,
                          data.frame(Type=paste(c("Un","Deux"),names(which.max(table(des)))),
                                     Marque=c(which(des==names(which.max(table(des))))[1],
                                              char_marq(which(des==names(which.max(table(des))))[c(1,2)])),
                                     Relance=c(4,3),
                                     Score=ifelse(names(which.max(table(des)))=="1",100,50)*c(1,2)))
      }
      return(possible)
    }
    if(length(des)==6){
      if(all(des %in% c(1,5))){
        return(data.frame(Type=paste("Quatre identiques & deux",names(which.min(table(des)))),
                          Marque="1:6",
                          Relance=0,
                          Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==4,"Score"]+ifelse(names(which.min(table(des)))==1,100,50)*2))
      }
      if(all(des[des!=names(which.max(table(des)))]==1)){
        return(data.frame(Type="Quatre identiques & deux 1",
                          Marque="1:6",
                          Relance=0,
                          Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==4,"Score"]+200))
      }
      if(all(des[des!=names(which.max(table(des)))]==5)){
        return(data.frame(Type="Quatre identiques & deux 5",
                          Marque="1:6",
                          Relance=0,
                          Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==4,"Score"]+100))
      }
      if(all(des[des!=names(which.max(table(des)))] %in% c(1,5))){
        return(data.frame(Type="Quatre identiques & un 1 & un 5",
                          Marque="1:6",
                          Relance=0,
                          Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==4,"Score"]+150))
      }

      possible <- rbind(possible,
                        data.frame(Type=paste(c("Trois","Quatre"),"identiques"),
                                   Marque=c(char_marq(which(des==names(which.max(table(des))))[c(1,2,3)]),
                                            char_marq(which(des==names(which.max(table(des))))[c(1,2,3,4)])),
                                   Relance=c(3,2),
                                   Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ %in% c(3,4),"Score"]))

      if(1 %in% des[des!=names(which.max(table(des)))]){
        possible <- rbind(possible,
                          data.frame(Type=paste(c("Trois","Quatre"),"identiques & un 1"),
                                     Marque=c(char_marq(union(which(des==names(which.max(table(des))))[c(1,2,3)],which(des==1))),
                                              char_marq(union(which(des==names(which.max(table(des))))[c(1,2,3,4)],which(des==1)))),
                                     Relance=c(2,1),
                                     Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ %in% c(3,4),"Score"]+100))
      }
      if(5 %in% des[des!=names(which.max(table(des)))]){
        possible <- rbind(possible,
                          data.frame(Type=paste(c("Trois","Quatre"),"identiques & un 5"),
                                     Marque=c(char_marq(union(which(des==names(which.max(table(des))))[c(1,2,3)],which(des==5))),
                                              char_marq(union(which(des==names(which.max(table(des))))[c(1,2,3,4)],which(des==5)))),
                                     Relance=c(2,1),
                                     Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ %in% c(3,4),"Score"]+50))
      }
      if(names(which.max(table(des))) %in% c(1,5)){
        possible <- rbind(possible,
                          data.frame(Type=paste(c("Un","Deux"),names(which.max(table(des)))),
                                     Marque=c(which(des==names(which.max(table(des))))[1],
                                              char_marq(which(des==names(which.max(table(des))))[c(1,2)])),
                                     Relance=c(5,4),
                                     Score=ifelse(names(which.max(table(des)))=="1",100,50)*c(1,2)))
        if(1 %in% des[des!=names(which.max(table(des)))]){
          possible <- rbind(possible,
                            data.frame(Type=paste(c("Un","Deux"),names(which.max(table(des))),"& un 1"),
                                       Marque=c(char_marq(union(which(des==names(which.max(table(des))))[1],which(des==1))),
                                                char_marq(union(which(des==names(which.max(table(des))))[c(1,2)],which(des==1)))),
                                       Relance=c(4,3),
                                       Score=ifelse(names(which.max(table(des)))=="1",100,50)*c(1,2)+100))
        }
        if(5 %in% des[des!=names(which.max(table(des)))]){
          possible <- rbind(possible,
                            data.frame(Type=paste(c("Un","Deux"),names(which.max(table(des))),"& un 5"),
                                       Marque=c(char_marq(union(which(des==names(which.max(table(des))))[1],which(des==5))),
                                                char_marq(union(which(des==names(which.max(table(des))))[c(1,2)],which(des==5)))),
                                       Relance=c(4,3),
                                       Score=ifelse(names(which.max(table(des)))=="1",100,50)*c(1,2)+50))
        }
      }

      return(possible)
    }
  }
  if(setequal(rep(3,2),table(des))){
    return(data.frame(Type="2 Trois identiques",
                      Marque="1:6",
                      Relance=0,
                      Score=sum(fData[fData$Des %in% names(table(des)) & fData$Occ==3,"Score"])))
  }
  if(3 %in% table(des)){
    if(length(des)==3){
      return(data.frame(Type="Trois identiques",
                        Marque="1:3",
                        Relance=0,
                        Score=fData[fData$Des==des[1] & fData$Occ==3,"Score"]))
    }
    if(length(des)==4){
      if(names(which.min(table(des))) %in% c(1,5)){
        return(data.frame(Type=paste("Trois identiques & un",names(which.min(table(des)))),
                          Marque="1:4",
                          Relance=0,
                          Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==4,"Score"]+ifelse(names(which.min(table(des)))=="1",100,50)))
      }
      possible <- rbind(possible,
                        data.frame(Type="Trois identiques",
                                   Marque=char_marq(which(des==names(which.max(table(des))))[c(1,2,3)]),
                                   Relance=1,
                                   Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==3,"Score"]))

      if(names(which.max(table(des))) %in% c(1,5)){
        possible <- rbind(possible,
                          data.frame(Type=paste(c("Un","Deux"),names(which.max(table(des)))),
                                     Marque=c(which(des==names(which.max(table(des))))[1],
                                              char_marq(which(des==names(which.max(table(des))))[c(1,2)])),
                                     Relance=c(3,2),
                                     Score=ifelse(names(which.max(table(des)))=="1",100,50)*c(1,2)))
      }
      return(possible)
    }
    if(length(des)==5){
      if(all(des %in% c(1,5))){
        return(data.frame(Type=paste("Trois identiques & deux",names(which.min(table(des)))),
                          Marque="1:5",
                          Relance=0,
                          Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==3,"Score"]+ifelse(names(which.min(table(des)))==1,100,50)*2))
      }
      if(all(des[des!=names(which.max(table(des)))]==1)){
        return(data.frame(Type="Trois identiques & deux 1",
                          Marque="1:5",
                          Relance=0,
                          Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==3,"Score"]+200))
      }
      if(all(des[des!=names(which.max(table(des)))]==5)){
        return(data.frame(Type="Trois identiques & deux 5",
                          Marque="1:5",
                          Relance=0,
                          Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==3,"Score"]+100))
      }
      if(all(des[des!=names(which.max(table(des)))] %in% c(1,5))){
        return(data.frame(Type="Trois identiques & un 1 & un 5",
                          Marque="1:5",
                          Relance=0,
                          Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==3,"Score"]+150))
      }

      possible <- rbind(possible,
                        data.frame(Type="Trois identiques",
                                   Marque=char_marq(which(des==names(which.max(table(des))))),
                                   Relance=2,
                                   Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==3,"Score"]))

      if(1 %in% des[des!=names(which.max(table(des)))]){
        possible <- rbind(possible,
                          data.frame(Type="Trois identiques & un 1",
                                     Marque=char_marq(union(which(des==names(which.max(table(des)))),which(des==1))),
                                     Relance=1,
                                     Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==3,"Score"]+100))
      }
      if(5 %in% des[des!=names(which.max(table(des)))]){
        possible <- rbind(possible,
                          data.frame(Type="Trois identiques & un 5",
                                     Marque=char_marq(union(which(des==names(which.max(table(des)))),which(des==5))),
                                     Relance=1,
                                     Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==3,"Score"]+50))
      }
      if(names(which.max(table(des))) %in% c(1,5)){
        possible <- rbind(possible,
                          data.frame(Type=paste(c("Un","Deux"),names(which.max(table(des)))),
                                     Marque=c(which(des==names(which.max(table(des))))[1],
                                              char_marq(which(des==names(which.max(table(des))))[c(1,2)])),
                                     Relance=c(4,3),
                                     Score=ifelse(names(which.max(table(des)))=="1",100,50)*c(1,2)))
        if(1 %in% des[des!=names(which.max(table(des)))]){
          possible <- rbind(possible,
                            data.frame(Type=paste(c("Un","Deux"),names(which.max(table(des))),"& un 1"),
                                       Marque=c(char_marq(union(which(des==names(which.max(table(des))))[1],which(des==1))),
                                                char_marq(union(which(des==names(which.max(table(des))))[c(1,2)],which(des==1)))),
                                       Relance=c(3,2),
                                       Score=ifelse(names(which.max(table(des)))=="1",100,50)*c(1,2)+100))
        }
        if(5 %in% des[des!=names(which.max(table(des)))]){
          possible <- rbind(possible,
                            data.frame(Type=paste(c("Un","Deux"),names(which.max(table(des))),"& un 5"),
                                       Marque=c(char_marq(union(which(des==names(which.max(table(des))))[1],which(des==5))),
                                                char_marq(union(which(des==names(which.max(table(des))))[c(1,2)],which(des==5)))),
                                       Relance=c(3,2),
                                       Score=ifelse(names(which.max(table(des)))=="1",100,50)*c(1,2)+50))
        }
      }
      return(possible)
    }
    if(length(des)==6){
      if(all(des[des!=names(which.max(table(des)))] %in% c(1,5))){
        return(data.frame(Type=paste("Trois identiques &",c("un","deux")[table(des)[["1"]]],"1 &",c("un","deux")[table(des)[["5"]]],"5"),
                          Marque="1:6",
                          Relance=0,
                          Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==3,"Score"]+sum(ifelse(des[des!=names(which.max(table(des)))]==1,100,50))))
      }

      possible <- rbind(possible,
                        data.frame(Type="Trois identiques",
                                   Marque=char_marq(which(des==names(which.max(table(des))))),
                                   Relance=3,
                                   Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==3,"Score"]))

      if(1 %in% des[des!=names(which.max(table(des)))]){
        possible <- rbind(possible,
                          data.frame(Type="Trois identiques & un 1",
                                     Marque=char_marq(union(which(des==names(which.max(table(des)))),which(des==1)[1])),
                                     Relance=2,
                                     Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==3,"Score"]+100))
        if(table(des)[["1"]]==2){
          possible <- rbind(possible,
                            data.frame(Type="Trois identiques & deux 1",
                                       Marque=char_marq(union(which(des==names(which.max(table(des)))),which(des==1))),
                                       Relance=1,
                                       Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==3,"Score"]+200))
        }
      }
      if(5 %in% des[des!=names(which.max(table(des)))]){
        possible <- rbind(possible,
                          data.frame(Type="Trois identiques & un 5",
                                     Marque=char_marq(union(which(des==names(which.max(table(des)))),which(des==5)[1])),
                                     Relance=2,
                                     Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==3,"Score"]+50))
        if(table(des)[["5"]]==2){
          possible <- rbind(possible,
                            data.frame(Type="Trois identiques & deux 5",
                                       Marque=char_marq(union(which(des==names(which.max(table(des)))),which(des==5))),
                                       Relance=1,
                                       Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==3,"Score"]+100))
        }
      }
      if(all(c(1,5) %in% des[des!=names(which.max(table(des)))])){
        possible <- rbind(possible,
                          data.frame(Type="Trois identiques & un 5 & un 1",
                                     Marque=char_marq(union(which(des==names(which.max(table(des)))),which(des %in% c(1,5)))),
                                     Relance=1,
                                     Score=fData[fData$Des==names(which.max(table(des))) & fData$Occ==3,"Score"]+150))
      }

      if(names(which.max(table(des))) %in% c(1,5)){
        possible <- rbind(possible,
                          data.frame(Type=paste(c("Un","Deux"),names(which.max(table(des)))),
                                     Marque=c(which(des==names(which.max(table(des))))[1],
                                              char_marq(which(des==names(which.max(table(des))))[c(1,2)])),
                                     Relance=c(4,3),
                                     Score=ifelse(names(which.max(table(des)))=="1",100,50)*c(1,2)))
        if(1 %in% des[des!=names(which.max(table(des)))]){
          possible <- rbind(possible,
                            data.frame(Type=paste(c("Un","Deux"),names(which.max(table(des))),"& un 1"),
                                       Marque=c(char_marq(union(which(des==names(which.max(table(des))))[1],which(des==1)[1])),
                                                char_marq(union(which(des==names(which.max(table(des))))[c(1,2)],which(des==1)[1]))),
                                       Relance=c(4,3),
                                       Score=ifelse(names(which.max(table(des)))=="1",100,50)*c(1,2)+100))
          if(table(des)[["1"]]==2){
            possible <- rbind(possible,
                              data.frame(Type=paste(c("Un","Deux"),names(which.max(table(des))),"& deux 1"),
                                         Marque=c(char_marq(union(which(des==names(which.max(table(des))))[1],which(des==1))),
                                                  char_marq(union(which(des==names(which.max(table(des))))[c(1,2)],which(des==1)))),
                                         Relance=c(3,2),
                                         Score=ifelse(names(which.max(table(des)))=="1",100,50)*c(1,2)+200))
          }
        }
        if(5 %in% des[des!=names(which.max(table(des)))]){
          possible <- rbind(possible,
                            data.frame(Type=paste(c("Un","Deux"),names(which.max(table(des))),"& un 5"),
                                       Marque=c(char_marq(union(which(des==names(which.max(table(des))))[1],which(des==5)[1])),
                                                char_marq(union(which(des==names(which.max(table(des))))[c(1,2)],which(des==5)[1]))),
                                       Relance=c(4,3),
                                       Score=ifelse(names(which.max(table(des)))=="1",100,50)*c(1,2)+50))
          if(table(des)[["5"]]==2){
            possible <- rbind(possible,
                              data.frame(Type=paste(c("Un","Deux"),names(which.max(table(des))),"& deux 5"),
                                         Marque=c(char_marq(union(which(des==names(which.max(table(des))))[1],which(des==5))),
                                                  char_marq(union(which(des==names(which.max(table(des))))[c(1,2)],which(des==5)))),
                                         Relance=c(3,2),
                                         Score=ifelse(names(which.max(table(des)))=="1",100,50)*c(1,2)+100))
          }
        }
      }
      return(possible)
    }
  }
  if(any(c(1,5) %in% des)){
    if(1 %in% des){
      possible <- rbind(possible,
                        data.frame(Type="Un 1",
                                   Marque = which(des==1)[1],
                                   Relance=length(des)-1,
                                   Score=100))
      if(table(des)[["1"]]==2){
        possible <- rbind(possible,
                          data.frame(Type="Deux 1",
                                     Marque = char_marq(which(des==1)),
                                     Relance=length(des)-2,
                                     Score=200))
      }
    }
    if(5 %in% des){
      possible <- rbind(possible,
                        data.frame(Type="Un 5",
                                   Marque = which(des==5)[1],
                                   Relance=length(des)-1,
                                   Score=50))
      if(table(des)[["5"]]==2){
        possible <- rbind(possible,
                          data.frame(Type="Deux 5",
                                     Marque = char_marq(which(des==5)),
                                     Relance=length(des)-2,
                                     Score=100))
      }
    }
    if(all(c(1,5) %in% des)){
      possible <- rbind(possible,
                        data.frame(Type="Un 1 & un 5",
                                   Marque = char_marq(union(which(des==1)[1],which(des==5)[1])),
                                   Relance=length(des)-2,
                                   Score=150))
      if(table(des)[["1"]]==2){
        possible <- rbind(possible,
                          data.frame(Type="Deux 1 & un 5",
                                     Marque = char_marq(union(which(des==1),which(des==5)[1])),
                                     Relance=length(des)-3,
                                     Score=250))
      }
      if(table(des)[["5"]]==2){
        possible <- rbind(possible,
                          data.frame(Type="Un 1 & deux 5",
                                     Marque = char_marq(union(which(des==1)[1],which(des==5))),
                                     Relance=length(des)-3,
                                     Score=200))
      }
      if(table(des)[["1"]]==2 && table(des)[["5"]]==2){
        possible <- rbind(possible,
                          data.frame(Type="Deux 1 & deux 5",
                                     Marque = char_marq(union(which(des==1),which(des==5))),
                                     Relance=length(des)-4,
                                     Score=300))
      }
    }
    return(possible)
  }
  if(length(des)==6 & identical(possible,list())){
    return(data.frame(Type="Sans marquage",
                      Marque="1:6",
                      Relance=0,
                      Score=500))
  }
  return(possible)
}

print.marq <- function(marq,des){
  cat("Marquage possible :")
  for(i in 1:nrow(marq)){
    cat(paste0("\n\t-(",i,") : ",marq[i,1]," pour ",marq[i,4]," points (marque ",length(des)-marq[i,3],"/",length(des)," dés)."))
  }
  cat("\n")
}

tour_farkle <- function(joueur,score=0){
  if(score!=0){
    cat("  >>> HOT DICES !<<< \n")
    readline("")
    cat("Score actuel :",score,"\n")
  }
  stop <- FALSE
  restant = 6

  while(!stop){
    des <- sort(lancer_des(n=restant))
    cat("Lancé de dés :",des,"\n")

    marq <- marquage_possible(des)
    if(nrow(marq)==0){
      cat("Aucun marquage possible... Fin du tour.\n")
      score <- 0
      break
    }
    marq = dplyr::arrange(marq,Score)
    print.marq(marq,des)
    if(any(marq$Relance==0)){
      marq <- marq[marq$Relance==0,]
      if(nrow(marq)==1){
        score <- tour_farkle(joueur,score + marq$Score)
        break
      }
    }

    reponse <- readline("Saisir la combinaison à marquer :")
    test.help_farkle(reponse)
    reponse <- as.numeric(stringr::str_remove(stringr::str_remove(reponse,"\\("),"\\)"))
    while(!(reponse %in% 1:nrow(marq))){
      reponse <- readline("[Saisie invalide], Saisir la combinaison à marquer :")
      test.help_farkle(reponse)
      reponse <- as.numeric(stringr::str_remove(stringr::str_remove("\\("),"\\)"))
    }
    score <- score + marq[reponse,"Score"]
    cat(marq[reponse,"Score"],"sont ajoutés à la réserve. Somme cumulée:",score,"\n")
    restant <- marq[reponse,"Relance"]
    reponse <- readline(paste0("\t",restant," dé(s) restant(s). Relancer ? [Y/N]"))
    test.help_farkle(reponse)
    while(!(tolower(reponse) %in% c("y","n",""))){
      reponse <- readline("[Saisie invalide] Relancer ? [Y/N]")
      test.help_farkle(reponse)
    }
    if(tolower(reponse)=="n"){
      stop <- TRUE
    }
  }
  return(score)
}



help_farkle <- function(){
  cat("Informations :\n")
  cat("\t- Pour quitter à tout moment, saisir 'Q';\n")
  cat("\t- Pour afficher les combinaisons possible à tout moment, saisir 'S';\n")
  cat("\t- Pour ravoir ces règles, saisir '?'.\n")
  # cat("Au début de chaque tour, le joueur lance tous les dés à la fois. Après chaque lancer, un ou plusieurs dés de score doivent 'marqué', permettant d'ajouter des points à une réserve. Le joueur peut décider de terminer son tour et valider le score accumulé dans la réserve jusqu'à présent, ou continuer à lancer les dés restants, au risque de perdre la réserve. Si le joueur peut marqué les six dés et donc plus de dés à relancer, il a des 'dés chauds' et peut continuer son tour en relançant les six dés et continuer d'accumuler le score du tour dans sa réserve. Il n'y a pas de limite au nombre de 'dés chauds' qu'un joueur peut lancer en un tour. Si le joueur lance des dés et n'a aucun marquage possible, alors il perd sa réserve de points et passe au tour suivant. Le but est de dépasser 5000 points en 10 tours maximum ou en premier si plusieurs joueurs.\n")
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
}

test.help_farkle <- function(reponse){
  if(identical(tolower(reponse),"q")){
    stop()
  }else if(identical(reponse,"?")){
    help_farkle()
  }else if(identical(tolower(reponse),"s")){
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
  }
}


