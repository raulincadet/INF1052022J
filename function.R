######################################
#####  CREATION DE FONCTIONS    ######
######################################


# fonction pour identifier les nombres pairs
fpair<-function(x){
  if(x%%2==0){
    print("Nombre pair")
  }
}
#
fpair(2)
fpair(3)
gg<-1:8
fpair(gg)

# reecrire la fonction pour obetnir un message aussi quand
# le nombre est impair

fpair2<-function(x){
  if(x%%2==0){
    print("Nombre pair")
  }
  if(x%%2!=0){
    print("Nombre impair")
  }
}
#
fpair2(2)
fpair2(3)  
########

# Identifier si un nombre est pair ou impair, dans un vecteur
fpair_vec<-function(x){
  y=NULL
  for (i in 1:length(x)) {
    if(x[i]%%2==0){
      y[i]="Pair"
    }
    if(x[i]%%2!=0){
      y[i]="Impair"
    }
  }
  return(y)
}
##
gg
fpair_vec(gg)
##################

# creer une fonction qui genere un data frame contenant la 
# moyenne, l'ecart type et la taille d'un echantillon
fstat<-function(x){
  Moyenne=mean(x,na.rm=T)
  `Eccart type`=sd(x,na.rm = T)
  N=length(x)
  
  return(data.frame(Moyenne,`Eccart type`,N))
}

#
gg

fstat(gg)

  
  