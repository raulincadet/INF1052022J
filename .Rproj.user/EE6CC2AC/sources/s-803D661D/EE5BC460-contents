# boucle repeat

x=1
# on ajoute 1 a x jusqu'a ce que l'on obtient un multiple de 10
repeat{
  x=x+1
  print(x)
  if(x%%10==0){
    break
    }
}

######
# je veux que les resultats dbutent par 1 au lieu de 2
x=1
# on ajoute 1 a x jusqu'a ce que l'on obtient un multiple de 10
repeat{
  print(x)
  x=x+1
  if(x%%10==0){
    break
  }
}

### Et si on souhaite debuter par 1 et terminer avec 10
# on ajoute 1 a x jusqu'a ce que l'on obtient un multiple de 10
x<-0
repeat{
  x=x+1
  print(x)
  if(x%%10==0){
    break
  }
}

##############
# Boucle while

# compter des nombres impairs de 1 a 11

x<-1

while(x<12){
  print(x)
  x=x+2
}
###

# Meme exercice, en utilisant break pour s'arreter a 9
x<-1
while(x<12){
  print(x)
  x=x+2
  if(x==9){
    break
  }
}

######
# Boucle for
x<-c("Joseph","Marie","Magdala","Jocelyne","Jeannine")
length(x) # la taille de x
seq_along(x)  # l'indice des elements de x

# je veux imprimer les 3 premiers prenoms
for(i in 1:3){
  print(x[i])
}

# imprimer tous les prenoms
for(i in seq_along(x)){
  print(x[i])
}

# Imprimer les lettres de mon premier prenom
nomComplet<-"Raulin Lincifort Cadet"
y=strsplit(nomComplet,split="")
y
class(y)    # verifier la classe de l'objet y
y=unlist(y) # convertir la liste en vecteur
class(y)
y
length(y)   # taille de y
####
# afficher les 6 premiere lettres de mon nom complet
for (i in 1:6) {
  print(y[i])
  
}



