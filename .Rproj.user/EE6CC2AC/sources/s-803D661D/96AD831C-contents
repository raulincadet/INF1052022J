library(dplyr) # package utile pour utiliser %>% et 
# les verbes comme : rename

# importation des donnees 
df=data.frame(x=LETTERS[1:5],y=sample(5))
df          # affichage de l'objet

class(df)   # class de l'objet
str(df)     # explorer les caracteristiques de l'objet

# transposer df
dft<-t(df)  
dft
class(dft)  # apres la transposition, c'est une matrice que l'on a

dft<-data.frame(dft) # on transforme la matrice en data frame

class(dft)  # classe de l'objet transforme en data frame

dft3<-t(dft)  # on a decide de transposer a nouveau
class(dft3)

dft3<-data.frame(dft3)
class(dft3)

str(dft3)

mean(dft3$y)  # calcul de la moyenne de y

# convertir la variable en donnees numeriques
dft3$y<-as.numeric(dft3$y)

#calculons la moyenne de y
mean(dft3$y)

##################
## Utilisation paste()
prenom="Raulin"       # mon premier prenom
prenom2="Lincifort"   # mon second prenom
nom="Cadet"           # mon nom

prenom
prenom2
nom

# combiner les 3 donnees de type charactere
paste(prenom,prenom2,nom)

paste(prenom,prenom2,nom,sep = "-")

#############################################
#### Illustration Manipulation Data Frame ###
#############################################
df_ht<-readxl::read_excel("WB_HT_DATA.xlsx")

# transposer df_ht
df_ht<-t(df_ht)
class(df_ht)

# convertir df_ht en data frame
df_ht<-data.frame(df_ht)
str(df_ht)

# utiliser la ligne 5 comme noms des colonnes
colnames(df_ht)<-df_ht[5,]

# enlever les lignes et colonnes inutiles
df_hg<-df_ht[-c(1:6),-c(1:2)]

# changer le nom de la 1ere colonne en "Annees"
df_hg<-df_hg%>%rename(Années=`Indicator Name`)

# verifier le type de donnees dans chaque colonne
apply(df_hg,2,class)

# visualiser les memes resultats dans un data frame
col_class<-data.frame(apply(df_hg, 2, class))

# fonction as.numeric
a="123"
class(a)
a<-as.numeric(a)
class(a)

# convertir colonnes 1 a 1444 de df_hg en donnees numeriques
df_hg1<-apply(df_hg, 2, as.numeric)

# convertir colonnes 2 a 1444 en donnees numer
df_hg2<-apply(df_hg[,2:1444],2,as.numeric)

# Ajouter la colonne des annees a df_hg2
df_hg3<-cbind.data.frame(df_hg[,1],df_hg2)

# modifier le nom de la 1ere colonne
df_hg3<-df_hg3%>%rename(Années=`df_hg[, 1]`)


###########################
### 

# calculer la moyenne du PIB per capita a prix courant
df_hg3%>%summarise(Moyenne=mean(`GDP per capita (current US$)`), na.rm=T)

# realiser le graphique du PIB per capita a prix courant
df_hg3%>%select(Années,`GDP per capita (current US$)`)%>%
  plot()

#
df_hg3%>%select(Années,`GDP per capita (current US$)`)%>%
  plot(type="l")
