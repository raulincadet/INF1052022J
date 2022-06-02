library(dplyr)
####### Regression lineaire   #############
cre<-read.csv("CreditBancaire.csv")

##########
reg_1<-lm(data = cre,Jours~Credit+Type)

reg_1

reg_resum<-summary(reg_1)

reg_resum  # les resultats detailles

# afficher que le tableau des coefficients
reg_resum$coefficients

# coefficients et t student
reg_resum$coefficients[,c(1,3)]

####### une fonction pour fournir que coeff et t student ##########33
result<-function(y,x,data){
  reg=lm(data[,y]~data[,x])
  reg_details<-summary(reg)
  reg_details$coef[,c(1,3)]
}


# verifier si la fonction marche
result('Jours','Credit',data=cre)

# moyenne de jours par type de credit
cre%>%
  group_by(Type)%>%
  summarise(Moyenne_jours=mean(Credit))


# Tests de difference de la moyenne
# verifions si cons=769294 qui est la moyenne du credit en production
cons<-cre%>%filter(Type=='Consommation')%>%select(Credit)
prod<-cre%>%filter(Type=='Production')%>%select(Credit)
t.test(cons,mu=769294,alternative = "two.sided")
#pvalue is = 0.52, than superieur a 0.05 nous acceptons l'hypothese nulle
# que cons=769294


# verifions si le credit moyen cons est inferieur a production
t.test(cons,prod,alternative = "less")
# puisque pvalue superieur a 0.05 on accepte l'hypothese nuelle
# que cons est sup ou egal a prod




# verifions si cons est superieur a prod
t.test(cons,prod,alternative = 'greater')
#pval=0.6. Donc on accepte H0 que cons est inferieur ou eegal a:


# verifions si cons est egal a prod
t.test(cons, prod,alternative = 'two.sided')
#pvalue=0.79 est superieur a 0.05. Donc, on accepte l'hypothese nulle
# que cons = prod.



###################################
#### GGPLOT2 #########
######################

# nuage de point
library(ggplot2)
ggplot(data=cre,aes(x=Jours,y=Credit))+
  geom_point()
###
# size of points
ggplot(data=cre,aes(x=Jours,y=Credit,color='red'))+
  geom_point(size=2.5)
##
# shape
ggplot(data=cre,aes(x=Jours,y=Credit,color='red'))+
  geom_point(size=2.5,shape=6)

# find help on shape
help(shape,"ggplot2")

##
#theme
ggplot(data=cre,aes(x=Jours,y=Credit,color='red'))+
  geom_point(size=2.5,shape=1)+
  theme_bw()
 
## theme classic
ggplot(data=cre,aes(x=Jours,y=Credit,color='red'))+
  geom_point(size=2.5,shape=1)+
  theme_classic()

## change axis title
ggplot(data=cre,aes(x=Jours,y=Credit,color='red'))+
  geom_point(size=2.5,shape=1)+
  xlab("Nombre de jours")+
  theme_classic()

### ajouter une tendance
ggplot(data=cre,aes(x=Jours,y=Credit,color='red'))+
  geom_point(size=2.5)+
  geom_smooth(color='black')+
  xlab("Nombre de jours")+
  theme_classic()

### enlever la partie achuree et le theme
ggplot(data=cre,aes(x=Jours,y=Credit,color='red'))+
  geom_point(size=2.5)+
  geom_smooth(color='black',se=FALSE)+
  xlab("Nombre de jours")+
  theme_light()


#######################
####Geom_bar  ########
tsecteur<-prop.table(table(cre$Type))
tsecteur=data.frame(tsecteur)
tsecteur
###
ggplot(data=tsecteur,aes(x=Var1,y=Freq))+
  geom_bar(stat = 'identity')

#### remplir les batons de la couleur rouge
ggplot(data=tsecteur,aes(x=Var1,y=Freq,fill='red'))+
  geom_bar(stat = 'identity')

## changer noms des axes
ggplot(data=tsecteur,aes(x=Var1,y=Freq,fill='red'))+
  geom_bar(stat = 'identity')+xlab("Secteurs")+ylab('Proportion')
  
###  A vous de jouer


#### Graphique en nuage de points avec des groupes

ggplot(data=cre,aes(x=Jours,y=Credit,color=Type))+
  geom_point()+
  theme_light()

### modifier la taille des points avec size 
ggplot(data=cre,aes(x=Jours,y=Credit,color=Type))+
  geom_point(size=3)+
  theme_light()


### ajouter une tendance
ggplot(data=cre,aes(x=Jours,y=Credit,color=Type))+
  geom_point()+
  geom_smooth()+
  theme_light()

## enlever la partie achuree
ggplot(data=cre,aes(x=Jours,y=Credit,color=Type))+
  geom_point()+
  geom_smooth(se=F)+
  theme_light()


#######################
### graphique en ligne

ggplot(data=cre,aes(x=Jours,y=Credit))+
  geom_line()+
  theme_classic()

## change line color
ggplot(data=cre,aes(x=Jours,y=Credit))+
  geom_line(col='navy')+
  theme_classic()

#####
ggplot(data = cre,aes(Jours))+
  geom_histogram(bins=10)+
  theme_light()
