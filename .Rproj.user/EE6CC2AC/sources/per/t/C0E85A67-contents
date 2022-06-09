library(dplyr)
library(ggplot2)


##### Regression lineaire ########
cre<-read.csv("CreditBancaire.csv")

## regresssion de jours de retard paiement sur le montant et le type
# de credit
reg_1<-lm(data=cre,Jours~Credit+Type)
reg_1

# pour avoir resultats detaille
reg_resum<-summary(reg_1)
reg_resum

# on veut avoir que les coefficients et le t-student
reg_resum$coefficients[,c(1,3)]

# creation d'une fonction pour generer les coeff et t-student
# de n'importe quelle regression lineaire
result<-function(y,x,data){
  reg=lm(data[,y]~data[,x])
  reg_detail=summary(reg)
  return(reg_detail$coefficients[,c(1,3)])
}

# tester la fonction
result(y='Jours',x='Credit',data=cre)


############################
##### Manipulation #######

## moyenne des jours de retard, par type de credit
cre%>%group_by(Type)%>%
  summarise(Moyenne_jours=mean(Jours))

# moyenne du montant de credit, par type de credit
cre%>%group_by(Type)%>%
  summarise(Moyenne_credit=mean(Credit))


###########################################
#### Test de difference de la moyenne  ###

# verifie si la moyenne pour la consommation est 769295
# t.test()
cons<-cre%>%
  filter(Type=='Consommation')%>%
  select(Credit)
cons

# realisons le test 
t.test(cons,mu=769295,alternative = 'two.sided')

# verifions si le credit moyen a la consommation est < a celui de la production
prod<-cre%>%  # on cree un vecteur de donnees de credit a la production
  filter(Type=='Production')%>%
  select(Credit)
prod

# on realise le test
t.test(cons, prod,alternative = 'less')
# H): Ucons>=Uprod (si p-value > 0.05, on accepte HO)
# H1: Ucons<Uprod

# verifier si la moyenne des deux sont identiques
# HO: Ucons=Uprod
# H1: Ucons !=Uprod
t.test(cons,prod,alternative = )
# p.value=0.79, etant > 0.05, on accepte H0. Donc les deux moyennes
# sont siginifcativement identiques.

#################################
#### GRAPHIQUES AVEC ggplot2 ####
################################
library(ggplot2)

ggplot(data = cre,aes(x=Jours,y=Credit))+
  geom_point()

# modification de la taille des points
ggplot(data=cre,aes(x=Jours,y=Credit))+
  geom_point(size=3)

# modification des formes de points
ggplot(data=cre,aes(x=Jours,y=Credit))+
  geom_point(size=3,shape=1)

# pour avoir de l'aide
help(shape,"ggplot2")

# On peut indiquer un theme
ggplot(data=cre,aes(x=Jours,y=Credit))+
  geom_point(size=3,shape=1)+
  theme_bw()

# on prend le theme classic
ggplot(data=cre,aes(x=Jours,y=Credit))+
  geom_point(size=3,shape=1)+
  theme_classic()

# le theme light
ggplot(data=cre,aes(x=Jours,y=Credit))+
  geom_point(size=3,shape=1)+
  theme_light()

# modificaiton des titres des axes
ggplot(data=cre,aes(x=Jours,y=Credit))+
  geom_point(size=3,shape=1)+
  xlab('Nombre de jours')+ylab('Montant du crédit')+
  theme_classic()

# modification de la couleur des points
ggplot(data=cre,aes(x=Jours,y=Credit,color='red'))+
  geom_point(size=3,shape=1)+
  theme_classic()

# ajouter la tendance
ggplot(data=cre,aes(x=Jours,y=Credit))+
  geom_point(size=3,shape=1)+
  geom_smooth()+
  theme_classic()

# pour enlever la partie grise de la tendance,
# on met se=FALSE
ggplot(data=cre,aes(x=Jours,y=Credit))+
  geom_point(size=3,shape=1,aes(color='red'))+
  geom_smooth(se=FALSE)+
  theme_light()

# un graphique en nuage de points, mais en groupe
ggplot(data=cre,aes(x=Jours,y=Credit,color=Type))+
  geom_point(size=3)


###### Graphique en baton ######
ttype<-table(cre$Type)        # Frequence des types de contrats de credits
ptype<-prop.table(ttype)*100  # pourcentage de types de contrats 

                              # de credits
ptype
class(ptype)                  # verifier le type d'objet
ptype<-data.frame(ptype)      # convertir ptype en data.frame
ptype

ggplot(data=ptype,aes(x=Var1,y=Freq))+
  geom_bar(stat='identity')

#### modifier les titres des axes
ggplot(data=ptype,aes(x=Var1,y=Freq))+
  geom_bar(stat='identity')+
  xlab('Types de crédit')+ylab('Pourcentages')

### modifier la couleur
ggplot(data=ptype,aes(x=Var1,y=Freq))+
  geom_bar(stat='identity',fill='orangered')+
  xlab('Types de crédit')+ylab('Pourcentages')


### modifier le theme
### modifier la couleur
ggplot(data=ptype,aes(x=Var1,y=Freq))+
  geom_bar(stat='identity',fill='orangered')+
  xlab('Types de crédit')+ylab('Pourcentages')+
  theme_light()


####### Graphique en ligne ########
ggplot(data=cre,aes(x=Jours,y=Credit))+
  geom_line()+
  theme_minimal()

## changer la couleur de la ligne
ggplot(data=cre,aes(x=Jours,y=Credit))+
  geom_line(col='navy')+
  theme_minimal()
