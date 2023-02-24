library(lmerTest)
library(emmeans)
library(ggplot2)
library(multcomp)
library(multcompView)
library(dplyr)
library(nlme)
library(bazar)
library(stringr)
library(tidyverse)
library(ggpubr)
library(cowplot)
library(lmtest)
library(ggrepel)

#1. Pr?paration des donn?es
#a. Import
setwd(dir = "C:/Users/HP/Desktop/Cambodge 2021/II.T?ches annexes/Article PierrePengli/Mod?lisation/Mod?lisation FINAL")
TAB <- read.table("POXCSituRespTriTRUE.csv", header = T, sep = ";", dec = ".", stringsAsFactors = T)


cor(TAB$POXC,TAB$SituResp88, method = 'pearson')

#b.Elimination des NA
TAB = TAB %>%
  dplyr::filter(!is.na(TAB$SituResp88))
TAB$Stage3 = as.factor(TAB$Stage3)
TAB$Replicates = as.factor(TAB$Replicates)


X <- TAB %>% group_by(Treatments) %>% summarise_at(vars(POXC), list(name = mean))

#Boxplot
boxplot(TAB$POXC~TAB$Stage3, main = "POXC", ylab = "POXC", xlab = "Stage")

#2. Mod?le et validation
#a. Mod?le
mod=lmer(POXC~Plots+Treatments*Stage3+(1|Plots:Treatments)+(1|Plots:Treatments:Replicates),data=TAB)

#V?rification des mod?les choisis mod?le "mod"
#b.R?cup?ration des r?sidus
TAB$fit <- fitted(mod)
TAB$resid <- residuals(mod)

#c. Homog?n?it?
#Graphiques pour l'homog?n?it?
homo <- ggplot(TAB, aes(x=fit, y=resid))+geom_point()
#Tests pour l'homog?n?it?
bartlett.test(POXC1$resid~POXC1$Plots)
bartlett.test(TAB$resid~TAB$Stage3)
bartlett.test(POXC1$resid~POXC1$Treatments)
#Test Bopt pour l'homog?n?it?
#Ajustement mod?le lin?aire sur variable brute
mod1=glm(POXC~Stage3, data = TAB)
#20 classes sur ?tendue de la valeur pr?dite
predites<-predict(mod1)
predmin<-min(predites)
predmax<-max(predites)
nclass=20
classpred=floor(nclass*(predites-predmin)/(predmax-predmin))+1
classpred[which(classpred==nclass+1)]<-nclass
#calculer les moyennes des valeurs pr?dites et les 
#variances des r?sidus du mod?le par classes de valeurs pr?dites :
residus<-resid(mod1)
tabpred<-data.frame(predites,residus,classpred)
library(sqldf)
moyvar<-sqldf("select classpred,count(*) as n,avg(predites) as
moypred,stdev(residus) as stdres
               from tabpred group by classpred having count(*)>5")
moyvar$varres<-moyvar$stdres**2
#puis de tracer le graphique moyenne x variance :
plot(moyvar$moypred,moyvar$varres)
#puis de calculer l'exposant "b" de la courbe s2=a*m^b
moyvar$logmoy<-log(moyvar$moypred)
moyvar$logvar<-log(moyvar$varres)
reg<-lm(logvar~logmoy,data=moyvar)
bopt<-reg$coefficients[2]
bopt


#d.Ind?pendance
#Graphiques pour l'ind?pendance
p1 <- ggplot(TAB, aes(x=Treatments, y=resid), jitter= TRUE) + geom_jitter()
p2 <- ggplot(TAB, aes(x=Stage2, y=resid)) + geom_jitter()+theme(axis.text.x = element_text(size=6,angle = 45))
p3 <- ggplot(TAB, aes(x=Plots, y=resid)) + geom_jitter()+theme(axis.text.x = element_text(size=7, angle=30))
p4 <- ggplot(TAB, aes(x=Replicates, y=resid)) + geom_jitter()
plot_grid(p1,p2,p3,p4)

#e. Normalit?
#Graphiques pour la normalit?
ggqq <- ggqqplot(TAB$resid)
hist <- ggplot(TAB, aes(resid))+geom_histogram(bins=16)
#Tests pour la normalit?
shapiro.test(TAB$resid)

#On fait un m?gagraphique
plot_grid(homo,ggqq,hist)

#3. Extraction des r?sultats
#a.Resultats de l'ajustement : regarder les variances estimees des effets aleatoires
#(Tableau "Random effets"). Elles doivent etre proches des carr?s des ecart-type avec
#lesquelles les donn?es ont ete simulees. Le tableau des estimations des effets fixes n'est pas interessant en general.
summary(mod)

#b.Tableau d'analyse de la variance avec des tests de Fisher et les proba associ?es. Ddl au denominateur calcul?s par Satterthwaite
anova(mod)

#c. Moyennes de chaque traitement tous stades confondus
moyTreat=emmeans(mod,"Treatments")
#Lettres du test de Tukey
cld(moyTreat)

#d. Moyennes par Stade tous traitements confondus
moySt=emmeans(mod,"Stage3")
#Lettres du test de Tukey
X <- as.data.frame(cld(moySt))

#e.Test des tendances lineaires et quadratiques sur l'ensemble des stades par la methode des contrastes
#Pour 12 stades repartis regulierement dans le temps, les coefficients des contrastes s'obtiennent par poly(1:12,degree=2)
#Pour tester la progression sur les 10 derniers stades, l'alternative est de prendre
#des coefficients nuls pour les 2 premieres moyennes et poly(1:10) pour les 10 suivantes
coefContLin=poly(c(14,31,50,69,79,94,109,123,140,158,170,184),degree=2)[,1]  #alternative : coefContLin=c(0,0,poly(1:10,degree=2)[,1])
coefContQuad=poly(c(14,31,50,69,79,94,109,123,140,158,170,184),degree=2)[,2] #alternative : coefContQuad=c(0,0,poly(1:10,degree=2)[,2])
test(contrast(moySt,list(Lineaire=coefContLin,Courbure=coefContQuad)))
#Si les stades sont ?quirepartis, c'est ?quivalent d'ecrire : test(contrast(moySt,method="poly"))
#Si les stades ne sont pas r?partis r?gulierement, on peut remplacer la serie 1:12 dans la fonction poly
#par la serie des vrais nombres de semaines ou des sommes de degres-jour ou autre chose de pertinent

#f. Moyennes pour chaque combinaison de l'interaction Traitement x Stade.
moyint=emmeans(mod, ~Treatments*Stage3)

#g. Tests par tranches qui decomposent l'interaction en testant separement les differences entre niveaux d'un simple facteur pour chacun des niveaux
#de l'autre facteur
#Test de l'effet traitement ? chaque stade
test(contrast(moyint,method="consec",by="Stage3"),joint=TRUE)
#Test des differences entre stades, pour chacun des traitements
test(contrast(moyint,method="consec",by="Treatments"),joint=TRUE)     

#h. Moyennes par traitement, lettres de tukey et diagrammes en baton avec barres d'erreur separemment pour chaque stade
#Moyennes par traitement
moyTreatBySt=emmeans(mod, ~Treatments|Stage3)
tukTreatBySt=cld(moyTreatBySt)
print(tukTreatBySt)

#i. Graphs finaux
#Pr?pare tukTreatBySt pour ins?rer ?tiquettes sur le graph
#Recharger les tableaux si besoin
X <- as.data.frame(cld(moySt))
tukTreatBySt=cld(moyTreatBySt)
#Pr?paration des tableaux
nb_row = nrow(tukTreatBySt)
col <- data.frame(rep(NA,nb_row))
colnames(col) <- c("Stage4")
tukTreatBySt <- cbind(tukTreatBySt,col)
#Ordonne ta table de ref pour pas avoir ? chercher chaque ligne
X <-X[order(X$Stage3),]
#Boucle "Recherche.si" pour changer automatiquement les Nums Tukeys
for (i in 1:nb_row) {
  chiffre = tukTreatBySt[i,2]
  tukTreatBySt[i,9] <- X[chiffre,7]
}
#On recode les r?sulats Tukey chiffres en lettres
#.groups
tukTreatBySt$.group2 <- tukTreatBySt$.group %>%
  fct_recode("A" = " 1","A" = " 1 ","AB" = " 12","B" = "  2")
#Stage
tukTreatBySt$Stage4b <- tukTreatBySt$Stage4 %>%
  fct_recode("CD" = "   34  ", "BCD" = "  234  ","E" = "     5 ", "DE" = "    45 ","AB" =" 12    ", 	
  "A" = " 1     ", "ABC" = " 123   ","F" = "      6", "DE" = "    45 ")
#On recode aussi le nom des traitements
tukTreatBySt$Treatments <- tukTreatBySt$Treatments %>%
  fct_recode("CAS" = "CA1CC","CAM" = "CAMIXCC")
#On concat?ne maintenant
tukTreatBySt$Stage5 <- as.factor(paste(tukTreatBySt$Stage3,tukTreatBySt$Stage4b, sep = " -"))
X <- as.data.frame(str_split(unique(tukTreatBySt$Stage5), "-"))
rownames(X) = c("Stage", "Stage2")
X <- as.data.frame(t(X))
X$Stage <- as.numeric(X$Stage)
#Courbes

  ggplot(tukTreatBySt, aes_string(x="Stage3", y="emmean", colour="Treatments", group="Treatments")) + geom_line(aes_string(linetype="Treatments"), size=.6) +
  geom_point(aes_string(shape="Treatments"), size=3) +
  geom_errorbar(aes_string(ymax="upper.CL", ymin="lower.CL"), width=.1) +
  scale_x_discrete(labels = scales::label_wrap(2))+geom_text_repel(aes(label = .group2), size = 3)+ylim(500,850)+
  labs(x = "Stage", y = "POXC (mg kg-1)")+annotate("text", x = X$Stage, y = 850, label = X$Stage2, size =3)
  
  