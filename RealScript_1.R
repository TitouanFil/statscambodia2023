##1. Pre-requisites
# Packages
library(tidyverse)
library(ggpubr)
library(rstatix)
library(emmeans)
library(multcomp)
library(multcompView)
library(lmerTest)

# Importing data
setwd(dir = "C:/Users/titou/OneDrive/Bureau/Cambodge 2021/II.Tâches annexes/3.Statistiques & appuiT/statscambodia2023/Thèse Vira & Writing session 2023")
TAB <- read.table("230205_Soil data arrangement for R 2011 vs 2021.csv", header = T,sep = ";", dec = ".", stringsAsFactors = T)

##2. Data preparation
#Reordering Depth factor values
TAB$Depth <- factor(TAB$Depth, levels=c('0-5','5-10','10-20','20-40','40-60','60-80','80-100'))
#Checking for aberrant value through statistics
summary(TAB)
#-> We need to remove negative %N and C/N data
TAB2 <- subset(TAB, N > 0)
#Plotting and checking for outliers - Histogramm
for (i in 7:14){
hist <- ggplot(TAB2, aes(x=TAB2[,i])) + 
  geom_histogram(binwidth=mean(TAB2[,i])/10)+xlab(colnames(TAB2[i]))
print(hist)
}
#-> We remove N value < 0.3 (Too low) and N value == 2.86 (unexplained high value)
TAB3 <- subset(TAB2, N > 0.3)
TAB4 <- subset(TAB3, N != 2.86)
#-> Some C values are low, we remove all values < 0.03
TAB5 <- subset(TAB4, C > 0.03)
#We remove values of Referent Vegetaion, we will be able to analyze these values later
TAB6 <- subset(TAB5, Experiment != "Referentvegetation")

##3. Descriptive statistics
#Mean & standard deviation
Expe <- TAB6[!duplicated(TAB5$Experiment),1]
Expe <- Expe[2:4]
Sum <- list()
SUM <- list()
for (i in c(7,8,10,13,14)){
  for (j in 1:3){
  TAB7 <- subset(TAB6, Experiment == Expe[j])
Sum[[j]] <- TAB6 %>%
  group_by(Year,Depth,Treatment) %>%
  get_summary_stats(colnames(TAB7[i]), type = "mean_sd")
  }
  SUM[[i]] <- Sum
}
NsumMaiEx <- SUM[[7]][[1]]
NsumSoyEx <- SUM[[7]][[2]]
NsumCasEx <- SUM[[7]][[3]]
CsumMaiEx <- SUM[[8]][[1]]
CsumSoyEx <- SUM[[8]][[2]]
CsumCasEx <- SUM[[8]][[3]]
BDsumMaiEx <- SUM[[10]][[1]]
BDsumSoyEx <- SUM[[10]][[2]]
BDsumCasEx <- SUM[[10]][[3]]
StNsumMaiEx <- SUM[[13]][[1]]
StNsumSoyEx <- SUM[[13]][[2]]
StNsumCasEx <- SUM[[13]][[3]]
StCsumMaiEx <- SUM[[14]][[1]]
StCsumSoyEx <- SUM[[14]][[2]]
StCsumCasEx <- SUM[[14]][[3]]

#Visualization - Boxplots
Dept <- TAB6[!duplicated(TAB5$Depth),3]
for (i in c(7,8,10,13,14)){
  for (j in 1:3){
  TAB7 <- subset(TAB6, Experiment == Expe[j])
  for (k in 1:7){
    TAB8 <- subset(TAB7, Depth == Dept[k])
    bxp <- ggboxplot(TAB8, x = "Year", y = colnames(TAB[i]),
      color = "Treatment", palette = "jco")+ ggtitle(paste(colnames(TAB[i]),Expe[j],Dept[k]))
    print(bxp)
    }
  }
}

##4. hypothesis checking + ANOVA
#Normality hypothesis
Yea <- TAB6[!duplicated(TAB5$Yea),5]
for (i in c(7,8,10,13,14)){
  for (j in 1:3){
    TAB7 <- subset(TAB6, Experiment == Expe[j])
    for (k in 1:2){
      TAB8 <- subset(TAB7, Year == Yea[k])
      for (l in 1:7){
        TAB9 <- subset(TAB8, Depth == Dept[l])
        #Modeling
        model  <- lm(TAB9[,i] ~ Treatment,
                     data = TAB9)
        #Normality plot
        plot <- ggqqplot(residuals(model))+ ggtitle(paste("Normality",colnames(TAB9[i]),Expe[j],Yea[k],Dept[l]))
        print(plot)
        #Normality test
        print("")
        print("")
        print(paste(colnames(TAB9[i]),Expe[j],Yea[k],Dept[l]))
        print(shapiro_test(residuals(model)))
        #Homoscedasticity plot
        
        #Homoscedasticity test
        print(TAB9 %>% levene_test(TAB9[,i] ~ Treatment))
        #ANOVA + pairtests
        moyTreat=emmeans(model,"Treatment")
        print(cld(moyTreat))
      }
    }
  }
}


### Modeling
for (i in c(7,8,10,13,14)){
  for (j in 1:3){
    TAB7 <- subset(TAB6, Experiment == Expe[j])
      for (l in 1:7){
        TAB8 <- subset(TAB7, Depth == Dept[l])
        print("")
        print("")
        print(paste(colnames(TAB8[i]),Expe[j],Dept[l]))
        #Model creation
        mod=lmer(TAB8[,i] ~ Treatment*Year+(1|Replicate),data=TAB8)
        #ANOVA, fisher test results
        anova(mod)
        #emmeans + Tukey
        moyTreatByYea =emmeans(mod, ~Treatment|Year)
        tukTreatByYea=cld(moyTreatByYea)
        print(tukTreatByYea)
        #Plot
         plot <- ggplot(tukTreatByYea, aes_string(x="Year", y="emmean", colour="Treatment", group="Treatment")) +
                 geom_line(aes_string(linetype="Treatment"), size=.6) +
                 geom_point(aes_string(shape="Treatment"), size=3) +
                 geom_errorbar(aes_string(ymax="upper.CL", ymin="lower.CL"), width=.1) +
                 ggtitle(paste(colnames(TAB8[i]),Expe[j],Dept[l]))+
                 scale_x_continuous(breaks = c(2011,2021))
         print(plot)
    }
  }
}



Yea[k]
i = 8
j = 1
k = 1
l = 1
