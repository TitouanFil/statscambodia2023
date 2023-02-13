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
TAB$NSTOCK <- as.numeric(TAB$NSTOCK)
TAB$NSTOCK2 <- as.numeric(TAB$NSTOCK2)
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
for (i in c(7,8,10,13)){
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
#For the last variable, we need a 2nd loop as we create 105 plots and Rstudio 
#can print only 100 at a time
  for (j in 1:3){
    TAB7 <- subset(TAB6, Experiment == Expe[j])
    for (k in 1:7){
      TAB8 <- subset(TAB7, Depth == Dept[k])
      bxp <- ggboxplot(TAB8, x = "Year", y = colnames(TAB[14]),
                       color = "Treatment", palette = "jco")+ ggtitle(paste(colnames(TAB[i]),Expe[j],Dept[k]))
      print(bxp)
    }
  }
#Variable value depending on soil depth plots
#Table preparation with average values
TAB6$ID <- paste(TAB6$Experiment,TAB6$Treatment,TAB6$Depth,TAB6$Year)
TAB7 <- as.data.frame(apply(TAB6[,7:14],2,tapply, TAB6$ID, mean))
ID <- TAB6[,c(1:3,5,16)]
ID <- ID %>% distinct(ID, .keep_all = TRUE)
TAB7$ID <- rownames(TAB7)
TAB8 <- merge(ID, TAB7, by ="ID")
for (i in c(7,8,10,13)){
#Inverser x et y, pb boucle
  for (j in 1:3){
    TAB9 <- subset(TAB8, Experiment == Expe[j])
    plot <- ggplot(TAB9, aes(x = Depth, y = TAB9[,i], group = Treatment,color = Treatment)) +
    geom_line(aes(color=Treatment))+ geom_point(aes(color=Treatment)) + facet_grid(cols =vars(Year))+
    ggtitle(colnames(TAB9[i]),Expe[j])
    print(plot)
    }
  }

i = 7
j = 1
k = 1
l = 1
##4. hypothesis checking + ANOVA for each soil depth individually
#Normality hypothesis
#For this part, all plots and text will be created but only the last part will appear
#As R console and plot had limit. Better to run variable one by one if needed
#to get all results
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
        plot2 <- plot(model, 1, main = paste("Normality",colnames(TAB9[i]),Expe[j],Yea[k],Dept[l]))
        print(plot2)
        #Homoscedasticity test
        print(TAB9 %>% levene_test(TAB9[,i] ~ Treatment))
        #ANOVA + pairtests
        moyTreat=emmeans(model,"Treatment")
        print(cld(moyTreat))
      }
    }
  }
}

TAB8[,i]
###5. Modeling for each soil depth individually
#For this part, all plots and text will be created but only the last part will appear
#As R console and plot had limit. Better to run variable one by one if needed
#to get all results
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
        moyTreatByYea =emmeans(mod, ~Year|Treatment)
        tukTreatByYea=cld(moyTreatByYea)
        print(tukTreatByYea)
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

l = 4
j = 1
i = 14

###6. Creation of cumulated stock for soil depths
TAB6$ID <- paste(TAB6$Experiment,TAB6$Treatment,TAB6$Replicate,TAB6$Year)
ID <- TAB6[,c(1:2,4:5,16)]
ID <- ID %>% distinct(ID, .keep_all = TRUE)
# Data prep For Cumulated layer 0-10, 0-20, 0-60, 0-100
#0-10
TAB010 <- subset(TAB6, Depth == "0-5" | Depth == "5-10")
TAB010Cum <- as.data.frame(apply(TAB010[,13:14],2,tapply, TAB010$ID, sum))
TAB010Cum$ID <- rownames(TAB010Cum)
TABCumDept <- merge(ID, TAB010Cum, by ="ID")
colnames(TABCumDept)[6] <- paste("NSTOCK2-010")
colnames(TABCumDept)[7] <- paste("CSTOCK2-010")
#0-20
TAB020 <- subset(TAB6, Depth == "0-5" | Depth == "5-10" | Depth == "10-20")
TAB020Cum <- as.data.frame(apply(TAB020[,13:14],2,tapply, TAB020$ID, sum))
TAB020Cum$ID <- rownames(TAB020Cum)
TABCumDept2 <- merge(TABCumDept, TAB020Cum, by ="ID")
colnames(TABCumDept2)[8] <- paste("NSTOCK2-020")
colnames(TABCumDept2)[9] <- paste("CSTOCK2-020")
#0-40
TAB040 <- subset(TAB6, Depth == "0-5" | Depth == "5-10" | Depth == "10-20" | Depth == "20-40")
TAB040Cum <- as.data.frame(apply(TAB040[,13:14],2,tapply, TAB040$ID, sum))
TAB040Cum$ID <- rownames(TAB040Cum)
TABCumDept3 <- merge(TABCumDept2, TAB040Cum, by ="ID")
colnames(TABCumDept3)[8] <- paste("NSTOCK2-040")
colnames(TABCumDept3)[9] <- paste("CSTOCK2-040")
#0-60
TAB060 <- subset(TAB6, Depth != "60-80" | Depth != "80-100")
TAB060Cum <- as.data.frame(apply(TAB060[,13:14],2,tapply, TAB060$ID, sum))
TAB060Cum$ID <- rownames(TAB060Cum)
TABCumDept4 <- merge(TABCumDept3, TAB060Cum, by ="ID")
colnames(TABCumDept4)[10] <- paste("NSTOCK2-060")
colnames(TABCumDept4)[11] <- paste("CSTOCK2-060")
#0-80
TAB080 <- subset(TAB6, Depth != "80-100")
TAB080Cum <- as.data.frame(apply(TAB080[,13:14],2,tapply, TAB080$ID, sum))
TAB080Cum$ID <- rownames(TAB080Cum)
TABCumDept5 <- merge(TABCumDept4, TAB080Cum, by ="ID")
colnames(TABCumDept5)[10] <- paste("NSTOCK2-080")
colnames(TABCumDept5)[11] <- paste("CSTOCK2-080")
#0-100
TAB0100 <- TAB6
TAB0100Cum <- as.data.frame(apply(TAB0100[,13:14],2,tapply, TAB0100$ID, sum))
TAB0100Cum$ID <- rownames(TAB0100Cum)
colnames(TAB0100Cum)[1] <- paste("NSTOCK2-0100")
colnames(TAB0100Cum)[2] <- paste("CSTOCK2-0100")
TABCumDept6 <- merge(TABCumDept5, TAB0100Cum, by ="ID")



###7. Modeling for cumulated soil depths
#For this part, all plots and text will be created but only the last part will appear
#As R console and plot had limit. Better to run variable one by one if needed
#to get all results
for (i in c(6:13)){
  for (j in 1:3){
    TABCumDept5 <- subset(TABCumDept4, Experiment == Expe[j])
      print("")
      print("")
      print(paste(colnames(TABCumDept4[i]),"Cumulated",Expe[j]))
      #Model creation
      mod=lmer(TABCumDept4[,i] ~ Treatment*Year+(1|Replicate),data=TABCumDept4)
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
        ggtitle(paste(colnames(TABCumDept4[i]),"Cumulated",Expe[j]))+
        scale_x_continuous(breaks = c(2011,2021))
      print(plot)
    }
  }




