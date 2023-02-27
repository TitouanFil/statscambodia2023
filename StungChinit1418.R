library(corrplot)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(emmeans)
library(multcomp)
library(tidyverse)
library(data.table)

i = 7

### 1. Importation des données
setwd(dir = "C:/Users/HP/Desktop/Cambodge 2021/II.Tâches annexes/Recherches FlorentMathilde/Analyses Diachroniques Stung chinit July 2022")
TAB <- read.table("StungChinit2022.csv", header = T,sep = ";", dec = ".", stringsAsFactors = T)
TAB <- subset(TAB, Sampling.year == "2018")

### 2. Statistiques Descriptives
##Boxplots sur chaque variable avec NV
for (i in 7:13){
  bxp <- ggboxplot(TAB, x = "Tillage", y = colnames(TAB[i]), color = "Soil.Depth", palette = "jco")
  print(bxp)
}

##Boxplots sur chaque variable sans NV avec toutes les variables incluses
TABCTCA <- subset(TAB, Tillage != "NV")
TDep <- as.data.frame(c("a0-5cm","b5-10cm","c10-20cm","d20-40cm"))
j = 1
i = 7
for (i in 7:13){
  for (j in 1:4){
    TABNew <- subset(TABCTCA, Soil.Depth == as.character(TDep[j,]))
    bxp <- ggboxplot(TABNew, x = "Fertilizer.rate", y = colnames(TAB[i]), color = "Rice.cycle", palette = "jco")
    bxp <- bxp + facet_grid(. ~ Tillage) + ggtitle(paste(colnames(TAB[i]), " - ", TDep[j,]))
    print(bxp)
  }
}




##Corrélation
TABQuant <- TAB[,7:13]
mcor <- cor(TABQuant, use = "complete.obs")
options( "digits"=2)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)


### 3. Test d'hypothèses
##
TABCTCA <- subset(TAB, Tillage != "NV")
TDep <- as.data.frame(c("a0-5cm","b5-10cm","c10-20cm","d20-40cm"))
matShap = matrix(ncol = 7, nrow = 4)
matLev = matrix(ncol = 7, nrow = 4)
for (i in 7:13){
  X <- ggplot(TABCTCA, aes(x = TABCTCA[,i])) + geom_histogram() + facet_wrap(~Soil.Depth, ncol = 1) +   xlab(colnames(TABCTCA[i]))
  print(X)
  for (j in 1:4){
    TABNew <- subset(TABCTCA, Soil.Depth == as.character(TDep[j,]))
    mod <- lm(TABNew[,i] ~ Tillage*Fertilizer.rate*Rice.cycle, data = TABNew)
    c <- shapiro_test(residuals(mod))
    c$p.value <- format(c$p.value, scientific=TRUE, digits=3)
    hist(resid(mod),col = "grey", main = paste(colnames(TABNew[i]),(TDep[j,]), sep = " "))
    mtext(paste("Shapiro p-value:", c$p.value), side=3)
    typeof(c)
    matShap[j,i-5] <- c$p.value
    print(c)
    b <- ggqqplot(resid(mod)) + ggtitle(paste(colnames(TABNew[i]),(TDep[j,]), sep = " "))
    print(b)
    c <- plot(mod, 1)
    print(c)
    d <- TABNew %>% levene_test(TABNew[,i] ~ Tillage*Fertilizer.rate*Rice.cycle)
    matLev[j,i-5] <- d$p
    print(d)
  }
}


### 4. Tests statistiques
## A. ANOVA
# Résultats Généraux d'ANOVA
DFList <- list()
X <- matrix(ncol = 4, nrow = 7)
X <- as.data.frame(X)
rownames(X) <- rownames(a[1:7,])
SumANOV <- X
SumNoPar <- X[1:3,]
for (i in 7:13){
  print(colnames(TAB[i]))
  for (j in 1:4){
    print(TDep[j,])
    print("ANOVA")
    TABNew <- subset(TABCTCA, Soil.Depth == as.character(TDep[j,]))
    pw <- aov(TABNew[,i] ~ Tillage*Fertilizer.rate*Rice.cycle, data = TABNew)
    print(summary(pw))
    a <- summary(pw)
    a <- a[[1]]
    # B. Tests Post-Hoc automatisés en fonction des résultats de l'ANOVA
    #Préparation post-ANOVA, Automatisation des effets
    Signif <- rownames(a[a$`Pr(>F)`<=0.05,])
    Signif <- Signif[-length(Signif)]
    IntL <- str_subset(Signif, ":")
    IntL <- str_replace_all(IntL," ","")
    idx <- grep( ":", Signif, fixed=TRUE, invert=TRUE)
    MainL <- Signif[idx]
    MainL <- str_replace_all(MainL," ","")
    #Préparation post-ANOVA, Tableaux finaux des p-values
    SumANOV[,j] <- a[1:7,]$`Pr(>F)`
    #Post-ANOVA: Comparaison par paire, effets principaux
    if (length(MainL) != 0){
      for (k in 1:length(MainL)){
        print("pairwise t-test")
        print(MainL[k])
        Ntab <- matrix(nrow = nrow(TABNew), ncol = 2)
        Ntab <- as.data.frame(Ntab)
        Ntab[,1] <- TABNew[,grep(MainL[k],colnames(TABNew))]
        Ntab[,2] <- TABNew[,i]
        colnames(Ntab) <- c("IndVar","ResVar")
        p <- pairwise_t_test(Ntab, ResVar ~ IndVar, p.adjust.method = "bonferroni")
        print(p)
      }
      } else { 
        print(" ")
      }
    #Post-ANOVA: Comparaison par paire, interaction
    if (length(IntL) != 0){
      for (k in 1:length(IntL)){
        New <- tstrsplit(IntL[k], ":")
        if (length(New) == 2){
          for (l in 1:2){
            print("emmeans t-test")
            print(paste(New[1]," | ", New[2]))
            Ntab <- matrix(nrow = nrow(TABNew), ncol = length(New)+1)
            Ntab <- as.data.frame(Ntab)
            Ntab[,1] <- TABNew[,grep(New[1],colnames(TABNew))]
            Ntab[,2] <- TABNew[,grep(New[2],colnames(TABNew))]
            Ntab[,3] <- TABNew[,i]
            colnames(Ntab) <- c("IndVar1", "IndVar2","ResVar")
            Ntab$IndVar1 <- as.character(Ntab$IndVar1)
            Ntab$IndVar1 <- as.factor(Ntab$IndVar1)
            Ntab$IndVar2 <- as.character(Ntab$IndVar2)
            Ntab$IndVar2 <- as.factor(Ntab$IndVar2)
            pwc <- Ntab %>% group_by(IndVar1) %>% emmeans_test(ResVar ~ IndVar2 , p.adjust.method = "bonferroni")
            print(pwc)
            New <- rev(New)
          }
        } else {
          for (l in 1:3){
            print("emmeans t-test")
            print(paste(New[1]," | ", New[2], " | ", New[3]))
            Ntab <- matrix(nrow = nrow(TABNew), ncol = length(New)+1)
            Ntab <- as.data.frame(Ntab)
            Ntab[,1] <- TABNew[,grep(New[1],colnames(TABNew))]
            Ntab[,2] <- TABNew[,grep(New[2],colnames(TABNew))]
            Ntab[,3] <- TABNew[,grep(New[3],colnames(TABNew))]
            Ntab[,4] <- TABNew[,i]
            colnames(Ntab) <- c("IndVar1","IndVar2","IndVar3","ResVar")
            Ntab$IndVar1 <- as.character(Ntab$IndVar1)
            Ntab$IndVar1 <- as.factor(Ntab$IndVar1)
            Ntab$IndVar2 <- as.character(Ntab$IndVar2)
            Ntab$IndVar2 <- as.factor(Ntab$IndVar2)
            Ntab$IndVar3 <- as.character(Ntab$IndVar3)
            Ntab$IndVar3 <- as.factor(Ntab$IndVar3)
            pwc <- Ntab %>% group_by(IndVar1,IndVar2) %>% emmeans_test(ResVar ~ IndVar3 , p.adjust.method = "bonferroni")
            print(pwc)
            New2 <- New
            New <- c(New2[3],New2[1],New2[2])
          }
        }
      }
    } else { 
      print(" ")
    }
    ## Non-paramétrique (Kruskal-Wallis)
    New <- c("Tillage","Fertilizer.rate","Rice.cycle")
    for (k in 1:length(New)){
      print("Non-parametric Dunn test")
      print(paste(New[1]," | ", New[2], " | ", New[3]))
      Ntab <- matrix(nrow = nrow(TABNew), ncol = length(New)+1)
      Ntab <- as.data.frame(Ntab)
      Ntab[,1] <- TABNew[,grep(New[1],colnames(TABNew))]
      Ntab[,2] <- TABNew[,grep(New[2],colnames(TABNew))]
      Ntab[,3] <- TABNew[,grep(New[3],colnames(TABNew))]
      Ntab[,4] <- TABNew[,i]
      colnames(Ntab) <- c("IndVar1","IndVar2","IndVar3","ResVar")
      res.kruskal <- Ntab %>% kruskal_test(ResVar ~ IndVar1)
      p <- Ntab %>% group_by(IndVar1, IndVar2) %>% dunn_test(ResVar ~ IndVar3, p.adjust.method = "bonferroni")
      print(p)
      New2 <- New
      New <- c(New2[3],New2[1],New2[2])
      SumNoPar[k,j] <- res.kruskal$p
    }
  }
  DFList[[i-5]] <- SumANOV
  DFList[[i+2]] <- SumNoPar
}
#Tableaux d'hypothèse
colnames(matShap) <- colnames(TAB[,7:13])
colnames(matLev) <- colnames(TAB[,7:13])
rownames(matShap) <- TDep[,1]
rownames(matLev) <- TDep[,1]
print(matShap)
print(matLev)
for (i in 1:(length(DFList)/2)){
  print(paste("ANOVA", colnames(TAB[i+5]), "p-value Summary"))
  print(DFList[[i]])
  print(paste("Kruskal-Wallis", colnames(TAB[i+5]), "p-value Summary"))
  print(DFList[[i+7]])
}


# 4. Comparaison 2014 & 2018
TABp <- read.table("StungChinit2022b.csv", header = T,sep = ";", dec = ".", stringsAsFactors = T)
TABp <- subset(TABp, Tillage != "NV")
TAB05 <- subset(TABp, Soil.Depth == "a0-5cm")
TAB510 <- subset(TABp, Soil.Depth == "b5-10cm")
TAB1020 <- subset(TABp, Soil.Depth == "c10-20cm")
TAB2040 <- subset(TABp, Soil.Depth == "d20-40cm")

#Stats descriptives
TAB2040 %>%
  group_by(Sampling.year, Tillage) %>%
  get_summary_stats(C.stock..t.ha., type = "mean_sd")
#Test t de student pour données appariées
res <- t.test(TAB05$C.stock..t.ha. ~ TAB05$Sampling.year , paired = T)
res
#T-test through rstatix
stat.test <- TAB  %>% 
  t_test(Total.carbon.stock..t.ha. ~ Sampling.year, paired = TRUE) %>%
  add_significance()
stat.test

TABCA <- subset(TAB, Tillage == "CA")

TABCA  %>% 
  t_test(Total.carbon.stock..t.ha. ~ Sampling.year, paired = TRUE) %>%
  add_significance()
stat.test

#Boxplot avec infos
# Créer un box plot
bxp <- ggpaired(TAB, x = "Sampling.year", y = "Total.carbon.stock..t.ha.", col = "Sampling.year",
                order = c("2014", "2018"), facet.by = "Tillage",
                ylab = "Total.carbon.stock..t.ha.", xlab = "Sampling.year")
bxp <- ggboxplot(TAB, x = "Sampling.year", y = "Total.carbon.stock..t.ha.", col = "Sampling.year")
bxp

# Ajouter la p-value et les niveaux de significativité
stat.test <- stat.test %>% add_xy_position(x = "Sampling.year")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed= TRUE))

# #5. Analyses stock de carbone
#Import et tri des données
TAB <- read.table("StungChinit2022Stocks.csv", header = T,sep = ";", dec = ".", stringsAsFactors = T)
TAB <- subset(TAB, Tillage != "NV")
TAB <- subset(TAB, Sampling.year != "2014")
# Boxplot General
ggboxplot(TAB, x = "Tillage", y = "Total.carbon.stock..t.ha.", color = "Rice.cycle", palette = "jco")
print(bxp)

#Descriptive statistics general
B <- TAB %>%
  group_by(Soil.Depth, Tillage, Rice.cycle) %>%
  get_summary_stats(X.TOC, type = "mean_sd")
#On dispatch les données par couche:
TAB05 <- subset(TAB, Soil.Depth == "a0-5cm")
TAB510 <- subset(TAB, Soil.Depth == "b5-10cm")
TAB1020 <- subset(TAB, Soil.Depth == "c10-20cm")
TAB2040 <- subset(TAB, Soil.Depth == "d20-40cm")
#Descriptive statistics 
TABp %>%
  group_by(Tillage, Rice.cycle) %>%
  get_summary_stats(X.TOC, type = "mean_sd")
#Hypothesis check
mod <- lm(Total.carbon.stock..t.ha. ~ Tillage*Rice.cycle, data=TAB)
#Get the results
summary(mod)
#Check validity conditions
#Normality
hist(resid(mod),col = "grey", main = "")
ggqqplot(resid(mod))
shapiro_test(resid(mod))
dum <- TAB2a %>%
  group_by(Landuse.2,Depth..cm.) %>%
  shapiro_test(Bulk.density..g.cm3.)
p2 <- plot(mod, 1)
TABp %>% levene_test(C.stock..t.ha. ~ Tillage*Rice.cycle)

# ANOVA Cstock total
res.aov <- TAB2 %>% anova_test(Stock.NV1 ~ Land.use)
res.aov
mod <- lm(Stock.NV1 ~ Land.use, data=TAB2)
moyLUbySD=emmeans(mod, ~  Land.use)
tukLUbySD=cld(moyLUbySD)
print(tukLUbySD)

# Kruskal-WallisCstock total

TAB %>%
  group_by(Rice.cycle) %>% 
  dunn_test(Total.carbon.stock..t.ha. ~ Tillage, p.adjust.method = "bonferroni")
TABp %>%
  group_by(Tillage) %>% 
  kruskal_test(C.stock..t.ha. ~ Rice.cycle)
TAB %>%
  group_by(Tillage) %>% 
  dunn_test(Total.carbon.stock..t.ha. ~ Rice.cycle, p.adjust.method = "bonferroni")
