#Importing data
setwd(dir = "C:/Users/HP/Desktop/Cambodge 2021/II.T?ches annexes/Recherches FlorentMathilde/Stats Trial Rovieng 012022/Analyses R")
TAB <- read.table("RDBProdD1.csv", header = T,sep = ";", dec = ".", stringsAsFactors = T)

#Visualize the data
View(TAB)
names(TAB)
summary(TAB)

#Multiple linear regression with categorical variables - Analysis of variance
mod <- lm(TAB$X2.Dry.weight.1000.FG..g.~Soil.preparation.2+Rice.variety+Treatment,data=TAB)
hist(TAB$X2.Dry.weight.1000.FG..g.)
library(emmeans)
library(multcomp)
moyTreat=emmeans(mod,"Soil.preparation.2")
cld(moyTreat)
summary(mod)
hist(resid(mod),col = "grey", main = "")

mod <- lm(TAB$POXC~Soil.preparation.2+Rice.cultivar+Treatment,data=TAB)
summary(mod)
hist(resid(mod),col = "grey", main = "")

mod <- lm(TAB$Avail.N~Soil.preparation.2+Rice.cultivar+Treatment,data=TAB)
summary(mod)
hist(resid(mod),col = "grey", main = "")

mod <- lm(TAB$NH4~Soil.preparation.2+Rice.cultivar+Treatment,data=TAB)
summary(mod)
hist(resid(mod),col = "grey", main = "")

mod <- lm(TAB$NO3~Soil.preparation.2+Rice.cultivar+Treatment,data=TAB)
summary(mod)
hist(resid(mod),col = "grey", main = "")

mod <- lm(TAB$Infiltration.rate....~Soil.preparation.2+Rice.cultivar+Treatment,data=TAB)
summary(mod)
hist(resid(mod),col = "grey", main = "")