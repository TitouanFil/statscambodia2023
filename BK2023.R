##1. Pre-requisites
# Packages
library(tidyverse)
library(ggpubr)
library(rstatix)
library(emmeans)

# Importing data
setwd(dir = "C:/Users/titou/OneDrive/Bureau/Cambodge 2021/II.Tâches annexes/3.Statistiques & appuiT/statscambodia2023/DataBK2023")
TAB <- read.table("Paired plot 2 year experiment.csv", header = T,sep = ";", dec = ".", stringsAsFactors = T)

#Check all variables
summary(TAB)

#Create a new table
TAB2022 <- TAB[c(1:192),c(1:7)]

#Histogramm to check outliers
ggplot(TAB2022, aes(x=X2022...Fresh.Biomass.plant...t.ha.))+
  geom_histogram()+ggtitle("Fresh biomass (t.ha) 2022")

#Boxplots
ggboxplot(TAB2022, x = "System", y = "X2022...Fresh.Biomass.plant...t.ha.",
                 color = "Treatment", palette = "jco")+ggtitle("Fresh biomass (t.ha) 2022")

#Summary statistics
TAB2022 %>%
group_by(System,Treatment) %>%
  get_summary_stats(X2022...Fresh.Biomass.plant...t.ha., type = "mean_sd")


#Variance analysis
#Creation of the model to analyse effects of Treatments and cropping systems
model  <- lm(X2022...Fresh.Biomass.plant...t.ha.~ Treatment*System, data = TAB2022)
#Test of Normality hypothesis
ggqqplot(residuals(model))
shapiro_test(residuals(model))
#Test of Homoscedasticity hypothesis
TAB2022 %>% levene_test(X2022...Fresh.Biomass.plant...t.ha. ~ Treatment*System)
plot(model,1)
#ANOVA, Variance analysis, fisher test results
anova(model)
#emmeans + Tukey to have differences between each factors modalities effects
moyTreat=emmeans(model,"Treatment","System")
print(cld(moyTreat))

#Non-parametric test if no normality or homoscedasticity is rejected
## Kruskal-Wallis
res.aov2 <- TAB2022 %>% kruskal_test(X2022...Fresh.Biomass.plant...t.ha. ~ System)
res.aov2





