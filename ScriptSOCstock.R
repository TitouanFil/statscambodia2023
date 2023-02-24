## Packages
library(ggplot2)
library(ggpubr)
library(rstatix)
library(emmeans)
library(multcomp)
library(dplyr)
library(lmerTest)
library(emmeans)
library(multcompView)

#1. Importing data
setwd(dir = "C:/Users/HP/Desktop/Cambodge 2021/II.Tâches annexes/Chronoséquence Battambang/Résultats")
TAB <- read.table("SanghaStocksF.csv", header = T,sep = ";", dec = ".", stringsAsFactors = T)


# Plot with nb of year / Carbon
#On enleve les NA
TAB <- TAB[-c(13,26,30),]
#On calcule les barycentres - All NV
Centroid <- TAB %>% 
  group_by(Landuse.2) %>%
  summarize(Nb.of.cultivated.years = mean(Nb.of.cultivated.years),
            C.stock..t.ha..All.NV = mean(C.stock..t.ha..All.NV), .groups="drop")

TAB$Nb.of.cultivated.years <- as.factor(TAB$Nb.of.cultivated.years)
Centroid$Nb.of.cultivated.years <- as.factor(rCentroid$Nb.of.cultivated.years)

ggplot(TAB, aes(x=Nb.of.cultivated.years, y=C.stock..t.ha..All.NV, color=Landuse.2, shape=Landuse.2)) +
  scale_shape_manual(values=1:20) +
  geom_point() +
  geom_point(data = Centroid, mapping = aes(x=Nb.of.cultivated.years, y=C.stock..t.ha..All.NV, color=Landuse.2), shape = 7, size = 5)


#4. ANOVA
#A. Multiple linear regression with categorical variables - Analysis of variance
mod <- lm(C.stock..t.ha..NV3 ~ Landuse.2, data=TAB)
#Get the results
summary(mod)
#Check validity conditions
#Normality
hist(resid(mod),col = "grey", main = "")
library(ggpubr)
ggqqplot(resid(mod))
shapiro_test(residuals(mod))
TAB %>% levene_test(C.stock..t.ha..NV3 ~ Landuse.2)
#B. Avec emeans
moyLUbySD=emmeans(mod, ~ Landuse.2)
tukLUbySD=cld(moyLUbySD)
print(tukLUbySD)
pw <- TAB %>% tukey_hsd(C.stock..t.ha..NV3 ~ Landuse.2)
pw

#5.Non-parametric
## Anova de Welch 
res.aov2 <- TAB %>% welch_anova_test(C.stock..t.ha..All.NV ~ Landuse.2)
res.aov2
# Comparaisons par paires (Games-Howell) - INUTILE
pwc2 <- TAB %>% games_howell_test(C.stock..t.ha..All.NV ~ Landuse.2)
pwc2
write.csv(pwc2, file = "I:/10-20W.csv")

#6. Kruskal-Wallis
res.aovT <- TAB010 %>% kruskal_test(P..ppm..Available.Phosphorus..Bray.II. ~ Landuse.1)
res.aovT
# Comparaisons par paires (Games-Howell)
pwc2 <- TAB010 %>% pairwise_wilcox_test(P..ppm..Available.Phosphorus..Bray.II. ~ Landuse.1)
pwc2
write.csv(pwc2, file = "I:/0-10.csv")




