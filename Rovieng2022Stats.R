## Packages
library(ggplot2)
library(ggpubr)
library(rstatix)
library(emmeans)
library(multcomp)

## Importing data
setwd(dir = "C:/Users/HP/Desktop/Cambodge 2021/II.T?ches annexes/Recherches FlorentMathilde/Stats Trial Rovieng 012022/Analyses R")
TAB <- read.table("RDBProd.csv", header = T,sep = ";", dec = ".", stringsAsFactors = T)

## Selection des donn?es
#On selectionne que la d?mo 1
TABD1 = subset(TAB, Demo == "1") 

## Etude sur 1 facteur
#Statistiques sommaires
TABD1 %>%
  group_by(Soil.preparation) %>%
  get_summary_stats(X1.Fresh.biomass..t.ha., type = "mean_sd")
#Boxplot
ggboxplot(data = TABD1, x = "Soil.preparation", y = "X1.Fresh.biomass..t.ha.",
          color = "Soil.preparation", palette = "jco", ylab = "Fresh biomass (t/ha)", xlab = "Soil practices")
#On cherche les outliers (1 outlier, on cr?? un nouveau tableau en l'?liminant)
TABD1b <- TABD1[-67,]
TABD1 %>% 
  group_by(Soil.preparation) %>%
  identify_outliers(X1.Fresh.biomass..t.ha.)
#On v?rifie les hypoth?ses de plusieurs mani?re:
#1. Normalit? via r?sidus
# Construire le mod?le lin?aire
mod  <- lm(X1.Fresh.biomass..t.ha. ~ Soil.preparation, data = TABD1)
# Cr?er un QQ plot des r?sidus
ggqqplot(residuals(mod))
# Calculer le test de normalit? de Shapiro-Wilk
shapiro_test(residuals(mod))
#2.Normalit? via les r?sultats par groupe
#QQplot
ggqqplot(TABD1, "X1.Fresh.biomass..t.ha.", facet.by = "Soil.preparation")
#Test Shapiro
TABD1 %>%
  group_by(Soil.preparation) %>%
  shapiro_test(X1.Fresh.biomass..t.ha.)
#3.Homog?n?it? des variances
#Plot
plot(mod, 1)
#Levene test
TABD1 %>% levene_test(X1.Fresh.biomass..t.ha. ~ Soil.preparation)
#Finalement, plusieurs mani?res de tester l'effet:
#Ttest sur GM/CT
t.test(TABD1$X1.Fresh.biomass..t.ha.~TABD1$Soil.preparation,)
#ANOVA sur GM/CT
res.aov <- TABD1 %>% anova_test(X1.Fresh.biomass..t.ha. ~ Soil.preparation)
res.aov
#Pour pr?ciser les r?sultats de l'ANOVA on utilise Tukey
pwc <- TABD1 %>% tukey_hsd(X1.Fresh.biomass..t.ha. ~ Soil.preparation)
pwc
# Pour illustrer les r?sultats, visualisation : Boxplots avec p-values
pwc <- pwc %>% add_xy_position(x = "Soil.preparation")
ggboxplot(TABD1, x = "Soil.preparation", y = "X1.Fresh.biomass..t.ha.", col = c("brown","green")) +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
#Le test de Levene ?tant significatif (=pas d'homog?n?it? des variances), on essaie l'ANOVA de Welch:
res.aov2 <- TABD1 %>% welch_anova_test(X1.Fresh.biomass..t.ha. ~ Soil.preparation)
res.aov2
# Comparaisons par paires (Games-Howell)
pwc2 <- TABD1 %>% games_howell_test(X1.Fresh.biomass..t.ha. ~ Soil.preparation)
pwc2
#Boxplot avec les r?sultats de l'ANOVA de Welch
pwc2 <- pwc2 %>% add_xy_position(x = "Soil.preparation", step.increase = 1)
ggboxplot(TABD1, x = "Soil.preparation", y = "X1.Fresh.biomass..t.ha.", col = c("brown","green")) +
  stat_pvalue_manual(pwc2, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov2, detailed = TRUE),
    caption = get_pwc_label(pwc2)
  )

##Etude sur 2 facteurs (focale: GM/CT et mod?ratrice: Rice variety)
#Tableau avec les principales statistiques descriptives
TABD1 %>%
  group_by(Rice.variety, Soil.preparation) %>%
  get_summary_stats(X1.Fresh.biomass..t.ha., type = "mean_sd")
#Boxplot 
bxp <- ggboxplot(
  TABD1, x = "Rice.variety", y = "X1.Fresh.biomass..t.ha.",
  color = "Soil.preparation", palette = "jco")
bxp
#On d?tecte les outliers (m?me outlier que pr?cedemment)
TABD1 %>%
  group_by(Rice.variety, Soil.preparation) %>%
  identify_outliers(X1.Fresh.biomass..t.ha.)
#1. Normalit? via r?sidus
# Construire le mod?le lin?aire
mod  <- lm(X1.Fresh.biomass..t.ha. ~ Soil.preparation*Rice.variety, data = TABD1)
# Cr?er un QQ plot des r?sidus
ggqqplot(residuals(mod))
# Calculer le test de normalit? de Shapiro-Wilk
shapiro_test(residuals(mod))
#2.Normalit? via les r?sultats par groupe
#QQplot
ggqqplot(TABD1, "X1.Fresh.biomass..t.ha.", ggtheme = theme_bw()) +
  facet_grid(Rice.variety ~ Soil.preparation)
#Test Shapiro
TABD1 %>%
  group_by(Rice.variety, Soil.preparation) %>%
  shapiro_test(X1.Fresh.biomass..t.ha.)
#3.Homog?n?it? des variances
#Plot
plot(mod, 1)
#Levene test
TABD1 %>% levene_test(X1.Fresh.biomass..t.ha. ~ Rice.variety*Soil.preparation)
#Finalement, plusieurs mani?res de tester l'effet:
#ANOVA sur GM/CT
res.aov <- TABD1 %>% anova_test(X1.Fresh.biomass..t.ha. ~ Soil.preparation*Rice.variety)
res.aov
## Tests Post-hoc
#Calcul des effets principaux - Regrouper les donn?es par sexe et calculer l'anova
model <- lm(X1.Fresh.biomass..t.ha. ~ Soil.preparation*Rice.variety, data = TABD1)
TABD1 %>%
  group_by(Rice.variety) %>%
  anova_test(X1.Fresh.biomass..t.ha. ~ Soil.preparation, error = model)
# comparaisons par paires
pwc <- TABD1 %>% 
  group_by(Rice.variety) %>%
  emmeans_test(X1.Fresh.biomass..t.ha. ~ Soil.preparation, p.adjust.method = "bonferroni") 
pwc
#Si pas d'interactions on inspecte les effets principaux
res.aov
#T-test par paires version 1
TABD1 %>%
  pairwise_t_test(
    X1.Fresh.biomass..t.ha. ~ Soil.preparation, 
    p.adjust.method = "bonferroni"
  )
#Test par paires version 2
mod <- lm(X1.Fresh.biomass..t.ha. ~ Rice.variety * Soil.preparation, data = TABD1)
TABD1 %>% 
  emmeans_test(
    X1.Fresh.biomass..t.ha. ~ Soil.preparation, p.adjust.method = "bonferroni",
    model = mod
  )
# Visualisation : Boxplots avec p-values
pwc <- pwc %>% add_xy_position(x = "Rice.variety")
ggboxplot(TABD1, x = "Rice.variety", y = "X1.Fresh.biomass..t.ha.",color = "Soil.preparation", palette = "jco") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  ) 


##Etude sur 3 facteurs ()
#Tableau avec les principales statistiques descriptives
TABD1 %>%
  group_by(Rice.variety, Soil.preparation, Treatment) %>%
  get_summary_stats(X1.Fresh.biomass..t.ha., type = "mean_sd")
#Boxplot 
bxp <- ggboxplot(
  TABD1, x = "Soil.preparation", y = "X1.Fresh.biomass..t.ha.",
  color = "Treatment", palette = "jco", facet.by = "Rice.variety")
bxp
#On d?tecte les outliers (m?me outlier que pr?cedemment)
TABD1 %>%
  group_by(Rice.variety, Treatment, Soil.preparation) %>%
  identify_outliers(X1.Fresh.biomass..t.ha.)
#1. Normalit? via r?sidus
# Construire le mod?le lin?aire
mod  <- lm(X1.Fresh.biomass..t.ha. ~ Rice.variety*Treatment*Soil.preparation, data = TABD1)
# Cr?er un QQ plot des r?sidus
ggqqplot(residuals(mod))
# Calculer le test de normalit? de Shapiro-Wilk
shapiro_test(residuals(mod))
#2.Normalit? via les r?sultats par groupe
#QQplot
ggqqplot(TABD1, "X1.Fresh.biomass..t.ha.", ggtheme = theme_bw()) +
  facet_grid(Rice.variety + Treatment ~ Soil.preparation,labeller = "label_both")
#Test Shapiro
TABD1 %>%
  group_by(Rice.variety, Treatment, Soil.preparation) %>%
  shapiro_test(X1.Fresh.biomass..t.ha.)
#3.Homog?n?it? des variances
#Plot
plot(mod, 1)
#Levene test
TABD1 %>% levene_test(X1.Fresh.biomass..t.ha. ~ Rice.variety*Treatment*Soil.preparation)
#Finalement, plusieurs mani?res de tester l'effet:
#ANOVA sur GM/CT
res.aov <- TABD1 %>% anova_test(X1.Fresh.biomass..t.ha. ~ Soil.preparation*Treatment*Rice.variety)
res.aov
## Tests Post-hoc
#Pas d'interaction ? 3 facteurs, on fait quand m?me ce processus
# calculer l'interaction ? deux facteurs 
mod  <- lm(X1.Fresh.biomass..t.ha. ~ Rice.variety*Treatment*Soil.preparation, data = TABD1)
TABD1 %>%
  group_by(Rice.variety) %>%
  anova_test(X1.Fresh.biomass..t.ha. ~ Treatment*Soil.preparation, error = model)
# Regrouper les donn?es par variété et traitement, et calculer l'anova
treatment.effect <- TABD1 %>%
  group_by(Rice.variety, Treatment) %>%
  anova_test(X1.Fresh.biomass..t.ha. ~ Soil.preparation, error = mod)
# Comparaisons par paires
pwc <- TABD1 %>%
  group_by(Rice.variety, Treatment) %>%
  emmeans_test(X1.Fresh.biomass..t.ha. ~ Soil.preparation, p.adjust.method = "bonferroni") %>%
  select(-df, -statistic, -p) # Supprimer les d?tails
# Montrer les r?sultats de la comparaison pour les hommes ? risque ?lev?
pwc
# Visualisation : Boxplots avec p-values
pwc <- pwc %>% add_xy_position(x = "Soil.preparation")