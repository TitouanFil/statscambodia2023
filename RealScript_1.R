#Packages
library(tidyverse)
library(ggpubr)
library(rstatix)

#Importing data
setwd(dir = "C:/Users/HP/Desktop/Cambodge 2021/II.T?ches annexes/Recherches FlorentMathilde/Stats Trial Rovieng 012022/Analyses R")
TAB <- read.table("RDBProdD1.csv", header = T,sep = ";", dec = ".", stringsAsFactors = T)

#Prepare the data
 #if needed
melt(data-frame, na.rm = FALSE, value.name = “name”, id = 'columns')

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


## Script de base datanovia ANOVA 2 facteurs
#Data preparation
set.seed(123)
data("jobsatisfaction", package = "datarium")
jobsatisfaction %>% sample_n_by(gender, education_level, size = 1)
#Descriptive statistics
jobsatisfaction %>%
  group_by(gender, education_level) %>%
  get_summary_stats(score, type = "mean_sd")

#Visualization
bxp <- ggboxplot(
  jobsatisfaction, x = "gender", y = "score",
  color = "education_level", palette = "jco"
)
bxp
#Outliers identification
jobsatisfaction %>%
  group_by(gender, education_level) %>%
  identify_outliers(score)

#Normality hypothesis
model  <- lm(score ~ gender*education_level,
             data = jobsatisfaction)
# Créer un QQ plot des résidus
ggqqplot(residuals(model))
#Shapiro-Wilk normality test
shapiro_test(residuals(model))
#per group
jobsatisfaction %>%
  group_by(gender, education_level) %>%
  shapiro_test(score)
#QQ plots per group
ggqqplot(jobsatisfaction, "score", ggtheme = theme_bw()) +
  facet_grid(gender ~ education_level)
#Homoscedasticity
jobsatisfaction %>% levene_test(score ~ gender*education_level)

#ANOVA
res.aov <- jobsatisfaction %>% anova_test(score ~ gender * education_level)
res.aov
 #Post-hoc tests
# Regrouper les données par sexe et calculer l'anova
model <- lm(score ~ gender * education_level, data = jobsatisfaction)
jobsatisfaction %>%
  group_by(gender) %>%
  anova_test(score ~ education_level, error = model)
# pairs comparison
library(emmeans)
pwc <- jobsatisfaction %>% 
  group_by(gender) %>%
  emmeans_test(score ~ education_level, p.adjust.method = "bonferroni") 
pwc
#Boxplots avec p-value
# Visualisation : Boxplots avec p-values
pwc <- pwc %>% add_xy_position(x = "gender")
bxp +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
