install.packages("haven", dependencies = T)
library(haven)
library(car)
library(psych)

matrica <- read_spss(file.choose())
colnames(matrica)

ajtemi_SWLS <- subset(matrica, select=c(417:421)) 
colnames(ajtemi_SWLS)


#Cronbach's alpha coefficient
alpha(ajtemi_SWLS, check.keys = T)


#1 Step: Predictor - Life Satisfaction (SWLS)
HIREGAN_mod1<- lm( Negativni_NASuzd ~ SWLS, data = matrica)
summary(HIREGAN_mod1)

#2 Step: Predictors - Positive and Negative Affect (PA_PANAS and NA_PANAS)
HIREGAN_mod2 <- lm(Negativni_NASuzd ~ SWLS + PA_panas + NA_panas, data = matrica)
summary(HIREGAN_mod2)

#3 Step: Predictors - BFI personality traits
HIREGAN_mod3 <- lm(Negativni_NASuzd ~ SWLS + PA_panas + NA_panas + prijatnostBFI + emocionalna_stabilnostBFI + ekstraverzijaBFI + intelektBFI + savesnostBFI, data = matrica)
summary(HIREGAN_mod3)

#ANOVA

anova(HIREGAN_mod1, HIREGAN_mod2, HIREGAN_mod3)
#Removing participants with outliers outside ±2.5 standard deviations (SD)
matrica_template <- subset(matrica, select=c(417:421))
options(max.print=1000000)


matrica_template
matrica_svi_odg <- matrica_template[1:933,]

#standardized residuals
std.resid <- rstandard(HIREGAN_mod1)
matrica2 <- cbind(matrica_svi_odg, std.resid)
colnames(matrica2)

matrica2[std.resid >2.50,]
matrica2[std.resid < -2.50,]



#VIF
vif(HIREGAN_mod3)
tolerance <- 1/vif(HIREGAN_mod3)
tolerance
