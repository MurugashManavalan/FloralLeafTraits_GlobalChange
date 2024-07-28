# Installing Packages ----
install.packages("gglot2")
library(readr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(vegan)

# Loading Files into R ----
setwd("/Users/murugash/Desktop/PhD/Austria Experiment/R Analysis/Crepis and Lotus Samples - R Analysis")
AusL <- read_csv("Austria Experiment - Lotus Data - Core Response Variables - 17 May 2024.csv")
AusC <- read_csv("Austria Experiment - Crepis Data - Core Response Variables - 17 May 2024.csv")
AusPC <- read_csv("Austria Experiement - Lotus and Crepis - PCA.csv")
AusRD <- read_csv("Crepis and Leaf Data - RDA - 26 May 2024.csv")
AusL1 <- AusL[AusL$`Plot No.`!= "P16",]
View(AusL)
View(AusPC)
dir()

# Histogram for Response Variables ----
## Histogram for Response Variables for Lotus ----
View(AusL)
hist(AusL$`Specific Standard Petal Area (SSA) (cm2/g)`, main = "Histogram of SSA", xlab = "SSA cm2/g")
hist(AusL$`Specific Wing 1 Petal Area (SW1A) (cm2/g)`, main = "Histogram of SW1A", xlab = "SW1A cm2/g")
hist(AusL$`Specific Wing 2 Petal Area (SW2A) (cm2/g)`, main = "Histogram of SW2A", xlab = "SW2a cm2/g")
hist(AusL$`Specific Keel Petal Area (SKA) (cm2/g)`, main = "Histogram of SKA", xlab = "SKA cm2/g")
hist(AusL$`Standard Petal Mass per Area (SPMA)(g/cm2)`, main = "Histogram of SPMA", xlab = "SPMA g/cm2")
hist(AusL$`Wing 1 Petal Mass per Area (W1PMA)(g/cm2)`, main = "Histogram of W1PMA", xlab = "W1PMA g/cm2")
hist(AusL$`Wing 2 Petal Mass per Area (W2PMA)(g/cm2)`, main = "Histogram of W2PMA", xlab = "W2PMA g/cm2")
hist(AusL1$`Keel Petal Mass per Area (KPMA)(g/cm2)`, main = "Histogram of KPMA", xlab = "KPMA g/cm2")
hist(AusL$`Specific Petal Area (SPA) (cm2/g)`, main = "Histogram of SPA", xlab = "SPA g/cm2")
hist(AusL$`Petal Mass per Area (PMA)(g/cm2)`, main = "Histogram of PMA", xlab = "PMA g/cm2")
hist(AusL$`Petal Dry Matter Content (PDMC) (g/g)`, main = "Histogram of PDMC", xlab = "PDMC g/cm2")
hist(AusL$`Specific Leaf Area (SLA) (cm2/g)`, main = "Histogram of SLA", xlab = "SLA cm2/g")
hist(AusL$`Leaf Dry Matter Content (LDMC) (g/g)`, main = "Histogram of LDMC", xlab = "LDMC")
## Histogram for Response Variables in Crepis ----
hist(AusC$`Specific Petal Area (SPA)(cm2/g)`, main = "Histogram of SPA", xlab = "SPA cm2/g")
hist(AusC$`Petal Mass Per Area (PMA)(g/cm2)`, main = "Histogram of PMA", xlab = "PMA g/cm2")
hist(AusC$`Petal Dry Matter Content (PDMC)(g/g)`, main = "Histogram of PDMC", xlab = "PDMC g/g")
hist(AusC$`Specific Leaf Area (SLA)(cm2/g)`, main = "Histogram of SLA", xlab = "SLA cm2/g")
hist(AusC$`Leaf Dry Matter Content (g/g)`, main = "Histogram of LDMC", xlab = "LDMC g/g")
hist(AusC$`Average No. of Seeds per Flower Head (AFH)`, main = "Histogram of AFH", xlab = "AFH")
hist(AusC$`Average Individual Seed Mass (ASM)(g)`, main = "Histogram of ASM", xlab = "ASM g")
## Histogram for Response Variables in PCA Sheet ----
hist(AusPC$`Specific Standard Petal Area (SSA) (cm2/g)`, main = "Histogram of SSA", xlab = "SSA cm2/g" )
hist(AusPC$`Specific Wing 1 Petal Area (SW1A) (cm2/g)`, main = "Histogram of SW1A", xlab = "SW1A cm2/g")
hist(AusPC$`Specific Wing 2 Petal Area (SW2A) (cm2/g)`, main = "Histogram of SW2A", xlab = "SW2a cm2/g")
hist(AusPC$`Specific Keel Petal Area (SKA) (cm2/g)`, main = "Histogram of SKA", xlab = "SKA cm2/g")
hist(AusPC$`Standard Petal Mass per Area (SPMA)(g/cm2)`, main = "Histogram of SPMA", xlab = "SPMA g/cm2")
hist(AusPC$`Wing 1 Petal Mass per Area (W1PMA)(g/cm2)`, main = "Histogram of W1PMA", xlab = "W1PMA g/cm2")
hist(AusPC$`Wing 2 Petal Mass per Area (W2PMA)(g/cm2)`, main = "Histogram of W2PMA", xlab = "W2PMA g/cm2")
hist(AusPC$`Keel Petal Mass per Area (KPMA)(g/cm2)`, main = "Histogram of KPMA", xlab = "KPMA g/cm2")
hist(AusPC$`Specific Petal Area (SPA) (cm2/g)`[AusPC$Species == "Lotus"], main = "Histogram of SPA in Lotus", xlab = "SPA cm2/g")
hist(AusPC$`Petal Mass per Area (PMA)(g/cm2)`[AusPC$Species == "Lotus"], main = "Histogram of PMA in Lotus", xlab = "PMA g/cm2")
hist(AusPC$`Petal Dry Matter Content (PDMC) (g/g)`[AusPC$Species == "Lotus"], main = "Histogram of PDMC in Lotus", xlab = "PDMC g/g")
hist(AusPC$`Specific Leaf Area (SLA) (cm2/g)`[AusPC$Species == "Lotus"], main = "Histogram of SLA in Lotus", xlab = "SLA cm2/g")
hist(AusPC$`Leaf Dry Matter Content (LDMC) (g/g)`[AusPC$Species == "Lotus"], main = "Histogram of LDMC in Lotus", xlab = "LDMC g/g")
hist(AusPC$`Specific Petal Area (SPA) (cm2/g)`[AusPC$Species == "Crepis"], main = "Histogram of SPA in Crepis", xlab = "SPA cm2/g")
hist(AusPC$`Petal Mass per Area (PMA)(g/cm2)`[AusPC$Species == "Crepis"], main = "Histogram of PMA in Crepis", xlab = "PMA g/cm2")
hist(AusPC$`Petal Dry Matter Content (PDMC) (g/g)`[AusPC$Species == "Crepis"], main = "Histogram of PDMC in Crepis", xlab = "PDMC g/g")
hist(AusPC$`Specific Leaf Area (SLA) (cm2/g)`[AusPC$Species == "Crepis"], main = "Histogram of SLA in Crepis", xlab = "SLA cm2/g")
hist(AusPC$`Leaf Dry Matter Content (LDMC) (g/g)`[AusPC$Species == "Crepis"], main = "Histogram of LDMC in Crepis", xlab = "LDMC g/g")
hist(AusPC$`Average No. of Seeds per Flower Head (AFH)`, main = "Histogram of AFH", xlab = "AFH")
hist(AusPC$`Average Individual Seed Mass (ASM)(g)`, main = "Histogram of ASM", xlab = "ASM g")



# Transformation for Response Variables ----
## Log-transformation for Response Variables in Lotus ----
AusL$log_SSA <- log(AusL$`Specific Standard Petal Area (SSA) (cm2/g)`)
AusL$log_SW1A <- log(AusL$`Specific Wing 1 Petal Area (SW1A) (cm2/g)`)
AusL$log_SW2A <- log(AusL$`Specific Wing 2 Petal Area (SW2A) (cm2/g)`)
AusL$log_SKA <- log(AusL$`Specific Keel Petal Area (SKA) (cm2/g)`)
AusL$log_SPMA <- log(AusL$`Standard Petal Mass per Area (SPMA)(g/cm2)`)
AusL$log_W1PMA <- log(AusL$`Wing 1 Petal Mass per Area (W1PMA)(g/cm2)`)
AusL$log_W2PMA <- log(AusL$`Wing 2 Petal Mass per Area (W2PMA)(g/cm2)`)
AusL$log_KPMA <- log(AusL$`Keel Petal Mass per Area (KPMA)(g/cm2)`)
AusL$logL_SPA <- log(AusL$`Specific Petal Area (SPA) (cm2/g)`)
AusL$logL_PMA <- log(AusL$`Petal Mass per Area (PMA)(g/cm2)`)
AusL$logL_PDMC <- log(AusL$`Petal Dry Matter Content (PDMC) (g/g)`)
AusL$logL_SLA <- log(AusL$`Specific Leaf Area (SLA) (cm2/g)`)
AusL$logL_LDMC <- log(AusL$`Leaf Dry Matter Content (LDMC) (g/g)`)

## Square-Root Transformation for Response Variables in Lotus ----
AusL$sqrt_SSA <- sqrt(AusL$`Specific Standard Petal Area (SSA) (cm2/g)`)
AusL$sqrt_SW1A <- sqrt(AusL$`Specific Wing 1 Petal Area (SW1A) (cm2/g)`)
AusL$sqrt_SW2A <- sqrt(AusL$`Specific Wing 2 Petal Area (SW2A) (cm2/g)`)
AusL$sqrt_SKA <- sqrt(AusL$`Specific Keel Petal Area (SKA) (cm2/g)`)
AusL$sqrt_SPMA <- sqrt(AusL$`Standard Petal Mass per Area (SPMA)(g/cm2)`)
AusL$sqrt_W1PMA <- sqrt(AusL$`Wing 1 Petal Mass per Area (W1PMA)(g/cm2)`)
AusL$sqrt_W2PMA <- sqrt(AusL$`Wing 2 Petal Mass per Area (W2PMA)(g/cm2)`)
AusL$sqrt_KPMA <- sqrt(AusL$`Keel Petal Mass per Area (KPMA)(g/cm2)`)
AusL$sqrtL_SPA <- sqrt(AusL$`Specific Petal Area (SPA) (cm2/g)`)
AusL$sqrtL_PMA <- sqrt(AusL$`Petal Mass per Area (PMA)(g/cm2)`)
AusL$sqrtL_PDMC <- sqrt(AusL$`Petal Dry Matter Content (PDMC) (g/g)`)
AusL$sqrtL_SLA <- sqrt(AusL$`Specific Leaf Area (SLA) (cm2/g)`)
AusL$sqrtL_LDMC <- sqrt(AusL$`Leaf Dry Matter Content (LDMC) (g/g)`)

## Log-transformation for Response Variables in Crepis ----
AusC$logC_SPA <- log(AusC$`Specific Petal Area (SPA)(cm2/g)`)
AusC$logC_PMA <- log(AusC$`Petal Mass Per Area (PMA)(g/cm2)`)
AusC$logC_PDMC <- log(AusC$`Petal Dry Matter Content (PDMC)(g/g)`)
AusC$logC_SLA <- log(AusC$`Specific Leaf Area (SLA)(cm2/g)`)
AusC$logC_LDMC <- log(AusC$`Leaf Dry Matter Content (g/g)`)
AusC$log_AFH <- log(AusC$`Average No. of Seeds per Flower Head (AFH)`)
AusC$log_ASM <- log(AusC$`Average Individual Seed Mass (ASM)(g)`)

## Square-root transformations for Response Variables in Crepis ----
AusC$sqrtC_SPA <- sqrt(AusC$`Specific Petal Area (SPA)(cm2/g)`)
AusC$sqrtC_PMA <- sqrt(AusC$`Petal Mass Per Area (PMA)(g/cm2)`)
AusC$sqrtC_PDMC <- sqrt(AusC$`Petal Dry Matter Content (PDMC)(g/g)`)
AusC$sqrtC_SLA <- sqrt(AusC$`Specific Leaf Area (SLA)(cm2/g)`)
AusC$sqrtC_LDMC <- sqrt(AusC$`Leaf Dry Matter Content (g/g)`)
AusC$sqrt_AFH <- sqrt(AusC$`Average No. of Seeds per Flower Head (AFH)`)
AusC$sqrt_ASM <- sqrt(AusC$`Average Individual Seed Mass (ASM)(g)`)




# Histogram for Transformed Response Variables ----
## Histogram for Log-transformed Response Variables of Lotus ----
hist(AusL$log_SSA, main = "Histogram of log(SSA)", xlab = "log(SSA) cm2/g")
hist(AusL$log_SW1A, main = "Histogram of log(SW1A)", xlab = "log(SW1A) cm2/g")
hist(AusL$log_SW2A, main = "Histogram of log(SW2A)", xlab = "log(SW2A) cm2/g")
hist(AusL$log_SKA, main = "Histogram of log(SKA)", xlab = "log(SKA) cm2/g")
hist(AusL$log_SPMA, main = "Histogram of log(SPMA)", xlab = "log(SPMA) g/cm2")
hist(AusL$log_W1PMA, main = "Histogram of log(W1PMA)", xlab = "log(W1PMA) g/cm2")
hist(AusL$log_W2PMA, main = "Histogram of log(W2PMA)", xlab = "log(W2PMA) g/cm2")
hist(AusL$log_KPMA, main = "Histogram of log(KPMA)", xlab = "log(KPMA) g/cm2")
hist(AusL$logL_SPA, main = "Histogram of log(SPA)", xlab = "log(SPA) g/cm2")
hist(AusL$logL_PMA, main = "Histogram of log(PMA)", xlab = "log(PMA) g/cm2")
hist(AusL$logL_PDMC, main = "Histogram of log(PDMC)", xlab = "log(PDMC) g/cm2")
hist(AusL$logL_SLA, main = "Histogram of log(SLA)", xlab = "log(SLA) cm2/g")
hist(AusL$logL_LDMC, main = "Histogram of log(LDMC)", xlab = "log(LDMC) g/g")

## Histogram for Square-root transformed Response Variables of Lotus ----
hist(AusL$sqrt_SSA[AusL$`Plot No.`!= "P16"], main="sqrt(SSA)", xlab="sqrt(SSA)")
hist(AusL$sqrt_SW1A, main="sqrt(SW1A)", xlab="sqrt(SW1A)")
hist(AusL$sqrt_SW2A, main="sqrt(SW2A)", xlab="sqrt(SW2A)")
hist(AusL$sqrt_SKA, main="sqrt(SKA)", xlab="sqrt(SKA)")
hist(AusL$sqrt_SPMA, main="sqrt(SPMA)", xlab="sqrt(SPMA)")
hist(AusL$sqrt_W1PMA, main="sqrt(W1PMA)", xlab="sqrt(W1PMA)")
hist(AusL$sqrt_W2PMA, main="sqrt(W2PMA)", xlab="sqrt(W2PMA)")
hist(AusL$sqrt_KPMA, main="sqrt(KPMA)", xlab="sqrt(KPMA)")
hist(AusL$sqrtL_SPA, main="sqrt(SPA)", xlab="sqrt(SPA)")
hist(AusL$sqrtL_PMA, main="sqrt(PMA)", xlab="sqrt(PMA)")
hist(AusL$sqrtL_PDMC, main="sqrt(PDMC)", xlab="sqrt(PDMC)")
hist(AusL$sqrtL_SLA, main="sqrt(SLA)", xlab="sqrt(SLA)")
hist(AusL$sqrtL_LDMC, main="sqrt(LDMC)", xlab="sqrt(LDMC)")

## Histogram of Log-transformed Response Variables of Crepis ----
hist(AusC$logC_SPA, main="log(SPA)", xlab="log(SPA)")
hist(AusC$logC_PMA, main="log(PMA)", xlab="log(PMA)")
hist(AusC$logC_PDMC, main="log(PDMC)", xlab="log(PDMC)")
hist(AusC$logC_SLA, main="log(SLA)", xlab="log(SLA)")
hist(AusC$logC_LDMC, main="log(LDMC)", xlab="log(LDMC)")
hist(AusC$log_AFH, main="log(AFH)", xlab="log(AFH)")
hist(AusC$log_ASM, main="log(ASM)", xlab="log(ASM)")

## Histogram for Square-root transfored Response Variables for Crepis ----
hist(AusC$sqrtC_SPA, main="sqrt(SPA)", xlab="sqrt(SPA)")
hist(AusC$sqrtC_PMA, main="sqrt(PMA)", xlab="sqrt(PMA)")
hist(AusC$sqrtC_PDMC, main="sqrt(PDMC)", xlab="sqrt(PDMC)")
hist(AusC$sqrtC_SLA, main="sqrt(SLA)", xlab="sqrt(SLA)")
hist(AusC$sqrtC_LDMC, main="sqrt(LDMC)", xlab="sqrt(LDMC)")
hist(AusC$sqrt_AFH, main="sqrt(AFH)", xlab="sqrt(AFH)")
hist(AusC$sqrt_ASM, main="sqrt(ASM)", xlab="sqrt(ASM)")







# Converting Grouping variables, fixed effect predictor variables and response variables to Factors ----
## In Lotus Dataset ----
AusL$`CO2 Level` <- as.factor(AusL$`CO2 Level`)
AusL$`Temperature Level` <- as.factor(AusL$`Temperature Level`)
AusL$Drought <- as.factor(AusL$Drought)
AusL$`Plot No.` <- as.factor(AusL$`Plot No.`)

## In Crepis Dataset ----
AusC$`C02 Level` <- as.factor(AusC$`C02 Level`)
AusC$`Temperature Level` <- as.factor(AusC$`Temperature Level`)
AusC$Drought <- as.factor(AusC$Drought)
AusC$`Plot No.` <- as.factor(AusC$`Plot No.`)

## In PCA Dataset ----
AusPC$`CO2 Level` <- as.factor(AusPC$`CO2 Level`)
AusPC$`Temperature Level` <- as.factor(AusPC$`Temperature Level`)
AusPC$Drought <- as.factor(AusPC$Drought)
AusPC$`Plot No.` <- as.factor(AusPC$`Plot No.`)


# Preparation of Mixed Effect Model ----
## Mixed-effect Models for Response Variables of Lotus ----
sq_SSAmodel <- lmer(data = AusL, AusL$sqrt_SSA ~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))
sq_SW1Amodel <- lmer(data = AusL, AusL$sqrt_SW1A ~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))
SW2Amodel <- lmer(data = AusL, AusL$`Specific Wing 2 Petal Area (SW2A) (cm2/g)` ~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))
SKAmodel <- lmer(data = AusL, AusL$`Specific Keel Petal Area (SKA) (cm2/g)` ~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))
sq_SPMAmodel <- lmer(data = AusL, AusL$`Standard Petal Mass per Area (SPMA)(g/cm2)` ~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))
sq_W1PMAmodel <- lmer(data = AusL, AusL$sqrt_W1PMA ~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))
W2PMAmodel <- lmer(data = AusL, AusL$`Wing 2 Petal Mass per Area (W2PMA)(g/cm2)` ~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))
log_KPMAmodel <- lmer(data = AusL, AusL$log_KPMA ~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))
log_SPAmodel <- lmer(data = AusL, AusL$logL_SPA ~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))
PMAmodel <- lmer(data = AusL, AusL$`Petal Mass per Area (PMA)(g/cm2)` ~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))
PDMCmodel <- lmer(data = AusL, AusL$`Petal Dry Matter Content (PDMC) (g/g)`~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))
log_LDMCmodel <- lmer(data = AusL, AusL$logL_LDMC ~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))                 
sq_LDMCmodel <- lmer(data = AusL, AusL$sqrtL_LDMC ~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))
SLAmodel <- lmer(data = AusL, AusL$`Specific Leaf Area (SLA) (cm2/g)` ~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))

## Mixed Effect Models for Response Variables of Crepis ----
sq_SPAmodel <- lmer(data = AusC, AusC$sqrtC_SPA ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought + (1|`Plot No.`))
sq_PMAmodel <- lmer(data = AusC, AusC$sqrtC_PMA ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought + (1|`Plot No.`))
sq_PDMCmodel <- lmer(data = AusC, AusC$sqrtC_PDMC ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought + (1|`Plot No.`))
log_SLAmodel <- lmer(data = AusC, AusC$logC_SLA ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought + (1|`Plot No.`))
LDMCmodel <- lmer(data = AusC, AusC$`Leaf Dry Matter Content (g/g)` ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought + (1|`Plot No.`))
log_LDMCmodel <- lmer(data = AusC, AusC$logC_LDMC ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought + (1|`Plot No.`))
AFHmodel <- lmer(data = AusC, AusC$`Average No. of Seeds per Flower Head (AFH)` ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought + (1|`Plot No.`))
log_ASMmodel <- lmer(data = AusC, AusC$log_ASM ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought + (1|`Plot No.`))

# ANOVA Analysis

# Anova Test ----
## Anova test for Reponse Variables in Lotus ----
anova(sq_SSAmodel)
anova(sq_SW1Amodel)
anova(SW2Amodel)
anova(SKAmodel)
anova(sq_SPMAmodel)
anova(sq_W1PMAmodel)
anova(W2PMAmodel)
anova(log_KPMAmodel)
anova(log_SPAmodel)
anova(PMAmodel)
anova(PDMCmodel)
anova(log_LDMCmodel)
anova(sq_LDMCmodel)
anova(SLAmodel)

## Anova test for Response Variables in Crespis ----
anova(sq_SPAmodel)
anova(sq_PMAmodel)
anova(sq_PDMCmodel)
anova(log_SLAmodel)
anova(LDMCmodel)
anova(log_LDMCmodel)
anova(AFHmodel)
anova(log_ASMmodel)




# Plotting Significant Response Variables against Predictor Variables ----
## Plots in Lotus ----
plot(AusL$`Leaf Dry Matter Content (LDMC) (g/g)`~AusL$`CO2 Level`, main = "CO2 on LDMC", xlab = "CO2 Level", ylab = "Leaf Dry Matter Content (LDMC) (g/g)")

plot(AusL$`Leaf Dry Matter Content (LDMC) (g/g)`~AusL$Drought, main = "Drought on LDMC", xlab = "Drought Level", ylab = "Leaf Dry Matter Content (LDMC) (g/g)")

boxplot(AusL$`Leaf Dry Matter Content (LDMC) (g/g)`~AusL$`CO2 Level`+AusL$Drought)
ggplot(AusL, aes(x = `CO2 Level`, y = `Leaf Dry Matter Content (LDMC) (g/g)` , color = `Drought`)) +
  geom_point() +
  facet_wrap(~AusL$Drought)

## Plots in Crepis ----
plot(AusC$`Specific Petal Area (SPA)(cm2/g)`~AusC$`Temperature Level`, main = "Temperature on SPA", xlab = "Temperature Level", ylab = "Specific Petal Area (SPA) (cm2/g)")
plot(AusC$`Petal Dry Matter Content (PDMC)(g/g)`~AusC$`Temperature Level`, main = "Temperature on PDMC", xlab = "Temperature Level", ylab = "Petal Dry Matter Content (PDMC) (g/g)")
plot(AusC$`Average No. of Seeds per Flower Head (AFH)`~AusC$`Temperature Level`, main = "Temperature on No. of Seeds", xlab = "Temperature Level", ylab = "No. of Seeds")
plot(AusC$`Average No. of Seeds per Flower Head (AFH)`~AusC$Drought, main = "Drought on No. of Seeds", xlab = "Drought", ylab = "No. of Seeds")



# Principal Component Analysis ----
## Creation of PCA models ----
lav <- prcomp(AusPC[AusPC$Species == "Lotus",8:20], scale = TRUE)
lmv <- prcomp(AusPC[AusPC$Species == "Lotus", c(17, 18,19,20)], scale = TRUE)
cav <- prcomp(AusPC[AusPC$Species == "Crepis", c(16, 18,19,20)], scale = TRUE)
cmv <- prcomp(AusPC[AusPC$Species == "Crepis", c(16, 18,19,20)], scale = TRUE)
lcav <- prcomp(AusPC[,c(17, 18,19,20)], scale = TRUE)

## Plotting of PCA models ----
biplot(lav, scale = 0)
R.version
citation()
biplot(lmv, scale = 0)
biplot(cav, scale = 0)
biplot(cmv, scale = 0)
biplot(lcav, scale = 0)
?biplot

## Extracting PC Scores ----
str(lcav)
lcav$x
AusPC1 <- cbind(AusPC, lcav$x[,1:2])
View(AusPC1)

## Plotting with GG plot ----
ggplot(AusPC1, aes(PC1,PC2, colour = Species, fill = Species)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")



standRDL <- scale(AusRDL[,c(17,19,20,21)])
summary(standRDL)                 
# Redundancy Analysis ----
## Redundancy Analysis for Lotus ----
AusRD$`Plot No.` <- as.factor(AusRD$`Plot No.`)
AusRD$`CO2 Level` <- as.factor(AusRD$`CO2 Level`)
AusRD$`Temperature Level` <- as.factor(AusRD$`Temperature Level`)
AusRD$Drought <- as.factor(AusRD$Drought)
AusRDL <- AusRD[AusRD$Species == "Lotus",]
View(AusRDL)
rda.l <- rda(standRDL ~ `CO2 Level`*`Temperature Level`*Drought, data = AusRDL, scale = TRUE)

rda.ln <- rda(AusRDL[,c(17,19,20,21)] ~ Drought + Condition(AusRDL$`Temperature Level`*AusRDL$`CO2 Level`), data = AusRDL, scale = TRUE)

rda.ln
CTRL <- how(nperm = 500, plots=Plots(strata=odber,type="none"),within=Within(type="free"))

ordiplot(rda.l,type = "text", scaling = 2)
anova.cca(rda.l, permutations = h)
anova.cca(rda.l, permutations = 9999, by = "axis")
anova.cca(rda.ln, permutations = 9999, by = "terms")
h <- how(nperm = 9999, plots=Plots(strata="Plot No.",type="free"),within=Within(type="none"))
anova(rda.ln,by="terms",permutations=h) 
h
View(AusRD)

## Redundancy Analysis for Crepis ----
AusRDC <- AusRD[AusRD$Species == "Crepis",]
rda.c <- rda(AusRDC[,c(17,19,20,21)] ~ `CO2 Level`*`Temperature Level`*Drought, data = AusRDC, scale = TRUE)
ordiplot(rda.c,type = "points", scaling = 2)
anova.cca(rda.c, permutations = 9999)
anova.cca(rda.c, permutations = 9999, by = "axis")
anova.cca(rda.c, permutations = 9999, by = "terms")

## Redundancy Analysis for both Crepis and Lotus ----
rda.1 <- rda(AusRD[,c(17,19,20,21)] ~ `CO2 Level`*`Temperature Level`*Drought, data = AusRD, scale = TRUE)
ordiplot(rda.1, type = "points", scaling = 2, ylim = c(-1,1))
anova.cca(rda.1, permutations = 9999)
anova.cca(rda.1, permutations = 9999, by = "axis")
anova.cca(rda.1, permutations = 9999, by = "terms")
