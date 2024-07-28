# Installing Packages ----
install.packages("corrplot")
install.packages("ggrepel")
install.packages("Hmisc")
library(readr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(vegan)
library(dplyr)
library(corrplot)
library(Hmisc)
library(ggrepel)
library(tidyr)

# Loading Files into R ----
setwd("/Users/murugash/Desktop/PhD/Austria Experiment/R Analysis/Crepis and Lotus Samples - R Analysis")
AusL <- read_csv("Austria Experiment - Lotus Data - Core Response Variables - 17 June 2024.csv")
AusC <- read_csv("Austria Experiment - Crepis Data - Core Response Variables - 17 May 2024.csv")
AusPC <- read_csv("Austria Experiement - Lotus and Crepis - PCA.csv")
AusRD <- read_csv("Crepis and Leaf Data - RDA - 26 May 2024.csv")
AusL1 <- AusL[AusL$`Plot No.`!= "P16",]
View(AusL)
View(AusPC)
dir()
setwd(dir = /Users/murugash/Desktop/PhD/Austria Experiment/R Analysis/Crepis and Lotus Samples - R Analysis)


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
AusPC1$Species <- as.factor(AusPC$Species)



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
sq_SPAmodel <- lmer(data = AusL, AusL$sqrtL_SPA ~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))
PMAmodel <- lmer(data = AusL, AusL$`Petal Mass per Area (PMA)(g/cm2)` ~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))
sq_PDMCmodel <- lmer(data = AusL, AusL$sqrtL_PDMC~ `CO2 Level` * `Temperature Level` * `Drought` + (1|`Plot No.`))
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


# Preparation of Linear Models ----
## Linear Models for Response Variables of Lotus ----
sq_SSAmodel <- lm(data = AusL, AusL$sqrt_SSA ~ `CO2 Level` * `Temperature Level` * `Drought`) 
sq_SW1Amodel <- lm(data = AusL, AusL$sqrt_SW1A ~ `CO2 Level` * `Temperature Level` * `Drought`) 
SW2Amodel <- lm(data = AusL, AusL$`Specific Wing 2 Petal Area (SW2A) (cm2/g)` ~ `CO2 Level` * `Temperature Level` * `Drought`)
SKAmodel <- lm(data = AusL, AusL$`Specific Keel Petal Area (SKA) (cm2/g)` ~ `CO2 Level` * `Temperature Level` * `Drought`)
sq_SKAmodel <- lm(data = AusL, AusL$sqrt_SKA~ `CO2 Level` * `Temperature Level` * `Drought`)
sq_SPMAmodel <- lm(data = AusL, AusL$`Standard Petal Mass per Area (SPMA)(g/cm2)` ~ `CO2 Level` * `Temperature Level` * `Drought`)
sq_W1PMAmodel <- lm(data = AusL, AusL$sqrt_W1PMA ~ `CO2 Level` * `Temperature Level` * `Drought`)
W2PMAmodel <- lm(data = AusL, AusL$`Wing 2 Petal Mass per Area (W2PMA)(g/cm2)` ~ `CO2 Level` * `Temperature Level` * `Drought`)
log_KPMAmodel <- lm(data = AusL, AusL$log_KPMA ~ `CO2 Level` * `Temperature Level` * `Drought`)
log_SPAmodel <- lm(data = AusL, AusL$logL_SPA ~ `CO2 Level` * `Temperature Level` * `Drought`)
sq_SPAmodel <- lm(data = AusL, AusL$sqrtL_SPA~ `CO2 Level` * `Temperature Level` * `Drought`)
PMAmodel <- lm(data = AusL, AusL$`Petal Mass per Area (PMA)(g/cm2)` ~ `CO2 Level` * `Temperature Level` * `Drought`)
PDMCmodel <- lm(data = AusL, AusL$`Petal Dry Matter Content (PDMC) (g/g)`~ `CO2 Level` * `Temperature Level` * `Drought`)
sq_PDMCmodel <- lm(data = AusL,AusL$sqrtL_PDMC ~ `CO2 Level` * `Temperature Level` * `Drought`)
log_LDMCmodel <- lm(data = AusL, AusL$logL_LDMC ~ `CO2 Level` * `Temperature Level` * `Drought`)                 
sq_LDMCmodel <- lm(data = AusL, AusL$sqrtL_LDMC ~ `CO2 Level` * `Temperature Level` * `Drought`)
SLAmodel <- lm(data = AusL, AusL$`Specific Leaf Area (SLA) (cm2/g)` ~ `CO2 Level` * `Temperature Level` * `Drought`)

## Linear Models for Response Variables of Crepis ----
sq_SPAmodel <- lm(data = AusC, AusC$sqrtC_SPA ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought)
sq_PMAmodel <- lm(data = AusC, AusC$sqrtC_PMA ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought)
sq_PDMCmodel <- lm(data = AusC, AusC$sqrtC_PDMC ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought)
log_SLAmodel <- lm(data = AusC, AusC$logC_SLA ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought)
LDMCmodel <- lm(data = AusC, AusC$`Leaf Dry Matter Content (g/g)` ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought)
log_LDMCmodel <- lm(data = AusC, AusC$logC_LDMC ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought)
AFHmodel <- lm(data = AusC, AusC$`Average No. of Seeds per Flower Head (AFH)` ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought)
log_ASMmodel <- lm(data = AusC, AusC$log_ASM ~ AusC$`C02 Level` * AusC$`Temperature Level` * AusC$Drought)


# ANOVA Analysis ----
## Anova test for Reponse Variables in Lotus ----
anova(sq_SSAmodel)
anova(sq_SW1Amodel)
anova(SW2Amodel)
anova(SKAmodel)
anova(sq_SKAmodel)
anova(sq_SPMAmodel)
anova(sq_W1PMAmodel)
anova(W2PMAmodel)
anova(log_KPMAmodel)
anova(log_SPAmodel)
anova(sq_SPAmodel)
anova(PMAmodel)
anova(PDMCmodel)
anova(sq_PDMCmodel)
anova(log_LDMCmodel)
anova(sq_LDMCmodel)
anova(SLAmodel)
View(AusL)
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
plot(AusL$`CO2 Level`, AusL$`Specific Leaf Area (SLA) (cm2/g)`, main = "CO2 on SLA", xlab = "CO2 Level", ylab = "Specific Leaf Area (SLA) (cm2/g)" )
plot(AusL$Drought, AusL$`Specific Leaf Area (SLA) (cm2/g)`, main = "Drought on SLA", xlab = "Drought Level", ylab = "Specific Leaf Area (SLA) (cm2/g)")

str(AusL$`Temperature Level`)
View(AusL)

## Plots in Lotus (Using ggplot) ----
ggplot(AusL, aes(x = AusL$`CO2 Level`,y= `Leaf Dry Matter Content (LDMC) (g/g)`)) +
  geom_boxplot() +
  labs(
    title = "Lotus - Effect of CO2 Level on LDMC",
    x = "CO2 Level",
    y = "Leaf Dry Matter Content (LDMC) (g/g)"
  )

ggplot(AusL, aes(x = AusL$Drought, y= AusL$`Leaf Dry Matter Content (LDMC) (g/g)`)) +
  geom_boxplot()+
  labs(
    title = "Lotus - Effect of Drought on LDMC",
    x = "Drought Level",
    y = "Leaf Dry Matter Content (LDMC) (g/g)"
  )

ggplot(AusL, aes(x = `CO2 Level`, y = `Leaf Dry Matter Content (LDMC) (g/g)`, fill = `Drought`)) +
  geom_boxplot() +
  facet_wrap(~ Drought) +
  labs(
    title = "Lotus - Effect of CO2 Level on LDMC by Drought Level",
    x = "CO2 Level",
    y = "Leaf Dry Matter Content (LDMC) (g/g)",
    fill = "Drought"
  ) +
  theme_minimal()

ggplot(AusL, aes(x = AusL$`CO2 Level`,y= `Specific Leaf Area (SLA) (cm2/g)`)) +
  geom_boxplot() +
  labs(
    title = "Lotus - Effect of CO2 Level on SLA",
    x = "CO2 Level",
    y = "Specific Leaf Area (SLA) (cm2/g)"
  )

ggplot(AusL, aes(x = AusL$Drought, y= AusL$`Specific Leaf Area (SLA) (cm2/g)`)) +
  geom_boxplot()+
  labs(
    title = "Lotus - Effect of Drought on SLA",
    x = "Drought Level",
    y = "Specific Leaf Area (SLA) (cm2/g)"
  )

ggplot(AusL, aes(x = `CO2 Level`, y = `Specific Leaf Area (SLA) (cm2/g)`, fill = `Temperature Level`)) +
  geom_boxplot() +
  facet_wrap(~ `Temperature Level`) +
  labs(
    title = "Lotus - Effect of CO2 Level on SLA by Temperature Level",
    x = "CO2 Level",
    y = "Specific Leaf Area (SLA) (cm2/g)",
    fill = "Temperature"
  ) +
  theme_minimal()

ggplot(AusL, aes(x = `CO2 Level`, y = `Specific Leaf Area (SLA) (cm2/g)`, fill = `Drought`)) +
  geom_boxplot() +
  facet_wrap(~ `Drought`) +
  labs(
    title = "Lotus - Effect of CO2 Level on SLA by Drought",
    x = "CO2 Level",
    y = "Specific Leaf Area (SLA) (cm2/g)",
    fill = "Drought"
  ) +
  theme_minimal()

## Plots in Crepis ----
plot(AusC$`Specific Petal Area (SPA)(cm2/g)`~AusC$Drought, main = "Drought on SPA", xlab = "Drought Level", ylab = "Specific Petal Area (SPA) (cm2/g)")
plot(AusC$`Petal Mass Per Area (PMA)(g/cm2)`~AusC$Drought, main = "Drought on PMA", xlab = "Drought Level", ylab = "Petal Mass Per Area (PMA)(g/cm2)")
plot(AusC$`Petal Dry Matter Content (PDMC)(g/g)`~AusC$Drought, main = "Drought on PDMC", xlab = "Drought Level", ylab = "Petal Dry Matter Content (PDMC) (g/g)")
plot(AusC$`Leaf Dry Matter Content (g/g)`~AusC$Drought, main = "Drought on LDMC", xlab = "Drought Level", ylab = "Leaf Dry Matter Content (PDMC) (g/g)")
plot(AusC$`Average No. of Seeds per Flower Head (AFH)`~AusC$`Temperature Level`, main = "Temperature on No. of Seeds", xlab = "Temperature Level", ylab = "No. of Seeds")
plot(AusC$`Average No. of Seeds per Flower Head (AFH)`~AusC$Drought, main = "Drought on No. of Seeds", xlab = "Drought", ylab = "No. of Seeds")

## Plots in Crepis (Using ggplot) ----
ggplot(AusC, aes(x = AusC$Drought, y= `Specific Petal Area (SPA)(cm2/g)`)) +
  geom_boxplot()+
  labs(
    title = "Crepis - Effect of Drought on SPA",
    x = "Drought Level",
    y = "Specific Petal Area (SPA)(cm2/g)"
  )

ggplot(AusC, aes(x = AusC$Drought, y= `Petal Mass Per Area (PMA)(g/cm2)`)) +
  geom_boxplot()+
  labs(
    title = "Crepis - Effect of Drought on PMA",
    x = "Drought Level",
    y = "Petal Mass Per Area (PMA)(g/cm2)"
  )

ggplot(AusC, aes(x = AusC$Drought, y= `Petal Dry Matter Content (PDMC)(g/g)`)) +
  geom_boxplot()+
  labs(
    title = "Crepis - Effect of Drought on PDMC",
    x = "Drought Level",
    y = "Petal Dry Matter Content (PDMC)(g/g)"
  )

ggplot(AusC, aes(x = AusC$Drought, y= `Leaf Dry Matter Content (g/g)`)) +
  geom_boxplot()+
  labs(
    title = "Crepis - Effect of Drought on LDMC",
    x = "Drought Level",
    y = "Leaf Dry Matter Content (g/g)"
  )

ggplot(AusC, aes(x = AusC$`Temperature Level`, y= `Average No. of Seeds per Flower Head (AFH)`)) +
  geom_boxplot()+
  labs(
    title = "Crepis - Effect of Temperature on AFH",
    x = "Temperature Level",
    y = "Average No. of Seeds per Flower Head (AFH)"
  )

ggplot(AusC, aes(x = AusC$Drought, y= `Average No. of Seeds per Flower Head (AFH)`)) +
  geom_boxplot()+
  labs(
    title = "Crepis - Effect of Drought on AFH",
    x = "Drought Level",
    y = "Average No. of Seeds per Flower Head (AFH)"
  )

ggplot(AusC, aes(x = `Drought`, y = `Average No. of Seeds per Flower Head (AFH)`, fill = `Temperature Level`)) +
  geom_boxplot() +
  facet_wrap(~ `Temperature Level`) +
  labs(
    title = "Crepis - Effect of Drought on AFH by Temperature Level",
    x = "Drought Level",
    y = "Average No. of Seeds per Flower Head (AFH)",
    fill = "Temperature Level"
  ) +
  theme_minimal()



# Principal Component Analysis ----
## Creation of PCA models ----
lav <- prcomp(AusPC[AusPC$Species == "Lotus",8:20], scale = TRUE)
lmv <- prcomp(AusPC[AusPC$Species == "Lotus", c(17, 18,19,20)], scale = TRUE)
cav <- prcomp(AusPC[AusPC$Species == "Crepis", c(16, 18,19,20, 21,22)], scale = TRUE)
cmv <- prcomp(AusPC[AusPC$Species == "Crepis", c(17, 18,19,20)], scale = TRUE)
lcav <- prcomp(AusPC[,c(16, 18,19,20)], center = TRUE, scale = TRUE)
names(AusPC)

## Plotting of PCA models ----
names(AusPC)[names(AusPC) == "Petal Mass per Area (PMA)(g/cm2)"] <- "PMA"
names(AusPC)[names(AusPC) == "Petal Dry Matter Content (PDMC) (g/g)"] <- "PDMC"
names(AusPC)[names(AusPC) == "Specific Leaf Area (SLA) (cm2/g)"] <- "SLA"
names(AusPC)[names(AusPC) == "Leaf Dry Matter Content (LDMC) (g/g)"] <- "LDMC"

biplot(lav, scale = 0)
biplot(lmv, scale = 0)
biplot(cav, scale = 0)
biplot(cmv, scale = 0, type = "n")
biplot(lcav, scale = 0)
summary(lcav)
?biplot

## Extracting PC Scores ----
str(lcav)
lcav$x
AusPC1 <- cbind(AusPC, lcav$x[,1:4])
View(AusPC1)

## Plotting with GG plot ----
ggplot(AusPC1, aes(PC1,PC2, colour = Species, fill = Species)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

## Extracting Variance Explained ----
### For Lotus ----

lcav$rotation
lcav$sdev
variance <- lcav$sdev^2
variance_explained <- variance /sum(variance)
variance_explained
cumsum(variance_explained)

# Redundancy Analysis ----
## Redundancy Analysis for Lotus ----
AusRD$`Plot No.` <- as.factor(AusRD$`Plot No.`)
AusRD$`CO2 Level` <- as.factor(AusRD$`CO2 Level`)
AusRD$`Temperature Level` <- as.factor(AusRD$`Temperature Level`)
AusRD$Drought <- as.factor(AusRD$Drought)
AusRDL <- AusRD[AusRD$Species == "Lotus",]
View(AusRDL)

rda.l <- rda(AusRDL[,c(18:21)] ~ `CO2 Level`*`Temperature Level`*Drought, data = AusRDL, scale = TRUE)
rda.ln <- rda(AusRDL[,c(17:21)] ~ Drought + Condition(AusRDL$`Temperature Level`*AusRDL$`CO2 Level`), data = AusRDL, scale = TRUE)
ordiplot(rda.l,type = "p", main = "Lotus RDA", arrows = TRUE)
?ordiplot
anova.cca(rda.l, permutations = 9999)
Lotus_RDA <- anova.cca(rda.l, permutations = 9999, by = "terms")
Lotus_RDA
summary(rda.l)
Lotus_RDA$F
Lotus_RDA$`Pr(>F)`
summary(rda.l)

## Redundancy Analysis for Crepis ----
View(AusRDC)
AusRDC <- AusRD[AusRD$Species == "Crepis",]
rda.c <- rda(AusRDC[,c(18:23)] ~ `CO2 Level`*`Temperature Level`*Drought, data = AusRDC, scale = TRUE)
rda.cn <- rda(AusRDC[,c(18:23)] ~ Drought + Condition(AusRDC$`Temperature Level`*AusRDC$`CO2 Level`), data = AusRDC, scale = TRUE)
ordiplot(rda.c,type = "text", main = "Crepis RDA", xlim = c(-2,3), ylim = c(-2,2))
anova.cca(rda.c, permutations = 9999)
anova.cca(rda.c, permutations = 9999, by = "axis")
Crepis_RDA <- anova.cca(rda.c, permutations = 9999, by = "terms")
Crepis_RDA
Crepis_RDA$F
Crepis_RDA$`Pr(>F)`

## Redundancy Analysis for both Crepis and Lotus ----
rda.1 <- rda(AusRD[,c(18:21)] ~ `CO2 Level`*`Temperature Level`*Drought, data = AusRD, scale = TRUE)
ordiplot(rda.1, type = "text", scale = TRUE)
anova.cca(rda.1, permutations = 9999)
anova.cca(rda.1, permutations = 9999, by = "axis")
anova.cca(rda.1, permutations = 9999, by = "terms")
Combined_RDA <- anova.cca(rda.1, permutations = 9999, by = "terms")
Combined_RDA
Combined_RDA$F
Combined_RDA$`Pr(>F)`

summary_rda <- summary(rda.1)
summary_rda

## Export RDA results to CSV ----
### For Lotus ----



# Preparing function which exports anova results to CSV ----
anova_to_csv <- function(model, csv_file_path) {
  # Perform the ANOVA test
  anova_results <- anova(model)
  
  # Convert the results to a data frame
  anova_df <- as.data.frame(anova_results)
  
  # Add a column with the model formula to distinguish different models
  model_formula <- paste(deparse(formula(model)), collapse = " ")
  anova_df$model <- rep(model_formula, nrow(anova_df))
  
  # Check if the CSV file already exists
  if (file.exists(csv_file_path)) {
    # Read the existing data
    existing_df <- read_csv(csv_file_path)
    
    # Append the new results
    combined_df <- bind_rows(existing_df, anova_df)
  } else {
    # If the file does not exist, the combined data frame is the new ANOVA results
    combined_df <- anova_df
  }
  
  # Write the combined DataFrame to the CSV file
  write_csv(combined_df, file = csv_file_path)
  
  cat("ANOVA results have been written to", csv_file_path, "\n")
}

## Output ANOVA results to CSV file for linear models in Lotus ----
anova_output_csv <- 'anova_LMLo.csv'
anova_to_csv(sq_SSAmodel, anova_output_csv)
anova_to_csv(sq_SW1Amodel, anova_output_csv)
anova_to_csv(SW2Amodel, anova_output_csv)
anova_to_csv(SKAmodel, anova_output_csv)
anova_to_csv(sq_SPMAmodel, anova_output_csv)
anova_to_csv(sq_W1PMAmodel, anova_output_csv)
anova_to_csv(W2PMAmodel, anova_output_csv)
anova_to_csv(log_KPMAmodel, anova_output_csv)
anova_to_csv(sq_SPAmodel, anova_output_csv)
anova_to_csv(PMAmodel, anova_output_csv)
anova_to_csv(sq_PDMCmodel, anova_output_csv)
anova_to_csv(log_LDMCmodel, anova_output_csv)
anova_to_csv(SLAmodel, anova_output_csv)

## Output ANOVA results to CSV file for linear models in Crepis ----
anova_output_csv <- 'anova_LCr.csv'
anova_to_csv(sq_SPAmodel, anova_output_csv)
anova_to_csv(sq_PMAmodel, anova_output_csv)
anova_to_csv(sq_PDMCmodel, anova_output_csv)
anova_to_csv(log_SLAmodel, anova_output_csv)
anova_to_csv(LDMCmodel, anova_output_csv)
anova_to_csv(AFHmodel, anova_output_csv)
anova_to_csv(log_ASMmodel, anova_output_csv)

# Pearson Correlation ----
## Lotus ----
cor.test(AusL$`Specific Petal Area (SPA) (cm2/g)`, AusL$`Petal Dry Matter Content (PDMC) (g/g)`, method = "pearson") # Significant
cor.test(AusL$`Petal Mass per Area (PMA)(g/cm2)`, AusL$`Petal Dry Matter Content (PDMC) (g/g)`, method = "pearson") # Significant
cor.test(AusL$`Specific Leaf Area (SLA) (cm2/g)`, AusL$`Leaf Dry Matter Content (LDMC) (g/g)`, method = "pearson") # Significant
cor.test(AusL$`Specific Petal Area (SPA) (cm2/g)`, AusL$`Specific Leaf Area (SLA) (cm2/g)`, method = "pearson")
cor.test(AusL$`Petal Mass per Area (PMA)(g/cm2)`, AusL$`Specific Leaf Area (SLA) (cm2/g)`, method = "pearson")
cor.test(AusL$`Petal Dry Matter Content (PDMC) (g/g)`, AusL$`Leaf Dry Matter Content (LDMC) (g/g)`, method = "pearson")
## Crepis ----
cor.test(AusC$`Specific Petal Area (SPA)(cm2/g)`, AusC$`Petal Dry Matter Content (PDMC)(g/g)`, method = "pearson") # Significant
cor.test(AusC$`Petal Mass Per Area (PMA)(g/cm2)`, AusC$`Petal Dry Matter Content (PDMC)(g/g)`, method = "pearson") # Significant
cor.test(AusC$`Specific Leaf Area (SLA)(cm2/g)`, AusC$`Leaf Dry Matter Content (g/g)`, method = "pearson") 
cor.test(AusC$`Specific Petal Area (SPA)(cm2/g)`, AusC$`Specific Leaf Area (SLA)(cm2/g)`, method = "pearson")
cor.test(AusC$`Petal Mass Per Area (PMA)(g/cm2)`, AusC$`Specific Leaf Area (SLA)(cm2/g)`, method = "pearson")
cor.test(AusC$`Leaf Dry Matter Content (g/g)`, AusC$`Petal Dry Matter Content (PDMC)(g/g)`, method = "pearson")
cor.test(AusC$`Average No. of Seeds per Flower Head (AFH)`, AusC$`Average Individual Seed Mass (ASM)(g)`, method = "pearson")
cor.test(AusC$`Average No. of Seeds per Flower Head (AFH)`, AusC$`Petal Dry Matter Content (PDMC)(g/g)`, method = "pearson") # Significant
cor.test(AusC$`Average No. of Seeds per Flower Head (AFH)`, AusC$`Leaf Dry Matter Content (g/g)`, method = "pearson") # Significant

# Correlation Plots ----
## Lotus ----
?ggplot
SPA_PDMC_Plot <- ggplot(AusL, aes(x = AusL$`Specific Petal Area (SPA) (cm2/g)`, y = AusL$`Petal Dry Matter Content (PDMC) (g/g)`)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", col = "blue") +  # Regression line
  labs(title = "Specific Petal Area (SPA) and Petal Dry Matter Content (PDMC)",
       x = "Specfic Petal Area (SPA) (cm2/g)",
       y = "Petal Dry Matter Content (PDMC) (g/g)") +
  theme_minimal()
print(SPA_PDMC_Plot)

PMA_PDMC_Plot <- ggplot(AusL, aes(x = AusL$`Petal Mass per Area (PMA)(g/cm2)`, y = AusL$`Petal Dry Matter Content (PDMC) (g/g)`)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", col = "blue") +  # Regression line
  labs(title = "Lotus - PMA and PDMC",
       x = "Petal Mass per Area (PMA) (g/cm2)",
       y = "Petal Dry Matter Content (PDMC) (g/g)") +
  theme_minimal()
print(PMA_PDMC_Plot)

SLA_LDMC_Plot <- ggplot(AusL, aes(x = AusL$`Specific Leaf Area (SLA) (cm2/g)`, y = AusL$`Leaf Dry Matter Content (LDMC) (g/g)`)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", col = "blue") +  # Regression line
  labs(title = "Lotus - SLA and LDMC",
       x = "Specific Leaf Area (SLA) (cm2/g)",
       y = "Leaf Dry Matter Content (LDMC) (g/g)") +
  theme_minimal()
print(SLA_LDMC_Plot)

## Crepis ----
SPA_PDMC_Plot <- ggplot(AusC, aes(x = AusC$`Specific Petal Area (SPA)(cm2/g)`, y = AusC$`Petal Dry Matter Content (PDMC)(g/g)`)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", col = "blue") +  # Regression line
  labs(title = "Specific Petal Area (SPA) and Petal Dry Matter Content (PDMC)",
       x = "Specfic Petal Area (SPA) (cm2/g)",
       y = "Petal Dry Matter Content (PDMC) (g/g)") +
  theme_minimal()
print(SPA_PDMC_Plot)

PMA_PDMC_Plot <- ggplot(AusC, aes(x = AusC$`Petal Mass Per Area (PMA)(g/cm2)`, y = AusC$`Petal Dry Matter Content (PDMC)(g/g)`)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", col = "blue") +  # Regression line
  labs(title = "Crepis - PMA and PDMC",
       x = "Petal Mass per Area (PMA) (g/cm2)",
       y = "Petal Dry Matter Content (PDMC) (g/g)") +
  theme_minimal()
print(PMA_PDMC_Plot)

AFH_PDMC_Plot <- ggplot(AusC, aes(x = AusC$`Average No. of Seeds per Flower Head (AFH)`, y = AusC$`Petal Dry Matter Content (PDMC)(g/g)`)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", col = "blue") +  # Regression line
  labs(title = "Crepis - AFH and PDMC",
       x = "Average No. of Seeds per Flower Head (AFH)",
       y = "Petal Dry Matter Content (PDMC) (g/g)") +
  theme_minimal()
print(AFH_PDMC_Plot)

AFH_LDMC_Plot <- ggplot(AusC, aes(x = AusC$`Average No. of Seeds per Flower Head (AFH)`, y = AusC$`Leaf Dry Matter Content (g/g)`)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", col = "blue") +  # Regression line
  labs(title = "Crepis - AFH and LDMC",
       x = "Average No. of Seeds per Flower Head (AFH)",
       y = "Leaf Dry Matter Content (g/g)") +
  theme_minimal()
print(AFH_LDMC_Plot)

combined_data <- AusC %>%
  select(AFH = `Average No. of Seeds per Flower Head (AFH)`, PDMC = `Petal Dry Matter Content (PDMC)(g/g)`, LDMC = `Leaf Dry Matter Content (g/g)`) %>%
  gather(key = "Type", value = "Content", PDMC, LDMC)

# Create the combined plot
combined_plot <- ggplot(combined_data, aes(x = AFH, y = Content)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", col = "blue") +  # Regression line
  labs(title = "Crepis - AFH and Dry Matter Content",
       x = "Average No. of Seeds per Flower Head (AFH)",
       y = "Dry Matter Content (g/g)") +
  facet_wrap(~Type, scales = "free_y") +
  theme_minimal()

print(combined_plot)


# Correlogram ----

## For Lotus ----
View(AusL)
Lotcor_Data <- AusL[,c(18,19,20,21)]
names(Lotcor_Data)
names(Lotcor_Data)[names(Lotcor_Data) == "Petal Mass per Area (PMA)(g/cm2)"] <- "PMA"
names(Lotcor_Data)[names(Lotcor_Data) == "Petal Dry Matter Content (PDMC) (g/g)"] <- "PDMC"
names(Lotcor_Data)[names(Lotcor_Data) == "Specific Leaf Area (SLA) (cm2/g)"] <- "SLA"
names(Lotcor_Data)[names(Lotcor_Data) == "Leaf Dry Matter Content (LDMC) (g/g)"] <- "LDMC" 
Lotcor_Matrix <- cor(Lotcor_Data, use = "complete.obs", method = "pearson")
corrplot(Lotcor_Matrix, method = "number")
corrplot.mixed(Lotcor_Matrix, order = 'AOE')

View(Lotcor_Data)
## For Crepis ----
View(AusC)
Crecor_Data <- AusC[,c(10,11,12,14,15,16)]
names(Crecor_Data)
names(Crecor_Data)[names(Crecor_Data) == "Petal Mass Per Area (PMA)(g/cm2)"] <- "PMA"
names(Crecor_Data)[names(Crecor_Data) == "Petal Dry Matter Content (PDMC)(g/g)"] <- "PDMC"
names(Crecor_Data)[names(Crecor_Data) == "Specific Leaf Area (SLA)(cm2/g)"] <-"SLA"
names(Crecor_Data)[names(Crecor_Data) == "Leaf Dry Matter Content (g/g)"] <- "LDMC"
names(Crecor_Data)[names(Crecor_Data) == "Average No. of Seeds per Flower Head (AFH)"] <- "AFH"
names(Crecor_Data)[names(Crecor_Data) == "Average Individual Seed Mass (ASM)(g)"] <- "ASM"
Crecor_Matrix <- cor(Crecor_Data, use = "complete.obs", method = "pearson")
corrplot(Crecor_Matrix, method = "number")
corrplot.mixed(Crecor_Matrix, order = 'AOE')
