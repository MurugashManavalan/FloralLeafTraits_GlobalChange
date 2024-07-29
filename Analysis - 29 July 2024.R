# Installing Packages ----
install.packages("corrplot")
install.packages("ggrepel")
install.packages("Hmisc")
install.packages("VennDiagram")
install.packages("devtools")
install.github("vqv/ggbiplot") # Need to load devtools package before installing

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
library(VennDiagram)
library(ggbiplot)
library(devtools)
library(ggbiplot)
library(plyr)

# Loading Files into R ----
setwd("/Users/murugash/Desktop/PhD/Austria Experiment/R Analysis/Crepis and Lotus Samples - R Analysis")
AusL <- read_csv("Austria Experiment - Lotus Data - Core Response Variables - 10 July 2024.csv")
AusC <- read_csv("Austria Experiment - Crepis Data - Core Response Variables - 29 July 2024.csv")
AusC <- AusC[-c(63:163),]
AusPC <- read_csv("Austria Experiment - Crepis and Leaf Data - RDA - 29 July 2024.csv")
AusPC <- AusPC[-c(85:117),]
AusRD <- read_csv("Austria Experiment - Crepis and Leaf Data - RDA - 29 July 2024.csv")
AusRD <- AusRD[-c(85:117),]
AusL1 <- AusL[AusL$`Plot No.`!= "P16",]
View(AusRD)
dir()

# Function to Log and Square Root transform Response Variables ----
log_sqrt_transform <- function(df, cols) {
  # Check if cols are numeric (indices)
  if (is.numeric(cols)) {
    cols <- names(df)[cols]
  }
  
  for (col in cols) {
    log_col_name <- paste0("log_", col)
    sqrt_col_name <- paste0("sqrt_", col)
    df[[log_col_name]] <- log(df[[col]])
    df[[sqrt_col_name]] <- sqrt(df[[col]])
  }
  
  return(df)
}

AusL <- log_sqrt_transform(AusL, c(9:22))
AusC <- log_sqrt_transform(AusC, c(9:16))
View(AusC)
# Function to create Histogram of Response Variables ----

hist_df <- function(df, cols) {
  # Check if cols are numeric (indices)
  if (is.numeric(cols)) {
    cols <- names(df)[cols]
  }
  for(col in cols) {
    hist(df[[col]], main = paste0("Histogram of ", col), xlab = col)
  }
}

# Histogram of Response Variables ----

hist_df(AusL, c(9,50))
hist_df(AusC, c(9,35))
hist_df(AusPC, c(9,25))

# Shapiro-Wilk test for Response Variables ----
Lotus_shapiro_results <- apply(AusL[,c(9:50)], 2, shapiro.test)
Lotus_shapiro_results
Crepis_shapiro_results <- apply(AusC[,c(9:32)], 2, shapiro.test)
Crepis_shapiro_results
colnames(AusC)

# Converting Grouping variables, fixed effect predictor variables and response variables to Factors ----
## In Lotus Dataset ----
AusL$CO2 <- as.factor(AusL$CO2)
AusL$Temperature <- as.factor(AusL$Temperature)
AusL$Drought <- as.factor(AusL$Drought)
AusL$`Plot No.` <- as.factor(AusL$`Plot No.`)

## In Crepis Dataset ----
AusC$CO2 <- as.factor(AusC$CO2)
AusC$Temperature <- as.factor(AusC$Temperature)
AusC$Drought <- as.factor(AusC$Drought)
AusC$`Plot No.` <- as.factor(AusC$`Plot No.`)

## In PCA Dataset ----
AusPC$CO2 <- as.factor(AusPC$CO2)
AusPC$Temperature <- as.factor(AusPC$Temperature)
AusPC$Drought <- as.factor(AusPC$Drought)
AusPC$`Plot No.` <- as.factor(AusPC$`Plot No.`)
AusPC$Species <- as.factor(AusPC$Species)

## In RDA Dataset----
AusRD$`Plot No.` <- as.factor(AusRD$`Plot No.`)
AusRD$CO2 <- as.factor(AusRD$CO2)
AusRD$Temperature <- as.factor(AusRD$Temperature)
AusRD$Drought <- as.factor(AusRD$Drought)
AusRD$Species <- as.factor(AusRD$Species)

# Preparation of Mixed Effect Model ----
## Mixed-effect Models for Response Variables of Lotus ----
l_DAmodel <- lmer(`Display Area (DA)(cm2)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusL)
log_SSPAmodel <- lmer(`log_Specific Standard Petal Area (SSA)(cm2/g)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusL)
log_SW1PAmodel <- lmer(`log_Specific Wing 1 Petal Area (SW1A)(cm2/g)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusL)
SW2PAmodel <- lmer(`Specific Wing 2 Petal Area (SW2A)(cm2/g)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusL)
sq_SKPAmodel <- lmer(`sqrt_Specific Keel Petal Area (SKA)(cm2/g)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusL)
log_SPMAmodel <- lmer(`log_Standard Petal Mass per Area (SPMA)(g/cm2)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusL)
log_W1PMAmodel <- lmer(`log_Wing 1 Petal Mass per Area (W1PMA)(g/cm2)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusL)
log_W2PMAmodel <- lmer(`log_Wing 2 Petal Mass per Area (W2PMA)(g/cm2)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusL)
log_KPMAmodel <- lmer(`log_Keel Petal Mass per Area (KPMA)(g/cm2)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusL)
l_sq_SPAmodel <- lmer(`sqrt_Specific Petal Area (SPA)(cm2/g)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusL)
l_log_PMAmodel <- lmer(`log_Petal Mass per Area (PMA)(g/cm2)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusL)
l_PDMCmodel <- lmer(`Petal Dry Matter Content (PDMC)(g/g)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusL)
l_SLAmodel <- lmer(`Specific Leaf Area (SLA)(cm2/g)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusL)
l_log_LDMCmodel <- lmer(`log_Leaf Dry Matter Content (LDMC)(g/g)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusL)

## Mixed Effect Models for Response Variables of Crepis ----
c_sq_DAmodel <- lmer(`Display Area (DA)(cm2)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusC)
c_sq_SPAmodel <- lmer(`sqrt_Specific Petal Area (SPA)(cm2/g)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusC)
c_log_PMAmodel <- lmer(`log_Petal Mass Per Area (PMA)(g/cm2)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusC)
c_sq_PDMCmodel <- lmer(`sqrt_Petal Dry Matter Content (PDMC)(g/g)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusC)
c_log_SLAmodel <- lmer(`log_Specific Leaf Area (SLA)(cm2/g)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusC)
c_sq_LDMCmodel <- lmer(`sqrt_Leaf Dry Matter Content (LDMC)(g/g)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusC)
sq_AFHmodel <- lmer(`sqrt_Average No. of Seeds per Flower Head (AFH)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusC)
log_ASMmodel <- lmer(`log_Average Individual Seed Mass (ASM)(g)` ~ CO2 * Temperature * Drought + (1|`Plot No.`), data = AusC)

# Preparation of Linear Models ----
## Linear Models for Response Variables of Lotus ----
l_DAmodel <- lm(`Display Area (DA)(cm2)` ~ CO2 * Temperature * Drought, data = AusL)
log_SSPAmodel <- lm(`log_Specific Standard Petal Area (SSA)(cm2/g)` ~ CO2 * Temperature * Drought, data = AusL)
log_SW1PAmodel <- lm(`log_Specific Wing 1 Petal Area (SW1A)(cm2/g)` ~ CO2 * Temperature * Drought, data = AusL)
SW2PAmodel <- lm(`Specific Wing 2 Petal Area (SW2A)(cm2/g)` ~ CO2 * Temperature * Drought, data = AusL)
sq_SKPAmodel <- lm(`sqrt_Specific Keel Petal Area (SKA)(cm2/g)` ~ CO2 * Temperature * Drought, data = AusL)
log_SPMAmodel <- lm(`log_Standard Petal Mass per Area (SPMA)(g/cm2)` ~ CO2 * Temperature * Drought, data = AusL)
log_W1PMAmodel <- lm(`log_Wing 1 Petal Mass per Area (W1PMA)(g/cm2)` ~ CO2 * Temperature * Drought, data = AusL)
log_W2PMAmodel <- lm(`log_Wing 2 Petal Mass per Area (W2PMA)(g/cm2)` ~ CO2 * Temperature * Drought, data = AusL)
log_KPMAmodel <- lm(`log_Keel Petal Mass per Area (KPMA)(g/cm2)` ~ CO2 * Temperature * Drought, data = AusL)
l_sq_SPAmodel <- lm(`sqrt_Specific Petal Area (SPA)(cm2/g)` ~ CO2 * Temperature * Drought, data = AusL)
l_log_PMAmodel <- lm(`log_Petal Mass per Area (PMA)(g/cm2)` ~ CO2 * Temperature * Drought, data = AusL)
l_PDMCmodel <- lm(`Petal Dry Matter Content (PDMC)(g/g)` ~ CO2 * Temperature * Drought, data = AusL)
l_SLAmodel <- lm(`Specific Leaf Area (SLA)(cm2/g)` ~ CO2 * Temperature * Drought, data = AusL)
l_log_LDMCmodel <- lm(`log_Leaf Dry Matter Content (LDMC)(g/g)` ~ CO2 * Temperature * Drought, data = AusL)

## Linear Models for Response Variables of Crepis ----
c_sq_DAmodel <- lm(`Display Area (DA)(cm2)`~ CO2 * Temperature * Drought, data = AusC)
c_sq_SPAmodel <- lm(`sqrt_Specific Petal Area (SPA)(cm2/g)` ~ CO2 * Temperature * Drought, data = AusC)
c_log_PMAmodel <- lm(`log_Petal Mass Per Area (PMA)(g/cm2)` ~ CO2 * Temperature * Drought, data = AusC)
c_sq_PDMCmodel <- lm(`sqrt_Petal Dry Matter Content (PDMC)(g/g)` ~ CO2 * Temperature * Drought, data = AusC)
c_log_SLAmodel <- lm(`log_Specific Leaf Area (SLA)(cm2/g)` ~ CO2 * Temperature * Drought, data = AusC)
c_sq_LDMCmodel <- lm(`sqrt_Leaf Dry Matter Content (LDMC)(g/g)` ~ CO2 * Temperature * Drought, data = AusC)
sq_AFHmodel <- lm(`sqrt_Average No. of Seeds per Flower Head (AFH)` ~ CO2 * Temperature * Drought, data = AusC)
log_ASMmodel <- lm(`log_Average Individual Seed Mass (ASM)(g)` ~ CO2 * Temperature * Drought, data = AusC)


# ANOVA Analysis ----
## Anova test for Reponse Variables in Lotus ----
anova(l_DAmodel)
anova(log_SSPAmodel)
anova(log_SW1PAmodel)
anova(SW2PAmodel)
anova(sq_SKPAmodel)
anova(log_SPMAmodel)
anova(log_W1PMAmodel)
anova(log_W2PMAmodel)
anova(log_KPMAmodel)
anova(l_sq_SPAmodel)
anova(l_log_PMAmodel)
anova(l_PDMCmodel)
anova(l_SLAmodel)
anova(l_log_LDMCmodel)

## Anova test for Response Variables in Crepis ----
anova(c_sq_DAmodel)
anova(c_sq_SPAmodel)
anova(c_log_PMAmodel)
anova(c_sq_PDMCmodel)
anova(c_log_SLAmodel)
anova(c_sq_LDMCmodel)
anova(sq_AFHmodel)
anova(log_ASMmodel)

# Plotting Significant Response Variables against Predictor Variables ----
## Plots in Lotus ----
plot(AusL$`Display Area (DA)(cm2)`~AusL$Temperature, main = "Temperature on DA", xlab = "Temperature Level", ylab = "Display Area (DA)(cm2)")
plot(AusL$`Display Area (DA)(cm2)`~AusL$Drought)
plot(AusL$`Specific Petal Area (SPA)(cm2/g)`~AusL$Drought, main= "Drought on SPA", xlab = "Drought Level", ylab = "Specific Petal Area (SPA) (cm2/g)")
plot(AusL$`Specific Leaf Area (SLA)(cm2/g)`~AusL$CO2, main = "CO2 on SLA", xlab = "CO2 Level", ylab = "Specific Leaf Area (SLA)(cm2/g)")
plot(AusL$`Specific Leaf Area (SLA)(cm2/g)`~AusL$Drought, main = "Drought on SLA", xlab = "Drought Level", ylab = "Specific Leaf Area (SLA)(cm2/g)")
plot(AusL$`Leaf Dry Matter Content (LDMC)(g/g)`~AusL$CO2, main = "CO2 on LDMC", xlab = "CO2 Level", ylab = "Leaf Dry Matter Content (LDMC)(g/g)")
plot(AusL$`Leaf Dry Matter Content (LDMC)(g/g)`~AusL$Drought, main = "Drought on LDMC", xlab = "Drought Level", ylab = "Leaf Dry Matter Content (LDMC)(g/g)")

## Plots in Lotus (Using ggplot) ----
### Display Area ----
ggplot(AusL, aes(x = AusL$Temperature, y = AusL$`Display Area (DA)(cm2)`))+
  geom_boxplot() +
  labs(
    title = "Lotus - Effect of Temperature on Display Area",
    x = "Temperature Level",
    y = "Display Area (DA)(cm2)"
  )

ggplot(AusL, aes(x = CO2, y = `Display Area (DA)(cm2)`, fill = Temperature)) +
  geom_boxplot() +
  facet_wrap(~ Temperature) +
  labs(
    title = "Lotus - Effect of CO2 Level on DA by Temperature",
    x = "CO2 Level",
    y = "Display Area (DA)(cm2)",
    fill = "Temperature"
  ) +
  theme_minimal()

ggplot(AusL, aes(x = CO2, y = `Display Area (DA)(cm2)`, fill = Drought)) +
  geom_boxplot() +
  facet_wrap(~ Drought) +
  labs(
    title = "Lotus - Effect of CO2 Level on DA by Drought Level",
    x = "CO2 Level",
    y = "Display Area (DA)(cm2)",
    fill = "Drought Level"
  ) +
  theme_minimal()

### Specific Leaf Area ----
ggplot(AusL, aes(x = CO2,y= `Specific Leaf Area (SLA)(cm2/g)`)) +
  geom_boxplot() +
  labs(
    title = "Lotus - Effect of CO2 Level on SLA",
    x = "CO2 Level",
    y = "Specific Leaf Area (SLA) (cm2/g)"
  )

ggplot(AusL, aes(x = AusL$Drought, y= AusL$`Specific Leaf Area (SLA)(cm2/g)`)) +
  geom_boxplot()+
  labs(
    title = "Lotus - Effect of Drought on SLA",
    x = "Drought Level",
    y = "Specific Leaf Area (SLA) (cm2/g)"
  )

ggplot(AusL, aes(x = CO2, y = `Specific Leaf Area (SLA)(cm2/g)`, fill = Temperature)) +
  geom_boxplot() +
  facet_wrap(~ Temperature) +
  labs(
    title = "Lotus - Effect of CO2 Level on SLA by Temperature Level",
    x = "CO2 Level",
    y = "Specific Leaf Area (SLA) (cm2/g)",
    fill = "Temperature"
  ) +
  theme_minimal()


### Leaf Dry Matter Content ----

ggplot(AusL, aes(x = CO2,y= `Leaf Dry Matter Content (LDMC)(g/g)`)) +
  geom_boxplot() +
  labs(
    title = "Lotus - Effect of CO2 Level on LDMC",
    x = "CO2 Level",
    y = "Leaf Dry Matter Content (LDMC) (g/g)"
  )

ggplot(AusL, aes(x = Drought, y= AusL$`Leaf Dry Matter Content (LDMC)(g/g)`)) +
  geom_boxplot()+
  labs(
    title = "Lotus - Effect of Drought on LDMC",
    x = "Drought Level",
    y = "Leaf Dry Matter Content (LDMC) (g/g)"
  )

ggplot(AusL, aes(x = CO2, y = `Leaf Dry Matter Content (LDMC)(g/g)`, fill = Drought)) +
  geom_boxplot() +
  facet_wrap(~ Drought) +
  labs(
    title = "Lotus - Effect of CO2 on LDMC by Drought",
    x = "CO2",
    y = "Leaf Dry Matter Content (LDMC)(g/g)",
    fill = "Drought"
  ) +
  theme_minimal()


## Plots in Crepis ----
plot(AusC$`Display Area (DA)(cm2)`~AusC$Drought, main = "Drought on DA", xlab = "Drought Level", ylab = "Display Area (DA)(cm2)")
plot(AusC$`Specific Petal Area (SPA)(cm2/g)`~AusC$Drought, main = "Drought on SPA", xlab = "Drought Level", ylab = "Specific Petal Area (SPA) (cm2/g)")
plot(AusC$`Petal Dry Matter Content (PDMC)(g/g)`~AusC$Drought, main = "Drought on PDMC", xlab = "Drought Level", ylab = "Petal Dry Matter Content (PDMC) (g/g)")
plot(AusC$`Leaf Dry Matter Content (g/g)`~AusC$Drought, main = "Drought on LDMC", xlab = "Drought Level", ylab = "Leaf Dry Matter Content (PDMC) (g/g)")
plot(AusC$`Average No. of Seeds per Flower Head (AFH)`~AusC$Temperature, main = "Temperature on No. of Seeds", xlab = "Temperature Level", ylab = "No. of Seeds")
plot(AusC$`Average No. of Seeds per Flower Head (AFH)`~AusC$Drought, main = "Drought on No. of Seeds", xlab = "Drought", ylab = "No. of Seeds")

## Plots in Crepis (Using ggplot) ----
View(AusC)
ggplot(AusC, aes(x = AusC$Drought, y= `Display Area (DA)(cm2)`)) +
  geom_boxplot()+
  labs(
    title = "Crepis - Effect of Drought on DA",
    x = "Drought Level",
    y = "Display Area (DA)(cm2)"
  )

ggplot(AusC, aes(x = AusC$Drought, y= `Specific Petal Area (SPA)(cm2/g)`)) +
  geom_boxplot()+
  labs(
    title = "Crepis - Effect of Drought on SPA",
    x = "Drought Level",
    y = "Specific Petal Area (SPA)(cm2/g)"
  )

ggplot(AusC, aes(x = AusC$Drought, y= `Petal Dry Matter Content (PDMC)(g/g)`)) +
  geom_boxplot()+
  labs(
    title = "Crepis - Effect of Drought on PDMC",
    x = "Drought Level",
    y = "Petal Dry Matter Content (PDMC)(g/g)"
  )

ggplot(AusC, aes(x = AusC$Drought, y= `Leaf Dry Matter Content (LDMC)(g/g)`)) +
  geom_boxplot()+
  labs(
    title = "Crepis - Effect of Drought on LDMC",
    x = "Drought Level",
    y = "Leaf Dry Matter Content (g/g)"
  )

ggplot(AusC, aes(x = AusC$Temperature, y= `Average No. of Seeds per Flower Head (AFH)`)) +
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

ggplot(AusC, aes(x = Drought, y = `Average No. of Seeds per Flower Head (AFH)`, fill = Temperature)) +
  geom_boxplot() +
  facet_wrap(~ Temperature) +
  labs(
    title = "Crepis - Effect of Drought on AFH by Temperature Level",
    x = "Drought Level",
    y = "Average No. of Seeds per Flower Head (AFH)",
    fill = "Temperature Level"
  ) +
  theme_minimal()


# Principal Component Analysis ----
## Creation of PCA models ----
View(AusPC)
lav <- prcomp(AusPC[AusPC$Species == "Lotus",c(9:13,18,20:22)], scale = TRUE)
lmv <- prcomp(AusPC[AusPC$Species == "Lotus", c(9,18,20:22)], scale = TRUE)
cav <- prcomp(AusPC[AusPC$Species == "Crepis", c(9,18,20:24)], scale = TRUE)
cmv <- prcomp(AusPC[AusPC$Species == "Crepis", c(9,18,20:24)], scale = TRUE)
lcav <- prcomp(AusPC[,c(16, 18,19,20)], center = TRUE, scale = TRUE)
?prcomp
names(AusPC)

colnames(AusPC)
## Plotting of PCA models ----
names(AusPC)[names(AusPC) == "Specific Petal Area (SPA)(cm2/g)"] <- "SPA"
names(AusPC)[names(AusPC) == "Petal Dry Matter Content (PDMC)(g/g)"] <- "PDMC"
names(AusPC)[names(AusPC) == "Specific Leaf Area (SLA)(cm2/g)"] <- "SLA"
names(AusPC)[names(AusPC) == "Leaf Dry Matter Content (LDMC)(g/g)"] <- "LDMC"
names(AusPC)[names(AusPC) == "Average No. of Seeds per Flower Head (AFH)"] <- "AFH"
names(AusPC)[names(AusPC) == "Average Individual Seed Mass (ASM)(g)"] <- "ASM"
names(AusPC)[names(AusPC) == "Display Area (DA)(cm2)"] <- "DA"

biplot(lav, scale = 0)
biplot(lmv, scale = 0)
biplot(cav, scale = 0)
biplot(cmv, scale = 1)
biplot(lcav, scale = 0)
scores(lmv, display = "species")
?biplot
lmv_scores <- lmv$x
lmv_scores[,3]

### PCA plots using ggbiplot ----

ggbiplot(lmv)
ggbiplot(cmv)

## Extracting PC Scores ----
summary(cmv)
lcav$x
AusPC1 <- cbind(AusPC, lcav$x[,1:4])
View(AusPC1)

## Plotting with GG plot ----
ggplot(AusPC, aes(PC1,PC2, colour = Species, fill = Species)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

## Extracting PCA Results----
### For Lotus ----

lotus_pca_trait_loadings <- lmv$rotation*lmv$sdev
lotus_pca_eigenvalues <- lmv$sdev^2
lotus_pca_variance <- lotus_pca_eigenvalues/sum(lotus_pca_eigenvalues)
lotus_pca_cum_variance <- cumsum(lotus_pca_variance)
lotus_pca_trait_loadings


write.csv(lotus_pca_trait_loadings, "lotus_pca_trait_loadings.csv", row.names = FALSE)
write.csv(lotus_pca_eigenvalues, "lotus_pca_eigenvalues.csv", row.names = FALSE)
write.csv(lotus_pca_variance, "lotus_pca_variance.csv")
write.csv(lotus_pca_cum_variance, "lotus_pca_cum_variance.csv")

### For Crepis ----
cav
crepis_pca_trait_loadings <- cav$rotation*cav$sdev
crepis_pca_eigenvalues <- cav$sdev^2
crepis_pca_variance <- crepis_pca_eigenvalues/sum(crepis_pca_eigenvalues)
crepis_pca_cum_variance <- cumsum(crepis_pca_variance)

write.csv(crepis_pca_trait_loadings, "crepis_pca_trait_loadings.csv", row.names = FALSE)
write.csv(crepis_pca_eigenvalues, "crepis_pca_eigenvalues.csv", row.names = FALSE)
write.csv(crepis_pca_variance, "crepis_pca_variance.csv")
write.csv(crepis_pca_cum_variance, "crepis_pca_cum_variance.csv")



# Proportion of variance explained
proportion_variance_explained <- eigenvalues / sum(eigenvalues)
proportion_variance_explained
# Redundancy Analysis ----
### Sub-setting Data frames for Lotus and Crepis ----
lotus_rows <- AusRD[AusRD$Species == "Lotus", ]
lotus_rows <- lotus_rows[-c(40:72), ]
AusRDL <- lotus_rows[, -c(23:25)]

crepis_rows <- AusRD[AusRD$Species == "Crepis",]
crepis_rows <- crepis_rows[-c(46:78),]
AusRDC <- crepis_rows[,-c(10:17)]
## Redundancy Analysis for Lotus ----
names(AusRDL)[names(AusRDL) == "Specific Petal Area (SPA)(cm2/g)"] <- "SPA"
names(AusRDL)[names(AusRDL) == "Petal Dry Matter Content (PDMC)(g/g)"] <- "PDMC"
names(AusRDL)[names(AusRDL) == "Specific Leaf Area (SLA)(cm2/g)"] <- "SLA"
names(AusRDL)[names(AusRDL) == "Leaf Dry Matter Content (LDMC)(g/g)"] <- "LDMC"
names(AusRDL)[names(AusRDL) == "Display Area (DA)(cm2)"] <- "DA"

rda.l <- rda(AusRDL[,c(9,18,20:22)] ~ CO2*Temperature*Drought, data = AusRDL, scale = TRUE)
summary(rda.l)
scores(rda.l)
ordistep(rda.l)

anova(rda.l, step = 1000)

names(AusRD)
### Extracting Values for Lotus RDA ----
summary_text <- capture.output(summary(rda.l))
writeLines(summary_text, "lotus_rda_summary.txt")
### Plot for Lotus RDA ----
plot(rda.l, type = "n",xlim = c(-1,1), ylim = c(-1,1))
site_scores <- scores(rda.l, display = "sites")
plot(rda.l, display = "both", cex = 0.7)
points(site_scores, pch = 16, col = "red", cex = 0.5) # Ignore this to not get points
### Adding Arrows and Text for Response Variables
arrows(0, 0, scores(rda.l, display = "species")[,1], scores(rda.l, display = "species")[,2], col = 'blue', length = 0.1)
text(scores(rda.l, display = "species")[,1], scores(rda.l, display = "species")[,2], labels = rownames(scores(rda.l, display = "species")), col = 'blue', pos = 3, cex = 0.6)
### Adding Arrows and Text for Predictor Variables
arrows(0, 0, scores(rda.l, display = "bp")[,1], scores(rda.l, display = "bp")[,2], col = 'red', length = 0.1)

text(scores(rda.l, display = "bp")[,1], scores(rda.l, display = "bp")[,2], labels = rownames(scores(rda.l, display = "bp")), col = 'red', pos = 3, cex = 0.6)

## Redundancy Analysis for Lotus (Contd.)----
anova.cca(rda.l, permutations = 9999)
Lotus_RDA <- anova.cca(rda.l, permutations = 9999, by = "terms")
Lotus_RDA # Temperature and Drought are Significant
writeLines(capture.output(Lotus_RDA), "lotus_rda_anova.txt")
ordistep(rda.l)
summary(rda.l)
scores(rda.l)
Lotus_RDA$F
Lotus_RDA$`Pr(>F)`
summary(rda.l)
rda.ln <- rda(AusRDL[,c(17:21)] ~ Drought + Condition(AusRDL$`Temperature Level`*AusRDL$`CO2 Level`), data = AusRDL, scale = TRUE)


## Redundancy Analysis for Crepis ----
names(AusRDC)[names(AusRDC) == "Specific Petal Area (SPA)(cm2/g)"] <- "SPA"
names(AusRDC)[names(AusRDC) == "Petal Dry Matter Content (PDMC)(g/g)"] <- "PDMC"
names(AusRDC)[names(AusRDC) == "Specific Leaf Area (SLA)(cm2/g)"] <- "SLA"
names(AusRDC)[names(AusRDC) == "Leaf Dry Matter Content (LDMC)(g/g)"] <- "LDMC"
names(AusRDC)[names(AusRDC) == "Display Area (DA)(cm2)"] <- "DA"
names(AusRDC)[names(AusRDC) == "Average No. of Seeds per Flower Head (AFH)"] <- "AFH"
names(AusRDC)[names(AusRDC) == "Average Individual Seed Mass (ASM)(g)"] <- "ASM"
rda.c <- rda(AusRDC[,c(9:10,12:16)] ~ CO2*Temperature*Drought, data = AusRDC, scale = TRUE)

anova.cca(rda.c, permutations = 9999)
anova.cca(rda.c, permutations = 9999, by = "axis")
Crepis_RDA <- anova.cca(rda.c, permutations = 9999, by = "terms")
Crepis_RDA # Drought and CO2:Temperature are significant
writeLines(capture.output(Crepis_RDA), "crepis_rda_anova.txt")
Crepis_RDA$F
Crepis_RDA$`Pr(>F)`

summary(rda.c)
scores(rda.c)
ordistep(rda.c)
anova(rda.c, step = 1000)

### Extracting Values for Crepis RDA ----
summary_text <- capture.output(summary(rda.c))
writeLines(summary_text, "crepis_rda_summary.txt")

### Plot for Crepis RDA ----
plot(rda.c, type = "n", xlim = c(-1,1), ylim = c(-1,1))
site_scores <- scores(rda.l, display = "sites")
plot(rda.c, display = "both", cex = 0.7)
points(site_scores, pch = 16, col = "red", cex = 0.5) # Ignore this to not get row names 
### Adding Arrows and Text for Response Variables
arrows(0, 0, scores(rda.c, display = "species")[,1], scores(rda.c, display = "species")[,2], col = 'blue', length = 0.1)
text(scores(rda.c, display = "species")[,1], scores(rda.c, display = "species")[,2], labels = rownames(scores(rda.c, display = "species")), col = 'blue', pos = 3, cex = 0.6)
### Adding Arrows and Text for Predictor Variables
arrows(0, 0, scores(rda.c, display = "bp")[,1], scores(rda.c, display = "bp")[,2], col = 'red', length = 0.1)
text(scores(rda.c, display = "bp")[,1], scores(rda.c, display = "bp")[,2], labels = rownames(scores(rda.c, display = "bp")), col = 'red', pos = 3, cex = 0.6)

rda.cn <- rda(AusRDC[,c(18:23)] ~ Drought + Condition(AusRDC$`Temperature Level`*AusRDC$`CO2 Level`), data = AusRDC, scale = TRUE)
View(AusRD)
## Redundancy Analysis for both Crepis and Lotus ----

rda.1 <- rda(AusRD[,c(18,20:22)] ~ CO2*Temperature*Drought, data = AusRD, scale = TRUE)
View(AusRD)
ordiplot(rda.1, type = "text", scale = TRUE)
anova.cca(rda.1, permutations = 9999)
anova.cca(rda.1, permutations = 9999, by = "axis")
anova.cca(rda.1, permutations = 9999, by = "terms")
Combined_RDA <- anova.cca(rda.1, permutations = 9999, by = "terms")
Combined_RDA # Only Drought is Significant
writeLines(capture.output(Combined_RDA), "Combined_rda_anova.txt")
Combined_RDA$F
Combined_RDA$`Pr(>F)`

summary_rda <- summary(rda.1)
summary_rda

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

## Output ANOVA results to CSV file for Mixed Effect Linear models in Lotus ----
anova_output_csv <- 'anova_LMLo.csv'
anova_to_csv(DAmodel, anova_output_csv)
anova_to_csv(log_SSPAmodel, anova_output_csv)
anova_to_csv(log_SW1PAmodel, anova_output_csv)
anova_to_csv(SW2PAmodel, anova_output_csv)
anova_to_csv(sq_SKPAmodel, anova_output_csv)
anova_to_csv(l_sq_SPAmodel, anova_output_csv)
anova_to_csv(l_PDMCmodel, anova_output_csv)
anova_to_csv(l_SLAmodel, anova_output_csv)
anova_to_csv(l_log_LDMCmodel, anova_output_csv)

## Output ANOVA results to CSV file for Mixed Effect Linear models in Crepis ----
anova_output_csv <- 'anova_LMCr.csv'
anova_to_csv(c_sq_DAAmodel, anova_output_csv)
anova_to_csv(c_sq_SPAmodel, anova_output_csv)
anova_to_csv(c_sq_PDMCmodel, anova_output_csv)
anova_to_csv(c_log_SLAmodel, anova_output_csv)
anova_to_csv(c_sq_LDMCmodel, anova_output_csv)
anova_to_csv(sq_AFHmodel, anova_output_csv)
anova_to_csv(log_ASMmodel, anova_output_csv)

## Output ANOVA results to CSV file for Linear models in Lotus ----
anova_output_csv <- 'anova_LLo.csv'
anova_to_csv(DAmodel, anova_output_csv)
anova_to_csv(log_SSPAmodel, anova_output_csv)
anova_to_csv(log_SW1PAmodel, anova_output_csv)
anova_to_csv(SW2PAmodel, anova_output_csv)
anova_to_csv(sq_SKPAmodel, anova_output_csv)
anova_to_csv(l_sq_SPAmodel, anova_output_csv)
anova_to_csv(l_PDMCmodel, anova_output_csv)
anova_to_csv(l_SLAmodel, anova_output_csv)
anova_to_csv(l_log_LDMCmodel, anova_output_csv)

## Output ANOVA results to CSV file for Linear models in Crepis ----
anova_output_csv <- 'anova_LCr.csv'
anova_to_csv(c_sq_DAmodel, anova_output_csv)
anova_to_csv(c_sq_SPAmodel, anova_output_csv)
anova_to_csv(c_sq_PDMCmodel, anova_output_csv)
anova_to_csv(c_log_SLAmodel, anova_output_csv)
anova_to_csv(c_sq_LDMCmodel, anova_output_csv)
anova_to_csv(sq_AFHmodel, anova_output_csv)
anova_to_csv(log_ASMmodel, anova_output_csv)


# Pearson Correlation ----
## Lotus ----
cor.test(AusL$`Specific Petal Area (SPA)(cm2/g)`, AusL$`Petal Dry Matter Content (PDMC) (g/g)`, method = "pearson") # Significant
cor.test(AusL$`Petal Mass per Area (PMA)(g/cm2)`, AusL$`Petal Dry Matter Content (PDMC) (g/g)`, method = "pearson") # Significant
cor.test(AusL$`Specific Leaf Area (SLA) (cm2/g)`, AusL$`Leaf Dry Matter Content (LDMC) (g/g)`, method = "pearson") # Significant
cor.test(AusL$`Specific Petal Area (SPA)(cm2/g)`, AusL$`Specific Leaf Area (SLA)(cm2/g)`, method = "pearson")
cor.test(AusL$`Petal Mass per Area (PMA)(g/cm2)`, AusL$`Specific Leaf Area (SLA)(cm2/g)`, method = "pearson")
cor.test(AusL$`Petal Dry Matter Content (PDMC) (g/g)`, AusL$`Leaf Dry Matter Content (LDMC)(g/g)`, method = "pearson")
cor.test(AusL$`Display Area (DA)(cm2)`, AusL$`Petal Dry Matter Content (PDMC) (g/g)`, method = "pearson") #Significant
cor.test(AusL$`Display Area (DA)(cm2)`, AusL$`Specific Petal Area (SPA)(cm2/g)`, method = "pearson") # Significant

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
cor.test(AusC$`Average No. of Seeds per Flower Head (AFH)`, AusC$`Specific Leaf Area (SLA)(cm2/g)`, method = "pearson")
cor.test(AusC$`Display Area (DA)(cm2)`, AusC$`Petal Dry Matter Content (PDMC)(g/g)`, method = "pearson")
cor.test(AusC$`Display Area (DA)(cm2)`, AusC$`Specific Petal Area (SPA)(cm2/g)`, method = "pearson") # Significant


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
Lotcor_Data <- AusL[,c(9,18,20:22)]
names(Lotcor_Data)
names(Lotcor_Data)[names(Lotcor_Data) == "Display Area (DA)(cm2)"] <- "DA"
names(Lotcor_Data)[names(Lotcor_Data) == "Specific Petal Area (SPA)(cm2/g)"] <- "SPA"
names(Lotcor_Data)[names(Lotcor_Data) == "Petal Dry Matter Content (PDMC)(g/g)"] <- "PDMC"
names(Lotcor_Data)[names(Lotcor_Data) == "Specific Leaf Area (SLA)(cm2/g)"] <- "SLA"
names(Lotcor_Data)[names(Lotcor_Data) == "Leaf Dry Matter Content (LDMC)(g/g)"] <- "LDMC" 
Lotcor_Matrix <- cor(Lotcor_Data, use = "complete.obs", method = "pearson")
corrplot(Lotcor_Matrix, method = "color", type = "full", 
         addCoef.col = "black", # Add black text with correlation values
         tl.col = "black", tl.srt = 45, # Rotate labels and set their color
         number.cex = 0.8) 
corrplot.mixed(Lotcor_Matrix, order = 'AOE')

View(Lotcor_Data)
## For Crepis ----
View(AusC)
Crecor_Data <- AusC[,c(9,10,12:16)]
names(Crecor_Data)
names(Crecor_Data)[names(Crecor_Data) == "Display Area (DA)(cm2)"] <- "DA"
names(Crecor_Data)[names(Crecor_Data) == "Specific Petal Area (SPA)(cm2/g)"] <- "SPA"
names(Crecor_Data)[names(Crecor_Data) == "Petal Dry Matter Content (PDMC)(g/g)"] <- "PDMC"
names(Crecor_Data)[names(Crecor_Data) == "Specific Leaf Area (SLA)(cm2/g)"] <-"SLA"
names(Crecor_Data)[names(Crecor_Data) == "Leaf Dry Matter Content (LDMC)(g/g)"] <- "LDMC"
names(Crecor_Data)[names(Crecor_Data) == "Average No. of Seeds per Flower Head (AFH)"] <- "AFH"
names(Crecor_Data)[names(Crecor_Data) == "Average Individual Seed Mass (ASM)(g)"] <- "ASM"
Crecor_Matrix <- cor(Crecor_Data, use = "complete.obs", method = "pearson")
corrplot(Crecor_Matrix, method = "color", type = "full", 
         addCoef.col = "black", # Add black text with correlation values
         tl.col = "black", tl.srt = 45, # Rotate labels and set their color
         number.cex = 0.8) 
corrplot.mixed(Crecor_Matrix, order = 'AOE')
?corrplot
# Variation Partitioning ----
## For Lotus ----
### Performing RDA for each predictor ----
lotus_rda_temp <- rda(AusRDL[,c(9,18,20:22)] ~ Temperature, data = AusRDL)
lotus_rda_co2 <- rda(AusRDL[,c(9,18,20:22)] ~ CO2, data = AusRDL)
lotus_rda_drought <- rda(AusRDL[,c(9,18,20:22)] ~ Drought, data = AusRDL)
lotus_rda_full <- rda(AusRDL[,c(9,18,20:22)] ~ Temperature + CO2 + Drought, data = AusRDL)

### Calculate adjusted R2 values ----
lotus_r2_temp <- RsquareAdj(lotus_rda_temp)$adj.r.squared
lotus_r2_co2 <- RsquareAdj(lotus_rda_co2)$adj.r.squared
lotus_r2_drought <- RsquareAdj(lotus_rda_drought)$adj.r.squared
lotus_r2_full <- RsquareAdj(lotus_rda_full)$adj.r.squared

### Calculate unique and shared variances ----
lotus_unique_temp <- lotus_r2_full - (lotus_r2_co2 + lotus_r2_drought - lotus_r2_full)
lotus_unique_co2 <- lotus_r2_full - (lotus_r2_temp + lotus_r2_drought - lotus_r2_full)
lotus_unique_drought <- lotus_r2_full - (lotus_r2_co2 + lotus_r2_temp - lotus_r2_full)
shared_temp_co2 <- lotu_r2_full - (lotus_unique_temp)

