source(here::here("Analysis", "Functions.R"))
install_and_load_all_packages()

# Loading Files into R ----
AusL <- read_csv(here("Data", "Austria Experiment - Lotus Data - Core Response Variables - 7 March 2025.csv"))
AusL <- AusL[c(1:93),]
AusC <- read_csv(here("Data", "Austria Experiment - Crepis Data - Core Response Variables - 7 March 2025.csv"))
AusC <- AusC[c(1:62),]
AusPC <- read_csv(here("Data", "Austria Experiment - Crepis and Lotus Data - RDA - 7 March 2025.csv"))
AusPC <- AusPC[c(1:84),]
AusRD <- read_csv(here("Data", "Austria Experiment - Crepis and Lotus Data - RDA - 7 March 2025.csv"))
AusRD <- AusRD[c(1:84),]
AusRC <- read_csv(here("Data", "Austria Experiment - Crepis and Lotus Data - RAC - 7 March 2025.csv"))
AusRC <- AusRC[c(1:66),]
AusRC <- AusRD %>%
  group_by(Species, Treatment) %>%
  group_split() %>%
  map_dfr(function(df) {
    n <- if (unique(df$Species) == "Crepis") 6 else if (unique(df$Species) == "Lotus") 5 else NA
    if (nrow(df) >= n) {
      df %>% slice_sample(n = n)
    } else {
      df  # return full group if too few rows
    }
  })

View(AusRC)
# Log and Square Root transform Response Variables ----
AusL <- log_sqrt_transform(AusL, c(9:23))
AusC <- log_sqrt_transform(AusC, c(9:17))
# Histogram of Response Variables ----
hist_df(AusL, c(9:53))
hist_df(AusC, c(9:35))
hist_df(AusPC, c(9:25))

# Shapiro-Wilk test for Response Variables ----
Lotus_shapiro_results <- apply(AusL[,c(9:53)], 2, shapiro.test)
Lotus_shapiro_results
Crepis_shapiro_results <- apply(AusC[,c(9:35)], 2, shapiro.test)
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

  ## In RAC Dataset ----
AusRC$`Plot No.` <- as.factor(AusRC$`Plot No.`)
AusRC$CO2 <- as.factor(AusRC$CO2)
AusRC$Temperature <- as.factor(AusRC$Temperature)
AusRC$Drought <- as.factor(AusRC$Drought)
AusRC$Species <- as.factor(AusRC$Species)

names(AusC)

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
l_log_LAmodel <- lm(`log_Leaf Area (LA)(cm2)` ~ CO2 * Temperature * Drought, data = AusL)
l_SLAmodel <- lm(`Specific Leaf Area (SLA)(cm2/g)` ~ CO2 * Temperature * Drought, data = AusL)
l_log_LDMCmodel <- lm(`log_Leaf Dry Matter Content (LDMC)(g/g)` ~ CO2 * Temperature * Drought, data = AusL)

  ## Linear Models for Response Variables of Crepis ----
c_sq_DAmodel <- lm(`Display Area (DA)(cm2)`~ CO2 * Temperature * Drought, data = AusC)
c_sq_SPAmodel <- lm(`sqrt_Specific Petal Area (SPA)(cm2/g)` ~ CO2 * Temperature * Drought, data = AusC)
c_log_PMAmodel <- lm(`log_Petal Mass Per Area (PMA)(g/cm2)` ~ CO2 * Temperature * Drought, data = AusC)
c_sq_PDMCmodel <- lm(`sqrt_Petal Dry Matter Content (PDMC)(g/g)` ~ CO2 * Temperature * Drought, data = AusC)
c_log_LAmodel <- lm(`log_Leaf Area (LA)(cm2)`~ CO2 * Temperature * Drought, data = AusC)
c_log_SLAmodel <- lm(`log_Specific Leaf Area (SLA)(cm2/g)` ~ CO2 * Temperature * Drought, data = AusC)
c_sq_LDMCmodel <- lm(`sqrt_Leaf Dry Matter Content (LDMC)(g/g)` ~ CO2 * Temperature * Drought, data = AusC)
sq_SNmodel <- lm(`sqrt_Number of Seeds (SN)` ~ CO2 * Temperature * Drought, data = AusC)
log_SMmodel <- lm(`log_Seed Mass (SM)(g)` ~ CO2 * Temperature * Drought, data = AusC)



# ANOVA Analysis ----
  ## Anova test for Response Variables in Lotus ----
  anova(l_DAmodel) # Significance: Temperature and CO2:Drought in ME; Temperature, CO2:Temperature and CO2:Drought in LM
  anova(log_SSPAmodel) # Significance: CO2: Drought marginally significant in LM
  anova(log_SW1PAmodel)
  anova(SW2PAmodel) # Significance: Drought in LM
  anova(sq_SKPAmodel)
  anova(log_SPMAmodel) # Significance: CO2: Drought marginally significant in LM
  anova(log_W1PMAmodel)
  anova(log_W2PMAmodel) # Significance: Drought and CO2: Temperature in LM (Latter marginally significant)
  anova(log_KPMAmodel)
  anova(l_sq_SPAmodel) # Significance: Drought marginally significant in LM
  anova(l_log_PMAmodel) # Significance: Drought marginally significant in LM
  anova(l_PDMCmodel)
  anova(l_log_LAmodel) # Significance: Drought in ME; Temperature and Drought in LM (Former marginally significant)
  anova(l_SLAmodel) # Significance: Drought and CO2: Temperature in ME (Latter marginally significant); CO2, Drought and CO2: Temperature in LM
  anova(l_log_LDMCmodel) # Significance: CO2, Drought and CO2:Drought in ME (Latter marginally significant); CO2, Drought and CO2: Drought in LM
  
  ## Anova test for Response Variables in Crepis ----
anova(c_sq_DAmodel) # Significance: Drought in ME; Drought in LM
anova(c_sq_SPAmodel) # Significance: Temperature and CO2: Temperature both marginally significant in ME; Drought and CO2: Temperature in LM (latter marginally significant) 
anova(c_log_PMAmodel) # Significance: Temperature marginally significant in ME; Drought in LM
anova(c_sq_PDMCmodel) # Significance: Temperature marginally significant in ME; Drought and CO2: Temperature in LM(Latter marginally significant)
anova(c_log_LAmodel) # Significance: Temperature marginally significant in LM
anova(c_log_SLAmodel)
anova(c_sq_LDMCmodel) # Significance: Drought and CO2: Drought in LM (Latter marginally significant)
anova(sq_SNmodel) # Significance: Temperature, Drought and CO2: Temperature in ME (Latter marginally significant); Same in LM
anova(log_SMmodel)


# Principal Component Analysis ----
  ## Renaming of PCA Names ---- 
names(AusPC)[names(AusPC) == "Specific Petal Area (SPA)(cm2/g)"] <- "SPA"
names(AusPC)[names(AusPC) == "Petal Dry Matter Content (PDMC)(g/g)"] <- "PDMC"
names(AusPC)[names(AusPC) == "Specific Leaf Area (SLA)(cm2/g)"] <- "SLA"
names(AusPC)[names(AusPC) == "Leaf Dry Matter Content (LDMC)(g/g)"] <- "LDMC"
names(AusPC)[names(AusPC) == "Number of Seeds (SN)"] <- "SN"
names(AusPC)[names(AusPC) == "Seed Mass (SM)(g)"] <- "SM"
names(AusPC)[names(AusPC) == "Display Area (DA)(cm2)"] <- "DA"
names(AusPC)[names(AusPC) == "Leaf Area (LA)(cm2)"] <- "LA"

pca_var_groups <- c(
  "LA" = "Leaf traits",
  "SLA" = "Leaf traits",
  "LDMC" = "Leaf traits",
  "DA" = "Floral traits",
  "SPA" = "Floral traits",
  "PDMC" = "Floral traits",
  "SN" = "Seed traits",
  "SM" = "Seed traits"
)

  ## Statistical Testing of PCAs ----
l_pc_test <- PCAtest(AusPC[AusPC$Species == "Lotus", c(9,18,20:23)], 100,100, 0.05, varcorr=FALSE, counter=FALSE, plot=TRUE)

View()
  ## Creation of PCA models  ----
View(AusPC)
colnames(AusPC)
lmv <- prcomp(AusPC[AusPC$Species == "Lotus", c(9,18,20:23)], scale = TRUE)
cmv <- prcomp(AusPC[AusPC$Species == "Crepis", c(9,18,20:23)], scale = TRUE)
cav <- prcomp(AusPC[AusPC$Species == "Crepis", c(9,18,20:25)], scale =  TRUE)
lmv_group_factor <- factor(pca_var_groups[rownames(lmv$rotation)])
cmv_group_factor <- factor(pca_var_groups[rownames(cmv$rotation)])
cav_group_factor <- factor(pca_var_groups[rownames(cav$rotation)])

    ### PCA plots ----
png("Lotus - PCA - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(lmv,
                col.var = lmv_group_factor,          # Color by custom groups
                palette = c("tomato", "seagreen"),   # One color per group
                label = "var",                       # Show variable names
                labelsize = 6,                       # Increase size of variable names
                repel = TRUE) +
  labs(colour = "Trait Group", x = "PC1 (35.2%)", y = "PC2 (28.8%)") +
  ggtitle(NULL) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 16),          # Legend label size
    legend.title = element_text(size = 18)          # Legend title size
  ) + 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

png("Crepis (Main Variables) - PCA - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(cmv,
                col.var = cmv_group_factor,
                palette = c("tomato", "seagreen"),
                label = "var",
                labelsize = 6,
                repel = TRUE) +
  labs(colour = "Trait Group", x = "PC1 (33.5%)", y = "PC2 (21.6%)") +
  ggtitle(NULL) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  ) + 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

png("Crepis (All Variables) - PCA - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(cav,
                col.var = cav_group_factor,
                palette = c("tomato", "seagreen", "saddlebrown"),
                label = "var",
                labelsize = 6,
                repel = TRUE) +
  labs(colour = "Trait Group", x = "PC1 (28%)", y = "PC2 (18.6%)") +
  ggtitle(NULL) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  ) + 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()


  ## Grouping based on Climatic Variables ----
  ## Rank Abundance Curves ----
    ### Renaming Trait Names ----
names(AusRC)[names(AusRC) == "Specific Petal Area (SPA)(cm2/g)"] <- "SPA"
names(AusRC)[names(AusRC) == "Petal Dry Matter Content (PDMC)(g/g)"] <- "PDMC"
names(AusRC)[names(AusRC) == "Specific Leaf Area (SLA)(cm2/g)"] <- "SLA"
names(AusRC)[names(AusRC) == "Leaf Dry Matter Content (LDMC)(g/g)"] <- "LDMC"
names(AusRC)[names(AusRC) == "Number of Seeds (SN)"] <- "SN"
names(AusRC)[names(AusRC) == "Seed Mass (SM)(g)"] <- "SM"
names(AusRC)[names(AusRC) == "Display Area (DA)(cm2)"] <- "DA"
names(AusRC)[names(AusRC) == "Leaf Area (LA)(cm2)"] <- "LA"
    ### Statistical Testing of PCAs ----
      #### Lotus ----
m_cols <- c(9, 18, 20:23)
a_cols <- c(9,18, 20:25)

PCAtest_lmv <- PCAtest(AusRC[AusRC$Species == "Lotus", m_cols], nperm = 1000, nboot = 1000, alpha = 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_lmv_control <- PCAtest(AusRC[AusRC$Species == "Lotus" & AusRC$Treatment == "C0T0D0", m_cols], 100, 100, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_lmv_drought <- PCAtest(AusRC[AusRC$Species == "Lotus" & AusRC$Treatment == "C0T0D1", m_cols], 100, 100, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_lmv_temperature <- PCAtest(AusRC[AusRC$Species == "Lotus" & AusRC$Treatment == "C0T2D0", m_cols], 100, 100, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_lmv_co2 <- PCAtest(AusRC[AusRC$Species == "Lotus" & AusRC$Treatment == "C2T0D0", m_cols], 100, 100, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_lmv_ct <- PCAtest(AusRC[AusRC$Species == "Lotus" & AusRC$Treatment == "C2T2D0", m_cols], 100, 100, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_lmv_ctd <- PCAtest(AusRC[AusRC$Species == "Lotus" & AusRC$Treatment == "C2T2D1", m_cols], 1000, 1000, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
View(AusRC)
      #### Crepis (Main Variables) ----
PCAtest_cmv <- PCAtest(AusRC[AusRC$Species == "Crepis", m_cols], nperm = 1000, nboot = 1000, alpha = 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_cmv_control <- PCAtest(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C0T0D0", m_cols], 100, 100, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_cmv_drought <- PCAtest(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C0T0D1", m_cols], 100, 100, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_cmv_temperature <- PCAtest(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C0T2D0", m_cols], 100, 100, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_cmv_co2 <- PCAtest(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C2T0D0", m_cols], 1000, 1000, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_cmv_ct <- PCAtest(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C2T2D0", m_cols], 1000, 1000, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_cmv_ctd <- PCAtest(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C2T2D1", m_cols], 1000, 1000, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)

      #### Crepis (All Variables) ----
PCAtest_cav <- PCAtest(AusRC[AusRC$Species == "Crepis", a_cols], nperm = 1000, nboot = 1000, alpha = 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_cav_control <- PCAtest(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C0T0D0", a_cols], 100, 100, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_cav_drought <- PCAtest(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C0T0D1", a_cols], 100, 100, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_cav_temperature <- PCAtest(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C0T2D0", a_cols], 100, 100, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_cav_co2 <- PCAtest(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C2T0D0", a_cols], 100, 100, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_cav_ct <- PCAtest(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C2T2D0", a_cols], 1000, 1000, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)
PCAtest_cav_ctd <- PCAtest(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C2T2D1", a_cols], 100, 100, 0.05, varcorr = TRUE, counter = FALSE, plot = TRUE)




    ### Creating PCA for each treatment ----
    ### Lotus ----

lmv <- prcomp(AusRC[AusRC$Species == "Lotus", c(9,18,20:23)], scale = TRUE)
lmv_control <- prcomp(AusRC[AusRC$Species == "Lotus" & AusRC$Treatment == "C0T0D0", c(9,18,20:23)], scale = TRUE)
lmv_drought <- prcomp(AusRC[AusRC$Species == "Lotus" & AusRC$Treatment == "C0T0D1", c(9,18,20:23)], scale = TRUE)
lmv_temperature <- prcomp(AusRC[AusRC$Species == "Lotus" & AusRC$Treatment == "C0T2D0", c(9,18,20:23)], scale = TRUE)
lmv_co2 <- prcomp(AusRC[AusRC$Species == "Lotus" & AusRC$Treatment == "C2T0D0", c(9,18,20:23)], scale = TRUE)
lmv_ct <- prcomp(AusRC[AusRC$Species == "Lotus" & AusRC$Treatment == "C2T2D0", c(9,18,20:23)], scale = TRUE)
lmv_ctd <- prcomp(AusRC[AusRC$Species == "Lotus" & AusRC$Treatment == "C2T2D1", c(9,18,20:23)], scale = TRUE)

### Extracting Variance Explained for each treatment
variance_lmv_control <- lmv_control$sdev^2 / sum(lmv_control$sdev^2)
variance_lmv_drought <- lmv_drought$sdev^2 / sum(lmv_drought$sdev^2)
variance_lmv_temperature <- lmv_temperature$sdev^2 / sum(lmv_temperature$sdev^2)
variance_lmv_co2 <- lmv_co2$sdev^2 / sum(lmv_co2$sdev^2)
variance_lmv_ct <- lmv_ct$sdev^2 / sum(lmv_ct$sdev^2)
variance_lmv_ctd <- lmv_ctd$sdev^2 / sum(lmv_ctd$sdev^2)

lengths <- sapply(list(variance_lmv_control, variance_lmv_drought, variance_lmv_temperature, variance_lmv_co2, variance_lmv_ct, variance_lmv_ctd), length)
print(lengths)


### Combining Variance Explained Results to a dataframe
AusL_PC <- data.frame(
  PCA_axis = rep(1:5,6),
  Variance_Explained = c(variance_lmv_control, variance_lmv_co2, variance_lmv_temperature, variance_lmv_drought, variance_lmv_ct, variance_lmv_ctd),
  Treatment = rep(c("C0T0D0", "C1T0D0", "C0T1D0", "C0T0D1", "C1T1D0", "C1T1D1"), each = 5)
)

AusL_PC$Treatment <- factor(AusL_PC$Treatment, levels = c("C0T0D0", "C1T0D0", "C0T1D0", "C0T0D1", "C1T1D0", "C1T1D1"))
png("Lotus - Proportion of Variance Explained - 27 March 2025.png", width = 2000, height = 1600, res = 200)
ggplot(AusL_PC, aes(x = PCA_axis, y = Variance_Explained, color = Treatment, #size = Treatment
                    )) +
  geom_line() +
  geom_point() +
  # scale_size_manual(values = c(
  #  "C0T0D0" = 1.5,
  #  "C1T0D0" = 0.5,  # Bold this one (larger size)
  #  "C0T1D0" = 0.5,
  #  "C0T0D1" = 0.5,
  #  "C1T1D0" = 1.5,  # Another bold one
  #  "C1T1D1" = 0.5
  # ))+ 
  scale_x_continuous(breaks = unique(AusL_PC$PCA_axis)) +
  scale_color_manual(
    values = c(
      "C0T0D0" = "#1976d2",      # Control
      "C1T0D0" = "#7e57c2",       # Elevated CO2
      "C0T1D0" = "#ff9800",        # Elevated temperature
      "C0T0D1" = "#f7756d",      # Drought stress
      "C1T1D0" = "#fbc02d",     # CO2 & temperature stress
      "C1T1D1" = "#00bfc4"      # CO2, temperature, and drought stress
    )
  ) +
  guides(size = "none") +  # Hide size from legend
  labs(
    x = "PCA Axis",
    y = "Proportion of Variance Explained",
    color = "Treatment"
  ) +
  theme_bw()+
  theme(
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 24),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 18),
  )
dev.off()

    ### Crepis (Main Variables) ----
cmv <- prcomp(AusRC[AusRC$Species == "Crepis", c(9,18,20:23)], scale = TRUE)
cmv_control <- prcomp(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C0T0D0", c(9,18,20:23)], scale = TRUE)
cmv_drought <- prcomp(AusRC[AusRC$ Species == "Crepis" & AusRC$Treatment == "C0T0D1", c(9,18,20:23)], scale = TRUE)
cmv_temperature <- prcomp(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C0T2D0", c(9,18,20:23)], scale = TRUE)
cmv_co2 <- prcomp(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C2T0D0", c(9,18,20:23)], scale = TRUE)
cmv_ct <- prcomp(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C2T2D0", c(9,18,20:23)], scale = TRUE)
cmv_ctd <- prcomp(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C2T2D1", c(9,18,20:23)], scale = TRUE)

### Extracting variance explained for each treatment 
variance_cmv_control <- cmv_control$sdev^2 / sum(cmv_control$sdev^2)
variance_cmv_drought <- cmv_drought$sdev^2 / sum(cmv_drought$sdev^2)
variance_cmv_temperature <- cmv_temperature$sdev^2 / sum(cmv_temperature$sdev^2)
variance_cmv_co2 <- cmv_co2$sdev^2 / sum(cmv_co2$sdev^2)
variance_cmv_ct <- cmv_ct$sdev^2 / sum(cmv_ct$sdev^2)
variance_cmv_ctd <- cmv_ctd$sdev^2 / sum(cmv_ctd$sdev^2)

lengths <- sapply(list(variance_cmv_control, variance_cmv_co2, variance_cmv_temperature, variance_cmv_drought, variance_cmv_ct, variance_cmv_ctd), length)
print(lengths)

### Combining variance explained results to a dataframe

AusCM_PC <- data.frame(
  PCA_axis = rep(1:6, 6),
  Variance_Explained = c(variance_cmv_control, variance_cmv_co2, variance_cmv_temperature, variance_cmv_drought, variance_cmv_ct, variance_cmv_ctd),
  Treatment = rep(c("C0T0D0", "C1T0D0", "C0T1D0", "C0T0D1", "C1T1D0", "C1T1D1"), each = 6)
)

AusCM_PC$Treatment <- factor(AusCM_PC$Treatment, levels = c("C0T0D0", "C1T0D0", "C0T1D0", "C0T0D1", "C1T1D0", "C1T1D1"))
png("Crepis (Main Variables) - Proportion of Variance Explained - 27 March 2025.png", width = 2000, height = 1600, res = 200)
ggplot(AusCM_PC, aes(x = PCA_axis, y = Variance_Explained, color = Treatment, # size = Treatment
                     )) +
  geom_line() +
  geom_point() +
  # scale_size_manual(values = c(
  #  "C0T0D0" = 1.5,
  #  "C1T0D0" = 0.5,  # Bold this one (larger size)
  #  "C0T1D0" = 0.5,
  #  "C0T0D1" = 0.5,
  #  "C1T1D0" = 1.5,  # Another bold one
  #  "C1T1D1" = 0.5
  # ))+ 
  scale_x_continuous(breaks = unique(AusL_PC$PCA_axis)) +
  scale_color_manual(
    values = c(
      "C0T0D0" = "#1976d2",      # Control
      "C1T0D0" = "#7e57c2",       # Elevated CO2
      "C0T1D0" = "#ff9800",        # Elevated temperature
      "C0T0D1" = "#f7756d",      # Drought stress
      "C1T1D0" = "#fbc02d",     # CO2 & temperature stress
      "C1T1D1" = "#00bfc4"      # CO2, temperature, and drought stress
    )
  ) +
  guides(size = "none") +  # Hide size from legend
  labs(
    x = "PCA Axis",
    y = "Proportion of Variance Explained",
    color = "Treatment"
  ) +
  theme_bw()+
  theme(
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 24),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 18),
  )
dev.off()

    ### Crepis (All Variables) ----
cav <- prcomp(AusRC[AusRC$Species == "Crepis", c(9,18,20:25)], scale = TRUE)
cav_control <- prcomp(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C0T0D0", c(9,18,20:25)], scale = TRUE)
cav_drought <- prcomp(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C0T0D1", c(9,18,20:25)], scale = TRUE)
cav_temperature <- prcomp(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C0T2D0", c(9,18,20:25)], scale = TRUE)
cav_co2 <- prcomp(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C2T0D0", c(9,18,20:25)], scale = TRUE)
cav_ct <- prcomp(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C2T2D0", c(9,18,20:25)], scale = TRUE)
cav_ctd <- prcomp(AusRC[AusRC$Species == "Crepis" & AusRC$Treatment == "C2T2D1", c(9,18,20:25)], scale = TRUE)

### Extracting variance explained for each treatment 
variance_cav_control <- cav_control$sdev^2 / sum(cav_control$sdev^2)
variance_cav_drought <- cav_drought$sdev^2 / sum(cav_drought$sdev^2)
variance_cav_temperature <- cav_temperature$sdev^2 / sum(cav_temperature$sdev^2)
variance_cav_co2 <- cav_co2$sdev^2 / sum(cav_co2$sdev^2)
variance_cav_ct <- cav_ct$sdev^2 / sum(cav_ct$sdev^2)
variance_cav_ctd <- cav_ctd$sdev^2 / sum(cav_ctd$sdev^2)

### Checking the lengths of variance vectors
lengths <- sapply(list(variance_cav_control, variance_cav_co2, variance_cav_temperature, variance_cav_drought, variance_cav_ct, variance_cav_ctd), length)
print(lengths)

### Combining variance explained results to a dataframe

AusCA_PC <- data.frame(
  PCA_axis = rep(1:6, 6),
  Variance_Explained = c(variance_cav_control, variance_cav_co2, variance_cav_temperature, variance_cav_drought, variance_cav_ct, variance_cav_ctd),
  Treatment = rep(c("C0T0D0", "C1T0D0", "C0T1D0", "C0T0D1", "C1T1D0", "C1T1D1"), each = 6)
)

AusCA_PC$Treatment <- factor(AusCA_PC$Treatment, levels = c("C0T0D0", "C1T0D0", "C0T1D0", "C0T0D1", "C1T1D0", "C1T1D1"))
png("Crepis (All Variables) - Proportion of Variance Explained - 27 March 2025.png", width = 2000, height = 1600, res = 200)
ggplot(AusCA_PC, aes(x = PCA_axis, y = Variance_Explained, color = Treatment)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:8, labels = 1:8) +
  scale_color_manual(
    values = c(
      "C0T0D0" = "#1976d2",      # Control
      "C1T0D0" = "#7e57c2",       # Elevated CO2
      "C0T1D0" = "#ff9800",        # Elevated temperature
      "C0T0D1" = "#f7756d",      # Drought stress
      "C1T1D0" = "#fbc02d",     # CO2 & temperature stress
      "C1T1D1" = "#00bfc4"      # CO2, temperature, and drought stress
    )
  ) +
  labs(
    x = "PCA Axis",
    y = "Proportion of Variance Explained",
    color = "Treatment"
  ) +
  theme_bw()+
  theme(
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 24),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 18),
  )
dev.off()

  ## Extracting PCA Results----
    ### For Lotus ----

lotus_pca_trait_loadings <- lmv$rotation
lotus_pca_eigenvalues <- lmv$sdev^2
lotus_pca_variance <- lotus_pca_eigenvalues/sum(lotus_pca_eigenvalues)
lotus_pca_cum_variance <- cumsum(lotus_pca_variance)

trait_loadings_df <- as.data.frame(lotus_pca_trait_loadings)
variance_percent <- lotus_pca_variance * 100
cumulative_variance_percent <- lotus_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = lotus_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "lotus_pca_combined.csv", row.names = TRUE)

### For Treatments
#### Control
lotus_control_pca_trait_loadings <- lmv_control$rotation
lotus_control_pca_eigenvalues <- lmv_control$sdev^2
lotus_control_pca_variance <- lotus_control_pca_eigenvalues / sum(lotus_control_pca_eigenvalues)
lotus_control_pca_cum_variance <- cumsum(lotus_control_pca_variance)

trait_loadings_df <- as.data.frame(lotus_control_pca_trait_loadings)
variance_percent <- lotus_control_pca_variance * 100
cumulative_variance_percent <- lotus_control_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = lotus_control_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "lotus_control_pca_combined.csv", row.names = TRUE)

#### CO2
lotus_co2_pca_trait_loadings <- lmv_co2$rotation
lotus_co2_pca_eigenvalues <- lmv_co2$sdev^2
lotus_co2_pca_variance <- lotus_co2_pca_eigenvalues / sum(lotus_co2_pca_eigenvalues)
lotus_co2_pca_cum_variance <- cumsum(lotus_co2_pca_variance)

trait_loadings_df <- as.data.frame(lotus_co2_pca_trait_loadings)
variance_percent <- lotus_co2_pca_variance * 100
cumulative_variance_percent <- lotus_co2_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = lotus_co2_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "lotus_co2_pca_combined.csv", row.names = TRUE)

#### Temperature
lotus_temperature_pca_trait_loadings <- lmv_temperature$rotation
lotus_temperature_pca_eigenvalues <- lmv_temperature$sdev^2
lotus_temperature_pca_variance <- lotus_temperature_pca_eigenvalues / sum(lotus_temperature_pca_eigenvalues)
lotus_temperature_pca_cum_variance <- cumsum(lotus_temperature_pca_variance)

trait_loadings_df <- as.data.frame(lotus_temperature_pca_trait_loadings)
variance_percent <- lotus_temperature_pca_variance * 100
cumulative_variance_percent <- lotus_temperature_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = lotus_temperature_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "lotus_temperature_pca_combined.csv", row.names = TRUE)

#### Drought
lotus_drought_pca_trait_loadings <- lmv_drought$rotation
lotus_drought_pca_eigenvalues <- lmv_drought$sdev^2
lotus_drought_pca_variance <- lotus_drought_pca_eigenvalues / sum(lotus_drought_pca_eigenvalues)
lotus_drought_pca_cum_variance <- cumsum(lotus_drought_pca_variance)

trait_loadings_df <- as.data.frame(lotus_drought_pca_trait_loadings)
variance_percent <- lotus_drought_pca_variance * 100
cumulative_variance_percent <- lotus_drought_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = lotus_drought_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "lotus_drought_pca_combined.csv", row.names = TRUE)

#### CO2 and Temperature
lotus_ct_pca_trait_loadings <- lmv_ct$rotation
lotus_ct_pca_eigenvalues <- lmv_ct$sdev^2
lotus_ct_pca_variance <- lotus_ct_pca_eigenvalues / sum(lotus_ct_pca_eigenvalues)
lotus_ct_pca_cum_variance <- cumsum(lotus_ct_pca_variance)

trait_loadings_df <- as.data.frame(lotus_ct_pca_trait_loadings)
variance_percent <- lotus_ct_pca_variance * 100
cumulative_variance_percent <- lotus_ct_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = lotus_ct_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "lotus_ct_pca_combined.csv", row.names = TRUE)

#### Combined Interaction
lotus_ctd_pca_trait_loadings <- lmv_ctd$rotation
lotus_ctd_pca_eigenvalues <- lmv_ctd$sdev^2
lotus_ctd_pca_variance <- lotus_ctd_pca_eigenvalues / sum(lotus_ctd_pca_eigenvalues)
lotus_ctd_pca_cum_variance <- cumsum(lotus_ctd_pca_variance)

trait_loadings_df <- as.data.frame(lotus_ctd_pca_trait_loadings)
variance_percent <- lotus_ctd_pca_variance * 100
cumulative_variance_percent <- lotus_ctd_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = lotus_ctd_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "lotus_ctd_pca_combined.csv", row.names = TRUE)


    ### For Crepis (Main Variables) ----

crepis_m_pca_trait_loadings <- cmv$rotation
crepis_m_pca_eigenvalues <- cmv$sdev^2
crepis_m_pca_variance <- crepis_m_pca_eigenvalues / sum(crepis_m_pca_eigenvalues)
crepis_m_pca_cum_variance <- cumsum(crepis_m_pca_variance)

trait_loadings_df <- as.data.frame(crepis_m_pca_trait_loadings)
variance_percent <- crepis_m_pca_variance * 100
cumulative_variance_percent <- crepis_m_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = crepis_m_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "crepis_m_pca_combined.csv", row.names = TRUE)

### For Treatments
#### Control
crepis_m_control_pca_trait_loadings <- cmv_control$rotation
crepis_m_control_pca_eigenvalues <- cmv_control$sdev^2
crepis_m_control_pca_variance <- crepis_m_control_pca_eigenvalues / sum(crepis_m_control_pca_eigenvalues)
crepis_m_control_pca_cum_variance <- cumsum(crepis_m_control_pca_variance)

trait_loadings_df <- as.data.frame(crepis_m_control_pca_trait_loadings)
variance_percent <- crepis_m_control_pca_variance * 100
cumulative_variance_percent <- crepis_m_control_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = crepis_m_control_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "crepis_m_control_pca_combined.csv", row.names = TRUE)

#### CO2
crepis_m_co2_pca_trait_loadings <- cmv_co2$rotation
crepis_m_co2_pca_eigenvalues <- cmv_co2$sdev^2
crepis_m_co2_pca_variance <- crepis_m_co2_pca_eigenvalues / sum(crepis_m_co2_pca_eigenvalues)
crepis_m_co2_pca_cum_variance <- cumsum(crepis_m_co2_pca_variance)

trait_loadings_df <- as.data.frame(crepis_m_co2_pca_trait_loadings)
variance_percent <- crepis_m_co2_pca_variance * 100
cumulative_variance_percent <- crepis_m_co2_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = crepis_m_co2_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "crepis_m_co2_pca_combined.csv", row.names = TRUE)

#### Temperature
crepis_m_temperature_pca_trait_loadings <- cmv_temperature$rotation
crepis_m_temperature_pca_eigenvalues <- cmv_temperature$sdev^2
crepis_m_temperature_pca_variance <- crepis_m_temperature_pca_eigenvalues / sum(crepis_m_temperature_pca_eigenvalues)
crepis_m_temperature_pca_cum_variance <- cumsum(crepis_m_temperature_pca_variance)

trait_loadings_df <- as.data.frame(crepis_m_temperature_pca_trait_loadings)
variance_percent <- crepis_m_temperature_pca_variance * 100
cumulative_variance_percent <- crepis_m_temperature_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = crepis_m_temperature_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "crepis_m_temperature_pca_combined.csv", row.names = TRUE)

#### Drought
crepis_m_drought_pca_trait_loadings <- cmv_drought$rotation
crepis_m_drought_pca_eigenvalues <- cmv_drought$sdev^2
crepis_m_drought_pca_variance <- crepis_m_drought_pca_eigenvalues / sum(crepis_m_drought_pca_eigenvalues)
crepis_m_drought_pca_cum_variance <- cumsum(crepis_m_drought_pca_variance)

trait_loadings_df <- as.data.frame(crepis_m_drought_pca_trait_loadings)
variance_percent <- crepis_m_drought_pca_variance * 100
cumulative_variance_percent <- crepis_m_drought_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = crepis_m_drought_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "crepis_m_drought_pca_combined.csv", row.names = TRUE)

#### CO2 and Temperature
crepis_m_ct_pca_trait_loadings <- cmv_ct$rotation
crepis_m_ct_pca_eigenvalues <- cmv_ct$sdev^2
crepis_m_ct_pca_variance <- crepis_m_ct_pca_eigenvalues / sum(crepis_m_ct_pca_eigenvalues)
crepis_m_ct_pca_cum_variance <- cumsum(crepis_m_ct_pca_variance)

trait_loadings_df <- as.data.frame(crepis_m_ct_pca_trait_loadings)
variance_percent <- crepis_m_ct_pca_variance * 100
cumulative_variance_percent <- crepis_m_ct_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = crepis_m_ct_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "crepis_m_ct_pca_combined.csv", row.names = TRUE)

#### Combined Interaction
crepis_m_ctd_pca_trait_loadings <- cmv_ctd$rotation
crepis_m_ctd_pca_eigenvalues <- cmv_ctd$sdev^2
crepis_m_ctd_pca_variance <- crepis_m_ctd_pca_eigenvalues / sum(crepis_m_ctd_pca_eigenvalues)
crepis_m_ctd_pca_cum_variance <- cumsum(crepis_m_ctd_pca_variance)

trait_loadings_df <- as.data.frame(crepis_m_ctd_pca_trait_loadings)
variance_percent <- crepis_m_ctd_pca_variance * 100
cumulative_variance_percent <- crepis_m_ctd_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = crepis_m_ctd_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "crepis_m_ctd_pca_combined.csv", row.names = TRUE)

    ### For Crepis (All Variables) ----

crepis_a_pca_trait_loadings <- cav$rotation
crepis_a_pca_eigenvalues <- cav$sdev^2
crepis_a_pca_variance <- crepis_a_pca_eigenvalues / sum(crepis_a_pca_eigenvalues)
crepis_a_pca_cum_variance <- cumsum(crepis_a_pca_variance)

trait_loadings_df <- as.data.frame(crepis_a_pca_trait_loadings)
variance_percent <- crepis_a_pca_variance * 100
cumulative_variance_percent <- crepis_a_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = crepis_a_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "crepis_a_pca_combined.csv", row.names = TRUE)

### For Treatments
#### Control
crepis_a_control_pca_trait_loadings <- cav_control$rotation
crepis_a_control_pca_eigenvalues <- cav_control$sdev^2
crepis_a_control_pca_variance <- crepis_a_control_pca_eigenvalues / sum(crepis_a_control_pca_eigenvalues)
crepis_a_control_pca_cum_variance <- cumsum(crepis_a_control_pca_variance)

trait_loadings_df <- as.data.frame(crepis_a_control_pca_trait_loadings)
variance_percent <- crepis_a_control_pca_variance * 100
cumulative_variance_percent <- crepis_a_control_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = crepis_a_control_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "crepis_a_control_pca_combined.csv", row.names = TRUE)

#### CO2
crepis_a_co2_pca_trait_loadings <- cav_co2$rotation
crepis_a_co2_pca_eigenvalues <- cav_co2$sdev^2
crepis_a_co2_pca_variance <- crepis_a_co2_pca_eigenvalues / sum(crepis_a_co2_pca_eigenvalues)
crepis_a_co2_pca_cum_variance <- cumsum(crepis_a_co2_pca_variance)

trait_loadings_df <- as.data.frame(crepis_a_co2_pca_trait_loadings)
variance_percent <- crepis_a_co2_pca_variance * 100
cumulative_variance_percent <- crepis_a_co2_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = crepis_a_co2_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "crepis_a_co2_pca_combined.csv", row.names = TRUE)

#### Temperature
crepis_a_temperature_pca_trait_loadings <- cav_temperature$rotation
crepis_a_temperature_pca_eigenvalues <- cav_temperature$sdev^2
crepis_a_temperature_pca_variance <- crepis_a_temperature_pca_eigenvalues / sum(crepis_a_temperature_pca_eigenvalues)
crepis_a_temperature_pca_cum_variance <- cumsum(crepis_a_temperature_pca_variance)

trait_loadings_df <- as.data.frame(crepis_a_temperature_pca_trait_loadings)
variance_percent <- crepis_a_temperature_pca_variance * 100
cumulative_variance_percent <- crepis_a_temperature_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = crepis_a_temperature_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "crepis_a_temperature_pca_combined.csv", row.names = TRUE)

#### Drought
crepis_a_drought_pca_trait_loadings <- cav_drought$rotation
crepis_a_drought_pca_eigenvalues <- cav_drought$sdev^2
crepis_a_drought_pca_variance <- crepis_a_drought_pca_eigenvalues / sum(crepis_a_drought_pca_eigenvalues)
crepis_a_drought_pca_cum_variance <- cumsum(crepis_a_drought_pca_variance)

trait_loadings_df <- as.data.frame(crepis_a_drought_pca_trait_loadings)
variance_percent <- crepis_a_drought_pca_variance * 100
cumulative_variance_percent <- crepis_a_drought_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = crepis_a_drought_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "crepis_a_drought_pca_combined.csv", row.names = TRUE)

#### CO2 and Temperature
crepis_a_ct_pca_trait_loadings <- cav_ct$rotation
crepis_a_ct_pca_eigenvalues <- cav_ct$sdev^2
crepis_a_ct_pca_variance <- crepis_a_ct_pca_eigenvalues / sum(crepis_a_ct_pca_eigenvalues)
crepis_a_ct_pca_cum_variance <- cumsum(crepis_a_ct_pca_variance)

trait_loadings_df <- as.data.frame(crepis_a_ct_pca_trait_loadings)
variance_percent <- crepis_a_ct_pca_variance * 100
cumulative_variance_percent <- crepis_a_ct_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = crepis_a_ct_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "crepis_a_ct_pca_combined.csv", row.names = TRUE)

#### Combined Interaction
crepis_a_ctd_pca_trait_loadings <- cav_ctd$rotation
crepis_a_ctd_pca_eigenvalues <- cav_ctd$sdev^2
crepis_a_ctd_pca_variance <- crepis_a_ctd_pca_eigenvalues / sum(crepis_a_ctd_pca_eigenvalues)
crepis_a_ctd_pca_cum_variance <- cumsum(crepis_a_ctd_pca_variance)

trait_loadings_df <- as.data.frame(crepis_a_ctd_pca_trait_loadings)
variance_percent <- crepis_a_ctd_pca_variance * 100
cumulative_variance_percent <- crepis_a_ctd_pca_cum_variance * 100
combined_df <- rbind(
  trait_loadings_df,
  Eigenvalues = crepis_a_ctd_pca_eigenvalues,
  Variance_Percent = variance_percent,
  Cumulative_Variance_Percent = cumulative_variance_percent
)
write.csv(combined_df, "crepis_a_ctd_pca_combined.csv", row.names = TRUE)



  ## PCA Plots for Each Treatment ----
names(AusRC)[names(AusRC) == "Specific Petal Area (SPA)(cm2/g)"] <- "SPA"
names(AusRC)[names(AusRC) == "Petal Dry Matter Content (PDMC)(g/g)"] <- "PDMC"
names(AusRC)[names(AusRC) == "Specific Leaf Area (SLA)(cm2/g)"] <- "SLA"
names(AusRC)[names(AusRC) == "Leaf Dry Matter Content (LDMC)(g/g)"] <- "LDMC"
names(AusRC)[names(AusRC) == "Number of Seeds (SN)"] <- "SN"
names(AusRC)[names(AusRC) == "Seed Mass (SM)(g)"] <- "SM"
names(AusRC)[names(AusRC) == "Display Area (DA)(cm2)"] <- "DA"
names(AusRC)[names(AusRC) == "Leaf Area (LA)(cm2)"] <- "LA"

    ### For Lotus ----
# C0T0D0
png("Lotus - C0T0D0 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(lmv_control,
                col.var = lmv_group_factor,
                palette = c("tomato", "seagreen"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C0T0D0") +
  labs(colour = "Trait Group", x = "PC1 (45%)", y = "PC2 (35.6%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  ) + 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# C1T0D0
png("Lotus - C1T0D0 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(lmv_co2,
                col.var = lmv_group_factor,
                palette = c("tomato", "seagreen"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C1T0D0") +
  labs(colour = "Trait Group", x = "PC1 (59.6%)", y = "PC2 (24.6%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  ) + 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# C0T1D0
png("Lotus - C0T1D0 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(lmv_temperature,
                col.var = lmv_group_factor,
                palette = c("tomato", "seagreen"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C0T1D0") +
  labs(colour = "Trait Group", x = "PC1 (36.5%)", y = "PC2 (28.2%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# C0T0D1
png("Lotus - C0T0D1 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(lmv_drought,
                col.var = lmv_group_factor,
                palette = c("tomato", "seagreen"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C0T0D1") +
  labs(colour = "Trait Group", x = "PC1 (46.5%)", y = "PC2 (33.8%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# C1T1D0
png("Lotus - C1T1D0 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(lmv_ct,
                col.var = lmv_group_factor,
                palette = c("tomato", "seagreen"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C1T1D0") +
  labs(colour = "Trait Group", x = "PC1 (35.7%)", y = "PC2 (33%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# C1T1D1
png("Lotus - C1T1D1 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(lmv_ctd,
                col.var = lmv_group_factor,
                palette = c("tomato", "seagreen"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C1T1D1") +
  labs(colour = "Trait Group", x = "PC1 (65.3%)", y = "PC2 (29.8%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

    ### For Crepis (Main Variables) ----
# C0T0D0
png("Crepis (Main Variables) - C0T0D0 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(cmv_control,
                col.var = cmv_group_factor,
                palette = c("tomato", "seagreen"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C0T0D0") +
  labs(colour = "Trait Group", x = "PC1 (38.3%)", y = "PC2 (33%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# C1T0D0
png("Crepis (Main Variables) - C1T0D0 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(cmv_co2,
                col.var = cmv_group_factor,
                palette = c("tomato", "seagreen"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C1T0D0") +
  labs(colour = "Trait Group", x = "PC1 (51.2%)", y = "PC2 (41.5%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# C0T1D0
png("Crepis (Main Variables) - C0T1D0 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(cmv_temperature,
                col.var = cmv_group_factor,
                palette = c("tomato", "seagreen"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C0T1D0") +
  labs(colour = "Trait Group", x = "PC1 (47.6%)", y = "PC2 (26.7%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# C0T0D1
png("Crepis (Main Variables) - C0T0D1 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(cmv_drought,
                col.var = cmv_group_factor,
                palette = c("tomato", "seagreen"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C0T0D1") +
  labs(colour = "Trait Group", x = "PC1 (57.4%)", y = "PC2 (21.4%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# C1T1D0
png("Crepis (Main Variables) - C1T1D0 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(cmv_ct,
                col.var = cmv_group_factor,
                palette = c("tomato", "seagreen"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C1T1D0") +
  labs(colour = "Trait Group", x = "PC1 (74.5%)", y = "PC2 (12.4%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# C1T1D1
png("Crepis (Main Variables) - C1T1D1 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(cmv_ctd,
                col.var = cmv_group_factor,
                palette = c("tomato", "seagreen"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C1T1D1") +
  labs(colour = "Trait Group", x = "PC1 (49.2%)", y = "PC2 (29.6%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

    ### For Crepis (All Variables) ----
# C0T0D0
png("Crepis (All Variables) - C0T0D0 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(cav_control,
                col.var = cav_group_factor,
                palette = c("tomato", "seagreen", "saddlebrown"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C0T0D0") +
  labs(colour = "Trait Group", x = "PC1 (44.9%)", y = "PC2 (25.2%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# C1T0D0
png("Crepis (All Variables) - C1T0D0 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(cav_co2,
                col.var = cav_group_factor,
                palette = c("tomato", "seagreen", "saddlebrown"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C1T0D0") +
  labs(colour = "Trait Group", x = "PC1 (52.1%)", y = "PC2 (36.4%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# C0T1D0
png("Crepis (All Variables) - C0T1D0 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(cav_temperature,
                col.var = cav_group_factor,
                palette = c("tomato", "seagreen", "saddlebrown"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C0T1D0") +
  labs(colour = "Trait Group", x = "PC1 (45.2%)", y = "PC2 (24.1%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# C0T0D1
png("Crepis (All Variables) - C0T0D1 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(cav_drought,
                col.var = cav_group_factor,
                palette = c("tomato", "seagreen", "saddlebrown"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C0T0D1") +
  labs(colour = "Trait Group", x = "PC1 (48.2%)", y = "PC2 (23.8%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# C1T1D0
png("Crepis (All Variables) - C1T1D0 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(cav_ct,
                col.var = cav_group_factor,
                palette = c("tomato", "seagreen", "saddlebrown"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C1T1D0") +
  labs(colour = "Trait Group", x = "PC1 (59.7%)", y = "PC2 (18.6%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# C1T1D1
png("Crepis (All Variables) - C1T1D1 - 27 August 2025.png", width = 1600, height = 1600, res = 200)
fviz_pca_biplot(cav_ctd,
                col.var = cav_group_factor,
                palette = c("tomato", "seagreen", "saddlebrown"),
                label = "var",
                repel = TRUE,
                labelsize = 6) +
  ggtitle("C1T1D1") +
  labs(colour = "Trait Group", x = "PC1 (43%)", y = "PC2 (23.6%)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  guides(color = guide_legend(override.aes = list(label = "")))
dev.off()

# Redundancy Analysis ----
  ### Sub-setting Data frames for Lotus and Crepis ----
lotus_rows <- AusRD[AusRD$Species == "Lotus", ]
names(lotus_rows)
lotus_rows <- lotus_rows[-c(40:72), ]
AusRDL <- lotus_rows[, -c(24,25)]
View(crepis_rows)
names(AusRDL)
crepis_rows <- AusRD[AusRD$Species == "Crepis",]
crepis_rows <- crepis_rows[-c(46:78),]
AusRDC <- crepis_rows[,-c(10:17)]
  ## Redundancy Analysis for Lotus ----
names(AusRDL)[names(AusRDL) == "Specific Petal Area (SPA)(cm2/g)"] <- "SPA"
names(AusRDL)[names(AusRDL) == "Petal Dry Matter Content (PDMC)(g/g)"] <- "PDMC"
names(AusRDL)[names(AusRDL) == "Specific Leaf Area (SLA)(cm2/g)"] <- "SLA"
names(AusRDL)[names(AusRDL) == "Leaf Dry Matter Content (LDMC)(g/g)"] <- "LDMC"
names(AusRDL)[names(AusRDL) == "Display Area (DA)(cm2)"] <- "DA"
names(AusRDL)[names(AusRDL) == "Leaf Area (LA)(cm2)"] <- "LA"


rda.l <- rda(AusRDL[,c(9,18,20:23)] ~ CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusRDL, scale = TRUE)
Lotus_RDA <- anova(rda.l, permutations = 9999, by = "terms")
Lotus_RDA <- anova(rda.l, permutations = 9999, by = "axis")
scores_RDA <- Lotus_RDA$`Pr(>F)`
print(Lotus_RDA$`Pr(>F)`)
Lotus_RDA <- as.data.frame(Lotus_RDA)
View(Lotus_RDA) # Temperature, Drought and CO2:Drought are significant (Latter marginally significant)
anova(rda.l)
trait_loadings <- scores(rda.l, display = "all")
trait_loadings

    ### Extracting Values for Lotus RDA ----
summary_text <- capture.output(summary(rda.l))
writeLines(summary_text, "lotus_rda_summary.txt")
l_rda_eigenvalues <- eigenvals(rda.l, constrained = TRUE)
l_rda_proportion_explained <- l_rda_eigenvalues/sum(l_rda_eigenvalues) 
l_rda_proportion_explained

    ### Preparing Model for Plotting ----
rda.ln <- rda(AusRDL[,c(9,18,20:23)] ~ Temperature + Drought + CO2:Drought + Condition(CO2), data = AusRDL, scale = TRUE) # Without C:T Interaction
l_bp_scores <- scores(rda.ln, display = "bp") #Extracting Names of Predictors
rownames(l_bp_scores)
rownames(l_bp_scores)<- c("T", "D", "CTxD")

    ### Plot for Lotus RDA ----
png("Lotus - RDA - 5 June 2025.png", width = 1600, height = 1600, res = 200)
plot(rda.ln, type = "n",xlim = c(-1,1), ylim = c(-1,1), xlab = "RDA1 (19.03%)", ylab = "RDA2 (4.7%)")
### Adding Individual Observations
points(scores(rda.ln, display = "sites")[,1], scores(rda.ln, display = "sites")[,2], pch = 21, bg = "white", col = "black", cex = 1.2)
### Adding Arrows and Text for Response Variables
arrows(0, 0, scores(rda.ln, display = "species")[,1], scores(rda.ln, display = "species")[,2], col = 'blue', length = 0.1)
text(scores(rda.ln, display = "species")[,1], scores(rda.ln, display = "species")[,2], labels = rownames(scores(rda.ln, display = "species")), col = 'blue', pos = 3, cex = 2)
### Adding Arrows and Text for Predictor Variables
arrows(0, 0, scores(rda.ln, display = "bp")[,1], scores(rda.ln, display = "bp")[,2], col = 'red', length = 0.1)
text(scores(rda.ln, display = "bp")[,1], scores(rda.ln, display = "bp")[,2], labels = rownames(l_bp_scores), col = 'red', pos = 3, cex = 2)
dev.off()

      #### Having Colored Response Variables ----
# Add predictor arrows
species_scores <- as.data.frame(scores(rda.ln, display = "species"))
species_scores$Trait <- rownames(species_scores)
species_scores$Group <- lmv_group_factor  # Your factor for trait groups

site_scores <- as.data.frame(scores(rda.ln, display = "sites"))
site_scores$Sample <- rownames(site_scores)

bp_scores <- as.data.frame(scores(rda.ln, display = "bp"))
bp_scores$Variable <- c("T", "D", "CTxD")

# Build the ggplot
svg("Lotus - RDA - 27 August 2025.svg", width = 10, height = 8)
ggplot() +
  # Sites
  geom_point(data = site_scores, aes(x = RDA1, y = RDA2), 
             shape = 21, fill = "white", color = "black", size = 3) +
  
  # Species (traits) arrows
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2, color = Group),
               arrow = arrow(length = unit(0.4, "cm")), size = 1) +
  
  # Species labels
  geom_text(data = species_scores, 
            aes(x = RDA1, y = RDA2, label = Trait, color = Group), 
            size = 7, vjust = -0.8) +
  
  # Predictor arrows
  geom_segment(data = bp_scores, 
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(length = unit(0.4, "cm")), 
               color = "blue", size = 1) +
  
  # Predictor labels
  geom_text(data = bp_scores, 
            aes(x = RDA1, y = RDA2, label = Variable), 
            color = "blue", size = 7, vjust = -0.8) +
  
  scale_color_manual(values = c("tomato", "seagreen")) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1)) +   
  theme_bw() +
  labs(x = "RDA1 (19.03%)", y = "RDA2 (4.7%)", color = "Trait Group") +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )+ 
  
  # Remove "a" below arrow in legend
  guides(color = guide_legend(override.aes = list(label = "")))

dev.off()
  ## Redundancy Analysis for Crepis ----
names(AusRDC)[names(AusRDC) == "Specific Petal Area (SPA)(cm2/g)"] <- "SPA"
names(AusRDC)[names(AusRDC) == "Petal Dry Matter Content (PDMC)(g/g)"] <- "PDMC"
names(AusRDC)[names(AusRDC) == "Specific Leaf Area (SLA)(cm2/g)"] <- "SLA"
names(AusRDC)[names(AusRDC) == "Leaf Dry Matter Content (LDMC)(g/g)"] <- "LDMC"
names(AusRDC)[names(AusRDC) == "Display Area (DA)(cm2)"] <- "DA"
names(AusRDC)[names(AusRDC) == "Leaf Area (LA)(cm2)"] <- "LA"
names(AusRDC)[names(AusRDC) == "Number of Seeds (SN)"] <- "SN"
names(AusRDC)[names(AusRDC) == "Seed Mass (SM)(g)"] <- "SM"

rda.c <- rda(AusRDC[,c(9:10,12:17)] ~ CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusRDC, scale = TRUE)
summary(rda.c)
Crepis_RDA <- anova(rda.c, permutations = 9999, by = "terms")
Crepis_RDA <- anova(rda.c, permutations = 9999, by = "axis")
Crepis_RDA # Drought and CO2: Temperature are significant
Crepis_RDA <- as.data.frame(Crepis_RDA)

    ### Extracting Values for Crepis RDA ----
summary_text <- capture.output(summary(rda.c))
writeLines(summary_text, "crepis_rda_summary.txt")
c_rda_eigenvalues <- eigenvals(rda.c, constrained = TRUE)
c_rda_proportion_explained <- c_rda_eigenvalues/sum(c_rda_eigenvalues) 
c_rda_proportion_explained

    ### Preparing Model for Plotting ----
rda.cn <- rda(AusRDC[,c(9:10,12:17)] ~ Drought + CO2:Temperature + Condition(CO2+Temperature), data = AusRDC, scale = TRUE)
c_bp_scores <- scores(rda.cn, display = "bp") #Extracting Names of Predictors
rownames(c_bp_scores)
rownames(c_bp_scores)<- c("D", "CxT")

    ### Plot for Crepis RDA ----
png("Crepis - RDA (Zoomed In) - 27 June 2025.png", width = 1600, height = 1600, res = 200)
plot(rda.cn, type = "n", xlim = c(-2,2), ylim = c(-2,2), xlab = "RDA1 (13.04%)", ylab = "RDA2 (4.11%)")
### Adding Individual Observations
points(scores(rda.cn, display = "sites")[,1], scores(rda.cn, display = "sites")[,2], pch = 21, bg = "white", col = "black", cex = 1.2)
### Adding Arrows and Text for Response Variables
arrows(0, 0, scores(rda.cn, display = "species")[,1], scores(rda.cn, display = "species")[,2], col = 'blue', length = 0.1)
text(scores(rda.cn, display = "species")[,1], scores(rda.cn, display = "species")[,2], labels = rownames(scores(rda.cn, display = "species")), col = 'blue', pos = 3, cex = 1.5)
### Adding Arrows and Text for Predictor Variables
arrows(0, 0, scores(rda.cn, display = "bp")[,1], scores(rda.cn, display = "bp")[,2], col = 'red', length = 0.1)
text(scores(rda.cn, display = "bp")[,1], scores(rda.cn, display = "bp")[,2], labels = rownames(c_bp_scores), col = 'red', pos = 3, cex = 1.5)
dev.off()

      #### Having Colored Response Variables ----
# Extract and prepare scores
species_scores <- as.data.frame(scores(rda.cn, display = "species"))
species_scores$Trait <- rownames(species_scores)
species_scores$Group <- cav_group_factor  # This must be a factor or character vector of same length

site_scores <- as.data.frame(scores(rda.cn, display = "sites"))
site_scores$Sample <- rownames(site_scores)

bp_scores <- as.data.frame(scores(rda.cn, display = "bp"))
bp_scores$Variable <- c("D", "CxT")  # must match row order!

arrow_scaling <- 2  # Adjust for appearance

# Apply scaling
species_scores_scaled <- species_scores %>%
  mutate(RDA1 = RDA1 * arrow_scaling,
         RDA2 = RDA2 * arrow_scaling)

bp_scores_scaled <- bp_scores %>%
  mutate(RDA1 = RDA1 * arrow_scaling,
         RDA2 = RDA2 * arrow_scaling)

# Prepare plot
svg("Crepis - RDA - 27 August 2025.svg", width = 10, height = 8)
ggplot() +
  # Sites (samples)
  geom_point(data = site_scores, aes(x = RDA1, y = RDA2), 
             shape = 21, fill = "white", color = "black", size = 3) +
  
  # Species (traits) arrows
  geom_segment(data = species_scores_scaled, 
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2, color = Group),
               arrow = arrow(length = unit(0.4, "cm")), linewidth = 1) +
  
  # Species labels
  geom_text(data = species_scores_scaled, 
            aes(x = RDA1, y = RDA2, label = Trait, color = Group), 
            size = 7, vjust = -0.8) +
  
  # Predictor arrows
  geom_segment(data = bp_scores_scaled, 
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(length = unit(0.4, "cm")), 
               color = "blue", linewidth = 1) +
  
  # Predictor labels
  geom_text(data = bp_scores_scaled, 
            aes(x = RDA1, y = RDA2, label = Variable), 
            color = "blue", size = 7, vjust = -0.8) +
  
  # Color scheme and theme
  scale_color_manual(values = c("tomato", "seagreen", "saddlebrown")) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2)) +
  theme_bw() +
  labs(x = "RDA1 (13.04%)", y = "RDA2 (4.11%)", color = "Trait Group") +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  ) + 
  
  # Remove "a" below arrow in legend
  guides(color = guide_legend(override.aes = list(label = "")))

dev.off()

# Variance Partitioning using Barplot ----
  ## Creation of Separate Models ----
    ### For Lotus ----
lv_DAmodel <- lm(AusL$`Display Area (DA)(cm2)`~ CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusL)
lv_sq_SPAmodel <- lm(AusL$`sqrt_Specific Petal Area (SPA)(cm2/g)` ~ CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusL)
lv_PDMCmodel <- lm(AusL$`Petal Dry Matter Content (PDMC)(g/g)` ~CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusL)
lv_log_LAmodel <- lm(AusL$`log_Leaf Area (LA)(cm2)` ~ CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusL)
lv_SLAmodel <- lm(AusL$`Specific Leaf Area (SLA)(cm2/g)` ~ CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusL)
lv_log_LDMCmodel <- lm(AusL$`log_Leaf Dry Matter Content (LDMC)(g/g)` ~ CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusL)

    ### For Crepis ----
cv_sq_DAmodel <- lm(AusC$`Display Area (DA)(cm2)`~ CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusC)
cv_sq_SPAmodel <- lm(AusC$`Specific Petal Area (SPA)(cm2/g)` ~ CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusC)
cv_sq_PDMCmodel <- lm(AusC$`Petal Dry Matter Content (PDMC)(g/g)` ~ CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusC)
cv_log_LAmodel <- lm(AusC$`log_Leaf Area (LA)(cm2)` ~ CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusC)
cv_log_SLAmodel <- lm(AusC$`log_Specific Leaf Area (SLA)(cm2/g)` ~ CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusC)
cv_sq_LDMCmodel <- lm(AusC$`sqrt_Leaf Dry Matter Content (LDMC)(g/g)` ~ CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusC)
cv_sq_SNmodel <- lm(AusC$`sqrt_Number of Seeds (SN)` ~ CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusC)
cv_log_SMmodel <- lm(AusC$`log_Seed Mass (SM)(g)` ~ CO2 + Temperature + Drought + CO2:Temperature + CO2:Drought, data = AusC)

    ### Creation of Plot for Lotus ----
l_model <- list(
  DA = lv_DAmodel,
  SPA = lv_sq_SPAmodel,
  PDMC = lv_PDMCmodel,
  LA = lv_log_LAmodel,
  SLA = lv_SLAmodel,
  LDMC = lv_log_LDMCmodel
)

      #### For both Significant and Marginally Significant Variables ----
all_results_df <- data.frame()

# Loop through each model in the l_model list
for (model_name in names(l_model)) {
  model <- l_model[[model_name]]
  
  # Perform ANOVA using the Anova function from the car package
  anova_results <- anova(model)
  
  # Convert ANOVA results to a data frame
  anova_results_df <- as.data.frame(anova_results)
  
  # Calculate the proportion of variance explained for each factor
  anova_results_df <- anova_results_df %>%
    mutate(PropVar = `Sum Sq` / sum(`Sum Sq`),
           Factor = rownames(anova_results_df),
           Trait = model_name) %>% # Add trait name
    filter(`Pr(>F)` < 0.1)%>%
    filter(Factor != "(Intercept)" & Factor != "Residuals") # Keep only significant variables # Exclude intercept and residuals
  
  # Combine results into the all_results_df
  all_results_df <- rbind(all_results_df, anova_results_df)
}


desired_order <- c("LDMC", "SLA", "LA", "PDMC", "SPA", "DA")

# Convert 'Trait' to a factor with the desired order
all_results_df$Trait <- factor(all_results_df$Trait, levels = desired_order)

# Create a stacked bar plot for all models 
ggplot(all_results_df, aes(x = Trait, y = PropVar, fill = Factor)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(x = NULL, y = "Proportion of Variance Explained", title = "L. corniculatus - Variance Explained by Climatic Variables") +
  scale_fill_manual(values = c("#7e57c2", "#00bfc4","#fbc02d","#f7756d", "#ff9800"), 
                    labels = c("CO2", "(CO2 + Temperature) : Drought", "CO2 : Temperature", "Drought", "Temperature")) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), # Increase x-axis text size
        axis.text.y = element_text(size = 14),  # Increase y-axis text size (trait names)
        axis.title = element_text(size = 16),   # Increase axis title font size
        plot.title = element_text(size = 18),   # Increase plot title font size
        legend.position = "bottom", 
        legend.title = element_blank())


      #### For Significant Variables Only ----

all_results_df <- data.frame()

# Loop through each model in the l_model list
for (model_name in names(l_model)) {
  model <- l_model[[model_name]]
  
  # Perform ANOVA using the Anova function from the car package
  anova_results <- anova(model)
  
  # Convert ANOVA results to a data frame
  anova_results_df <- as.data.frame(anova_results)
  
  # Calculate the proportion of variance explained for each factor
  anova_results_df <- anova_results_df %>%
    mutate(PropVar = `Sum Sq` / sum(`Sum Sq`),
           Factor = rownames(anova_results_df),
           Trait = model_name) %>% # Add trait name
    filter(`Pr(>F)` < 0.05)%>%
    filter(Factor != "(Intercept)" & Factor != "Residuals") # Keep only significant variables # Exclude intercept and residuals
  
  # Combine results into the all_results_df
  all_results_df <- rbind(all_results_df, anova_results_df)
}

desired_order <- c("LDMC", "SLA", "LA", "PDMC", "SPA", "DA")

# Convert 'Trait' to a factor with the desired order
all_results_df$Trait <- factor(all_results_df$Trait, levels = desired_order)

# Create a stacked bar plot for all models 
ggplot(all_results_df, aes(x = Trait, y = PropVar, fill = Factor)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(x = NULL, y = "Proportion of Variance Explained", title = "L. corniculatus - Variance Explained by Climatic Variables") +
  scale_fill_manual(values = c("#7e57c2", "#00bfc4","#fbc02d","#f7756d", "#ff9800"), 
                    labels = c("CO2", "(CO2 + Temperature) : Drought", "CO2 : Temperature", "Drought", "Temperature")) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), # Increase x-axis text size
        axis.text.y = element_text(size = 14),  # Increase y-axis text size (trait names)
        axis.title = element_text(size = 16),   # Increase axis title font size
        plot.title = element_text(size = 18),   # Increase plot title font size
        legend.position = "bottom", 
        legend.title = element_blank())


      #### Including RDA ----

lv_rda_results_df <- Lotus_RDA %>% 
  as.data.frame() %>%  # Ensure it is a data frame
  mutate(
    PropVar = Variance / sum(Variance),  # Calculate proportion of variance explained
    Factor = rownames(Lotus_RDA),           # Add Factor names as a new column
    Trait = "RDA"                        # Label this as RDA to differentiate in the plot
  ) %>%
  filter(Factor != "Residual")  # Correct filter condition to exclude residuals

all_results_selected <- all_results_df[, c("PropVar", "Factor", "Trait")]

# Select necessary columns from lv_rda_results_df
lv_rda_results_selected <- lv_rda_results_df[, c("PropVar", "Factor", "Trait")]

# Combine the selected columns
combined_results_df <- rbind(all_results_selected, lv_rda_results_selected)

combined_results_df
desired_order <- c("RDA", "LDMC", "SLA", "LA", "DA")

# Convert 'Trait' to a factor with the desired order
combined_results_df$Trait <- factor(combined_results_df$Trait, levels = desired_order)
combined_results_df
# Create a stacked bar plot for all models
svg("Lotus - Variance Partitioning Plot (With RDA) - 27 August 2025.svg", width = 10, height = 8)
ggplot(combined_results_df, aes(x = Trait, y = PropVar, fill = Factor)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(x = "Traits", y = "Proportion of Variance Explained") +
  scale_fill_manual(values = c("#7e57c2", "#00bfc4","#fbc02d","#f7756d", "#ff9800"), 
                    labels = c("C", "CTxD", "CxT", "D", "T")) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increase x-axis text size
        axis.text.y = element_text(size = 14),  # Increase y-axis text size (trait names)
        axis.title = element_text(size = 16),   # Increase axis title font size
        plot.title = element_text(size = 18),   # Increase plot title font size
        legend.text = element_text(size = 14),  # Increase legend labels size
        legend.position = "bottom", 
        legend.title = element_blank())
dev.off()

    ### For Crepis ----

c_model <- list(
  DA = cv_sq_DAmodel,
  SPA = cv_sq_SPAmodel,
  PDMC = cv_sq_PDMCmodel,
  LA = cv_log_LAmodel,
  SLA = cv_log_SLAmodel,
  LDMC = cv_sq_LDMCmodel,
  SN = cv_sq_SNmodel,
  SM = cv_log_SMmodel
)


      #### For both Significant and Marginally Significant Variables ----

all_results_df <- data.frame()

# Loop through each model in the l_model list
for (model_name in names(c_model)) {
  model <- c_model[[model_name]]
  
  # Perform ANOVA using the Anova function from the car package
  anova_results <- anova(model)
  
  # Convert ANOVA results to a data frame
  anova_results_df <- as.data.frame(anova_results)
  
  # Calculate the proportion of variance explained for each factor
  anova_results_df <- anova_results_df %>%
    mutate(PropVar = `Sum Sq` / sum(`Sum Sq`),
           Factor = rownames(anova_results_df),
           Trait = model_name) %>% # Add trait name
    filter(`Pr(>F)` < 0.1)%>%
    filter(Factor != "(Intercept)" & Factor != "Residuals") # Keep only significant variables # Exclude intercept and residuals
  
  # Combine results into the all_results_df
  all_results_df <- rbind(all_results_df, anova_results_df)
}

desired_order <- c("SM", "SN", "LDMC", "SLA", "LA", "PDMC", "SPA", "DA")

# Convert 'Trait' to a factor with the desired order
all_results_df$Trait <- factor(all_results_df$Trait, levels = desired_order)

# Create a stacked bar plot for all models 
ggplot(all_results_df, aes(x = Trait, y = PropVar, fill = Factor)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(x = " Traits", y = "Proportion of Variance Explained", title = "C. capillaris - Variance Explained by Climatic Variables") +
  scale_fill_manual(values = c("#00bfc4","#fbc02d","#f7756d","#7e57c2","#ff9800"), 
                    labels = c("(CO2 + Temperature) : Drought", "CO2 : Temperature", "Drought", "Temperature")) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), # Increase x-axis text size
        axis.text.y = element_text(size = 14),  # Increase y-axis text size (trait names)
        axis.title = element_text(size = 16),   # Increase axis title font size
        plot.title = element_text(size = 18),   # Increase plot title font size
        legend.position = "bottom", 
        legend.title = element_blank())

      #### For Significant Variables Alone ----

all_results_df <- data.frame()

# Loop through each model in the l_model list
for (model_name in names(c_model)) {
  model <- c_model[[model_name]]
  
  # Perform ANOVA using the Anova function from the car package
  anova_results <- anova(model)
  
  # Convert ANOVA results to a data frame
  anova_results_df <- as.data.frame(anova_results)
  
  # Calculate the proportion of variance explained for each factor
  anova_results_df <- anova_results_df %>%
    mutate(PropVar = `Sum Sq` / sum(`Sum Sq`),
           Factor = rownames(anova_results_df),
           Trait = model_name) %>% # Add trait name
    filter(`Pr(>F)` < 0.05)%>%
    filter(Factor != "(Intercept)" & Factor != "Residuals") # Keep only significant variables # Exclude intercept and residuals
  
  # Combine results into the all_results_df
  all_results_df <- rbind(all_results_df, anova_results_df)
}

desired_order <- c("SM", "SN", "LDMC", "PDMC", "SPA", "DA")

# Convert 'Trait' to a factor with the desired order
all_results_df$Trait <- factor(all_results_df$Trait, levels = desired_order)

# Create a stacked bar plot for all models 
ggplot(all_results_df, aes(x = Trait, y = PropVar, fill = Factor)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(x = NULL, y = "Proportion of Variance Explained", title = "C. capillaris - Variance Explained by Climatic Variables") +
  scale_fill_manual(values = c("#f7756d", "#00bfc4")) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), # Increase x-axis text size
        axis.text.y = element_text(size = 14),  # Increase y-axis text size (trait names)
        axis.title = element_text(size = 16),   # Increase axis title font size
        plot.title = element_text(size = 18),   # Increase plot title font size
        legend.position = "bottom", 
        legend.title = element_blank())


      #### Including RDA ----

cv_rda_results_df <- Crepis_RDA %>% 
  as.data.frame() %>%  # Ensure it is a data frame
  mutate(
    PropVar = Variance / sum(Variance),  # Calculate proportion of variance explained
    Factor = rownames(Crepis_RDA),           # Add Factor names as a new column
    Trait = "RDA"                        # Label this as RDA to differentiate in the plot
  ) %>%
  filter(Factor != "Residual")  # Correct filter condition to exclude residuals

all_results_selected <- all_results_df[, c("PropVar", "Factor", "Trait")]

# Select necessary columns from lv_rda_results_df
cv_rda_results_selected <- cv_rda_results_df[, c("PropVar", "Factor", "Trait")]

# Combine the selected columns
combined_results_df <- rbind(all_results_selected, cv_rda_results_selected)

combined_results_df
desired_order <- c("RDA", "SM", "SN", "LDMC", "PDMC", "SPA", "DA")

# Convert 'Trait' to a factor with the desired order
combined_results_df$Trait <- factor(combined_results_df$Trait, levels = desired_order)
combined_results_df
# Create a stacked bar plot for all models

svg("Crepis - Variance Partitioning Plot (With RDA) - 27 August 2025.svg", width = 10, height = 8)
ggplot(combined_results_df, aes(x = Trait, y = PropVar, fill = Factor)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(x = " Traits", y = "Proportion of Variance Explained") +
  scale_fill_manual(values = c("#7e57c2", "#00bfc4","#fbc02d","#f7756d", "#ff9800"), 
                    labels = c("C", "CTxD", "CxT", "D", "T")) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increase x-axis text size
        axis.text.y = element_text(size = 14),  # Increase y-axis text size (trait names)
        axis.title = element_text(size = 16),   # Increase axis title font size
        plot.title = element_text(size = 18),   # Increase plot title font size
        legend.text = element_text(size = 14),  # Increase legend labels size
        legend.position = "bottom", 
        legend.title = element_blank())
dev.off()

# Exporting Plots ----
  ## For Lotus ----

l_plots <- list(
  Lotus_CO2_SLA = ggplot(AusL, aes(x = CO2, y = `Specific Leaf Area (SLA)(cm2/g)`)) +
    scale_x_discrete(labels = c("0"="Ambient CO2", "1"="Elevated CO2")) +
    geom_boxplot() +
    labs(x = "CO2", y = "SLA (cm2/g)") + 
    theme(axis.title = element_text(size = 36),  
          axis.text = element_text(size = 32)),
  
  Lotus_CO2_LDMC = ggplot(AusL, aes(x = as.factor(CO2), y = `Leaf Dry Matter Content (LDMC)(g/g)`)) +
    scale_x_discrete(labels = c("0"="Ambient CO2", "1"="Elevated CO2")) +
    geom_boxplot() +
    labs(x = "CO2", y = "LDMC (g/g)") + 
    theme(axis.title = element_text(size = 36),  
          axis.text = element_text(size = 32)),
  
  Lotus_T_DA = ggplot(AusL, aes(x = Temperature, y = `Display Area (DA)(cm2)`)) +
    scale_x_discrete(labels = c("0"="Ambient Temp.", "1"="High Temp.")) +
    geom_boxplot() +
    labs(x = "Temperature", y = "DA (cm2)") +
    theme(axis.title = element_text(size = 36),
          axis.text = element_text(size = 32)),
  
  Lotus_D_LA = ggplot(AusL, aes(x = Drought, y = `Leaf Area (LA)(cm2)`)) +
    scale_x_discrete(labels = c("0"="Well-watered", "1"="Drought")) +
    geom_boxplot() +
    labs(x = "Drought", y = "LA (cm2)") + 
    theme(axis.title = element_text(size = 36),
          axis.text = element_text(size = 32)),
  
  Lotus_D_SLA = ggplot(AusL, aes(x = Drought, y = `Specific Leaf Area (SLA)(cm2/g)`)) +
    scale_x_discrete(labels = c("0"="Well-watered", "1"="Drought")) +
    geom_boxplot() +
    labs(x = "Drought", y = "SLA (cm2/g)") + 
    theme(axis.title = element_text(size = 36),
          axis.text = element_text(size = 32)),
  
  Lotus_D_LDMC = ggplot(AusL, aes(x = Drought, y = `Leaf Dry Matter Content (LDMC)(g/g)`)) +
    scale_x_discrete(labels = c("0"="Well-watered", "1"="Drought")) +
    geom_boxplot() +
    labs(x = "Drought", y = "LDMC (g/g)") +
    theme(axis.title = element_text(size = 36),
          axis.text = element_text(size = 32)),
  
  Lotus_CxT_DA = ggplot(AusL, aes(x = CO2, y = `Display Area (DA)(cm2)`)) +
    geom_boxplot() +
    facet_wrap(~ Temperature, labeller = as_labeller(c("0" = "Ambient Temp.", "1" = "High Temp."))) +
    scale_x_discrete(labels = c("0" = "Ambient", "1" = "Elevated")) +
    labs(x = "CO2", y = "DA (cm2)") +
    theme(
      axis.title = element_text(size = 36),
      axis.text = element_text(size = 32),
      strip.text = element_text(size = 32, face = "bold"),
      legend.position = "none"),
  
  Lotus_CxT_SLA = ggplot(AusL, aes(x = CO2, y = `Specific Leaf Area (SLA)(cm2/g)`)) +
    geom_boxplot() +
    facet_wrap(~Temperature, labeller = as_labeller(c("0" = "Ambient Temp.", "1" = "High Temp."))) +
    scale_x_discrete(labels = c("0" = "Ambient", "1" = "Elevated")) +
    labs(x = "CO2", y = "SLA (cm2/g)") +
    theme(
      axis.title = element_text(size = 36),
      axis.text = element_text(size = 32),
      strip.text = element_text(size = 32, face = "bold"),
      legend.position = "none"),
  
  Lotus_CTxD_LDMC = ggplot(AusL, aes(x = CO2, y = `Leaf Dry Matter Content (LDMC)(g/g)`)) +
    scale_x_discrete(labels = c("0" = "Ambient", "1" = "Elevated")) +
    geom_boxplot()+
    facet_wrap(~ Drought, labeller = as_labeller(c("0" = "Well Watered", "1" = "Drought"))) +
    labs(x = "CO2 + Temperature", y = "LDMC (g/g)", fill = "Drought") +
    theme(axis.title = element_text(size = 36),
          axis.text = element_text(size = 32),
          strip.text = element_text(size = 32, face = "bold"),
          legend.position = "none"))

# Save each plot as a separate PNG
for (i in seq_along(l_plots)) {
  ggsave(filename = paste0(names(l_plots)[i], ".png"), 
         plot = l_plots[[i]], 
         width = 12, height = 12, dpi = 300)
}

  ## For Crepis ----
# Create a list of ggplots for Crepis
c_plots<- list(
  Crepis_D_DA = ggplot(AusC, aes(x = Drought, y = `Display Area (DA)(cm2)`)) +
    scale_x_discrete(labels = c("0"="Well-watered", "1"="Drought")) +
    geom_boxplot() +
    labs(x = "Drought", y = "DA (cm2)") +
    theme(axis.title = element_text(size = 36),
          axis.text = element_text(size = 32)),
  
  Crepis_D_SPA = ggplot(AusC, aes(x = Drought, y = `Specific Petal Area (SPA)(cm2/g)`)) +
    scale_x_discrete(labels = c("0"="Well-watered", "1"="Drought")) +
    geom_boxplot() +
    labs(x = "Drought", y = "SPA (cm2/g)") + 
    theme(axis.title = element_text(size = 36),
          axis.text = element_text(size = 32)),
  
  Crepis_D_PDMC = ggplot(AusC, aes(x = Drought, y = `Petal Dry Matter Content (PDMC)(g/g)`)) +
    scale_x_discrete(labels = c("0"="Well-watered", "1"="Drought")) +
    geom_boxplot() +
    labs(x = "Drought", y = "PDMC (g/g)") + 
    theme(axis.title = element_text(size = 36),
          axis.text = element_text(size = 32)),
  
  Crepis_D_LDMC = ggplot(AusC, aes(x = Drought, y = `Leaf Dry Matter Content (LDMC)(g/g)`)) +
    scale_x_discrete(labels = c("0"="Well-watered", "1"="Drought")) +
    geom_boxplot() +
    labs(x = "Drought", y = "LDMC (g/g)") + 
    theme(axis.title = element_text(size = 36),
          axis.text = element_text(size = 32)),
  
  Crepis_D_SN = ggplot(AusC, aes(x = Drought, y = `Number of Seeds (SN)`)) +
    scale_x_discrete(labels = c("0"="Well-watered", "1"="Drought")) +
    geom_boxplot() +
    labs(x = "Drought", y = "SN") + 
    theme(axis.title = element_text(size = 36),
          axis.text = element_text(size = 32)),
  
  Crepis_T_SN = ggplot(AusC, aes(x = Temperature, y = `Number of Seeds (SN)`)) +
    scale_x_discrete(labels = c("0"="Ambient Temp.", "1"="High Temp.")) +
    geom_boxplot() +
    labs(x = "Temperature", y = "SN") + 
    theme(axis.title = element_text(size = 36),
          axis.text = element_text(size = 32)),
  
  # Interaction plots
  Crepis_CxT_SPA = ggplot(AusC, aes(x = CO2, y = `Specific Petal Area (SPA)(cm2/g)`)) +
    geom_boxplot() +
    facet_wrap(~Temperature, labeller = as_labeller(c("0" = "Ambient Temp.", "1" = "High Temp."))) +
    scale_x_discrete(labels = c("0" = "Ambient", "1" = "Elevated")) +
    labs(x = "CO2", y = "SPA (cm2/g)") +
    theme(
      axis.title = element_text(size = 36),
      axis.text = element_text(size = 32),
      strip.text = element_text(size = 32, face = "bold"),
      legend.position = "none"),
  
  Crepis_CxT_SN = ggplot(AusC, aes(x = CO2, y = `Number of Seeds (SN)`)) +
    geom_boxplot() +
    facet_wrap(~Temperature, labeller = as_labeller(c("0" = "Ambient Temp.", "1" = "High Temp."))) +
    scale_x_discrete(labels = c("0" = "Ambient", "1" = "Elevated")) +
    labs(x = "CO2", y = "SN") +
    theme(
      axis.title = element_text(size = 36),
      axis.text = element_text(size = 32),
      strip.text = element_text(size = 32, face = "bold"),
      legend.position = "none"
    ))

for (i in seq_along(c_plots)) {
  ggsave(filename = paste0(names(c_plots)[i], ".png"), 
         plot = c_plots[[i]], 
         width = 12, height = 12, dpi = 300)
}
# Network Analysis ----
  ## For Lotus ----
n_trait_data <- AusPC[AusPC$Species == "Lotus", m_cols] # Subset trait data for 'Lotus'
ln_trait_corr <- cor(n_trait_data, use = "pairwise.complete.obs") # Calculate Pearson correlations
ln_p_values <- cor.mtest(n_trait_data, use = "pairwise.complete.obs")$p # Get p-values for correlations
ln_adjusted_p_values <- p.adjust(ln_p_values, method = "fdr") # Adjust p-values using False Discovery Rate (FDR)
threshold <- 0.2
ln_weighted_adj <- ifelse(abs(ln_trait_corr) > threshold & ln_p_values < 0.05,
                          abs(ln_trait_corr), 0) # Create weighted adjacency matrix with significance and threshold
ln_network <- graph_from_adjacency_matrix(ln_weighted_adj, mode = "undirected", weighted = TRUE, diag = FALSE) # Create igraph object with weighted edges
deg <- strength(ln_network, weights = E(ln_network)$weight)  # Weighted degree
btw <- betweenness(ln_network, weights = 1 / E(ln_network)$weight, normalized = TRUE) # Weighted betweeness
V(ln_network)$degree <- deg # Assign centrality scores to nodes
V(ln_network)$betweenness <- btw
ln_ceb <- cluster_edge_betweenness(ln_network) 
ln_cluster_membership <- membership(ln_ceb)
ln_num_clusters <- length(unique(ln_cluster_membership))
ln_cluster_colors <- c("#f7756d","#fbc02d","#00bfc4", "#7e57c2", "#ff9800")
V(ln_network)$color <- ln_cluster_colors[ln_cluster_membership] # Assign cluster membership colors
layout <- layout_with_fr(ln_network)  # Fruchterman-Reingold layout
png("Lotus_trait_network - 23 June 2025.png", width = 7, height = 6, units = "in", res = 300)
plot(ln_network,
     layout = layout,
     vertex.size = deg / max(deg) * 20,
     vertex.label.cex = 1.5,
     vertex.label.color = "black",
     vertex.label.dist = 1,
     vertex.color = V(ln_network)$color,
     edge.width = E(ln_network)$weight * 5)
dev.off()
  ## For Lotus (Showing Positive and Negative Correlation) ----
ln_sign <- sign(ln_trait_corr)  # +1 for positive, -1 for negative
ln_weighted_adj <- ifelse(abs(ln_trait_corr) > threshold & ln_p_values < 0.05,
                          ln_trait_corr, 0)
ln_network <- graph_from_adjacency_matrix(ln_weighted_adj, mode = "undirected", weighted = TRUE, diag = FALSE) # Create igraph object with weighted edges
E(ln_network)$color <- ifelse(E(ln_network)$weight > 0, "blue", "red")
ln_layout <- layout_with_fr(ln_network, weights = abs(E(ln_network)$weight))
V(ln_network)$color <- ln_cluster_colors[ln_cluster_membership]
png("Lotus_network_plot_sign - 27 June 2025.png", width = 7, height = 6, units = "in", res = 300)
plot(ln_network,
     layout = ln_layout,
     vertex.label.dist = 1,
     vertex.label.cex = 1.5,
     vertex.label.color = "black",
     vertex.color = V(ln_network)$color,
     vertex.size = V(ln_network)$size,
     edge.width = abs(E(ln_network)$weight) * 5,
     edge.color = E(ln_network)$color)
dev.off()

  ## For Crepis (All Variables) ----
can_trait_data <- AusPC[AusPC$Species == "Crepis", a_cols]
can_trait_corr <- cor(can_trait_data, use = "pairwise.complete.obs") # Compute Pearson correlations and p-values
can_p_values <- cor.mtest(can_trait_data, use = "pairwise.complete.obs")$p
can_adjusted_p_values <- p.adjust(can_p_values, method = "fdr") # Adjust p-values using FDR
threshold <- 0.2
dim(ln_p_values)
can_weighted_adj <- ifelse(abs(can_trait_corr) > threshold & can_p_values < 0.05, # Not Corrected for fdr
                           abs(can_trait_corr), 0) # Create weighted adjacency matrix
can_network <- graph_from_adjacency_matrix(can_weighted_adj, mode = "undirected", weighted = TRUE, diag = FALSE) # Create igraph object with weighted edges
can_deg <- strength(can_network, weights = E(can_network)$weight)  # Weighted degree
?graph_from_adjacency_matrix
can_btw <- betweenness(can_network, weights = 1 / E(can_network)$weight, normalized = TRUE) # Weighted betweeness
V(can_network)$size <- can_deg / max(can_deg) * 30  # Scale node size
V(can_network)$label.cex <- 1.5
can_ceb <- cluster_edge_betweenness(can_network)
can_cluster_membership <- membership(can_ceb)
can_num_clusters <- length(unique(can_cluster_membership))
can_cluster_colors <- c("#fbc02d","#00bfc4", "#7e57c2", "#ff9800")
V(can_network)$color <- can_cluster_colors[can_cluster_membership] # Assign cluster membership colors
png("Crepis_network_plot - 27 June 2025.png", width = 7, height = 6, units = "in", res = 300)
plot(can_network,
     vertex.label.dist = 1,
     vertex.label.cex = 1.5,
     edge.width = E(can_network)$weight * 5,
     vertex.label.color = "black")
dev.off()
  ## For Crepis (Showing Positive and Negative Correlation) ----
can_sign <- sign(can_trait_corr)  # +1 for positive, -1 for negative
can_weighted_adj <- ifelse(abs(can_trait_corr) > threshold & can_p_values < 0.05,
                           can_trait_corr, 0)
can_network <- graph_from_adjacency_matrix(can_weighted_adj, mode = "undirected", weighted = TRUE, diag = FALSE) # Create igraph object with weighted edges
E(can_network)$color <- ifelse(E(can_network)$weight > 0, "blue", "red")
can_layout <- layout_with_fr(can_network, weights = abs(E(can_network)$weight))
V(can_network)$color <- can_cluster_colors[can_cluster_membership]
png("Crepis_network_plot_sign - 27 June 2025.png", width = 7, height = 6, units = "in", res = 300)
plot(can_network,
     layout = can_layout,
     vertex.label.dist = 1,
     vertex.label.cex = 1.5,
     vertex.label.color = "black",
     vertex.color = V(can_network)$color,
     vertex.size = V(can_network)$size,
     edge.width = abs(E(can_network)$weight) * 5,
     edge.color = E(can_network)$color)
dev.off()
