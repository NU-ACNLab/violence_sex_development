### This code generates models analogous to the Salience B
### analyses in the main body of the paper with the other 16
### Yeo17 networks for the purposes of addressing reviewer
### concerns 
###
### Ellyn Butler
### March 31, 2025 - April 23, 2025

set.seed(2000)

library(reshape2)
library(MASS)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(ggpubr)
library(sjmisc)
library(sjlabelled)

#library(lmerTest) #confint

d <- read.csv('~/Documents/Northwestern/projects/violence_sex_development/data/combined_data_2025-04-18.csv')
dall <- d
d <- d[!is.na(d$exp_salienceb_pos) & !is.na(d$FC_salienceb_pos) & !is.na(d$depression) & !is.na(d$num_pastyear), ]
d2 <- d[d$sesid == 2, ]

# Remove subjects with low variance vertices 
# NOTE: All IPR values are from the first time point
comb1 <- read.csv('~/Documents/Northwestern/projects/violence_sex_development/data/combined_data_2024-10-07.csv') #Reminder: settled on this because it excluded subjects with low variance vertices
comb1 <- comb1[!is.na(comb1$exp_b_pos) & !is.na(comb1$FC_b_pos) & !is.na(comb1$depression) & !is.na(comb1$num_pastyear), ]
comb1 <- comb1[comb1$sesid == 2, ]
comb1 <- comb1[, c('subid', 'sesid')]
d2 <- merge(comb1, d2)

dim(d2) # N = 220

################### Model
d1 <- dall[dall$sesid == 1, ]
d1 <- d1[, c('subid', 'BMIperc', 'PubCat')]
names(d1) <- c('subid', 'BMIperc_1', 'PubCat_1')
d2 <- merge(d2, d1)
vars <- c('depression', 'num_pastyear', 'BMIperc_1', 'PubCat_1', 'age_mri', 'IPR', names(d2)[grep('FC', names(d2))], names(d2)[grep('exp', names(d2))])
d2[, vars] <- scale(d2[, vars])

d2$BMIperc_2 <- d2$BMIperc
d2$PubCat_2 <- d2$PubCat
d2$num_pastyear_2 <- d2$num_pastyear

##### (a) Base models
# Visual A (1)
a_exp_visuala_viol <- lm(exp_visuala_pos ~ num_pastyear_2*female, d=d2) #sig: none
a_FC_visuala_viol <- lm(FC_visuala_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_visuala <- lm(depression ~ exp_visuala_pos*female, d=d2) #sig: female
a_depression_FC_visuala <- lm(depression ~ FC_visuala_pos*female, d=d2) #sig: female

# Visual B (2)
a_exp_visualb_viol <- lm(exp_visualb_pos ~ num_pastyear_2*female, d=d2) #sig: none
a_FC_visualb_viol <- lm(FC_visualb_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_visualb <- lm(depression ~ exp_visualb_pos*female, d=d2) #sig: female
a_depression_FC_visualb <- lm(depression ~ FC_visualb_pos*female, d=d2) #sig: female

# Somatomotor A (3)
a_exp_somatomotora_viol <- lm(exp_somatomotora_pos ~ num_pastyear_2*female, d=d2) #sig: none
a_FC_somatomotora_viol <- lm(FC_somatomotora_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_somatomotora <- lm(depression ~ exp_somatomotora_pos*female, d=d2) #sig: female
a_depression_FC_somatomotora <- lm(depression ~ FC_somatomotora_pos*female, d=d2) #sig: FC_somatomotora_pos, female, FC_somatomotora_pos:female

# Somatomotor B (4)
a_exp_somatomotorb_viol <- lm(exp_somatomotorb_pos ~ num_pastyear_2*female, d=d2) #sig: none
a_FC_somatomotorb_viol <- lm(FC_somatomotorb_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_somatomotorb <- lm(depression ~ exp_somatomotorb_pos*female, d=d2) #sig: female
a_depression_FC_somatomotorb <- lm(depression ~ FC_somatomotorb_pos*female, d=d2) #sig: female

# Dorsal Attention A (5)
a_exp_dorsalattentiona_viol <- lm(exp_dorsalattentiona_pos ~ num_pastyear_2*female, d=d2) #sig: none 
a_FC_dorsalattentiona_viol <- lm(FC_dorsalattentiona_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_dorsalattentiona <- lm(depression ~ exp_dorsalattentiona_pos*female, d=d2) #sig: female
a_depression_FC_dorsalattentiona <- lm(depression ~ FC_dorsalattentiona_pos*female, d=d2) #sig: female

# Dorsal Attention B (6)
a_exp_dorsalattentionb_viol <- lm(exp_dorsalattentionb_pos ~ num_pastyear_2*female, d=d2) #sig: none
a_FC_dorsalattentionb_viol <- lm(FC_dorsalattentionb_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_dorsalattentionb <- lm(depression ~ exp_dorsalattentionb_pos*female, d=d2) #sig: female
a_depression_FC_dorsalattentionb <- lm(depression ~ FC_dorsalattentionb_pos*female, d=d2) #sig: FC_dorsalattentionb_pos, female, FC_dorsalattentionb_pos:female

# Salience A (7)
a_exp_saliencea_viol <- lm(exp_saliencea_pos ~ num_pastyear_2*female, d=d2) #sig: none
a_FC_saliencea_viol <- lm(FC_saliencea_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_saliencea <- lm(depression ~ exp_saliencea_pos*female, d=d2) #sig: female
a_depression_FC_saliencea <- lm(depression ~ FC_saliencea_pos*female, d=d2) #sig: female

# Limbic A (9)
a_exp_limbica_viol <- lm(exp_limbica_pos ~ num_pastyear_2*female, d=d2) #sig: none
a_FC_limbica_viol <- lm(FC_limbica_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_limbica <- lm(depression ~ exp_limbica_pos*female, d=d2) #sig: female
a_depression_FC_limbica <- lm(depression ~ FC_limbica_pos*female, d=d2) #sig: FC_limbica_pos, female, FC_limbica_pos:female

# Limbic B (10)
a_exp_limbicb_viol <- lm(exp_limbicb_pos ~ num_pastyear_2*female, d=d2) #sig: num_pastyear:female
a_FC_limbicb_viol <- lm(FC_limbicb_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_limbicb <- lm(depression ~ exp_limbicb_pos*female, d=d2) #sig: female
a_depression_FC_limbicb <- lm(depression ~ FC_limbicb_pos*female, d=d2) #sig: FC_limbicb_pos, female, FC_limbicb_pos:female

# Control A (11)
a_exp_controla_viol <- lm(exp_controla_pos ~ num_pastyear_2*female, d=d2) #sig: none
a_FC_controla_viol <- lm(FC_controla_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_controla <- lm(depression ~ exp_controla_pos*female, d=d2) #sig: female
a_depression_FC_controla <- lm(depression ~ FC_controla_pos*female, d=d2) #sig: female, FC_controla_pos:female

# Control B (12)
a_exp_controlb_viol <- lm(exp_controlb_pos ~ num_pastyear_2*female, d=d2) #sig: none
a_FC_controlb_viol <- lm(FC_controlb_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_controlb <- lm(depression ~ exp_controlb_pos*female, d=d2) #sig: female
a_depression_FC_controlb <- lm(depression ~ FC_controlb_pos*female, d=d2) #sig: FC_controlb_pos, female, FC_controlb_pos:female

# Control C (13)
a_exp_controlc_viol <- lm(exp_controlc_pos ~ num_pastyear_2*female, d=d2) #sig: none
a_FC_controlc_viol <- lm(FC_controlc_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_controlc <- lm(depression ~ exp_controlc_pos*female, d=d2) #sig: female
a_depression_FC_controlc <- lm(depression ~ FC_controlc_pos*female, d=d2) #sig: female, FC_controlc_pos:female

# Default A (14)
a_exp_defaulta_viol <- lm(exp_defaulta_pos ~ num_pastyear_2*female, d=d2) #sig: none
a_FC_defaulta_viol <- lm(FC_defaulta_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_defaulta <- lm(depression ~ exp_defaulta_pos*female, d=d2) #sig: female
a_depression_FC_defaulta <- lm(depression ~ FC_defaulta_pos*female, d=d2) #sig: FC_defaulta_pos, female, FC_defaulta_pos:female

# Default B (15)
a_exp_defaultb_viol <- lm(exp_defaultb_pos ~ num_pastyear_2*female, d=d2) #sig: none
a_FC_defaultb_viol <- lm(FC_defaultb_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_defaultb <- lm(depression ~ exp_defaultb_pos*female, d=d2) #sig: female
a_depression_FC_defaultb <- lm(depression ~ FC_defaultb_pos*female, d=d2) #sig: FC_defaultb_pos, female, FC_defaultb_pos:female

# Default C (16)
a_exp_defaultc_viol <- lm(exp_defaultc_pos ~ num_pastyear_2*female, d=d2) #sig: none
a_FC_defaultc_viol <- lm(FC_defaultc_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_defaultc <- lm(depression ~ exp_defaultc_pos*female, d=d2) #sig: female
a_depression_FC_defaultc <- lm(depression ~ FC_defaultc_pos*female, d=d2) #sig: FC_defaultc_pos, female, FC_defaultc_pos:female

# Temporal Parietal (17)
a_exp_temporalparietal_viol <- lm(exp_temporalparietal_pos ~ num_pastyear_2*female, d=d2) #sig: none
a_FC_temporalparietal_viol <- lm(FC_temporalparietal_pos ~ num_pastyear_2*female, d=d2) #sig: female
a_depression_exp_temporalparietal <- lm(depression ~ exp_temporalparietal_pos*female, d=d2) #sig: female
a_depression_FC_temporalparietal <- lm(depression ~ FC_temporalparietal_pos*female, d=d2) #sig: FC_temporalparietal_pos, female, FC_temporalparietal_pos:female

##### (b) Controlling for demographics
# Visual A (1)
b_exp_visuala_viol <- lm(exp_visuala_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: none
b_FC_visuala_viol <- lm(FC_visuala_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_visuala <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_visuala_pos*female, d=d2) #sig: none
b_depression_FC_visuala <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_visuala_pos*female, d=d2) #sig: none

# Visual B (2)
b_exp_visualb_viol <- lm(exp_visualb_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: none
b_FC_visualb_viol <- lm(FC_visualb_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_visualb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_visualb_pos*female, d=d2) #sig: none
b_depression_FC_visualb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_visualb_pos*female, d=d2) #sig: none

# Somatomotor A (3)
b_exp_somatomotora_viol <- lm(exp_somatomotora_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: none
b_FC_somatomotora_viol <- lm(FC_somatomotora_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_somatomotora <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_somatomotora_pos*female, d=d2) #sig: none
b_depression_FC_somatomotora <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_somatomotora_pos*female, d=d2) #sig: none

# Somatomotor B (4)
b_exp_somatomotorb_viol <- lm(exp_somatomotorb_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: none
b_FC_somatomotorb_viol <- lm(FC_somatomotorb_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_somatomotorb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_somatomotorb_pos*female, d=d2) #sig: none
b_depression_FC_somatomotorb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_somatomotorb_pos*female, d=d2) #sig: none

# Dorsal Attention A (5)
b_exp_dorsalattentiona_viol <- lm(exp_dorsalattentiona_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: none
b_FC_dorsalattentiona_viol <- lm(FC_dorsalattentiona_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_dorsalattentiona <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_dorsalattentiona_pos*female, d=d2) #sig: none
b_depression_FC_dorsalattentiona <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_dorsalattentiona_pos*female, d=d2) #sig: none

# Dorsal Attention B (6)
b_exp_dorsalattentionb_viol <- lm(exp_dorsalattentionb_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: none
b_FC_dorsalattentionb_viol <- lm(FC_dorsalattentionb_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_dorsalattentionb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_dorsalattentionb_pos*female, d=d2) #sig: none
b_depression_FC_dorsalattentionb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_dorsalattentionb_pos*female, d=d2) #sig: none

# Salience A (7)
b_exp_saliencea_viol <- lm(exp_saliencea_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: none
b_FC_saliencea_viol <- lm(FC_saliencea_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_saliencea <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_saliencea_pos*female, d=d2) #sig: none
b_depression_FC_saliencea <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_saliencea_pos*female, d=d2) #sig: none

# Limbic A (9)
b_exp_limbica_viol <- lm(exp_limbica_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: num_pastyear:female
b_FC_limbica_viol <- lm(FC_limbica_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_limbica <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_limbica_pos*female, d=d2) #sig: none
b_depression_FC_limbica <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_limbica_pos*female, d=d2) #sig: none

# Limbic B (10)
b_exp_limbicb_viol <- lm(exp_limbicb_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: num_pastyear:female
b_FC_limbicb_viol <- lm(FC_limbicb_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_limbicb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_limbicb_pos*female, d=d2) #sig: none
b_depression_FC_limbicb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_limbicb_pos*female, d=d2) #sig: none

# Control A (11)
b_exp_controla_viol <- lm(exp_controla_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: num_pastyear:female
b_FC_controla_viol <- lm(FC_controla_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_controla <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_controla_pos*female, d=d2) #sig: none
b_depression_FC_controla <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_controla_pos*female, d=d2) #sig: none

# Control B (12)
b_exp_controlb_viol <- lm(exp_controlb_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: none
b_FC_controlb_viol <- lm(FC_controlb_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_controlb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_controlb_pos*female, d=d2) #sig: none
b_depression_FC_controlb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_controlb_pos*female, d=d2) #sig: none

# Control C (13)
b_exp_controlc_viol <- lm(exp_controlc_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: none
b_FC_controlc_viol <- lm(FC_controlc_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_controlc <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_controlc_pos*female, d=d2) #sig: none 
b_depression_FC_controlc <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_controlc_pos*female, d=d2) #sig: none

# Default A (14)
b_exp_defaulta_viol <- lm(exp_defaulta_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: none
b_FC_defaulta_viol <- lm(FC_defaulta_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_defaulta <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_defaulta_pos*female, d=d2) #sig: none
b_depression_FC_defaulta <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_defaulta_pos*female, d=d2) #sig: none

# Default B (15)
b_exp_defaultb_viol <- lm(exp_defaultb_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: none
b_FC_defaultb_viol <- lm(FC_defaultb_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_defaultb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_defaultb_pos*female, d=d2) #sig: none
b_depression_FC_defaultb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_defaultb_pos*female, d=d2) #sig: none

# Default C (16)
b_exp_defaultc_viol <- lm(exp_defaultc_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: none
b_FC_defaultc_viol <- lm(FC_defaultc_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_defaultc <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_defaultc_pos*female, d=d2) #sig: none
b_depression_FC_defaultc <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_defaultc_pos*female, d=d2) #sig: none

# Temporal Parietal (17)
b_exp_temporalparietal_viol <- lm(exp_temporalparietal_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: none
b_FC_temporalparietal_viol <- lm(FC_temporalparietal_pos ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female, d=d2) #sig: female
b_depression_exp_temporalparietal <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_temporalparietal_pos*female, d=d2) #sig: none
b_depression_FC_temporalparietal <- lm(depression ~ age_mri + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_temporalparietal_pos*female, d=d2) #sig: none

###### Imputation # TO DO: Will need to do this for all of the neuroimaging variables
# Impute brain values for subjects that have T2 but not T1 data
d1b <- dall[dall$sesid == 1 & dall$subid %in% d2$subid, ]
d1b <- d1b[, !(names(d1b) %in% c('sesid', 'otherrace'))]
d2b <- d2
d2b <- d2b[, !(names(d2b) %in% c('sesid', 'otherrace', 'BMIperc_1', 'PubCat_1', 'BMIperc', 'PubCat', 'num_pastyear'))]
same <- c('subid', 'black', 'white', 'hispanic', 'female', 'IPR', 'BMIperc_2', 'PubCat_2', 'num_pastyear_2')
names(d1b)[!(names(d1b) %in% same)] <- paste0(names(d1b)[!(names(d1b) %in% same)], '_1')
names(d2b)[!(names(d2b) %in% same)] <- paste0(names(d2b)[!(names(d2b) %in% same)], '_2')

# Assume age_mri_1 would have been two years before age_mri_2
subids <- d1b[is.na(d1b$age_mri_1), 'subid']
d1b[is.na(d1b$age_mri_1), 'age_mri_1'] <- d2b[d2b$subid %in% subids, 'age_mri_2'] - 2

# Visual A (1)
t.test(exp_visuala_pos_1 ~ black, data = d1b) #sig
t.test(exp_visuala_pos_1 ~ white, data = d1b) #sig
t.test(exp_visuala_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_visuala_pos_1), 'exp_visuala_pos_1'], d1b[!is.na(d1b$exp_visuala_pos_1), 'BMIperc_1']) #sig
cor.test(d1b[!is.na(d1b$exp_visuala_pos_1), 'exp_visuala_pos_1'], d1b[!is.na(d1b$exp_visuala_pos_1), 'PubCat_1']) #sig
cor.test(d1b[!is.na(d1b$exp_visuala_pos_1), 'exp_visuala_pos_1'], d1b[!is.na(d1b$exp_visuala_pos_1), 'IPR']) #sig
t.test(exp_visuala_pos_1 ~ female, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_visuala_pos_1), 'exp_visuala_pos_1'], d1b[!is.na(d1b$exp_visuala_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_visuala_pos_1), 'exp_visuala_pos_1'], d1b[!is.na(d1b$exp_visuala_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_visuala_pos_1 ~ black + white + BMIperc_1 + PubCat_1 + IPR, data = d1b) #black, PubCat_1, IPR
exp_mod <- lm(exp_visuala_pos_1 ~ black + PubCat_1 + IPR, data = d1b) 

d1b[is.na(d1b$exp_visuala_pos_1), 'exp_visuala_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_visuala_pos_1), ])

t.test(FC_visuala_pos_1 ~ black, data = d1b) #not
t.test(FC_visuala_pos_1 ~ white, data = d1b) #not
t.test(FC_visuala_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_visuala_pos_1), 'FC_visuala_pos_1'], d1b[!is.na(d1b$FC_visuala_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_visuala_pos_1), 'FC_visuala_pos_1'], d1b[!is.na(d1b$FC_visuala_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_visuala_pos_1), 'FC_visuala_pos_1'], d1b[!is.na(d1b$FC_visuala_pos_1), 'IPR']) #not
t.test(FC_visuala_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_visuala_pos_1), 'FC_visuala_pos_1'], d1b[!is.na(d1b$FC_visuala_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_visuala_pos_1), 'FC_visuala_pos_1'], d1b[!is.na(d1b$FC_visuala_pos_1), 'depression_1']) #not
FC_mod <- lm(FC_visuala_pos_1 ~ female, data = d1b) 

d1b[is.na(d1b$FC_visuala_pos_1), 'FC_visuala_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_visuala_pos_1), ])

# Visual B (2)
t.test(exp_visualb_pos_1 ~ black, data = d1b) #sig
t.test(exp_visualb_pos_1 ~ white, data = d1b) #sig
t.test(exp_visualb_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_visualb_pos_1), 'exp_visualb_pos_1'], d1b[!is.na(d1b$exp_visualb_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_visualb_pos_1), 'exp_visualb_pos_1'], d1b[!is.na(d1b$exp_visualb_pos_1), 'PubCat_1']) #sig
cor.test(d1b[!is.na(d1b$exp_visualb_pos_1), 'exp_visualb_pos_1'], d1b[!is.na(d1b$exp_visualb_pos_1), 'IPR']) #sig
t.test(exp_visualb_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$exp_visualb_pos_1), 'exp_visualb_pos_1'], d1b[!is.na(d1b$exp_visualb_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_visualb_pos_1), 'exp_visualb_pos_1'], d1b[!is.na(d1b$exp_visualb_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_visualb_pos_1 ~ black + white + PubCat_1 + IPR + female, data = d1b) #IPR, female
exp_mod <- lm(exp_visualb_pos_1 ~ IPR + female, data = d1b) 

d1b[is.na(d1b$exp_visualb_pos_1), 'exp_visualb_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_visualb_pos_1), ])

t.test(FC_visualb_pos_1 ~ black, data = d1b) #sig
t.test(FC_visualb_pos_1 ~ white, data = d1b) #sig
t.test(FC_visualb_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_visualb_pos_1), 'FC_visualb_pos_1'], d1b[!is.na(d1b$FC_visualb_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_visualb_pos_1), 'FC_visualb_pos_1'], d1b[!is.na(d1b$FC_visualb_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_visualb_pos_1), 'FC_visualb_pos_1'], d1b[!is.na(d1b$FC_visualb_pos_1), 'IPR']) #not
t.test(FC_visualb_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_visualb_pos_1), 'FC_visualb_pos_1'], d1b[!is.na(d1b$FC_visualb_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_visualb_pos_1), 'FC_visualb_pos_1'], d1b[!is.na(d1b$FC_visualb_pos_1), 'depression_1']) #not
FC_mod <- lm(FC_visualb_pos_1 ~ black + white + female, data = d1b) #
FC_mod <- lm(FC_visualb_pos_1 ~ white + female, data = d1b) 

d1b[is.na(d1b$FC_visualb_pos_1), 'FC_visualb_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_visualb_pos_1), ])


# Somatomotor A (3)
t.test(exp_somatomotora_pos_1 ~ black, data = d1b) #sig
t.test(exp_somatomotora_pos_1 ~ white, data = d1b) #sig
t.test(exp_somatomotora_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_somatomotora_pos_1), 'exp_somatomotora_pos_1'], d1b[!is.na(d1b$exp_somatomotora_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_somatomotora_pos_1), 'exp_somatomotora_pos_1'], d1b[!is.na(d1b$exp_somatomotora_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$exp_somatomotora_pos_1), 'exp_somatomotora_pos_1'], d1b[!is.na(d1b$exp_somatomotora_pos_1), 'IPR']) #sig
t.test(exp_somatomotora_pos_1 ~ female, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_somatomotora_pos_1), 'exp_somatomotora_pos_1'], d1b[!is.na(d1b$exp_somatomotora_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_somatomotora_pos_1), 'exp_somatomotora_pos_1'], d1b[!is.na(d1b$exp_somatomotora_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_somatomotora_pos_1 ~ black + white + IPR, data = d1b) #
exp_mod <- lm(exp_somatomotora_pos_1 ~ black + IPR, data = d1b) 

d1b[is.na(d1b$exp_somatomotora_pos_1), 'exp_somatomotora_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_somatomotora_pos_1), ])

t.test(FC_somatomotora_pos_1 ~ black, data = d1b) #not
t.test(FC_somatomotora_pos_1 ~ white, data = d1b) #sig
t.test(FC_somatomotora_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_somatomotora_pos_1), 'FC_somatomotora_pos_1'], d1b[!is.na(d1b$FC_somatomotora_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_somatomotora_pos_1), 'FC_somatomotora_pos_1'], d1b[!is.na(d1b$FC_somatomotora_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_somatomotora_pos_1), 'FC_somatomotora_pos_1'], d1b[!is.na(d1b$FC_somatomotora_pos_1), 'IPR']) #sig
t.test(FC_somatomotora_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_somatomotora_pos_1), 'FC_somatomotora_pos_1'], d1b[!is.na(d1b$FC_somatomotora_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_somatomotora_pos_1), 'FC_somatomotora_pos_1'], d1b[!is.na(d1b$FC_somatomotora_pos_1), 'depression_1']) #not
FC_mod <- lm(FC_somatomotora_pos_1 ~ white + IPR + female, data = d1b) #
FC_mod <- lm(FC_somatomotora_pos_1 ~ white, data = d1b) 

d1b[is.na(d1b$FC_somatomotora_pos_1), 'FC_somatomotora_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_somatomotora_pos_1), ])


# Somatomotor B (4)
t.test(exp_somatomotorb_pos_1 ~ black, data = d1b) #not
t.test(exp_somatomotorb_pos_1 ~ white, data = d1b) #sig
t.test(exp_somatomotorb_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_somatomotorb_pos_1), 'exp_somatomotorb_pos_1'], d1b[!is.na(d1b$exp_somatomotorb_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_somatomotorb_pos_1), 'exp_somatomotorb_pos_1'], d1b[!is.na(d1b$exp_somatomotorb_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$exp_somatomotorb_pos_1), 'exp_somatomotorb_pos_1'], d1b[!is.na(d1b$exp_somatomotorb_pos_1), 'IPR']) #sig
t.test(exp_somatomotorb_pos_1 ~ female, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_somatomotorb_pos_1), 'exp_somatomotorb_pos_1'], d1b[!is.na(d1b$exp_somatomotorb_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_somatomotorb_pos_1), 'exp_somatomotorb_pos_1'], d1b[!is.na(d1b$exp_somatomotorb_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_somatomotorb_pos_1 ~ white + IPR, data = d1b) #
exp_mod <- lm(exp_somatomotorb_pos_1 ~ IPR, data = d1b) 

d1b[is.na(d1b$exp_somatomotorb_pos_1), 'exp_somatomotorb_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_somatomotorb_pos_1), ])

t.test(FC_somatomotorb_pos_1 ~ black, data = d1b) #not
t.test(FC_somatomotorb_pos_1 ~ white, data = d1b) #not
t.test(FC_somatomotorb_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_somatomotorb_pos_1), 'FC_somatomotorb_pos_1'], d1b[!is.na(d1b$FC_somatomotorb_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_somatomotorb_pos_1), 'FC_somatomotorb_pos_1'], d1b[!is.na(d1b$FC_somatomotorb_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_somatomotorb_pos_1), 'FC_somatomotorb_pos_1'], d1b[!is.na(d1b$FC_somatomotorb_pos_1), 'IPR']) #sig
t.test(FC_somatomotorb_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_somatomotorb_pos_1), 'FC_somatomotorb_pos_1'], d1b[!is.na(d1b$FC_somatomotorb_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_somatomotorb_pos_1), 'FC_somatomotorb_pos_1'], d1b[!is.na(d1b$FC_somatomotorb_pos_1), 'depression_1']) #not
FC_mod <- lm(FC_somatomotorb_pos_1 ~ IPR + female, data = d1b) #
FC_mod <- lm(FC_somatomotorb_pos_1 ~ female, data = d1b) 

d1b[is.na(d1b$FC_somatomotorb_pos_1), 'FC_somatomotorb_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_somatomotorb_pos_1), ])


# Dorsal Attention A (5)
t.test(exp_dorsalattentiona_pos_1 ~ black, data = d1b) #sig
t.test(exp_dorsalattentiona_pos_1 ~ white, data = d1b) #sig
t.test(exp_dorsalattentiona_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_dorsalattentiona_pos_1), 'exp_dorsalattentiona_pos_1'], d1b[!is.na(d1b$exp_dorsalattentiona_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_dorsalattentiona_pos_1), 'exp_dorsalattentiona_pos_1'], d1b[!is.na(d1b$exp_dorsalattentiona_pos_1), 'PubCat_1']) #sig
cor.test(d1b[!is.na(d1b$exp_dorsalattentiona_pos_1), 'exp_dorsalattentiona_pos_1'], d1b[!is.na(d1b$exp_dorsalattentiona_pos_1), 'IPR']) #sig
t.test(exp_dorsalattentiona_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$exp_dorsalattentiona_pos_1), 'exp_dorsalattentiona_pos_1'], d1b[!is.na(d1b$exp_dorsalattentiona_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_dorsalattentiona_pos_1), 'exp_dorsalattentiona_pos_1'], d1b[!is.na(d1b$exp_dorsalattentiona_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_dorsalattentiona_pos_1 ~ black + white + PubCat_1 + IPR + female, data = d1b) #
exp_mod <- lm(exp_dorsalattentiona_pos_1 ~ black + IPR + female, data = d1b) 

d1b[is.na(d1b$exp_dorsalattentiona_pos_1), 'exp_dorsalattentiona_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_dorsalattentiona_pos_1), ])

t.test(FC_dorsalattentiona_pos_1 ~ black, data = d1b) #not
t.test(FC_dorsalattentiona_pos_1 ~ white, data = d1b) #sig
t.test(FC_dorsalattentiona_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_dorsalattentiona_pos_1), 'FC_dorsalattentiona_pos_1'], d1b[!is.na(d1b$FC_dorsalattentiona_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_dorsalattentiona_pos_1), 'FC_dorsalattentiona_pos_1'], d1b[!is.na(d1b$FC_dorsalattentiona_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_dorsalattentiona_pos_1), 'FC_dorsalattentiona_pos_1'], d1b[!is.na(d1b$FC_dorsalattentiona_pos_1), 'IPR']) #not
t.test(FC_dorsalattentiona_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_dorsalattentiona_pos_1), 'FC_dorsalattentiona_pos_1'], d1b[!is.na(d1b$FC_dorsalattentiona_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_dorsalattentiona_pos_1), 'FC_dorsalattentiona_pos_1'], d1b[!is.na(d1b$FC_dorsalattentiona_pos_1), 'depression_1']) #not
FC_mod <- lm(FC_dorsalattentiona_pos_1 ~ white + female, data = d1b) #

d1b[is.na(d1b$FC_dorsalattentiona_pos_1), 'FC_dorsalattentiona_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_dorsalattentiona_pos_1), ])


# Dorsal Attention B (6)
t.test(exp_dorsalattentionb_pos_1 ~ black, data = d1b) #sig
t.test(exp_dorsalattentionb_pos_1 ~ white, data = d1b) #sig
t.test(exp_dorsalattentionb_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_dorsalattentionb_pos_1), 'exp_dorsalattentionb_pos_1'], d1b[!is.na(d1b$exp_dorsalattentionb_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_dorsalattentionb_pos_1), 'exp_dorsalattentionb_pos_1'], d1b[!is.na(d1b$exp_dorsalattentionb_pos_1), 'PubCat_1']) #sig
cor.test(d1b[!is.na(d1b$exp_dorsalattentionb_pos_1), 'exp_dorsalattentionb_pos_1'], d1b[!is.na(d1b$exp_dorsalattentionb_pos_1), 'IPR']) #sig
t.test(exp_dorsalattentionb_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$exp_dorsalattentionb_pos_1), 'exp_dorsalattentionb_pos_1'], d1b[!is.na(d1b$exp_dorsalattentionb_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_dorsalattentionb_pos_1), 'exp_dorsalattentionb_pos_1'], d1b[!is.na(d1b$exp_dorsalattentionb_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_dorsalattentionb_pos_1 ~ black + white + PubCat_1 + IPR + female, data = d1b) #
exp_mod <- lm(exp_dorsalattentionb_pos_1 ~ black + IPR + female, data = d1b) 

d1b[is.na(d1b$exp_dorsalattentionb_pos_1), 'exp_dorsalattentionb_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_dorsalattentionb_pos_1), ])

t.test(FC_dorsalattentionb_pos_1 ~ black, data = d1b) #not
t.test(FC_dorsalattentionb_pos_1 ~ white, data = d1b) #not
t.test(FC_dorsalattentionb_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_dorsalattentionb_pos_1), 'FC_dorsalattentionb_pos_1'], d1b[!is.na(d1b$FC_dorsalattentionb_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_dorsalattentionb_pos_1), 'FC_dorsalattentionb_pos_1'], d1b[!is.na(d1b$FC_dorsalattentionb_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_dorsalattentionb_pos_1), 'FC_dorsalattentionb_pos_1'], d1b[!is.na(d1b$FC_dorsalattentionb_pos_1), 'IPR']) #not
t.test(FC_dorsalattentionb_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_dorsalattentionb_pos_1), 'FC_dorsalattentionb_pos_1'], d1b[!is.na(d1b$FC_dorsalattentionb_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_dorsalattentionb_pos_1), 'FC_dorsalattentionb_pos_1'], d1b[!is.na(d1b$FC_dorsalattentionb_pos_1), 'depression_1']) #not
FC_mod <- lm(FC_dorsalattentionb_pos_1 ~ female, data = d1b) #

d1b[is.na(d1b$FC_dorsalattentionb_pos_1), 'FC_dorsalattentionb_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_dorsalattentionb_pos_1), ])


# Salience A (7)
t.test(exp_saliencea_pos_1 ~ black, data = d1b) #sig
t.test(exp_saliencea_pos_1 ~ white, data = d1b) #sig
t.test(exp_saliencea_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_saliencea_pos_1), 'exp_saliencea_pos_1'], d1b[!is.na(d1b$exp_saliencea_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_saliencea_pos_1), 'exp_saliencea_pos_1'], d1b[!is.na(d1b$exp_saliencea_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$exp_saliencea_pos_1), 'exp_saliencea_pos_1'], d1b[!is.na(d1b$exp_saliencea_pos_1), 'IPR']) #sig
t.test(exp_saliencea_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$exp_saliencea_pos_1), 'exp_saliencea_pos_1'], d1b[!is.na(d1b$exp_saliencea_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_saliencea_pos_1), 'exp_saliencea_pos_1'], d1b[!is.na(d1b$exp_saliencea_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_saliencea_pos_1 ~ black + white + IPR + female, data = d1b) #
exp_mod <- lm(exp_saliencea_pos_1 ~ black + IPR, data = d1b) 

d1b[is.na(d1b$exp_saliencea_pos_1), 'exp_saliencea_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_saliencea_pos_1), ])

t.test(FC_saliencea_pos_1 ~ black, data = d1b) #not
t.test(FC_saliencea_pos_1 ~ white, data = d1b) #sig
t.test(FC_saliencea_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_saliencea_pos_1), 'FC_saliencea_pos_1'], d1b[!is.na(d1b$FC_saliencea_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_saliencea_pos_1), 'FC_saliencea_pos_1'], d1b[!is.na(d1b$FC_saliencea_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_saliencea_pos_1), 'FC_saliencea_pos_1'], d1b[!is.na(d1b$FC_saliencea_pos_1), 'IPR']) #sig
t.test(FC_saliencea_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_saliencea_pos_1), 'FC_saliencea_pos_1'], d1b[!is.na(d1b$FC_saliencea_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_saliencea_pos_1), 'FC_saliencea_pos_1'], d1b[!is.na(d1b$FC_saliencea_pos_1), 'depression_1']) #not
FC_mod <- lm(FC_saliencea_pos_1 ~ white + IPR + female, data = d1b) #
FC_mod <- lm(FC_saliencea_pos_1 ~ IPR + female, data = d1b) 

d1b[is.na(d1b$FC_saliencea_pos_1), 'FC_saliencea_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_saliencea_pos_1), ])


# Limbic A (9)
t.test(exp_limbica_pos_1 ~ black, data = d1b) #sig
t.test(exp_limbica_pos_1 ~ white, data = d1b) #sig
t.test(exp_limbica_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_limbica_pos_1), 'exp_limbica_pos_1'], d1b[!is.na(d1b$exp_limbica_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_limbica_pos_1), 'exp_limbica_pos_1'], d1b[!is.na(d1b$exp_limbica_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$exp_limbica_pos_1), 'exp_limbica_pos_1'], d1b[!is.na(d1b$exp_limbica_pos_1), 'IPR']) #sig
t.test(exp_limbica_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$exp_limbica_pos_1), 'exp_limbica_pos_1'], d1b[!is.na(d1b$exp_limbica_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_limbica_pos_1), 'exp_limbica_pos_1'], d1b[!is.na(d1b$exp_limbica_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_limbica_pos_1 ~ black + white + IPR + female, data = d1b) #
exp_mod <- lm(exp_limbica_pos_1 ~ black + IPR, data = d1b) 

d1b[is.na(d1b$exp_limbica_pos_1), 'exp_limbica_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_limbica_pos_1), ])

t.test(FC_limbica_pos_1 ~ black, data = d1b) #not
t.test(FC_limbica_pos_1 ~ white, data = d1b) #not
t.test(FC_limbica_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_limbica_pos_1), 'FC_limbica_pos_1'], d1b[!is.na(d1b$FC_limbica_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_limbica_pos_1), 'FC_limbica_pos_1'], d1b[!is.na(d1b$FC_limbica_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_limbica_pos_1), 'FC_limbica_pos_1'], d1b[!is.na(d1b$FC_limbica_pos_1), 'IPR']) #not
t.test(FC_limbica_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_limbica_pos_1), 'FC_limbica_pos_1'], d1b[!is.na(d1b$FC_limbica_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_limbica_pos_1), 'FC_limbica_pos_1'], d1b[!is.na(d1b$FC_limbica_pos_1), 'depression_1']) #not
FC_mod <- lm(FC_limbica_pos_1 ~ female, data = d1b) #

d1b[is.na(d1b$FC_limbica_pos_1), 'FC_limbica_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_limbica_pos_1), ])


# Limbic B (10)
t.test(exp_limbicb_pos_1 ~ black, data = d1b) #sig
t.test(exp_limbicb_pos_1 ~ white, data = d1b) #sig
t.test(exp_limbicb_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_limbicb_pos_1), 'exp_limbicb_pos_1'], d1b[!is.na(d1b$exp_limbicb_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_limbicb_pos_1), 'exp_limbicb_pos_1'], d1b[!is.na(d1b$exp_limbicb_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$exp_limbicb_pos_1), 'exp_limbicb_pos_1'], d1b[!is.na(d1b$exp_limbicb_pos_1), 'IPR']) #sig
t.test(exp_limbicb_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$exp_limbicb_pos_1), 'exp_limbicb_pos_1'], d1b[!is.na(d1b$exp_limbicb_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_limbicb_pos_1), 'exp_limbicb_pos_1'], d1b[!is.na(d1b$exp_limbicb_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_limbicb_pos_1 ~ black + white + IPR + female, data = d1b) #
exp_mod <- lm(exp_limbicb_pos_1 ~ black + female, data = d1b) 

d1b[is.na(d1b$exp_limbicb_pos_1), 'exp_limbicb_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_limbicb_pos_1), ])

t.test(FC_limbicb_pos_1 ~ black, data = d1b) #not
t.test(FC_limbicb_pos_1 ~ white, data = d1b) #not
t.test(FC_limbicb_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_limbicb_pos_1), 'FC_limbicb_pos_1'], d1b[!is.na(d1b$FC_limbicb_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_limbicb_pos_1), 'FC_limbicb_pos_1'], d1b[!is.na(d1b$FC_limbicb_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_limbicb_pos_1), 'FC_limbicb_pos_1'], d1b[!is.na(d1b$FC_limbicb_pos_1), 'IPR']) #not
t.test(FC_limbicb_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_limbicb_pos_1), 'FC_limbicb_pos_1'], d1b[!is.na(d1b$FC_limbicb_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_limbicb_pos_1), 'FC_limbicb_pos_1'], d1b[!is.na(d1b$FC_limbicb_pos_1), 'depression_1']) #not
FC_mod <- lm(FC_limbicb_pos_1 ~ female, data = d1b) #

d1b[is.na(d1b$FC_limbicb_pos_1), 'FC_limbicb_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_limbicb_pos_1), ])


# Control A (11)
t.test(exp_controla_pos_1 ~ black, data = d1b) #sig
t.test(exp_controla_pos_1 ~ white, data = d1b) #sig
t.test(exp_controla_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_controla_pos_1), 'exp_controla_pos_1'], d1b[!is.na(d1b$exp_controla_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_controla_pos_1), 'exp_controla_pos_1'], d1b[!is.na(d1b$exp_controla_pos_1), 'PubCat_1']) #sig
cor.test(d1b[!is.na(d1b$exp_controla_pos_1), 'exp_controla_pos_1'], d1b[!is.na(d1b$exp_controla_pos_1), 'IPR']) #sig
t.test(exp_controla_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$exp_controla_pos_1), 'exp_controla_pos_1'], d1b[!is.na(d1b$exp_controla_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_controla_pos_1), 'exp_controla_pos_1'], d1b[!is.na(d1b$exp_controla_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_controla_pos_1 ~ black + white + PubCat_1 + IPR + female, data = d1b) #
exp_mod <- lm(exp_controla_pos_1 ~ black + IPR + female, data = d1b) 

d1b[is.na(d1b$exp_controla_pos_1), 'exp_controla_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_controla_pos_1), ])

t.test(FC_controla_pos_1 ~ black, data = d1b) #not
t.test(FC_controla_pos_1 ~ white, data = d1b) #not
t.test(FC_controla_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_controla_pos_1), 'FC_controla_pos_1'], d1b[!is.na(d1b$FC_controla_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_controla_pos_1), 'FC_controla_pos_1'], d1b[!is.na(d1b$FC_controla_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_controla_pos_1), 'FC_controla_pos_1'], d1b[!is.na(d1b$FC_controla_pos_1), 'IPR']) #not
t.test(FC_controla_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_controla_pos_1), 'FC_controla_pos_1'], d1b[!is.na(d1b$FC_controla_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_controla_pos_1), 'FC_controla_pos_1'], d1b[!is.na(d1b$FC_controla_pos_1), 'depression_1']) #not
FC_mod <- lm(FC_controla_pos_1 ~ female, data = d1b) #

d1b[is.na(d1b$FC_controla_pos_1), 'FC_controla_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_controla_pos_1), ])


# Control B (12)
t.test(exp_controlb_pos_1 ~ black, data = d1b) #sig
t.test(exp_controlb_pos_1 ~ white, data = d1b) #sig
t.test(exp_controlb_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_controlb_pos_1), 'exp_controlb_pos_1'], d1b[!is.na(d1b$exp_controlb_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_controlb_pos_1), 'exp_controlb_pos_1'], d1b[!is.na(d1b$exp_controlb_pos_1), 'PubCat_1']) #sig
cor.test(d1b[!is.na(d1b$exp_controlb_pos_1), 'exp_controlb_pos_1'], d1b[!is.na(d1b$exp_controlb_pos_1), 'IPR']) #sig
t.test(exp_controlb_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$exp_controlb_pos_1), 'exp_controlb_pos_1'], d1b[!is.na(d1b$exp_controlb_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_controlb_pos_1), 'exp_controlb_pos_1'], d1b[!is.na(d1b$exp_controlb_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_controlb_pos_1 ~ black + white + PubCat_1 + IPR + female, data = d1b) #
exp_mod <- lm(exp_controlb_pos_1 ~ black + IPR, data = d1b) 

d1b[is.na(d1b$exp_controlb_pos_1), 'exp_controlb_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_controlb_pos_1), ])

t.test(FC_controlb_pos_1 ~ black, data = d1b) #not
t.test(FC_controlb_pos_1 ~ white, data = d1b) #not
t.test(FC_controlb_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_controlb_pos_1), 'FC_controlb_pos_1'], d1b[!is.na(d1b$FC_controlb_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_controlb_pos_1), 'FC_controlb_pos_1'], d1b[!is.na(d1b$FC_controlb_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_controlb_pos_1), 'FC_controlb_pos_1'], d1b[!is.na(d1b$FC_controlb_pos_1), 'IPR']) #not
t.test(FC_controlb_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_controlb_pos_1), 'FC_controlb_pos_1'], d1b[!is.na(d1b$FC_controlb_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_controlb_pos_1), 'FC_controlb_pos_1'], d1b[!is.na(d1b$FC_controlb_pos_1), 'depression_1']) #not
FC_mod <- lm(FC_controlb_pos_1 ~ female, data = d1b) #

d1b[is.na(d1b$FC_controlb_pos_1), 'FC_controlb_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_controlb_pos_1), ])


# Control C (13)
t.test(exp_controlc_pos_1 ~ black, data = d1b) #sig
t.test(exp_controlc_pos_1 ~ white, data = d1b) #sig
t.test(exp_controlc_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_controlc_pos_1), 'exp_controlc_pos_1'], d1b[!is.na(d1b$exp_controlc_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_controlc_pos_1), 'exp_controlc_pos_1'], d1b[!is.na(d1b$exp_controlc_pos_1), 'PubCat_1']) #sig
cor.test(d1b[!is.na(d1b$exp_controlc_pos_1), 'exp_controlc_pos_1'], d1b[!is.na(d1b$exp_controlc_pos_1), 'IPR']) #sig
t.test(exp_controlc_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$exp_controlc_pos_1), 'exp_controlc_pos_1'], d1b[!is.na(d1b$exp_controlc_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_controlc_pos_1), 'exp_controlc_pos_1'], d1b[!is.na(d1b$exp_controlc_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_controlc_pos_1 ~ black + white + PubCat_1 + IPR + female, data = d1b) #
exp_mod <- lm(exp_controlc_pos_1 ~ black + IPR + female, data = d1b) 

d1b[is.na(d1b$exp_controlc_pos_1), 'exp_controlc_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_controlc_pos_1), ])

t.test(FC_controlc_pos_1 ~ black, data = d1b) #not
t.test(FC_controlc_pos_1 ~ white, data = d1b) #sig
t.test(FC_controlc_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_controlc_pos_1), 'FC_controlc_pos_1'], d1b[!is.na(d1b$FC_controlc_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_controlc_pos_1), 'FC_controlc_pos_1'], d1b[!is.na(d1b$FC_controlc_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_controlc_pos_1), 'FC_controlc_pos_1'], d1b[!is.na(d1b$FC_controlc_pos_1), 'IPR']) #sig
t.test(FC_controlc_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_controlc_pos_1), 'FC_controlc_pos_1'], d1b[!is.na(d1b$FC_controlc_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_controlc_pos_1), 'FC_controlc_pos_1'], d1b[!is.na(d1b$FC_controlc_pos_1), 'depression_1']) #not
FC_mod <- lm(FC_controlc_pos_1 ~ white + IPR + female, data = d1b) #
FC_mod <- lm(FC_controlc_pos_1 ~ female, data = d1b) 

d1b[is.na(d1b$FC_controlc_pos_1), 'FC_controlc_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_controlc_pos_1), ])


# Default A (14)
t.test(exp_defaulta_pos_1 ~ black, data = d1b) #sig
t.test(exp_defaulta_pos_1 ~ white, data = d1b) #sig
t.test(exp_defaulta_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_defaulta_pos_1), 'exp_defaulta_pos_1'], d1b[!is.na(d1b$exp_defaulta_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_defaulta_pos_1), 'exp_defaulta_pos_1'], d1b[!is.na(d1b$exp_defaulta_pos_1), 'PubCat_1']) #sig
cor.test(d1b[!is.na(d1b$exp_defaulta_pos_1), 'exp_defaulta_pos_1'], d1b[!is.na(d1b$exp_defaulta_pos_1), 'IPR']) #sig
t.test(exp_defaulta_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$exp_defaulta_pos_1), 'exp_defaulta_pos_1'], d1b[!is.na(d1b$exp_defaulta_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_defaulta_pos_1), 'exp_defaulta_pos_1'], d1b[!is.na(d1b$exp_defaulta_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_defaulta_pos_1 ~ black + white + PubCat_1 + IPR + female, data = d1b) #
exp_mod <- lm(exp_defaulta_pos_1 ~ black + IPR, data = d1b) 

d1b[is.na(d1b$exp_defaulta_pos_1), 'exp_defaulta_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_defaulta_pos_1), ])

t.test(FC_defaulta_pos_1 ~ black, data = d1b) #not
t.test(FC_defaulta_pos_1 ~ white, data = d1b) #not
t.test(FC_defaulta_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_defaulta_pos_1), 'FC_defaulta_pos_1'], d1b[!is.na(d1b$FC_defaulta_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_defaulta_pos_1), 'FC_defaulta_pos_1'], d1b[!is.na(d1b$FC_defaulta_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_defaulta_pos_1), 'FC_defaulta_pos_1'], d1b[!is.na(d1b$FC_defaulta_pos_1), 'IPR']) #not
t.test(FC_defaulta_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_defaulta_pos_1), 'FC_defaulta_pos_1'], d1b[!is.na(d1b$FC_defaulta_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_defaulta_pos_1), 'FC_defaulta_pos_1'], d1b[!is.na(d1b$FC_defaulta_pos_1), 'depression_1']) #not
FC_mod <- lm(FC_defaulta_pos_1 ~ female, data = d1b) #

d1b[is.na(d1b$FC_defaulta_pos_1), 'FC_defaulta_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_defaulta_pos_1), ])


# Default B (15)
t.test(exp_defaultb_pos_1 ~ black, data = d1b) #sig
t.test(exp_defaultb_pos_1 ~ white, data = d1b) #sig
t.test(exp_defaultb_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_defaultb_pos_1), 'exp_defaultb_pos_1'], d1b[!is.na(d1b$exp_defaultb_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_defaultb_pos_1), 'exp_defaultb_pos_1'], d1b[!is.na(d1b$exp_defaultb_pos_1), 'PubCat_1']) #sig
cor.test(d1b[!is.na(d1b$exp_defaultb_pos_1), 'exp_defaultb_pos_1'], d1b[!is.na(d1b$exp_defaultb_pos_1), 'IPR']) #sig
t.test(exp_defaultb_pos_1 ~ female, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_defaultb_pos_1), 'exp_defaultb_pos_1'], d1b[!is.na(d1b$exp_defaultb_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_defaultb_pos_1), 'exp_defaultb_pos_1'], d1b[!is.na(d1b$exp_defaultb_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_defaultb_pos_1 ~ black + white + PubCat_1 + IPR, data = d1b) #
exp_mod <- lm(exp_defaultb_pos_1 ~ black + PubCat_1 + IPR, data = d1b) 

d1b[is.na(d1b$exp_defaultb_pos_1), 'exp_defaultb_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_defaultb_pos_1), ])

t.test(FC_defaultb_pos_1 ~ black, data = d1b) #not
t.test(FC_defaultb_pos_1 ~ white, data = d1b) #not
t.test(FC_defaultb_pos_1 ~ hispanic, data = d1b) #
cor.test(d1b[!is.na(d1b$FC_defaultb_pos_1), 'FC_defaultb_pos_1'], d1b[!is.na(d1b$FC_defaultb_pos_1), 'BMIperc_1']) #
cor.test(d1b[!is.na(d1b$FC_defaultb_pos_1), 'FC_defaultb_pos_1'], d1b[!is.na(d1b$FC_defaultb_pos_1), 'PubCat_1']) #
cor.test(d1b[!is.na(d1b$FC_defaultb_pos_1), 'FC_defaultb_pos_1'], d1b[!is.na(d1b$FC_defaultb_pos_1), 'IPR']) #
t.test(FC_defaultb_pos_1 ~ female, data = d1b) #
cor.test(d1b[!is.na(d1b$FC_defaultb_pos_1), 'FC_defaultb_pos_1'], d1b[!is.na(d1b$FC_defaultb_pos_1), 'age_lab_1']) #
cor.test(d1b[!is.na(d1b$FC_defaultb_pos_1), 'FC_defaultb_pos_1'], d1b[!is.na(d1b$FC_defaultb_pos_1), 'depression_1']) #
FC_mod <- lm(FC_defaultb_pos_1 ~ , data = d1b) #
FC_mod <- lm(FC_defaultb_pos_1 ~ , data = d1b) 

d1b[is.na(d1b$FC_defaultb_pos_1), 'FC_defaultb_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_defaultb_pos_1), ])


# Default C (16)
t.test(exp_defaultc_pos_1 ~ black, data = d1b) #sig
t.test(exp_defaultc_pos_1 ~ white, data = d1b) #sig
t.test(exp_defaultc_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_defaultc_pos_1), 'exp_defaultc_pos_1'], d1b[!is.na(d1b$exp_defaultc_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_defaultc_pos_1), 'exp_defaultc_pos_1'], d1b[!is.na(d1b$exp_defaultc_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$exp_defaultc_pos_1), 'exp_defaultc_pos_1'], d1b[!is.na(d1b$exp_defaultc_pos_1), 'IPR']) #sig
t.test(exp_defaultc_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$exp_defaultc_pos_1), 'exp_defaultc_pos_1'], d1b[!is.na(d1b$exp_defaultc_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_defaultc_pos_1), 'exp_defaultc_pos_1'], d1b[!is.na(d1b$exp_defaultc_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_defaultc_pos_1 ~ black + white + IPR + female, data = d1b) #
exp_mod <- lm(exp_defaultc_pos_1 ~ black + IPR + female, data = d1b) 

d1b[is.na(d1b$exp_defaultc_pos_1), 'exp_defaultc_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_defaultc_pos_1), ])

t.test(FC_defaultc_pos_1 ~ black, data = d1b) #not
t.test(FC_defaultc_pos_1 ~ white, data = d1b) #not
t.test(FC_defaultc_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_defaultc_pos_1), 'FC_defaultc_pos_1'], d1b[!is.na(d1b$FC_defaultc_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_defaultc_pos_1), 'FC_defaultc_pos_1'], d1b[!is.na(d1b$FC_defaultc_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_defaultc_pos_1), 'FC_defaultc_pos_1'], d1b[!is.na(d1b$FC_defaultc_pos_1), 'IPR']) #sig
t.test(FC_defaultc_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_defaultc_pos_1), 'FC_defaultc_pos_1'], d1b[!is.na(d1b$FC_defaultc_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_defaultc_pos_1), 'FC_defaultc_pos_1'], d1b[!is.na(d1b$FC_defaultc_pos_1), 'depression_1']) #not
FC_mod <- lm(FC_defaultc_pos_1 ~ IPR + female, data = d1b) #
FC_mod <- lm(FC_defaultc_pos_1 ~ female, data = d1b) 

d1b[is.na(d1b$FC_defaultc_pos_1), 'FC_defaultc_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_defaultc_pos_1), ])


# Temporal Parietal (17)
t.test(exp_temporalparietal_pos_1 ~ black, data = d1b) #sig
t.test(exp_temporalparietal_pos_1 ~ white, data = d1b) #sig
t.test(exp_temporalparietal_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_temporalparietal_pos_1), 'exp_temporalparietal_pos_1'], d1b[!is.na(d1b$exp_temporalparietal_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_temporalparietal_pos_1), 'exp_temporalparietal_pos_1'], d1b[!is.na(d1b$exp_temporalparietal_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$exp_temporalparietal_pos_1), 'exp_temporalparietal_pos_1'], d1b[!is.na(d1b$exp_temporalparietal_pos_1), 'IPR']) #sig
t.test(exp_temporalparietal_pos_1 ~ female, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_temporalparietal_pos_1), 'exp_temporalparietal_pos_1'], d1b[!is.na(d1b$exp_temporalparietal_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_temporalparietal_pos_1), 'exp_temporalparietal_pos_1'], d1b[!is.na(d1b$exp_temporalparietal_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_temporalparietal_pos_1 ~ black + white + IPR, data = d1b) #
exp_mod <- lm(exp_temporalparietal_pos_1 ~ black + IPR, data = d1b) 

d1b[is.na(d1b$exp_temporalparietal_pos_1), 'exp_temporalparietal_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_temporalparietal_pos_1), ])

t.test(FC_temporalparietal_pos_1 ~ black, data = d1b) #not
t.test(FC_temporalparietal_pos_1 ~ white, data = d1b) #sig
t.test(FC_temporalparietal_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_temporalparietal_pos_1), 'FC_temporalparietal_pos_1'], d1b[!is.na(d1b$FC_temporalparietal_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_temporalparietal_pos_1), 'FC_temporalparietal_pos_1'], d1b[!is.na(d1b$FC_temporalparietal_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_temporalparietal_pos_1), 'FC_temporalparietal_pos_1'], d1b[!is.na(d1b$FC_temporalparietal_pos_1), 'IPR']) #not
t.test(FC_temporalparietal_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_temporalparietal_pos_1), 'FC_temporalparietal_pos_1'], d1b[!is.na(d1b$FC_temporalparietal_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_temporalparietal_pos_1), 'FC_temporalparietal_pos_1'], d1b[!is.na(d1b$FC_temporalparietal_pos_1), 'depression_1']) #not
FC_mod <- lm(FC_temporalparietal_pos_1 ~ white + female, data = d1b) #

d1b[is.na(d1b$FC_temporalparietal_pos_1), 'FC_temporalparietal_pos_1'] <- predict(FC_mod, d1b[is.na(d1b$FC_temporalparietal_pos_1), ])

d1b <- d1b[, !(names(d1b) %in% c('IPR', 'black', 'white', 'hispanic', 'female'))]
d2b <- d2b[, !(names(d2b) %in% c('BMIperc_1', 'PubCat_1'))]
df <- merge(d1b, d2b) #keep the subjects that have complete session 2 data

##### (c) Controlling for dependent variables measured at T1
vars <- c('depression_2', 'num_pastyear_2', 'age_mri_2', 'depression_1', 'BMIperc_1', 'PubCat_1', 
            names(df)[grep('FC', names(df))], names(df)[grep('exp', names(df))]) #TO DO: Will need to scale all of the neuroimaging variables
df[, vars] <- scale(df[, vars])

# Visual A (1)
c_exp_visuala_viol <- lm(exp_visuala_pos_2 ~ num_pastyear_2*female + exp_visuala_pos_1, d=df) #sig: exp_visuala_pos_1 
c_FC_visuala_viol <- lm(FC_visuala_pos_2 ~ num_pastyear_2*female + FC_visuala_pos_1, d=df) #sig: FC_visuala_pos_1
c_depression_exp_visuala <- lm(depression_2 ~ exp_visuala_pos_2*female + depression_1, d=df) #sig: depression_1
c_depression_FC_visuala <- lm(depression_2 ~ FC_visuala_pos_2*female + depression_1, d=df) #sig: depression_1

# Visual B (2)
c_exp_visualb_viol <- lm(exp_visualb_pos_2 ~ num_pastyear_2*female + exp_visualb_pos_1, d=df) #sig: exp_visualb_pos_1
c_FC_visualb_viol <- lm(FC_visualb_pos_2 ~ num_pastyear_2*female + FC_visualb_pos_1, d=df) #sig: FC_visualb_pos_1
c_depression_exp_visualb <- lm(depression_2 ~ exp_visualb_pos_2*female + depression_1, d=df) #sig: depression_1
c_depression_FC_visualb <- lm(depression_2 ~ FC_visualb_pos_2*female + depression_1, d=df) #sig: depression_1

# Somatomotor A (3)
c_exp_somatomotora_viol <- lm(exp_somatomotora_pos_2 ~ num_pastyear_2*female + exp_somatomotora_pos_1, d=df) #sig: exp_somatomotora_pos_1
c_FC_somatomotora_viol <- lm(FC_somatomotora_pos_2 ~ num_pastyear_2*female + FC_somatomotora_pos_1, d=df) #sig: female, FC_somatomotora_pos_1
c_depression_exp_somatomotora <- lm(depression_2 ~ exp_somatomotora_pos_2*female + depression_1, d=df) #sig: depression_1
c_depression_FC_somatomotora <- lm(depression_2 ~ FC_somatomotora_pos_2*female + depression_1, d=df) #sig: FC_somatomotora_pos_2, female, FC_somatomotora_pos_2:female, depression_1

# Somatomotor B (4)
c_exp_somatomotorb_viol <- lm(exp_somatomotorb_pos_2 ~ num_pastyear_2*female + exp_somatomotorb_pos_1, d=df) #sig: female, exp_somatomotorb_pos_1
c_FC_somatomotorb_viol <- lm(FC_somatomotorb_pos_2 ~ num_pastyear_2*female + FC_somatomotorb_pos_1, d=df) #sig: female, FC_somatomotorb_pos_1
c_depression_exp_somatomotorb <- lm(depression_2 ~ exp_somatomotorb_pos_2*female + depression_1, d=df) #sig: depression_1
c_depression_FC_somatomotorb <- lm(depression_2 ~ FC_somatomotorb_pos_2*female + depression_1, d=df) #sig: FC_somatomotorb_pos_2, FC_somatomotorb_pos_2:female, depression_1

# Dorsal Attention A (5)
c_exp_dorsalattentiona_viol <- lm(exp_dorsalattentiona_pos_2 ~ num_pastyear_2*female + exp_dorsalattentiona_pos_1, d=df) #sig: exp_dorsalattentiona_pos_1
c_FC_dorsalattentiona_viol <- lm(FC_dorsalattentiona_pos_2 ~ num_pastyear_2*female + FC_dorsalattentiona_pos_1, d=df) #sig: female, FC_dorsalattentiona_pos_1
c_depression_exp_dorsalattentiona <- lm(depression_2 ~ exp_dorsalattentiona_pos_2*female + depression_1, d=df) #sig: depression_1
c_depression_FC_dorsalattentiona <- lm(depression_2 ~ FC_dorsalattentiona_pos_2*female + depression_1, d=df) #sig: depression_1

# Dorsal Attention B (6)
c_exp_dorsalattentionb_viol <- lm(exp_dorsalattentionb_pos_2 ~ num_pastyear_2*female + exp_dorsalattentionb_pos_1, d=df) #sig: exp_dorsalattentionb_pos_1
c_FC_dorsalattentionb_viol <- lm(FC_dorsalattentionb_pos_2 ~ num_pastyear_2*female + FC_dorsalattentionb_pos_1, d=df) #sig: female, FC_dorsalattentionb_pos_1
c_depression_exp_dorsalattentionb <- lm(depression_2 ~ exp_dorsalattentionb_pos_2*female + depression_1, d=df) #sig: depression_1
c_depression_FC_dorsalattentionb <- lm(depression_2 ~ FC_dorsalattentionb_pos_2*female + depression_1, d=df) #sig: FC_dorsalattentionb_pos_2, female, FC_dorsalattentionb_pos_2:female + depression_1

# Salience A (7)
c_exp_saliencea_viol <- lm(exp_saliencea_pos_2 ~ num_pastyear_2*female + exp_saliencea_pos_1, d=df) #sig: exp_saliencea_pos_1
c_FC_saliencea_viol <- lm(FC_saliencea_pos_2 ~ num_pastyear_2*female + FC_saliencea_pos_1, d=df) #sig: female, FC_saliencea_pos_1
c_depression_exp_saliencea <- lm(depression_2 ~ exp_saliencea_pos_2*female + depression_1, d=df) #sig: depression_1
c_depression_FC_saliencea <- lm(depression_2 ~ FC_saliencea_pos_2*female + depression_1, d=df) #sig: FC_saliencea_pos_2, FC_saliencea_pos_2:female, depression_1

# Limbic A (9)
c_exp_limbica_viol <- lm(exp_limbica_pos_2 ~ num_pastyear_2*female + exp_limbica_pos_1, d=df) #sig: exp_limbica_pos_1
c_FC_limbica_viol <- lm(FC_limbica_pos_2 ~ num_pastyear_2*female + FC_limbica_pos_1, d=df) #sig: female, FC_limbica_pos_1
c_depression_exp_limbica <- lm(depression_2 ~ exp_limbica_pos_2*female + depression_1, d=df) #sig: depression_1
c_depression_FC_limbica <- lm(depression_2 ~ FC_limbica_pos_2*female + depression_1, d=df) #sig: FC_limbica_pos_2, female, FC_limbica_pos_2:female + depression_1

# Limbic B (10)
c_exp_limbicb_viol <- lm(exp_limbicb_pos_2 ~ num_pastyear_2*female + exp_limbicb_pos_1, d=df) #sig: exp_limbicb_pos_1, num_pastyear_2:female
c_FC_limbicb_viol <- lm(FC_limbicb_pos_2 ~ num_pastyear_2*female + FC_limbicb_pos_1, d=df) #sig: female, FC_limbicb_pos_1
c_depression_exp_limbicb <- lm(depression_2 ~ exp_limbicb_pos_2*female + depression_1, d=df) #sig: depression_1
c_depression_FC_limbicb <- lm(depression_2 ~ FC_limbicb_pos_2*female + depression_1, d=df) #sig: FC_limbicb_pos_2, female, FC_limbicb_pos_2:female + depression_1

# Control A (11)
c_exp_controla_viol <- lm(exp_controla_pos_2 ~ num_pastyear_2*female + exp_controla_pos_1, d=df) #sig: exp_controla_pos_1
c_FC_controla_viol <- lm(FC_controla_pos_2 ~ num_pastyear_2*female + FC_controla_pos_1, d=df) #sig: female, FC_controla_pos_1 
c_depression_exp_controla <- lm(depression_2 ~ exp_controla_pos_2*female + depression_1, d=df) #sig: depression_1 
c_depression_FC_controla <- lm(depression_2 ~ FC_controla_pos_2*female + depression_1, d=df) #sig: FC_controla_pos_2, female, FC_controla_pos_2:female, depression_1

# Control B (12)
c_exp_controlb_viol <- lm(exp_controlb_pos_2 ~ num_pastyear_2*female + exp_controlb_pos_1, d=df) #sig: exp_controlb_pos_1
c_FC_controlb_viol <- lm(FC_controlb_pos_2 ~ num_pastyear_2*female + FC_controlb_pos_1, d=df) #sig: female, FC_controlb_pos_1
c_depression_exp_controlb <- lm(depression_2 ~ exp_controlb_pos_2*female + depression_1, d=df) #sig: depression_1
c_depression_FC_controlb <- lm(depression_2 ~ FC_controlb_pos_2*female + depression_1, d=df) #sig: FC_controlb_pos_2, female, FC_controlb_pos_2:female, depression_1

# Control C (13)
c_exp_controlc_viol <- lm(exp_controlc_pos_2 ~ num_pastyear_2*female + exp_controlc_pos_1, d=df) #sig: exp_controlc_pos_1
c_FC_controlc_viol <- lm(FC_controlc_pos_2 ~ num_pastyear_2*female + FC_controlc_pos_1, d=df) #sig: female, FC_controlc_pos_1
c_depression_exp_controlc <- lm(depression_2 ~ exp_controlc_pos_2*female + depression_1, d=df) #sig: depression_1
c_depression_FC_controlc <- lm(depression_2 ~ FC_controlc_pos_2*female + depression_1, d=df) #sig: FC_controlc_pos_2, female, FC_controlc_pos_2:female, depression_1

# Default A (14)
c_exp_defaulta_viol <- lm(exp_defaulta_pos_2 ~ num_pastyear_2*female + exp_defaulta_pos_1, d=df) #sig: exp_defaulta_pos_1 
c_FC_defaulta_viol <- lm(FC_defaulta_pos_2 ~ num_pastyear_2*female + FC_defaulta_pos_1, d=df) #sig: female, FC_defaulta_pos_1
c_depression_exp_defaulta <- lm(depression_2 ~ exp_defaulta_pos_2*female + depression_1, d=df) #sig: depression_1
c_depression_FC_defaulta <- lm(depression_2 ~ FC_defaulta_pos_2*female + depression_1, d=df) #sig: FC_defaulta_pos_2, FC_defaulta_pos_2:female, depression_1

# Default B (15)
c_exp_defaultb_viol <- lm(exp_defaultb_pos_2 ~ num_pastyear_2*female + exp_defaultb_pos_1, d=df) #sig: exp_defaultb_pos_1
c_FC_defaultb_viol <- lm(FC_defaultb_pos_2 ~ num_pastyear_2*female + FC_defaultb_pos_1, d=df) #sig: female, FC_defaultb_pos_1
c_depression_exp_defaultb <- lm(depression_2 ~ exp_defaultb_pos_2*female + depression_1, d=df) #sig: depression_1
c_depression_FC_defaultb <- lm(depression_2 ~ FC_defaultb_pos_2*female + depression_1, d=df) #sig: FC_defaultb_pos_2, female, FC_defaultb_pos_2:female, depression_1

# Default C (16)
c_exp_defaultc_viol <- lm(exp_defaultc_pos_2 ~ num_pastyear_2*female + exp_defaultc_pos_1, d=df) #sig: exp_defaultc_pos_1
c_FC_defaultc_viol <- lm(FC_defaultc_pos_2 ~ num_pastyear_2*female + FC_defaultc_pos_1, d=df) #sig: female, FC_defaultc_pos_1
c_depression_exp_defaultc <- lm(depression_2 ~ exp_defaultc_pos_2*female + depression_1, d=df) #sig: depression_1
c_depression_FC_defaultc <- lm(depression_2 ~ FC_defaultc_pos_2*female + depression_1, d=df) #sig: FC_defaultc_pos_2, FC_defaultc_pos_2:female, depression_1

# Temporal Parietal (17)
c_exp_temporalparietal_viol <- lm(exp_temporalparietal_pos_2 ~ num_pastyear_2*female + exp_temporalparietal_pos_1, d=df) #sig: exp_temporalparietal_pos_1
c_FC_temporalparietal_viol <- lm(FC_temporalparietal_pos_2 ~ num_pastyear_2*female + FC_temporalparietal_pos_1, d=df) #sig: female, FC_temporalparietal_pos_1
c_depression_exp_temporalparietal <- lm(depression_2 ~ exp_temporalparietal_pos_2*female + depression_1, d=df) #sig: depression_1
c_depression_FC_temporalparietal <- lm(depression_2 ~ FC_temporalparietal_pos_2*female + depression_1, d=df) #sig: FC_temporalparietal_pos_2, female, FC_temporalparietal_pos_2:female, depression_1

##### (d) Controlling for dependent variables measured at T1 and demographics

# Visual A (1)
d_exp_visuala_viol <- lm(exp_visuala_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_visuala_pos_1, d=df) #sig: exp_visuala_pos_1
d_FC_visuala_viol <- lm(FC_visuala_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_visuala_pos_1, d=df) #sig: FC_visuala_pos_1
d_depression_exp_visuala <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_visuala_pos_2*female + depression_1, d=df) #sig: depression_1
d_depression_FC_visuala <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_visuala_pos_2*female + depression_1, d=df) #sig: depression_1

# Visual B (2)
d_exp_visualb_viol <- lm(exp_visualb_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_visualb_pos_1, d=df) #sig: exp_visualb_pos_1
d_FC_visualb_viol <- lm(FC_visualb_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_visualb_pos_1, d=df) #sig: FC_visualb_pos_1
d_depression_exp_visualb <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_visualb_pos_2*female + depression_1, d=df) #sig: depression_1
d_depression_FC_visualb <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_visualb_pos_2*female + depression_1, d=df) #sig: depression_1

# Somatomotor A (3)
d_exp_somatomotora_viol <- lm(exp_somatomotora_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_somatomotora_pos_1, d=df) #sig: exp_somatomotora_pos_1
d_FC_somatomotora_viol <- lm(FC_somatomotora_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_somatomotora_pos_1, d=df) #sig: female, FC_somatomotora_pos_1
d_depression_exp_somatomotora <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_somatomotora_pos_2*female + depression_1, d=df) #sig: depression_1
d_depression_FC_somatomotora <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_somatomotora_pos_2*female + depression_1, d=df) #sig: depression_1

# Somatomotor B (4)
d_exp_somatomotorb_viol <- lm(exp_somatomotorb_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_somatomotorb_pos_1, d=df) #sig: exp_somatomotorb_pos_1
d_FC_somatomotorb_viol <- lm(FC_somatomotorb_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_somatomotorb_pos_1, d=df) #sig: FC_somatomotorb_pos_1
d_depression_exp_somatomotorb <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_somatomotorb_pos_2*female + depression_1, d=df) #sig: depression_1
d_depression_FC_somatomotorb <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_somatomotorb_pos_2*female + depression_1, d=df) #sig: depression_1

# Dorsal Attention A (5)
d_exp_dorsalattentiona_viol <- lm(exp_dorsalattentiona_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_dorsalattentiona_pos_1, d=df) #sig: exp_dorsalattentiona_pos_1
d_FC_dorsalattentiona_viol <- lm(FC_dorsalattentiona_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_dorsalattentiona_pos_1, d=df) #sig: FC_dorsalattentiona_pos_1
d_depression_exp_dorsalattentiona <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_dorsalattentiona_pos_2*female + depression_1, d=df) #sig: depression_1
d_depression_FC_dorsalattentiona <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_dorsalattentiona_pos_2*female + depression_1, d=df) #sig: depression_1

# Dorsal Attention B (6)
d_exp_dorsalattentionb_viol <- lm(exp_dorsalattentionb_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_dorsalattentionb_pos_1, d=df) #sig: exp_dorsalattentionb_pos_1
d_FC_dorsalattentionb_viol <- lm(FC_dorsalattentionb_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_dorsalattentionb_pos_1, d=df) #sig: female, FC_dorsalattentionb_pos_1
d_depression_exp_dorsalattentionb <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_dorsalattentionb_pos_2*female + depression_1, d=df) #sig: depression_1
d_depression_FC_dorsalattentionb <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_dorsalattentionb_pos_2*female + depression_1, d=df) #sig: depression_1

# Salience A (7)
d_exp_saliencea_viol <- lm(exp_saliencea_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_saliencea_pos_1, d=df) #sig: exp_saliencea_pos_1
d_FC_saliencea_viol <- lm(FC_saliencea_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_saliencea_pos_1, d=df) #sig: female, FC_saliencea_pos_1
d_depression_exp_saliencea <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_saliencea_pos_2*female + depression_1, d=df) #sig: depression_1
d_depression_FC_saliencea <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_saliencea_pos_2*female + depression_1, d=df) #sig: depression_1

# Limbic A (9)
d_exp_limbica_viol <- lm(exp_limbica_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_limbica_pos_1, d=df) #sig: exp_limbica_pos_1, num_pastyear_2:female
d_FC_limbica_viol <- lm(FC_limbica_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_limbica_pos_1, d=df) #sig: female, FC_limbica_pos_1
d_depression_exp_limbica <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_limbica_pos_2*female + depression_1, d=df) #sig: depression_1
d_depression_FC_limbica <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_limbica_pos_2*female + depression_1, d=df) #sig: FC_limbica_pos_2, depression_1

# Limbic B (10)
d_exp_limbicb_viol <- lm(exp_limbicb_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_limbicb_pos_1, d=df) #sig: exp_limbicb_pos_1, num_pastyear_2:female
d_FC_limbicb_viol <- lm(FC_limbicb_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_limbicb_pos_1, d=df) #sig: FC_limbicb_pos_1
d_depression_exp_limbicb <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_limbicb_pos_2*female + depression_1, d=df) #sig: depression_1
d_depression_FC_limbicb <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_limbicb_pos_2*female + depression_1, d=df) #sig: FC_limbicb_pos_2, depression_1, FC_limbicb_pos_2:female

# Control A (11)
d_exp_controla_viol <- lm(exp_controla_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_controla_pos_1, d=df) #sig: exp_controla_pos_1, num_pastyear_2:female
d_FC_controla_viol <- lm(FC_controla_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_controla_pos_1, d=df) #sig: female, FC_controla_pos_1 
d_depression_exp_controla <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_controla_pos_2*female + depression_1, d=df) #sig: depression_1
d_depression_FC_controla <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_controla_pos_2*female + depression_1, d=df) #sig: depression_1, FC_controla_pos_2:female

# Control B (12)
d_exp_controlb_viol <- lm(exp_controlb_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_controlb_pos_1, d=df) #sig: exp_controlb_pos_1
d_FC_controlb_viol <- lm(FC_controlb_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_controlb_pos_1, d=df) #sig: FC_controlb_pos_1
d_depression_exp_controlb <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_controlb_pos_2*female + depression_1, d=df) #sig: depression_1
d_depression_FC_controlb <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_controlb_pos_2*female + depression_1, d=df) #sig: FC_controlb_pos_2, depression_1, FC_controlb_pos_2:female

# Control C (13)
d_exp_controlc_viol <- lm(exp_controlc_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_controlc_pos_1, d=df) #sig: exp_controlc_pos_1
d_FC_controlc_viol <- lm(FC_controlc_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_controlc_pos_1, d=df) #sig: female, FC_controlc_pos_1
d_depression_exp_controlc <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_controlc_pos_2*female + depression_1, d=df) #sig: depression_1
d_depression_FC_controlc <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_controlc_pos_2*female + depression_1, d=df) #sig: depression_1

# Default A (14)
d_exp_defaulta_viol <- lm(exp_defaulta_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_defaulta_pos_1, d=df) #sig: exp_defaulta_pos_1
d_FC_defaulta_viol <- lm(FC_defaulta_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_defaulta_pos_1, d=df) #sig: FC_defaulta_pos_1
d_depression_exp_defaulta <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_defaulta_pos_2*female + depression_1, d=df) #sig: depression_1
d_depression_FC_defaulta <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_defaulta_pos_2*female + depression_1, d=df) #sig: depression_1, FC_defaulta_pos_2:female

# Default B (15)
d_exp_defaultb_viol <- lm(exp_defaultb_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_defaultb_pos_1, d=df) #sig: exp_defaultb_pos_1 
d_FC_defaultb_viol <- lm(FC_defaultb_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_defaultb_pos_1, d=df) #sig: FC_defaultb_pos_1
d_depression_exp_defaultb <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_defaultb_pos_2*female + depression_1, d=df) #sig: depression_1
d_depression_FC_defaultb <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_defaultb_pos_2*female + depression_1, d=df) #sig: depression_1, FC_defaultb_pos_2:female

# Default C (16)
d_exp_defaultc_viol <- lm(exp_defaultc_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_defaultc_pos_1, d=df) #sig: exp_defaultc_pos_1
d_FC_defaultc_viol <- lm(FC_defaultc_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_defaultc_pos_1, d=df) #sig: FC_defaultc_pos_1
d_depression_exp_defaultc <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_defaultc_pos_2*female + depression_1, d=df) #sig: depression_1 
d_depression_FC_defaultc <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_defaultc_pos_2*female + depression_1, d=df) #sig: depression_1, FC_defaultc_pos_2:female

# Temporal Parietal (17)
d_exp_temporalparietal_viol <- lm(exp_temporalparietal_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + exp_temporalparietal_pos_1, d=df) #sig: exp_temporalparietal_pos_1
d_FC_temporalparietal_viol <- lm(FC_temporalparietal_pos_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + num_pastyear_2*female + FC_temporalparietal_pos_1, d=df) #sig: FC_temporalparietal_pos_1
d_depression_exp_temporalparietal <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + exp_temporalparietal_pos_2*female + depression_1, d=df) #sig: depression_1 
d_depression_FC_temporalparietal <- lm(depression_2 ~ age_mri_2 + black + white + hispanic + BMIperc_1 + PubCat_1 + IPR + FC_temporalparietal_pos_2*female + depression_1, d=df) #sig: depression_1, FC_temporalparietal_pos_2:female

##### Tables

# Visual A (1)
tab_model(a_exp_visuala_viol, b_exp_visuala_viol, c_exp_visuala_viol, d_exp_visuala_viol)
tab_model(a_FC_visuala_viol, b_FC_visuala_viol, c_FC_visuala_viol, d_FC_visuala_viol)
tab_model(a_depression_exp_visuala, b_depression_exp_visuala, c_depression_exp_visuala, d_depression_exp_visuala)
tab_model(a_depression_FC_visuala, b_depression_FC_visuala, c_depression_FC_visuala, d_depression_FC_visuala)

# Visual B (2)
tab_model(a_exp_visualb_viol, b_exp_visualb_viol, c_exp_visualb_viol, d_exp_visualb_viol)
tab_model(a_FC_visualb_viol, b_FC_visualb_viol, c_FC_visualb_viol, d_FC_visualb_viol)
tab_model(a_depression_exp_visualb, b_depression_exp_visualb, c_depression_exp_visualb, d_depression_exp_visualb)
tab_model(a_depression_FC_visualb, b_depression_FC_visualb, c_depression_FC_visualb, d_depression_FC_visualb)

# Somatomotor A (3)
tab_model(a_exp_somatomotora_viol, b_exp_somatomotora_viol, c_exp_somatomotora_viol, d_exp_somatomotora_viol)
tab_model(a_FC_somatomotora_viol, b_FC_somatomotora_viol, c_FC_somatomotora_viol, d_FC_somatomotora_viol)
tab_model(a_depression_exp_somatomotora, b_depression_exp_somatomotora, c_depression_exp_somatomotora, d_depression_exp_somatomotora)
tab_model(a_depression_FC_somatomotora, b_depression_FC_somatomotora, c_depression_FC_somatomotora, d_depression_FC_somatomotora)
cor.test(df[df$female == 0, 'depression_2'], df[df$female == 0, 'FC_somatomotora_pos_2'])
cor.test(df[df$female == 1, 'depression_2'], df[df$female == 1, 'FC_somatomotora_pos_2'])

# Somatomotor B (4)
tab_model(a_exp_somatomotorb_viol, b_exp_somatomotorb_viol, c_exp_somatomotorb_viol, d_exp_somatomotorb_viol)
tab_model(a_FC_somatomotorb_viol, b_FC_somatomotorb_viol, c_FC_somatomotorb_viol, d_FC_somatomotorb_viol)
tab_model(a_depression_exp_somatomotorb, b_depression_exp_somatomotorb, c_depression_exp_somatomotorb, d_depression_exp_somatomotorb)
tab_model(a_depression_FC_somatomotorb, b_depression_FC_somatomotorb, c_depression_FC_somatomotorb, d_depression_FC_somatomotorb)

# Dorsal Attention A (5)
tab_model(a_exp_dorsalattentiona_viol, b_exp_dorsalattentiona_viol, c_exp_dorsalattentiona_viol, d_exp_dorsalattentiona_viol)
tab_model(a_FC_dorsalattentiona_viol, b_FC_dorsalattentiona_viol, c_FC_dorsalattentiona_viol, d_FC_dorsalattentiona_viol)
tab_model(a_depression_exp_dorsalattentiona, b_depression_exp_dorsalattentiona, c_depression_exp_dorsalattentiona, d_depression_exp_dorsalattentiona)
tab_model(a_depression_FC_dorsalattentiona, b_depression_FC_dorsalattentiona, c_depression_FC_dorsalattentiona, d_depression_FC_dorsalattentiona)

# Dorsal Attention B (6)
tab_model(a_exp_dorsalattentionb_viol, b_exp_dorsalattentionb_viol, c_exp_dorsalattentionb_viol, d_exp_dorsalattentionb_viol)
tab_model(a_FC_dorsalattentionb_viol, b_FC_dorsalattentionb_viol, c_FC_dorsalattentionb_viol, d_FC_dorsalattentionb_viol)
tab_model(a_depression_exp_dorsalattentionb, b_depression_exp_dorsalattentionb, c_depression_exp_dorsalattentionb, d_depression_exp_dorsalattentionb)
tab_model(a_depression_FC_dorsalattentionb, b_depression_FC_dorsalattentionb, c_depression_FC_dorsalattentionb, d_depression_FC_dorsalattentionb)
cor.test(df[df$female == 0, 'depression_2'], df[df$female == 0, 'FC_dorsalattentionb_pos_2'])
cor.test(df[df$female == 1, 'depression_2'], df[df$female == 1, 'FC_dorsalattentionb_pos_2'])

# Salience A (7)
tab_model(a_exp_saliencea_viol, b_exp_saliencea_viol, c_exp_saliencea_viol, d_exp_saliencea_viol)
tab_model(a_FC_saliencea_viol, b_FC_saliencea_viol, c_FC_saliencea_viol, d_FC_saliencea_viol)
tab_model(a_depression_exp_saliencea, b_depression_exp_saliencea, c_depression_exp_saliencea, d_depression_exp_saliencea)
tab_model(a_depression_FC_saliencea, b_depression_FC_saliencea, c_depression_FC_saliencea, d_depression_FC_saliencea)
cor.test(df[df$female == 1, 'num_pastyear_2'], df[df$female == 1, 'exp_saliencea_pos_2'])

# Limbic A (9)
tab_model(a_exp_limbica_viol, b_exp_limbica_viol, c_exp_limbica_viol, d_exp_limbica_viol)
tab_model(a_FC_limbica_viol, b_FC_limbica_viol, c_FC_limbica_viol, d_FC_limbica_viol)
tab_model(a_depression_exp_limbica, b_depression_exp_limbica, c_depression_exp_limbica, d_depression_exp_limbica)
tab_model(a_depression_FC_limbica, b_depression_FC_limbica, c_depression_FC_limbica, d_depression_FC_limbica)
cor.test(df[df$female == 0, 'depression_2'], df[df$female == 0, 'FC_limbica_pos_2'])
cor.test(df[df$female == 1, 'depression_2'], df[df$female == 1, 'FC_limbica_pos_2'])

# Limbic B (10)
tab_model(a_exp_limbicb_viol, b_exp_limbicb_viol, c_exp_limbicb_viol, d_exp_limbicb_viol)
cor.test(df[df$female == 0, 'num_pastyear_2'], df[df$female == 0, 'exp_limbicb_pos_2'])
cor.test(df[df$female == 1, 'num_pastyear_2'], df[df$female == 1, 'exp_limbicb_pos_2'])
tab_model(a_FC_limbicb_viol, b_FC_limbicb_viol, c_FC_limbicb_viol, d_FC_limbicb_viol)
tab_model(a_depression_exp_limbicb, b_depression_exp_limbicb, c_depression_exp_limbicb, d_depression_exp_limbicb)
tab_model(a_depression_FC_limbicb, b_depression_FC_limbicb, c_depression_FC_limbicb, d_depression_FC_limbicb)
cor.test(df[df$female == 0, 'depression_2'], df[df$female == 0, 'FC_limbicb_pos_2'])
cor.test(df[df$female == 1, 'depression_2'], df[df$female == 1, 'FC_limbicb_pos_2'])

# Control A (11)
tab_model(a_exp_controla_viol, b_exp_controla_viol, c_exp_controla_viol, d_exp_controla_viol)
tab_model(a_FC_controla_viol, b_FC_controla_viol, c_FC_controla_viol, d_FC_controla_viol)
tab_model(a_depression_exp_controla, b_depression_exp_controla, c_depression_exp_controla, d_depression_exp_controla)
tab_model(a_depression_FC_controla, b_depression_FC_controla, c_depression_FC_controla, d_depression_FC_controla)
cor.test(df[df$female == 0, 'depression_2'], df[df$female == 0, 'FC_controla_pos_2'])
cor.test(df[df$female == 1, 'depression_2'], df[df$female == 1, 'FC_controla_pos_2'])

# Control B (12)
tab_model(a_exp_controlb_viol, b_exp_controlb_viol, c_exp_controlb_viol, d_exp_controlb_viol)
tab_model(a_FC_controlb_viol, b_FC_controlb_viol, c_FC_controlb_viol, d_FC_controlb_viol)
tab_model(a_depression_exp_controlb, b_depression_exp_controlb, c_depression_exp_controlb, d_depression_exp_controlb)
tab_model(a_depression_FC_controlb, b_depression_FC_controlb, c_depression_FC_controlb, d_depression_FC_controlb)

# Control C (13)

# Default A (14)

# Default B (15)

# Default C (16)

# Temporal Parietal (17)