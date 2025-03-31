### This code generates models analogous to the Salience B
### analyses in the main body of the paper with the other 16
### Yeo17 networks for the purposes of addressing reviewer
### concerns 
###
### Ellyn Butler
### March 31, 2025

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

d <- read.csv('~/Documents/Northwestern/projects/violence_sex_development/data/combined_data_2025-04-XX.csv') # TO DO: finalize the date
dall <- d
d <- d[!is.na(d$exp_salienceb_pos) & !is.na(d$FC_salienceb_pos) & !is.na(d$depression) & !is.na(d$num_pastyear), ] # TO DO: add in other network metrics
d2 <- d[d$sesid == 2, ]

dim(d2) # N = 220

################### Model
d1 <- dall[dall$sesid == 1, ]
d1 <- d1[, c('subid', 'BMIperc', 'PubCat')]
names(d1) <- c('subid', 'BMIperc_1', 'PubCat_1')
d2 <- merge(d2, d1)
vars <- c('depression', 'num_pastyear', 'exp_salienceb_pos', 'FC_salienceb_pos', 'BMIperc_1', 'PubCat_1', 'age_mri', 'IPR')
d2[, vars] <- scale(d2[, vars])

d2$BMIperc_2 <- d2$BMIperc
d2$PubCat_2 <- d2$PubCat
d2$BMIperc <- d2$BMIperc_1
d2$PubCat <- d2$PubCat_1

##### (a) Base models
# Visual A (1)
a_exp_visuala_viol <- lm(exp_visuala_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_visuala_viol <- lm(FC_visuala_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_visuala <- lm(depression ~ exp_visuala_pos*female, d=d2) #sig: 
a_depression_FC_visuala <- lm(depression ~ FC_visuala_pos*female, d=d2) #sig: 

# Visual B (2)
a_exp_visualb_viol <- lm(exp_visualb_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_visualb_viol <- lm(FC_visualb_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_visualb <- lm(depression ~ exp_visualb_pos*female, d=d2) #sig: 
a_depression_FC_visualb <- lm(depression ~ FC_visualb_pos*female, d=d2) #sig: 

# Somatomotor A (3)
a_exp_somatomotora_viol <- lm(exp_somatomotora_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_somatomotora_viol <- lm(FC_somatomotora_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_somatomotora <- lm(depression ~ exp_somatomotora_pos*female, d=d2) #sig: 
a_depression_FC_somatomotora <- lm(depression ~ FC_somatomotora_pos*female, d=d2) #sig: 

# Somatomotor B (4)
a_exp_somatomotorb_viol <- lm(exp_somatomotorb_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_somatomotorb_viol <- lm(FC_somatomotorb_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_somatomotorb <- lm(depression ~ exp_somatomotorb_pos*female, d=d2) #sig: 
a_depression_FC_somatomotorb <- lm(depression ~ FC_somatomotorb_pos*female, d=d2) #sig: 

# Dorsal Attention A (5)
a_exp_dorsalattentiona_viol <- lm(exp_dorsalattentiona_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_dorsalattentiona_viol <- lm(FC_dorsalattentiona_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_dorsalattentiona <- lm(depression ~ exp_dorsalattentiona_pos*female, d=d2) #sig: 
a_depression_FC_dorsalattentiona <- lm(depression ~ FC_dorsalattentiona_pos*female, d=d2) #sig: 

# Dorsal Attention B (6)
a_exp_dorsalattentionb_viol <- lm(exp_dorsalattentionb_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_dorsalattentionb_viol <- lm(FC_dorsalattentionb_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_dorsalattentionb <- lm(depression ~ exp_dorsalattentionb_pos*female, d=d2) #sig: 
a_depression_FC_dorsalattentionb <- lm(depression ~ FC_dorsalattentionb_pos*female, d=d2) #sig: 

# Salience A (7)
a_exp_saliencea_viol <- lm(exp_saliencea_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_saliencea_viol <- lm(FC_saliencea_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_saliencea <- lm(depression ~ exp_saliencea_pos*female, d=d2) #sig: 
a_depression_FC_saliencea <- lm(depression ~ FC_saliencea_pos*female, d=d2) #sig: 

# Limbic A (9)
a_exp_limbica_viol <- lm(exp_limbica_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_limbica_viol <- lm(FC_limbica_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_limbica <- lm(depression ~ exp_limbica_pos*female, d=d2) #sig: 
a_depression_FC_limbica <- lm(depression ~ FC_limbica_pos*female, d=d2) #sig: 

# Limbic B (10)
a_exp_limbicb_viol <- lm(exp_limbicb_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_limbicb_viol <- lm(FC_limbicb_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_limbicb <- lm(depression ~ exp_limbicb_pos*female, d=d2) #sig: 
a_depression_FC_limbicb <- lm(depression ~ FC_limbicb_pos*female, d=d2) #sig: 

# Control A (11)
a_exp_controla_viol <- lm(exp_controla_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_controla_viol <- lm(FC_controla_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_controla <- lm(depression ~ exp_controla_pos*female, d=d2) #sig: 
a_depression_FC_controla <- lm(depression ~ FC_controla_pos*female, d=d2) #sig: 

# Control B (12)
a_exp_controlb_viol <- lm(exp_controlb_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_controlb_viol <- lm(FC_controlb_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_controlb <- lm(depression ~ exp_controlb_pos*female, d=d2) #sig: 
a_depression_FC_controlb <- lm(depression ~ FC_controlb_pos*female, d=d2) #sig: 

# Control C (13)
a_exp_controlc_viol <- lm(exp_controlc_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_controlc_viol <- lm(FC_controlc_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_controlc <- lm(depression ~ exp_controlc_pos*female, d=d2) #sig: 
a_depression_FC_controlc <- lm(depression ~ FC_controlc_pos*female, d=d2) #sig: 

# Default A (14)
a_exp_defaulta_viol <- lm(exp_defaulta_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_defaulta_viol <- lm(FC_defaulta_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_defaulta <- lm(depression ~ exp_defaulta_pos*female, d=d2) #sig: 
a_depression_FC_defaulta <- lm(depression ~ FC_defaulta_pos*female, d=d2) #sig: 

# Default B (15)
a_exp_defaultb_viol <- lm(exp_defaultb_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_defaultb_viol <- lm(FC_defaultb_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_defaultb <- lm(depression ~ exp_defaultb_pos*female, d=d2) #sig: 
a_depression_FC_defaultb <- lm(depression ~ FC_defaultb_pos*female, d=d2) #sig: 

# Default C (16)
a_exp_defaultc_viol <- lm(exp_defaultc_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_defaultc_viol <- lm(FC_defaultc_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_defaultc <- lm(depression ~ exp_defaultc_pos*female, d=d2) #sig: 
a_depression_FC_defaultc <- lm(depression ~ FC_defaultc_pos*female, d=d2) #sig: 

# Temporal Parietal (17)
a_exp_temporalparietal_viol <- lm(exp_temporalparietal_pos ~ num_pastyear*female, d=d2) #sig: 
a_FC_temporalparietal_viol <- lm(FC_temporalparietal_pos ~ num_pastyear*female, d=d2) #sig: 
a_depression_exp_temporalparietal <- lm(depression ~ exp_temporalparietal_pos*female, d=d2) #sig: 
a_depression_FC_temporalparietal <- lm(depression ~ FC_temporalparietal_pos*female, d=d2) #sig: 

##### (b) Controlling for demographics
# Visual A (1)
b_exp_visuala_viol <- lm(exp_visuala_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_visuala_viol <- lm(FC_visuala_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_visuala <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_visuala_pos*female, d=d2) #sig: 
b_depression_FC_visuala <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_visuala_pos*female, d=d2) #sig: 

# Visual B (2)
b_exp_visualb_viol <- lm(exp_visualb_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_visualb_viol <- lm(FC_visualb_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_visualb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_visualb_pos*female, d=d2) #sig: 
b_depression_FC_visualb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_visualb_pos*female, d=d2) #sig: 

# Somatomotor A (3)
b_exp_somatomotora_viol <- lm(exp_somatomotora_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_somatomotora_viol <- lm(FC_somatomotora_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_somatomotora <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_somatomotora_pos*female, d=d2) #sig: 
b_depression_FC_somatomotora <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_somatomotora_pos*female, d=d2) #sig: 

# Somatomotor B (4)
b_exp_somatomotorb_viol <- lm(exp_somatomotorb_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_somatomotorb_viol <- lm(FC_somatomotorb_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_somatomotorb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_somatomotorb_pos*female, d=d2) #sig: 
b_depression_FC_somatomotorb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_somatomotorb_pos*female, d=d2) #sig: 

# Dorsal Attention A (5)
b_exp_dorsalattentiona_viol <- lm(exp_dorsalattentiona_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_dorsalattentiona_viol <- lm(FC_dorsalattentiona_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_dorsalattentiona <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_dorsalattentiona_pos*female, d=d2) #sig: 
b_depression_FC_dorsalattentiona <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_dorsalattentiona_pos*female, d=d2) #sig: 

# Dorsal Attention B (6)
b_exp_dorsalattentionb_viol <- lm(exp_dorsalattentionb_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_dorsalattentionb_viol <- lm(FC_dorsalattentionb_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_dorsalattentionb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_dorsalattentionb_pos*female, d=d2) #sig: 
b_depression_FC_dorsalattentionb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_dorsalattentionb_pos*female, d=d2) #sig: 

# Salience A (7)
b_exp_saliencea_viol <- lm(exp_saliencea_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_saliencea_viol <- lm(FC_saliencea_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_saliencea <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_saliencea_pos*female, d=d2) #sig: 
b_depression_FC_saliencea <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_saliencea_pos*female, d=d2) #sig: 

# Limbic A (9)
b_exp_limbica_viol <- lm(exp_limbica_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_limbica_viol <- lm(FC_limbica_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_limbica <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_limbica_pos*female, d=d2) #sig: 
b_depression_FC_limbica <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_limbica_pos*female, d=d2) #sig: 

# Limbic B (10)
b_exp_limbicb_viol <- lm(exp_limbicb_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_limbicb_viol <- lm(FC_limbicb_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_limbicb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_limbicb_pos*female, d=d2) #sig: 
b_depression_FC_limbicb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_limbicb_pos*female, d=d2) #sig: 

# Control A (11)
b_exp_controla_viol <- lm(exp_controla_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_controla_viol <- lm(FC_controla_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_controla <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_controla_pos*female, d=d2) #sig: 
b_depression_FC_controla <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_controla_pos*female, d=d2) #sig: 

# Control B (12)
b_exp_controlb_viol <- lm(exp_controlb_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_controlb_viol <- lm(FC_controlb_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_controlb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_controlb_pos*female, d=d2) #sig: 
b_depression_FC_controlb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_controlb_pos*female, d=d2) #sig: 

# Control C (13)
b_exp_controlc_viol <- lm(exp_controlc_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_controlc_viol <- lm(FC_controlc_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_controlc <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_controlc_pos*female, d=d2) #sig: 
b_depression_FC_controlc <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_controlc_pos*female, d=d2) #sig: 

# Default A (14)
b_exp_defaulta_viol <- lm(exp_defaulta_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_defaulta_viol <- lm(FC_defaulta_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_defaulta <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_defaulta_pos*female, d=d2) #sig: 
b_depression_FC_defaulta <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_defaulta_pos*female, d=d2) #sig: 

# Default B (15)
b_exp_defaultb_viol <- lm(exp_defaultb_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_defaultb_viol <- lm(FC_defaultb_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_defaultb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_defaultb_pos*female, d=d2) #sig: 
b_depression_FC_defaultb <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_defaultb_pos*female, d=d2) #sig: 

# Default C (16)
b_exp_defaultc_viol <- lm(exp_defaultc_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_defaultc_viol <- lm(FC_defaultc_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_defaultc <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_defaultc_pos*female, d=d2) #sig: 
b_depression_FC_defaultc <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_defaultc_pos*female, d=d2) #sig: 

# Temporal Parietal (17)
b_exp_temporalparietal_viol <- lm(exp_temporalparietal_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_FC_temporalparietal_viol <- lm(FC_temporalparietal_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear*female, d=d2) #sig: 
b_depression_exp_temporalparietal <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_temporalparietal_pos*female, d=d2) #sig: 
b_depression_FC_temporalparietal <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_temporalparietal_pos*female, d=d2) #sig: 

###### Imputation # TO DO: Will need to do this for all of the neuroimaging variables
# Impute brain values for subjects that have T2 but not T1 data
d1b <- dall[dall$sesid == 1 & dall$subid %in% d2$subid, ]
d1b <- d1b[, !(names(d1b) %in% c('sesid', 'otherrace'))]
d2b <- d2
d2b <- d2b[, !(names(d2b) %in% c('sesid', 'otherrace'))]
same <- c('subid', 'black', 'white', 'hispanic', 'female', 'IPR')
names(d1b)[!(names(d1b) %in% same)] <- paste0(names(d1b)[!(names(d1b) %in% same)], '_1')
names(d2b)[!(names(d2b) %in% same)] <- paste0(names(d2b)[!(names(d2b) %in% same)], '_2')

# Assume age_mri_1 would have been two years before age_mri_2
subids <- d1b[is.na(d1b$age_mri_1), 'subid']
d1b[is.na(d1b$age_mri_1), 'age_mri_1'] <- d2b[d2b$subid %in% subids, 'age_mri_2'] - 2

# Predict exp_salienceb_pos_1
t.test(exp_salienceb_pos_1 ~ black, data = d1b) #sig... lower p-val than white
t.test(exp_salienceb_pos_1 ~ white, data = d1b) #sig
t.test(exp_salienceb_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_salienceb_pos_1), 'exp_salienceb_pos_1'], d1b[!is.na(d1b$exp_salienceb_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_salienceb_pos_1), 'exp_salienceb_pos_1'], d1b[!is.na(d1b$exp_salienceb_pos_1), 'PubCat_1']) #sig
cor.test(d1b[!is.na(d1b$exp_salienceb_pos_1), 'exp_salienceb_pos_1'], d1b[!is.na(d1b$exp_salienceb_pos_1), 'IPR']) #sig
t.test(exp_salienceb_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$exp_salienceb_pos_1), 'exp_salienceb_pos_1'], d1b[!is.na(d1b$exp_salienceb_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_salienceb_pos_1), 'exp_salienceb_pos_1'], d1b[!is.na(d1b$exp_salienceb_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_salienceb_pos_1 ~ black + IPR + PubCat_1 + female, data = d1b) #PubCat_1 not over female, IPR not conditionally sig
exp_mod <- lm(exp_salienceb_pos_1 ~ black + female, data = d1b) 

d1b[is.na(d1b$exp_salienceb_pos_1), 'exp_salienceb_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_salienceb_pos_1), ])

# Predict FC_salienceb_pos_1
t.test(FC_salienceb_pos_1 ~ black, data = d1b) #not
t.test(FC_salienceb_pos_1 ~ white, data = d1b) #not
t.test(FC_salienceb_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_salienceb_pos_1), 'FC_salienceb_pos_1'], d1b[!is.na(d1b$FC_salienceb_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_salienceb_pos_1), 'FC_salienceb_pos_1'], d1b[!is.na(d1b$FC_salienceb_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_salienceb_pos_1), 'FC_salienceb_pos_1'], d1b[!is.na(d1b$FC_salienceb_pos_1), 'IPR']) #not
t.test(FC_salienceb_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_salienceb_pos_1), 'FC_salienceb_pos_1'], d1b[!is.na(d1b$FC_salienceb_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_salienceb_pos_1), 'FC_salienceb_pos_1'], d1b[!is.na(d1b$FC_salienceb_pos_1), 'depression_1']) #not
fc_mod <- lm(FC_salienceb_pos_1 ~ female, data = d1b)
d1b[is.na(d1b$FC_salienceb_pos_1), 'FC_salienceb_pos_1'] <- predict(fc_mod, d1b[is.na(d1b$FC_salienceb_pos_1), ])

d1b <- d1b[, !(names(d1b) %in% c('IPR', 'black', 'white', 'hispanic', 'female'))]
df <- merge(d1b, d2b) #keep the subjects that have complete session 2 data

##### (c) Controlling for dependent variables measured at T1
vars <- c('depression_2', 'num_pastyear_2', 'age_mri_2', 'depression_1', 'BMIperc_2', 'PubCat_2', 'exp_salienceb_pos_1', 'FC_salienceb_pos_1') #TO DO: Will need to scale all of the neuroimaging variables
df[, vars] <- scale(df[, vars])

# Visual A (1)
c_exp_visuala_viol <- lm(exp_visuala_pos_2 ~ num_pastyear_2*female + exp_visuala_pos_1, d=d2) #sig: 
c_FC_visuala_viol <- lm(FC_visuala_pos_2 ~ num_pastyear_2*female + FC_visuala_pos_1, d=d2) #sig: 
c_depression_exp_visuala <- lm(depression_2 ~ exp_visuala_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_visuala <- lm(depression_2 ~ FC_visuala_pos*female + depression_1, d=d2) #sig: 

# Visual B (2)
c_exp_visualb_viol <- lm(exp_visualb_pos_2 ~ num_pastyear_2*female + exp_visualb_pos_1, d=d2) #sig: 
c_FC_visualb_viol <- lm(FC_visualb_pos_2 ~ num_pastyear_2*female + FC_visualb_pos_1, d=d2) #sig: 
c_depression_exp_visualb <- lm(depression_2 ~ exp_visualb_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_visualb <- lm(depression_2 ~ FC_visualb_pos*female + depression_1, d=d2) #sig: 

# Somatomotor A (3)
c_exp_somatomotora_viol <- lm(exp_somatomotora_pos_2 ~ num_pastyear_2*female + exp_somatomotora_pos_1, d=d2) #sig: 
c_FC_somatomotora_viol <- lm(FC_somatomotora_pos_2 ~ num_pastyear_2*female + FC_somatomotora_pos_1, d=d2) #sig: 
c_depression_exp_somatomotora <- lm(depression_2 ~ exp_somatomotora_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_somatomotora <- lm(depression_2 ~ FC_somatomotora_pos*female + depression_1, d=d2) #sig: 

# Somatomotor B (4)
c_exp_somatomotorb_viol <- lm(exp_somatomotorb_pos_2 ~ num_pastyear_2*female + exp_somatomotorb_pos_1, d=d2) #sig: 
c_FC_somatomotorb_viol <- lm(FC_somatomotorb_pos_2 ~ num_pastyear_2*female + FC_somatomotorb_pos_1, d=d2) #sig: 
c_depression_exp_somatomotorb <- lm(depression_2 ~ exp_somatomotorb_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_somatomotorb <- lm(depression_2 ~ FC_somatomotorb_pos*female + depression_1, d=d2) #sig: 

# Dorsal Attention A (5)
c_exp_dorsalattentiona_viol <- lm(exp_dorsalattentiona_pos_2 ~ num_pastyear_2*female + exp_dorsalattentiona_pos_1, d=d2) #sig: 
c_FC_dorsalattentiona_viol <- lm(FC_dorsalattentiona_pos_2 ~ num_pastyear_2*female + FC_dorsalattentiona_pos_1, d=d2) #sig: 
c_depression_exp_dorsalattentiona <- lm(depression_2 ~ exp_dorsalattentiona_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_dorsalattentiona <- lm(depression_2 ~ FC_dorsalattentiona_pos*female + depression_1, d=d2) #sig: 

# Dorsal Attention B (6)
c_exp_dorsalattentionb_viol <- lm(exp_dorsalattentionb_pos_2 ~ num_pastyear_2*female + exp_dorsalattentionb_pos_1, d=d2) #sig: 
c_FC_dorsalattentionb_viol <- lm(FC_dorsalattentionb_pos_2 ~ num_pastyear_2*female + FC_dorsalattentionb_pos_1, d=d2) #sig: 
c_depression_exp_dorsalattentionb <- lm(depression_2 ~ exp_dorsalattentionb_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_dorsalattentionb <- lm(depression_2 ~ FC_dorsalattentionb_pos*female + depression_1, d=d2) #sig: 

# Salience A (7)
c_exp_saliencea_viol <- lm(exp_saliencea_pos_2 ~ num_pastyear_2*female + exp_saliencea_pos_1, d=d2) #sig: 
c_FC_saliencea_viol <- lm(FC_saliencea_pos_2 ~ num_pastyear_2*female + FC_saliencea_pos_1, d=d2) #sig: 
c_depression_exp_saliencea <- lm(depression_2 ~ exp_saliencea_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_saliencea <- lm(depression_2 ~ FC_saliencea_pos*female + depression_1, d=d2) #sig: 

# Limbic A (9)
c_exp_limbica_viol <- lm(exp_limbica_pos_2 ~ num_pastyear_2*female + exp_limbica_pos_1, d=d2) #sig: 
c_FC_limbica_viol <- lm(FC_limbica_pos_2 ~ num_pastyear_2*female + FC_limbica_pos_1, d=d2) #sig: 
c_depression_exp_limbica <- lm(depression_2 ~ exp_limbica_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_limbica <- lm(depression_2 ~ FC_limbica_pos*female + depression_1, d=d2) #sig: 

# Limbic B (10)
c_exp_limbicb_viol <- lm(exp_limbicb_pos_2 ~ num_pastyear_2*female + exp_limbicb_pos_1, d=d2) #sig: 
c_FC_limbicb_viol <- lm(FC_limbicb_pos_2 ~ num_pastyear_2*female + FC_limbicb_pos_, d=d2) #sig: 
c_depression_exp_limbicb <- lm(depression_2 ~ exp_limbicb_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_limbicb <- lm(depression_2 ~ FC_limbicb_pos*female + depression_1, d=d2) #sig: 

# Control A (11)
c_exp_controla_viol <- lm(exp_controla_pos_2 ~ num_pastyear_2*female + exp_controla_pos_1, d=d2) #sig: 
c_FC_controla_viol <- lm(FC_controla_pos_2 ~ num_pastyear_2*female + FC_controla_pos_1, d=d2) #sig: 
c_depression_exp_controla <- lm(depression_2 ~ exp_controla_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_controla <- lm(depression_2 ~ FC_controla_pos*female + depression_1, d=d2) #sig: 

# Control B (12)
c_exp_controlb_viol <- lm(exp_controlb_pos_2 ~ num_pastyear_2*female + exp_controlb_pos_1, d=d2) #sig: 
c_FC_controlb_viol <- lm(FC_controlb_pos_2 ~ num_pastyear_2*female + FC_controlb_pos_1, d=d2) #sig: 
c_depression_exp_controlb <- lm(depression_2 ~ exp_controlb_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_controlb <- lm(depression_2 ~ FC_controlb_pos*female + depression_1, d=d2) #sig: 

# Control C (13)
c_exp_controlc_viol <- lm(exp_controlc_pos_2 ~ num_pastyear_2*female + exp_controlc_pos_1, d=d2) #sig: 
c_FC_controlc_viol <- lm(FC_controlc_pos_2 ~ num_pastyear_2*female + FC_controlc_pos_1, d=d2) #sig: 
c_depression_exp_controlc <- lm(depression_2 ~ exp_controlc_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_controlc <- lm(depression_2 ~ FC_controlc_pos*female + depression_1, d=d2) #sig: 

# Default A (14)
c_exp_defaulta_viol <- lm(exp_defaulta_pos_2 ~ num_pastyear_2*female + exp_defaulta_pos_1, d=d2) #sig: 
c_FC_defaulta_viol <- lm(FC_defaulta_pos_2 ~ num_pastyear_2*female + FC_defaulta_pos_1, d=d2) #sig: 
c_depression_exp_defaulta <- lm(depression_2 ~ exp_defaulta_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_defaulta <- lm(depression_2 ~ FC_defaulta_pos*female + depression_1, d=d2) #sig: 

# Default B (15)
c_exp_defaultb_viol <- lm(exp_defaultb_pos_2 ~ num_pastyear_2*female + exp_defaultb_pos_1, d=d2) #sig: 
c_FC_defaultb_viol <- lm(FC_defaultb_pos_2 ~ num_pastyear_2*female + FC_defaultb_pos_1, d=d2) #sig: 
c_depression_exp_defaultb <- lm(depression_2 ~ exp_defaultb_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_defaultb <- lm(depression_2 ~ FC_defaultb_pos*female + depression_1, d=d2) #sig: 

# Default C (16)
c_exp_defaultc_viol <- lm(exp_defaultc_pos_2 ~ num_pastyear_2*female + exp_defaultc_pos_1, d=d2) #sig: 
c_FC_defaultc_viol <- lm(FC_defaultc_pos_2 ~ num_pastyear_2*female + FC_defaultc_pos_1, d=d2) #sig: 
c_depression_exp_defaultc <- lm(depression_2 ~ exp_defaultc_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_defaultc <- lm(depression_2 ~ FC_defaultc_pos*female + depression_1, d=d2) #sig: 

# Temporal Parietal (17)
c_exp_temporalparietal_viol <- lm(exp_temporalparietal_pos_2 ~ num_pastyear_2*female + exp_temporalparietal_pos_1, d=d2) #sig: 
c_FC_temporalparietal_viol <- lm(FC_temporalparietal_pos_2 ~ num_pastyear_2*female + FC_temporalparietal_pos_, d=d2) #sig: 
c_depression_exp_temporalparietal <- lm(depression_2 ~ exp_temporalparietal_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_temporalparietal <- lm(depression_2 ~ FC_temporalparietal_pos*female + depression_1, d=d2) #sig: 

##### (d) Controlling for dependent variables measured at T1 and demographics
df$age_mri <- df$age_mri_2
df$BMIperc <- df$BMIperc_1
df$PubCat <- df$PubCat_1

# Visual A (1)
c_exp_visuala_viol <- lm(exp_visuala_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_visuala_pos_1, d=d2) #sig: 
c_FC_visuala_viol <- lm(FC_visuala_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_visuala_pos_1, d=d2) #sig: 
c_depression_exp_visuala <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_visuala_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_visuala <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_visuala_pos*female + depression_1, d=d2) #sig: 

# Visual B (2)
c_exp_visualb_viol <- lm(exp_visualb_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_visualb_pos_1, d=d2) #sig: 
c_FC_visualb_viol <- lm(FC_visualb_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_visualb_pos_1, d=d2) #sig: 
c_depression_exp_visualb <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_visualb_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_visualb <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_visualb_pos*female + depression_1, d=d2) #sig: 

# Somatomotor A (3)
c_exp_somatomotora_viol <- lm(exp_somatomotora_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_somatomotora_pos_1, d=d2) #sig: 
c_FC_somatomotora_viol <- lm(FC_somatomotora_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_somatomotora_pos_1, d=d2) #sig: 
c_depression_exp_somatomotora <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_somatomotora_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_somatomotora <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_somatomotora_pos*female + depression_1, d=d2) #sig: 

# Somatomotor B (4)
c_exp_somatomotorb_viol <- lm(exp_somatomotorb_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_somatomotorb_pos_1, d=d2) #sig: 
c_FC_somatomotorb_viol <- lm(FC_somatomotorb_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_somatomotorb_pos_1, d=d2) #sig: 
c_depression_exp_somatomotorb <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_somatomotorb_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_somatomotorb <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_somatomotorb_pos*female + depression_1, d=d2) #sig: 

# Dorsal Attention A (5)
c_exp_dorsalattentiona_viol <- lm(exp_dorsalattentiona_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_dorsalattentiona_pos_1, d=d2) #sig: 
c_FC_dorsalattentiona_viol <- lm(FC_dorsalattentiona_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_dorsalattentiona_pos_1, d=d2) #sig: 
c_depression_exp_dorsalattentiona <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_dorsalattentiona_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_dorsalattentiona <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_dorsalattentiona_pos*female + depression_1, d=d2) #sig: 

# Dorsal Attention B (6)
c_exp_dorsalattentionb_viol <- lm(exp_dorsalattentionb_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_dorsalattentionb_pos_1, d=d2) #sig: 
c_FC_dorsalattentionb_viol <- lm(FC_dorsalattentionb_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_dorsalattentionb_pos_1, d=d2) #sig: 
c_depression_exp_dorsalattentionb <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_dorsalattentionb_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_dorsalattentionb <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_dorsalattentionb_pos*female + depression_1, d=d2) #sig: 

# Salience A (7)
c_exp_saliencea_viol <- lm(exp_saliencea_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_saliencea_pos_1, d=d2) #sig: 
c_FC_saliencea_viol <- lm(FC_saliencea_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_saliencea_pos_1, d=d2) #sig: 
c_depression_exp_saliencea <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_saliencea_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_saliencea <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_saliencea_pos*female + depression_1, d=d2) #sig: 

# Limbic A (9)
c_exp_limbica_viol <- lm(exp_limbica_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_limbica_pos_1, d=d2) #sig: 
c_FC_limbica_viol <- lm(FC_limbica_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_limbica_pos_1, d=d2) #sig: 
c_depression_exp_limbica <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_limbica_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_limbica <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_limbica_pos*female + depression_1, d=d2) #sig: 

# Limbic B (10)
c_exp_limbicb_viol <- lm(exp_limbicb_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_limbicb_pos_1, d=d2) #sig: 
c_FC_limbicb_viol <- lm(FC_limbicb_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_limbicb_pos_, d=d2) #sig: 
c_depression_exp_limbicb <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_limbicb_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_limbicb <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_limbicb_pos*female + depression_1, d=d2) #sig: 

# Control A (11)
c_exp_controla_viol <- lm(exp_controla_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_controla_pos_1, d=d2) #sig: 
c_FC_controla_viol <- lm(FC_controla_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_controla_pos_1, d=d2) #sig: 
c_depression_exp_controla <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_controla_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_controla <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_controla_pos*female + depression_1, d=d2) #sig: 

# Control B (12)
c_exp_controlb_viol <- lm(exp_controlb_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_controlb_pos_1, d=d2) #sig: 
c_FC_controlb_viol <- lm(FC_controlb_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_controlb_pos_1, d=d2) #sig: 
c_depression_exp_controlb <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_controlb_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_controlb <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_controlb_pos*female + depression_1, d=d2) #sig: 

# Control C (13)
c_exp_controlc_viol <- lm(exp_controlc_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_controlc_pos_1, d=d2) #sig: 
c_FC_controlc_viol <- lm(FC_controlc_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_controlc_pos_1, d=d2) #sig: 
c_depression_exp_controlc <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_controlc_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_controlc <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_controlc_pos*female + depression_1, d=d2) #sig: 

# Default A (14)
c_exp_defaulta_viol <- lm(exp_defaulta_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_defaulta_pos_1, d=d2) #sig: 
c_FC_defaulta_viol <- lm(FC_defaulta_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_defaulta_pos_1, d=d2) #sig: 
c_depression_exp_defaulta <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_defaulta_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_defaulta <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_defaulta_pos*female + depression_1, d=d2) #sig: 

# Default B (15)
c_exp_defaultb_viol <- lm(exp_defaultb_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_defaultb_pos_1, d=d2) #sig: 
c_FC_defaultb_viol <- lm(FC_defaultb_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_defaultb_pos_1, d=d2) #sig: 
c_depression_exp_defaultb <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_defaultb_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_defaultb <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_defaultb_pos*female + depression_1, d=d2) #sig: 

# Default C (16)
c_exp_defaultc_viol <- lm(exp_defaultc_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_defaultc_pos_1, d=d2) #sig: 
c_FC_defaultc_viol <- lm(FC_defaultc_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_defaultc_pos_1, d=d2) #sig: 
c_depression_exp_defaultc <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_defaultc_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_defaultc <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_defaultc_pos*female + depression_1, d=d2) #sig: 

# Temporal Parietal (17)
c_exp_temporalparietal_viol <- lm(exp_temporalparietal_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + exp_temporalparietal_pos_1, d=d2) #sig: 
c_FC_temporalparietal_viol <- lm(FC_temporalparietal_pos_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + num_pastyear_2*female + FC_temporalparietal_pos_, d=d2) #sig: 
c_depression_exp_temporalparietal <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_temporalparietal_pos*female + depression_1, d=d2) #sig: 
c_depression_FC_temporalparietal <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_temporalparietal_pos*female + depression_1, d=d2) #sig: 

##### Tables
# 1
tab_model()
