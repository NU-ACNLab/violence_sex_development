### This script creates a correlation plot between violence, depression
### salience networks metrics, sex, and the potential confounders
###
### Ellyn Butler
### October 23, 2024 - January 6, 2025

library(ggplot2)
library(ggcorrplot) #Not playing nice with Quest
library(ggpubr)

# Load data
d <- read.csv('~/Documents/Northwestern/projects/violence_sex_development/data/combined_data_2024-10-07.csv')
d <- d[!is.na(d$exp_b_pos) & !is.na(d$FC_b_pos) & !is.na(d$depression) & !is.na(d$num_pastyear), ]
d2 <- d[d$sesid == 2, ]
d1 <- d[d$subid %in% d2$subid, ]
d1 <- d1[d1$sesid == 1, ]

vars <- c('num_pastyear', 'female', 'depression', 'exp_b_pos', 'FC_b_pos', 'age_mri', 'black', 'white', 'hispanic', 'BMIperc', 'PubCat', 'IPR') 
cleanvars <- c('Violence', 'Female', 'Depression', 'Expansion', 'Connectivity', 'Age',
                         'Black', 'White', 'Hispanic', 'BMI', 'Puberty', 'IPR')
# Session 1
corr_ses1_df <- d1[, vars]

names(corr_ses1_df) <- cleanvars

corr_ses1_all <- round(cor(corr_ses1_df), 3)
ggcorr_ses1_plot <- ggcorrplot(corr_ses1_all) #+ ggtitle('Time 1')

# Session 2
corr_ses2_df <- d2[, vars]

names(corr_ses2_df) <- cleanvars

corr_ses2_all <- round(cor(corr_ses2_df), 3)
ggcorr_ses2_plot <- ggcorrplot(corr_ses2_all) #+ ggtitle('Time 2')

png('/Users/flutist4129/Documents/Northwestern/projects/violence_sex_development/plots/time2_corrplot.png', width=8000, height=5000, res=1000)
ggcorr_ses2_plot
dev.off()

# Combine (January 6, 2025: Defunct)
ggcorr_plot <- ggarrange(ggcorr_ses1_plot, ggcorr_ses2_plot, 
                            common.legend = TRUE, legend= 'bottom')

pdf('/Users/flutist4129/Documents/Northwestern/projects/violence_sex_development/plots/allvars_corrplot.pdf', width=8, height=5)
ggcorr_plot
dev.off()

png('/Users/flutist4129/Documents/Northwestern/projects/violence_sex_development/plots/allvars_corrplot.png', width=8000, height=5000, res=1000)
ggcorr_plot
dev.off()