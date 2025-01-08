### This script creates Table 1
###
### Ellyn Butler
### October 9, 2024


library(table1) # v 1.4.3
library(dplyr) # v 1.1.4


d2 <- read.csv('~/Documents/Northwestern/projects/violence_sex_development/data/combined_data_2024-10-07.csv')
d2 <- d2[d2$sesid == 2, ]
d2 <- d2[!is.na(d2$exp_b_pos) & !is.na(d2$FC_b_pos), ]

d2$Age <- d2$age_mri

d2$Sex <- recode(d2$female, `1`='Female', `0`='Male')
d2$Sex <- factor(d2$Sex)

d2$Black <- recode(d2$black, `1`='Yes', `0`='No')
d2$Black <- factor(d2$Black)

d2$White <- recode(d2$white, `1`='Yes', `0`='No')
d2$White <- factor(d2$White)

d2$Hispanic <- recode(d2$hispanic, `1`='Yes', `0`='No')
d2$Hispanic <- factor(d2$Hispanic)

d2$BMI_Percentile <- d2$BMIperc
d2$Puberty_Category <- d2$PubCat
d2$Income_to_Poverty_Ratio <- d2$IPR
d2$Number_of_Violent_Events <- d2$num_pastyear
d2$Depression <- d2$depression


table1(~ Age + Black + White + Hispanic + BMI_Percentile + 
    Puberty_Category + Income_to_Poverty_Ratio + Number_of_Violent_Events +
    Depression | Sex, data=d2)
