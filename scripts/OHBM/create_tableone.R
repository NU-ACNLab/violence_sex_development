### This script creates Table 1
###
### Ellyn Butler
### October 13, 2023


library(table1) # v 1.4.3
library(dplyr) # v 1.0.10

# T1
#df <- read.csv('~/Documents/Northwestern/projects/violence_mediation/data/combined_data.csv')
#df <- df[df$sesid == 1, ]

demo_df <- read.csv('~/Documents/Northwestern/studies/mwmh/data/raw/demographic/MWMH_EB_July2022.csv')
df <- read.csv('~/Documents/Northwestern/studies/mwmh/data/processed/demographic/demographics_2023-12-04.csv')
demo_df$subid <- paste0('MWMH', demo_df$ID)


df <- merge(df, demo_df, by='subid')
#df <- merge(df, demo_df2)

df$Age <- df$age_mri

df$Sex <- recode(df$female, `1`='Female', `0`='Male')
df$Sex <- factor(df$Sex)

df$Black <- recode(df$black, `1`='Yes', `0`='No')
df$Black <- factor(df$Black)

df$White <- recode(df$white, `1`='Yes', `0`='No')
df$White <- factor(df$White)

df$Hispanic <- recode(df$v1.c.ahispan, `1`='Yes', `0`='No')
df$Hispanic <- factor(df$Hispanic)

#df$Violence <- recode(df$ever, `1`='Violence Exposed', `0`='Not Violence Exposed')

#df$Session <- df$sesid

df$Session <- recode(df$sesid, `1`='Session 1', `2`='Session 2')

df <- df[!is.na(df$Age), ]

table1(~ Age + Sex + Black + White + Hispanic | Session, data=df)

# Number of subjects with both time points
subid1 <- df[df$sesid == 1, 'subid']
subid2 <- df[df$sesid == 2, 'subid']
sum(subid1 %in% subid2)
