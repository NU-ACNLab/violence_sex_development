### Age trajectories for number of violence exposures & internalizing by sex
###
### Ellyn Butler
### October 10, 2022 - October 17, 2022

library('lme4')
library('gamm4')
library('pscl')
library('remotes')

install_github('nyiuab/NBZIMM', force=T, build_vignettes=F)

basedir <- '/projects/b1108/studies/mwmh/data/'

demo_df <- read.csv(paste0(basedir, 'processed/demographic/demographics_2022-10-04.csv'))
depanx_df <- read.csv(paste0(basedir, 'processed/clinical/depanx_2022-10-04.csv'))
viol_df <- read.csv(paste0(basedir, 'processed/violence/violence_2022-10-06.csv'))

df_list <- list(demo_df, depanx_df, viol_df)
final_df <- Reduce(function(x, y) merge(x, y), df_list)

final_df <- final_df[, c('subid', 'sesid', 'female', 'age_lab', 'ever', 'num_pastyear')]
final_df <- final_df[!is.na(final_df$female) & !is.na(final_df$age_lab) & !is.na(final_df$num_pastyear), ]

# Remove outliers
final_df <- final_df[final_df$num_pastyear < 1000, ]

final_df$pastyear <- ifelse(final_df$num_pastyear > 0, 1, 0)

########### Descriptive

table(final_df$female)
summary(final_df$age_lab)
table(final_df$ever)

summary(final_df$num_pastyear)
table(final_df$num_pastyear)

table(final_df$pastyear)

########### GAMMs

gamm_mod <- gamm4(num_pastyear ~ female + s(age_lab, k=4, bs='cr') +
              s(age_lab, by=female, k=4, bs='cr'), family=poisson,
              random=~(1|subid), data=final_df)


########### LMEs

first_df <- final_df[final_df$sesid == 1, ]
first_df <- first_df[first_df$num_pastyear < 1000, ]
pois_mod <- glm(num_pastyear ~ female*age_lab, data=first_df, family = poisson(link=log))
summary(pois_mod)


# poisson

# zero-inflated poisson (you can choose to not buy something if you go to the store)

# hurdle model (you have to buy something if you go to the store)

# negative binomial (no truncation!)

# ^ But we are always in the store


##### Questions

# 1.)


#
