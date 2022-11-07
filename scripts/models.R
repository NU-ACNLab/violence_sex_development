### Age trajectories for number of violence exposures & internalizing by sex
###
### Ellyn Butler
### October 10, 2022 - October 17, 2022

library('lme4') #glmer.nb
library('gamm4')
library('pscl')
#library('remotes')
library('NBZIMM')

#install_github('nyiuab/NBZIMM', force=T, build_vignettes=F)

basedir <- '/projects/b1108/studies/mwmh/data/'

final_df <- read.csv('/projects/b1108/projects/violence_sex_development/data/combined_data.csv')

# Remove outliers
final_df <- final_df[final_df$num_pastyear < 1000, ]

final_df$pastyear <- ifelse(final_df$num_pastyear > 0, 1, 0)


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
