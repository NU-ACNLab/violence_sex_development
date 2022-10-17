### Age trajectories for number of violence exposures & internalizing by sex
###
### Ellyn Butler
### October 10, 2022

library('lme4')
library('gamm4')

basedir <- '/projects/b1108/studies/mwmh/data/'

demo_df <- read.csv(paste0(basedir, 'processed/demographic/demographics_2022-10-04.csv'))
depanx_df <- read.csv(paste0(basedir, 'processed/clinical/depanx_2022-10-04.csv'))
viol_df <- read.csv(paste0(basedir, 'processed/violence/violence_2022-10-06.csv'))

df_list <- list(demo_df, depanx_df, immune_df, viol_df)
final_df <- Reduce(function(x, y) merge(x, y), df_list)

########### GAMMs

gamm_mod <- gamm4(num_pastyear ~ female + s(age_mri, k=4, bs='cr') +
              s(age_mri, by=female, k=4, bs='cr'), family=poisson,
              random=~(1|subid), data=final_df)


########### LMEs

first_df <- final_df[final_df$sesid == 1, ]
first_df <- first_df[first_df$num_pastyear < 1000, ]
pois_mod <- glm(num_pastyear ~ female*age_mri, data=first_df, family = poisson(link=log))
summary(pois_mod)
