### This script tests network connectivity in the Seitzman atlas as the mediators
### connecting violence exposure X sex with internalizing symptoms
### https://stats.oarc.ucla.edu/r/faq/how-can-i-perform-mediation-with-multilevel-data-method-2/
### ^ doesn't work with continuous treatment effects
### https://search.r-project.org/CRAN/refmans/causalweight/html/medweightcont.html
### ^ crappy documentation
###
### Ellyn Butler
### July 1, 2023 - July 2, 2023

library(mediation) # v4.5.0
library(causalweight) # v1.0.4
library(lavaan) # v0.6-13

conn_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/neuroimaging/tabulated/networkconn_2023-07-01.csv')
viol_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/violence/violence_2022-10-06.csv')
int_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/clinical/depanx_2022-10-04.csv')
demo_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/demographic/demographics_2022-11-07.csv')

df <- merge(conn_df, viol_df)
df <- merge(df, int_df)
df <- merge(df, demo_df)

mediators <- c('SomatomotorDorsal', 'SomatomotorLateral', 'CinguloOpercular',
               'Auditory', 'DefaultMode', 'ParietoMedial', 'Visual',
               'FrontoParietal', 'Salience', 'VentralAttention',
               'DorsalAttention', 'MedialTemporalLobe', 'Reward')

df <- df[, c('subid', 'sesid', 'female', 'num_pastyear', 'RCADS_sum',
             mediators, 'white', 'age_mri', 'IPR')]

# remove rows with NAs
df <- df[complete.cases(df), ]

# scale columns to make on similar scales
#df[, 4:18] <- scale(df[, 4:18])

df$femaleXnum_pastyear <- df$female*df$num_pastyear

# remove extreme outlier
df <- df[df$num_pastyear < 100, ]

# use only the first time point (get to multilevel later: p16 of `mediation` pdf)
df1 <- df[df$sesid == 1, ]

df1[, c(4:18, 20:22)] <- scale(df1[, c(4:18, 20:22)])


for (med in mediators) {
  model.m <- lm(as.formula(paste0(med, ' ~ female + num_pastyear + femaleXnum_pastyear + white + age_mri + IPR')),
                           data = df1)
  assign(paste0('model.m_', med), model.m)
  model.y <- lm(as.formula(paste0('RCADS_sum ~ ', med, ' + female + num_pastyear + femaleXnum_pastyear + white + age_mri + IPR')),
                           data = df1)
  assign(paste0('model.y_', med), model.y)
}

# conduct FDR on model.m's. For significant ones, test mediation with different
# levels of the treatment

ps <- c(summary(model.m_SomatomotorDorsal)$coefficients[4, 4],
        summary(model.m_SomatomotorLateral)$coefficients[4, 4],
        summary(model.m_CinguloOpercular)$coefficients[4, 4],
        summary(model.m_Auditory)$coefficients[4, 4],
        summary(model.m_DefaultMode)$coefficients[4, 4],
        summary(model.m_ParietoMedial)$coefficients[4, 4],
        summary(model.m_Visual)$coefficients[4, 4],
        summary(model.m_FrontoParietal)$coefficients[4, 4],
        summary(model.m_Salience)$coefficients[4, 4],
        summary(model.m_VentralAttention)$coefficients[4, 4],
        summary(model.m_DorsalAttention)$coefficients[4, 4],
        summary(model.m_MedialTemporalLobe)$coefficients[4, 4],
        summary(model.m_Reward)$coefficients[4, 4])
p.adjust(ps, method = 'fdr')
# only ventral attention less than 0.05


############################### Ventral Attention ##############################

model.dir <- lm(RCADS_sum ~ female + num_pastyear + femaleXnum_pastyear + white + age_mri + IPR, data = df1)

# the mods
summary(model.m_VentralAttention)
summary(model.y_VentralAttention)
summary(model.dir)

model <- "
  # direct effect
  RCADS_sum ~ c*femaleXnum_pastyear + c1*female + c2*num_pastyear + c3*white + c4*age_mri + c5*IPR
  # mediator
  VentralAttention ~ a*femaleXnum_pastyear + a1*female + a2*num_pastyear + a3*white + a4*age_mri + a5*IPR
  RCADS_sum ~ b*VentralAttention
  # indirect effect (a*b)
  ab := a*b
  # total effect
  total := c + (a*b)
"
fit <- lavaan::sem(model, data = df1, se = 'boot', bootstrap = 5000)

est <- lavaan::parameterEstimates(fit, boot.ci.type = 'bca.simple')


save(fit, file = '/projects/b1108/projects/violence_sex_development/models/lavaan_output/VentralAttention_control_fit.RData')
save(est, file = '/projects/b1108/projects/violence_sex_development/models/lavaan_output/VentralAttention_control_est.RData')
