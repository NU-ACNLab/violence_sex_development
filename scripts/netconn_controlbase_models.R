### This script tests network connectivity in the Seitzman atlas as the mediators
### connecting violence exposure X sex with internalizing symptoms
###
### Ellyn Butler
### July 1, 2023 - July 2, 2023

# Resources
# https://stats.oarc.ucla.edu/r/faq/how-can-i-perform-mediation-with-multilevel-data-method-2/
# ^ doesn't work with continuous treatment effects


library(mediation) # v4.5.0
library(lavaan) # v0.6-13
library(reshape2)

set.seed(2000)

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
             mediators)]

# remove rows with NAs
df <- df[complete.cases(df), ]

df$femaleXnum_pastyear <- df$female*df$num_pastyear

# remove extreme outlier
df <- df[df$num_pastyear < 100, ]

# scale columns to make on similar scales
df[, 4:18] <- scale(df[, 4:18])

# use only the first time point (get to multilevel later: p16 of `mediation` pdf)
df1 <- df[df$sesid == 1, ]
names(df1)[4:ncol(df1)] <- paste0(names(df1[4:ncol(df1)]), '_1')
df1 <- df1[, names(df1) != 'sesid']
df2 <- df[df$sesid == 2, ]
names(df2)[4:ncol(df2)] <- paste0(names(df2[4:ncol(df2)]), '_2')
df2 <- df2[, names(df2) != 'sesid']

short_df <- merge(df1, df2)

for (med in mediators) {
  model.m <- lm(as.formula(paste0(med, '_2 ~ female + num_pastyear_2 + femaleXnum_pastyear_2 + RCADS_sum_1')),
                           data = short_df)
  assign(paste0('model.m_', med), model.m)
  model.y <- lm(as.formula(paste0('RCADS_sum_2 ~ ', med, '_2 + female + num_pastyear_2 + femaleXnum_pastyear_2  + RCADS_sum_1')),
                           data = short_df)
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
mediators[p.adjust(ps, method = 'fdr') < .05]
# "CinguloOpercular" "VentralAttention" "Reward"


############################### Ventral Attention ##############################

# contrasts
model.dir <- lm(RCADS_sum_2 ~ female + num_pastyear_2 + femaleXnum_pastyear_2 + RCADS_sum_1, data = short_df)

# the mods
summary(model.m_VentralAttention)
summary(model.y_VentralAttention)
summary(model.dir)

############# lavaan
fem_df <- short_df[short_df$female == 1, ]
model <- "
  # mediator
  VentralAttention_2 ~ a*num_pastyear_2 + a1*RCADS_sum_1
  RCADS_sum_2 ~ b*VentralAttention_2 + b1*num_pastyear_2 + b2*RCADS_sum_1
  # indirect effect (a*b)
  ab := a*b
"
fit <- lavaan::sem(model, data = fem_df, se = 'boot', bootstrap = 10000)

est <- lavaan::parameterEstimates(fit, boot.ci.type = 'bca.simple')

save(fit, file = '/projects/b1108/projects/violence_sex_development/models/lavaan_output/VentralAttention_controlbase_fit.RData')
save(est, file = '/projects/b1108/projects/violence_sex_development/models/lavaan_output/VentralAttention_controlbase_est.RData')
