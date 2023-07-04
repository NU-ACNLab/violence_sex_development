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

# remove extreme outlier
df <- df[df$num_pastyear < 100, ]

# use only the first time point (get to multilevel later: p16 of `mediation` pdf)
df1 <- df[df$sesid == 1, ]

# scale columns to make on similar scales
df1[, 4:18] <- scale(df1[, 4:18])

# create interaction variable
df1$femaleXnum_pastyear <- df1$female*df1$num_pastyear

for (med in mediators) {
  model.m <- lm(as.formula(paste0(med, ' ~ female + num_pastyear + femaleXnum_pastyear')),
                           data = df1)
  assign(paste0('model.m_', med), model.m)
  model.y <- lm(as.formula(paste0('RCADS_sum ~ ', med, ' + female + num_pastyear + femaleXnum_pastyear')),
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
mediators[p.adjust(ps, method = 'fdr') < .05]
# only ventral attention less than 0.05


############################### Ventral Attention ##############################

# contrasts
model.dir <- lm(RCADS_sum ~ female + num_pastyear + femaleXnum_pastyear, data = df1)

med.out1 <- mediate(model.m_VentralAttention, model.y_VentralAttention,
                    treat = 'femaleXnum_pastyear', mediator = 'VentralAttention',
                    control.value = 0, treat.value = 1)

med.out2 <- mediate(model.m_VentralAttention, model.y_VentralAttention,
                    treat = 'femaleXnum_pastyear', mediator = 'VentralAttention',
                    control.value = 0, treat.value = 2)

med.out3 <- mediate(model.m_VentralAttention, model.y_VentralAttention,
                    treat = 'femaleXnum_pastyear', mediator = 'VentralAttention',
                    control.value = 0, treat.value = 3)

med.out4 <- mediate(model.m_VentralAttention, model.y_VentralAttention,
                    treat = 'femaleXnum_pastyear', mediator = 'VentralAttention',
                    control.value = 0, treat.value = 4)

med.out5 <- mediate(model.m_VentralAttention, model.y_VentralAttention,
                    treat = 'femaleXnum_pastyear', mediator = 'VentralAttention',
                    control.value = 0, treat.value = 5)

med.out10 <- mediate(model.m_VentralAttention, model.y_VentralAttention,
                    treat = 'femaleXnum_pastyear', mediator = 'VentralAttention',
                    control.value = 0, treat.value = 10)

med.out15 <- mediate(model.m_VentralAttention, model.y_VentralAttention,
                    treat = 'femaleXnum_pastyear', mediator = 'VentralAttention',
                    control.value = 0, treat.value = 15)

med.out20 <- mediate(model.m_VentralAttention, model.y_VentralAttention,
                    treat = 'femaleXnum_pastyear', mediator = 'VentralAttention',
                    control.value = 0, treat.value = 20)

med.out30 <- mediate(model.m_VentralAttention, model.y_VentralAttention,
                    treat = 'femaleXnum_pastyear', mediator = 'VentralAttention',
                    control.value = 0, treat.value = 30)

med.out40 <- mediate(model.m_VentralAttention, model.y_VentralAttention,
                    treat = 'femaleXnum_pastyear', mediator = 'VentralAttention',
                    control.value = 0, treat.value = 40)


# the mods
summary(model.m_VentralAttention)
summary(model.y_VentralAttention)
summary(model.dir)

############# lavaan
fem_df <- df1[df1$female == 1, ]
model <- "
  VentralAttention ~ a*num_pastyear
  RCADS_sum ~ b*VentralAttention + b1*num_pastyear
  # indirect effect (a*b)
  ab := a*b
"
fit <- lavaan::sem(model, data = fem_df, se = 'boot', bootstrap = 10000)

est <- lavaan::parameterEstimates(fit, boot.ci.type = 'bca.simple')
#lhs   op     rhs label   est    se     z pvalue ci.lower ci.upper
#total ab :=  a*b    ab 0.074 0.067 1.098  0.272   -0.006 0.272


save(fit, file = '/projects/b1108/projects/violence_sex_development/models/lavaan_output/VentralAttention_fit.RData')
save(est, file = '/projects/b1108/projects/violence_sex_development/models/lavaan_output/VentralAttention_est.RData')
