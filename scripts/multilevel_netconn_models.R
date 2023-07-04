### This script tests network connectivity in the Seitzman atlas as the mediators
### connecting violence exposure X sex with internalizing symptoms
### https://lavaan.ugent.be/tutorial/multilevel.html
###
### Ellyn Butler
### July 1, 2023 - July 2, 2023

library(mediation) # v4.5.0
library(lavaan) # v0.6-13
library(lme4) # v1.1-30
library(lmerTest) # v3.1-3

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


for (med in mediators) {
  model.m <- lmer(as.formula(paste0(med, ' ~ female + num_pastyear + femaleXnum_pastyear + (1 | subid)')),
                           data = df)
  assign(paste0('model.m_', med), model.m)
  model.y <- lmer(as.formula(paste0('RCADS_sum ~ ', med, ' + female + num_pastyear + femaleXnum_pastyear + (1 | subid)')),
                           data = df)
  assign(paste0('model.y_', med), model.y)
}

# conduct FDR on model.m's. For significant ones, test mediation with different
# levels of the treatment

ps <- c(anova(model.m_SomatomotorDorsal)[3, 'Pr(>F)'],
        anova(model.m_SomatomotorLateral)[3, 'Pr(>F)'],
        anova(model.m_CinguloOpercular)[3, 'Pr(>F)'],
        anova(model.m_Auditory)[3, 'Pr(>F)'],
        anova(model.m_DefaultMode)[3, 'Pr(>F)'],
        anova(model.m_ParietoMedial)[3, 'Pr(>F)'],
        anova(model.m_Visual)[3, 'Pr(>F)'],
        anova(model.m_FrontoParietal)[3, 'Pr(>F)'],
        anova(model.m_Salience)[3, 'Pr(>F)'],
        anova(model.m_VentralAttention)[3, 'Pr(>F)'],
        anova(model.m_DorsalAttention)[3, 'Pr(>F)'],
        anova(model.m_MedialTemporalLobe)[3, 'Pr(>F)'],
        anova(model.m_Reward)[3, 'Pr(>F)'])
p.adjust(ps, method = 'fdr')
mediators[p.adjust(ps, method = 'fdr') < 0.05]
# "CinguloOpercular" "Auditory" "FrontoParietal" "Salience" "VentralAttention" "Reward"

############################### Ventral Attention ##############################

# contrasts
model.dir <- lmer(RCADS_sum ~ female + num_pastyear + femaleXnum_pastyear + (1 | subid), data = df)

# the mods
anova(model.m_VentralAttention)
anova(model.y_VentralAttention)
anova(model.dir)

############# lavaan
fem_df <- df[df$female == 1, ]
# July 2, 2023: Not sure if what is below this is correct
# https://stats.stackexchange.com/questions/532616/mediation-with-multilevel-data-in-r-with-lavaan
model <- "
  level: 1
    # direct effect
    RCADS_sum ~ c*num_pastyear
    # mediator
    VentralAttention ~ a*num_pastyear
    RCADS_sum ~ b*VentralAttention
    # indirect effect (a*b)
    ab := a*b
    # total effect
    total := c + (a*b)
  level: 2
    # direct effect
    RCADS_sum ~ c*num_pastyear
    # mediator
    VentralAttention ~ a*num_pastyear
    RCADS_sum ~ b*VentralAttention
    # indirect effect (a*b)
    ab := a*b
    # total effect
    total := c + (a*b)
"
fit <- lavaan::sem(model, data = fem_df, bootstrap = 10000, cluster = 'subid',
                   se = 'robust.huber.white')

est <- lavaan::parameterEstimates(fit, boot.ci.type = 'bca.simple')


save(fit, file = '/projects/b1108/projects/violence_sex_development/models/lavaan_output/VentralAttention_multilevel_fit.RData')
save(est, file = '/projects/b1108/projects/violence_sex_development/models/lavaan_output/VentralAttention_multilevel_est.RData')
