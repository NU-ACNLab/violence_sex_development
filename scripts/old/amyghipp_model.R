### This script tests amyg-hipp connectivity in the Seitzman atlas as the mediator
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

conn_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/neuroimaging/tabulated/amygconn_2021-12-12.csv')
viol_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/violence/violence_2022-10-06.csv')
int_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/clinical/depanx_2022-10-04.csv')
demo_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/demographic/demographics_2022-11-07.csv')

df <- merge(conn_df, viol_df)
df <- merge(df, int_df)
df <- merge(df, demo_df)

df <- df[, c('subid', 'sesid', 'female', 'num_pastyear', 'RCADS_sum', 'region237')]

# remove rows with NAs
df <- df[complete.cases(df), ]

df$femaleXnum_pastyear <- df$female*df$num_pastyear

# remove extreme outlier
df <- df[df$num_pastyear < 100, ]

# use only the first time point (get to multilevel later: p16 of `mediation` pdf)
df1 <- df[df$sesid == 1, ]

# scale columns to make on similar scales
df1[, 4:6] <- scale(df1[, 4:6])

# models
model.m <- lm(region237 ~ female + num_pastyear + femaleXnum_pastyear, data = df1)

model.y <- lm(RCADS_sum ~ region237 + female + num_pastyear + femaleXnum_pastyear,
                           data = df1)

model.dir <- lm(RCADS_sum ~ female + num_pastyear + femaleXnum_pastyear, data = df1)

summary(model.m)
summary(model.y)
summary(model.dir)

############# lavaan
fem_df <- df1[df1$female == 1, ]
model <- "
  # direct effect
  RCADS_sum ~ c*num_pastyear
  # mediator
  region237 ~ a*num_pastyear
  RCADS_sum ~ b*region237
  # indirect effect (a*b)
  ab := a*b
  # total effect
  total := c + (a*b)
"
fit <- lavaan::sem(model, data = fem_df, se = 'boot', bootstrap = 10000)

est <- lavaan::parameterEstimates(fit, boot.ci.type = 'bca.simple')



save(fit, file = '/projects/b1108/projects/violence_sex_development/models/lavaan_output/amyghipp_fit.RData')
save(est, file = '/projects/b1108/projects/violence_sex_development/models/lavaan_output/amyghipp_est.RData')
