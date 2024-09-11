### This script contains a preliminary analysis using models from
### from Bauer, Preacher & Gil (2006) written in R (p7)
### https://stats.oarc.ucla.edu/r/faq/how-can-i-perform-mediation-with-multilevel-data-method-2/
###
### Ellyn Butler
### August 11, 2024 - September 9, 2024

set.seed(2000)

library(lme4)
library(reshape2)
library(nlme)
library(MASS)
library(dplyr)
#library(lmerTest) #confint

############################# 2 sessions per person ############################

d <- read.csv('~/Documents/Northwestern/projects/violence_sex_development/data/combined_data_2024-09-09.csv')
d <- d[!is.na(d$exp_b_pos) & !is.na(d$FC_b_pos) & !is.na(d$RCADS_sum) & !is.na(d$num_pastyear), ]

d <- d[d$num_pastyear < 365,]
#d <- d %>%
#  group_by(subid) %>%
#  filter(n_distinct(sesid) == 2) %>%
#  ungroup()

# checking correlations
cor(d$num_pastyear, d$RCADS_sum) #0.1083687
cor(d$num_pastyear, d$exp_b_pos) #0.002999601
cor(d$num_pastyear, d$FC_b_pos) #0.1265699
cor(d$exp_b_pos, d$RCADS_sum) #0.02270884
cor(d$FC_b_pos, d$RCADS_sum) #0.02911495

####################### Without covariates or interaction #######################
###### Y ~ X
ctrl <- lmeControl(opt='optim')
m1 <- nlme::lme(RCADS_sum ~ num_pastyear, random = ~ 1 + num_pastyear | subid, data = d,
                control = ctrl) # significant
summary(m1)

###### M ~ X
m2.1 <- nlme::lme(exp_b_pos ~ num_pastyear, random = ~ 1 + num_pastyear | subid, data = d,
                control = ctrl)
summary(m2.1)
m2.2 <- nlme::lme(FC_b_pos ~ num_pastyear, random = ~ 1 + num_pastyear | subid, data = d,
                control = ctrl) # significant!
summary(m2.2)

###### Y ~ M + X
m3.1 <- nlme::lme(RCADS_sum ~ exp_b_pos + num_pastyear, 
                random = list(subid = pdDiag(~ exp_b_pos + num_pastyear)), data = d,
                control = ctrl) #Error: ... fewer observations than random effects in all level 1 groups
summary(m3.1)
m3.2 <- nlme::lme(RCADS_sum ~ FC_b_pos + num_pastyear, 
                random = list(subid = pdDiag(~ FC_b_pos + num_pastyear)), data = d,
                control = ctrl) #Error: ... fewer observations than random effects in all level 1 groups
summary(m3.2)

###### Y ~ M 
m4.1 <- nlme::lme(RCADS_sum ~ exp_b_pos, random = ~ 1 + exp_b_pos | subid, data = d,
                control = ctrl)
summary(m4.1)
m4.2 <- nlme::lme(RCADS_sum ~ FC_b_pos, random = ~ 1 + FC_b_pos | subid, data = d,
                control = ctrl) 
summary(m4.2)

# reshape #fid to sesid
stacked <- melt(d, id.vars = c("sesid", "subid", "num_pastyear", "salvenb_pos"),
   measure.vars = c("RCADS_sum", "salvenb_pos"), value.name = "z")
stacked <- within(stacked, {
  sy <- as.integer(variable == "RCADS_sum")
  sm <- as.integer(variable == "salvenb_pos")
})

## show all data for id 1
stacked[stacked$subid == 'MWMH107', ]

## fit model
mm <- nlme::lme(z ~ 0 + sm + sm:num_pastyear + sy + sy:salvenb_pos + sy:num_pastyear, data = stacked,
                  random = list(~ 0 + sm + sm:num_pastyear + sy + sy:salvenb_pos + sy:num_pastyear | subid, ~ 0 + sm | sesid),
                  control = ctrl)
                  #No errors, but lots of warnings: Singular precision matrix in level -1, block 1
mma <- nlme::lme(z ~ 0 + sm + sm:num_pastyear + sy + sy:salvenb_pos + sy:num_pastyear, data = stacked,
                  random = list(~ 0 + sm + sm:num_pastyear + sy + sy:salvenb_pos | subid, ~ 0 + sm | sesid),
                  control = ctrl)
                  #Error in solve.default(estimates[dimE[1] - (p:1), dimE[2] - (p:1), drop = FALSE]) : 
                  #system is computationally singular: reciprocal condition number = 2.1593e-17

## view summary and save summary object to 'smm'
(smm <- summary(mm))

## fit model
mm.alt <- nlme::lme(z ~ 0 + sm + sm:num_pastyear + sy + sy:salvenb_pos + sy:num_pastyear,
    data = stacked, random = ~0 + sm + sm:num_pastyear + sy + sy:salvenb_pos +
        sy:num_pastyear | subid, weights = varIdent(form = ~1 | variable), control = ctrl)
        #Error in lme.formula(z ~ 0 + sm + sm:num_pastyear + sy + sy:salvenb_pos +  : 
        #fewer observations than random effects in all level 1 groups

## view summary
summary(mm.alt)

## view fixed effects estimates
fixef(mma)

## product of 'a' and 'b' paths
(ab <- prod(fixef(mma)[c("sm:num_pastyear", "sy:salvenb_pos")]))

## covariance between random effects
(rcov <- as.numeric(VarCorr(mma)[row.names(VarCorr(mma)) == 'sy:salvenb_pos', 5]))
#should be close to .113/.16
#Q: If we assume that the random effects are equal for everyone, this is 0, right?

## indirect effect
ab + rcov # .6^2 + .113/.16 = 1.066... very close! good

## total effect
ab + rcov + fixef(mma)["num_pastyear:sy"]


# TO DO
## can do with 2 sessions?
## assess significance? check out paper

####################### Without covariates but interaction ###########################

d[, c('depression', 'num_pastyear', 'age_mri', 'exp_b_pos', 'FC_b_pos')] <- scale(d[, c('depression', 'num_pastyear', 'age_mri', 'exp_b_pos', 'FC_b_pos')])

###### Y ~ X
m1 <- nlme::lme(depression ~ num_pastyear*female, random = ~ 1 | subid, data = d,
                control = ctrl) # just female significant
                #list(subid = pdDiag(~ 1 + num_pastyear))
summary(m1)

###### M ~ X
m2.1 <- nlme::lme(exp_b_pos ~ num_pastyear*female, random = ~ 1 | subid, data = d,
                control = ctrl) # female significant
summary(m2.1)
m2.2 <- nlme::lme(FC_b_pos ~ num_pastyear*female, random = ~ 1 | subid, data = d,
                control = ctrl) # female significant
summary(m2.2)

###### Y ~ M
m2.1 <- nlme::lme(depression ~ exp_b_pos*female, random = ~ 1 | subid, data = d,
                control = ctrl) # none
summary(m2.1)
m2.2 <- nlme::lme(depression ~ FC_b_pos*female, random = ~ 1 | subid, data = d,
                control = ctrl) # none
summary(m2.2)

######################### With covariates and interaction ############################
###### Y ~ X
d[, c('RCADS_sum', 'num_pastyear', 'age_mri', 'exp_b_pos', 'FC_b_pos')] <- scale(d[, c('RCADS_sum', 'num_pastyear', 'age_mri', 'exp_b_pos', 'FC_b_pos')])
m1 <- nlme::lme(RCADS_sum ~ num_pastyear*female*age_mri, random = ~ 1 | subid, data = d,
                control = ctrl) # interaction significant... the association between the number of violence (when include all subjects but not just 2 session subjects)
                # exposures in the past year and internalizing symptoms is higher among females than males
                # female significant (when include all subjects)
summary(m1)

###### M ~ X
m2.1 <- nlme::lme(exp_b_pos ~ num_pastyear*female*age_mri, random = ~ 1 | subid, data = d,
                control = ctrl) # female significant... females have a less expansive SN (only when include just two sessions)
                # age sig... SN expands with age (with all sessions and only two sessions)
summary(m2.1)
m2.2 <- nlme::lme(FC_b_pos ~ num_pastyear*female*age_mri, random = ~ 1 | subid, data = d,
                control = ctrl) # female significant... females have less connectivity in the SN (only when just include two sessions)
                # age not sig either way
summary(m2.2)

###### Y ~ M 
m4.1 <- nlme::lme(RCADS_sum ~ exp_b_pos*female*age_mri, random = ~ 1 | subid, data = d,
                control = ctrl) # after scaling, female sig (with or without all subjects)
summary(m4.1)
m4.2 <- nlme::lme(RCADS_sum ~ FC_b_pos*female*age_mri, random = ~ 1 | subid, data = d,
                control = ctrl) # after scaling, female sig (with or without all subjects)
summary(m4.2)


#try ses 1 outcome as covariates 


# try 3 way interaction... messy