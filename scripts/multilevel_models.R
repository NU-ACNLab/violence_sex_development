### This script contains a preliminary analysis using models from
### from Bauer, Preacher & Gil (2006) written in R (p7)
### https://stats.oarc.ucla.edu/r/faq/how-can-i-perform-mediation-with-multilevel-data-method-2/
###
### Ellyn Butler
### August 11, 2024

set.seed(2024)

library(lme4)
library(reshape2)
library(nlme)
library(MASS)
library(dplyr)
#library(lmerTest) #confint

############################# 2 sessions per person ############################

d <- read.csv('~/Documents/Northwestern/projects/violence_sex_development/data/tmp_2024-08-11.csv')
d <- d[!is.na(d$salvenb_pos) & !is.na(d$depression) & !is.na(d$num_pastyear), ]
d <- d %>%
  group_by(subid) %>%
  filter(n_distinct(sesid) == 2) %>%
  ungroup()

# checking correlations
cor(d$num_pastyear, d$salvenb_pos) #-0.005091636
cor(d$num_pastyear, d$depression) #0.09284513
cor(d$salvenb_pos, d$depression) #0.01357412

###### Y ~ X
ctrl <- lmeControl(opt='optim')
m1 <- nlme::lme(depression ~ num_pastyear, random = ~ 1 + num_pastyear | subid, data = d,
                control = ctrl)
summary(m1)

###### M ~ X
m2 <- nlme::lme(salvenb_pos ~ num_pastyear, random = ~ 1 + num_pastyear | subid, data = d,
                control = ctrl)
summary(m2)

###### Y ~ M + X
m3 <- nlme::lme(depression ~ salvenb_pos + num_pastyear, 
                random = list(subid = pdDiag(~ salvenb_pos + num_pastyear)), data = d,
                control = ctrl) #Error: ... fewer observations than random effects in all level 1 groups
summary(m3)

# reshape #fid to sesid
stacked <- melt(d, id.vars = c("sesid", "subid", "num_pastyear", "salvenb_pos"),
   measure.vars = c("depression", "salvenb_pos"), value.name = "z")
stacked <- within(stacked, {
  sy <- as.integer(variable == "depression")
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
