### This script runs all of the models for the sex differences in internalizing,
### violence exposure, and amygdala connectivity paper
###
### Ellyn Butler & Ryan Chen
### November 25, 2022

### Load libraries
library(nlme)
library(lme4)

### Load data
final_df <- read.csv('combined_data.csv')
final_df <- final_df[final_df$num_pastyear < 1000, ]
final_df$pastyear <- ifelse(final_df$num_pastyear > 0, 1, 0)
final_df$RCADS_sum <- final_df$RCADS_sum + 1


### ARE MALES EXPOSED TO VIOLENCE MORE THAN FEMALES, DOES GAP WIDEN ACROSS DEV
### violence.male is the main effect only model (female signif at p = 0.00508)
### control.stress.female is the model with interaction (interact not signif)
### for reporting purposes:
### confint(violence.male, parm = 'var-cov') = (1.6127, 3.1533), var.est=2.225
### confint(violence.male.age, parm = 'var-cov') = (1.62, 3.171), var.est=2.267

violence.nofemale <- glmer.nb(num_pastyear ~ age_lab + (1|subid), data = final_df, nAGQ = 17)
violence.female <- update(violence.nofemale, ~ . + female)
violence.female.age <- update(violence.female, ~ . + female:age_lab)
anova(violence.nofemale, violence.female)
anova(violence.female, violence.female.age)
confint(violence.female, method = 'boot')
confint(violence.female.age, method = 'boot')

### DO FEMALES EXPERIENCE MORE SEVERE IS THAN MALES, DOES GAP WIDEN ACROSS DEV
### internalize.female is the main effect only model (female signif at p = 0.0135)
### internalize.female.age is the model with interaction (interact not signif)
### for reporting purposes:
### confint(internalize.female) = (43.1597, 68.4616), var.est= 55.04156
### confint(internalize.female.age) = (43.1956, 68.505), var.est= 55.03

internalize.nofemale <- lmer(RCADS_sum ~ age_lab + (1|subid), data = final_df, na.action =  na.exclude)
internalize.female <- update(internalize.nofemale, ~ . + female, data = final_df)
internalize.female.age <- update(internalize.female, ~ . + female*age_lab, data = final_df)
anova(internalize.nofemale, internalize.female, REML = F)
anova(internalize.female, internalize.female.age, REML = F)
confint(internalize.female, method = 'boot')
# plot(internalize.female, resid(., type = 'n') ~ fitted(.)|sesid, abline=0)

################################################################################
### CORRELATION OF VIOLENCE AND INTERNALIZING SYMPTOM SEVERITY #################
### for each subject id, we sample either sesid=1 or sesid = 2 with equal prob.
### this sampling is done for males only, females only, and both combined.
### the correlations are calculated for each of the three cases and saved.
### Repeat for 1000 times for the bootstrap interval and estimate

rm = rep(NA, 1000)
rf = rep(NA, 1000)
ra = rep(NA, 1000)
for(i in c(1:1000)) {
  female.sampled.time <- final_df |> filter(female == 1) |> group_by(subid) |> slice_sample(n=1)
  male.sampled.time <- final_df |> filter(female == 0) |> group_by(subid) |> slice_sample(n=1)
  combined.sampled.time <- final_df |> group_by(subid) |> slice_sample(n=1)
  ra[i] <- with(combined.sampled.time, cor(num_pastyear, RCADS_sum, use = 'complete.obs'))
  rm[i] <- with(male.sampled.time, cor(num_pastyear, RCADS_sum, use = 'complete.obs'))
  rf[i] <- with(female.sampled.time, cor(num_pastyear, RCADS_sum, use = 'complete.obs'))
}
quantile(rm, probs = c(0.025, 0.975)) # (0.0346, 0.224587)
quantile(rf, probs = c(0.025, 0.975)) # (0.2086 0.3325)
mean(ra) # 0.1603

################################################################################
### CONTROLLING FOR PEER STRESSOR EVENTS, SEX DIFF IN ISS NO LONGER SIGNIF.#####
### control.stress is the main effect only model (female signif at p = 0.0156)
### control.stress.female is the model with interaction (interact not signif)

stress.nofemale <- lmer(RCADS_sum ~ age_lab + peer_conflict + (1|subid), data=final_df, na.action = na.exclude)
stress.female <- update(control.stress, ~. + female)
stress.female.stress <- update(control.stress, ~. + female:peer_conflict)

anova(stress.nofemale, stress.female, REML = F)
anova(stress.female, stress.female.stress, REML = F)

confint(stress.female.stress, method = 'boot')

### Amygdala connectivity
# nothing significant

region2 <- lmer(region2 ~ age_lab + female + num_pastyear + (1|subid), data = final_df, na.action = na.exclude)
region2.interact <- update(region2, ~. + female * num_pastyear)
region14 <- lmer(region14 ~ age_lab + female + num_pastyear + (1|subid), data = final_df)
region14.interact <- update(region14, ~. + female * num_pastyear)
region237 <- lmer(region237 ~ age_lab + female + num_pastyear + (1|subid), data = final_df, na.action = na.exclude)
region237.interact <- update(region237, ~. + female * num_pastyear)
region261 <- lmer(region261 ~ age_lab + female + num_pastyear + (1|subid), data = final_df, na.action = na.exclude)
region261.interact <- update(region261, ~. + female * num_pastyear)
region281 <- lmer(region281 ~ age_lab + female + num_pastyear + (1|subid), data = final_df, na.action = na.exclude)
region281.interact <- update(region281, ~. + female * num_pastyear)


### SOME DIAGNOSTICS, I.E. PART OF THE LIMITATIONS
# check random effects are normal with qqplots
qqnorm(scale(ranef(violence.male)[,1])); abline(0,1)
qqnorm(scale(ranef(violence.male.age)[,1])); abline(0,1)

qqnorm(scale(ranef(internalize.female)$subid[,1])); abline(0,1)
qqnorm(scale(ranef(internalize.female.age)$subid[,1])); abline(0,1)

qqnorm(scale((ranef(control.stress)$subid[,1]))); abline(0,1)
qqnorm(scale(ranef(control.stress.female)$subid[,1])); abline(0,1)

qqnorm(scale(ranef(region2)$subid[,1])); abline(0,1)
qqnorm(scale(ranef(region237)$subid[,1])); abline(0,1)
qqnorm(scale(ranef(region261)$subid[,1])); abline(0,1)
qqnorm(scale(ranef(region281)$subid[,1])); abline(0,1)

# check pearson residuals of regressions
plot(violence.female, resid(.) ~ fitted(.))
plot(internalize.female, resid(.) ~ fitted(.))
plot(control.stress, resid(.) ~ fitted(.))
plot(region2, resid(.) ~ fitted(.))
plot(region14, resid(.) ~ fitted(.))
plot(region237, resid(.) ~ fitted(.))
plot(region261, resid(.) ~ fitted(.))
plot(region281, resid(.) ~ fitted(.))
## residuals for regions 14, 281 have clusters, likely due to female/male

### GOF FOR NB GLMER
# checking the nAGQ provides good coefficient estimates
violence.female.nagq0 <- update(violence.female, nAGQ = 0)
violence.female.nagq15 <- update(violence.female, nAGQ = 15)
violence.female.nagq25 <- update(violence.female, nAGQ = 25)
violence.female.nagq0@beta; (violence.female.nagq0 |> summary())$varcor
violence.female.nagq15@beta; (violence.female.nagq15 |> summary())$varcor
violence.female.nagq25@beta; (violence.female.nagq25 |> summary())$varcor
