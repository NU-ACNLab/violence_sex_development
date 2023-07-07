### This script runs all of the models for the sex differences in internalizing,
### violence exposure, and amygdala connectivity paper
###
### Ellyn Butler & Ryan Chen
### November 25, 2022

set.seed(2022)

### Load libraries
library(nlme)
library(lme4)
library(sjPlot)
library(dplyr)
library(ggplot2)

### Load data
base_dir <- '/Users/flutist4129/Documents/Northwestern/projects/violence_sex_development/'
final_df <- read.csv(paste0(base_dir, 'data/combined_data.csv'))
final_df <- final_df[final_df$num_pastyear < 1000, ]
final_df$pastyear <- ifelse(final_df$num_pastyear > 0, 1, 0)
final_df$RCADS_sum <- final_df$RCADS_sum + 1


### 1) ARE MALES EXPOSED TO VIOLENCE MORE THAN FEMALES, DOES GAP WIDEN ACROSS DEV
### violence.male is the main effect only model (female signif at p = 0.00508)
# ^ ... THIS DOESN'T EXIST. Do you mean violence.female?
### control.stress.female is the model with interaction (interact not signif)
# ^ ... THIS DOESN'T EXIST. Do you mean violence.female.age?
### for reporting purposes:
### confint(violence.male, parm = 'var-cov') = (1.6127, 3.1533), var.est=2.225
### confint(violence.male.age, parm = 'var-cov') = (1.62, 3.171), var.est=2.267

violence.nofemale <- glmer.nb(num_pastyear ~ age_lab + (1|subid), data = final_df, nAGQ = 17)
violence.female <- update(violence.nofemale, ~ . + female)
violence.female.age <- update(violence.female, ~ . + female:age_lab)
anova(violence.nofemale, violence.female)
anova(violence.female, violence.female.age)
confint(violence.female, method = 'boot') # not converging... do we need bootstrap CIs?
confint(violence.female.age, method = 'boot')

violence.mods <- tab_model(violence.nofemale, violence.female, violence.female.age)


### 2) DO FEMALES EXPERIENCE MORE SEVERE INT SYMP IS THAN MALES, DOES GAP WIDEN ACROSS DEV
### internalize.female is the main effect only model (female signif at p = 0.0135)
### internalize.female.age is the model with interaction (interact not signif)
### for reporting purposes:
### confint(internalize.female) = (43.1597, 68.4616), var.est= 55.04156
### confint(internalize.female.age) = (43.1956, 68.505), var.est= 55.03

internalize.nofemale <- lmer(RCADS_sum ~ age_lab + (1|subid), data = final_df, na.action =  na.exclude)
internalize.female <- update(internalize.nofemale, ~ . + female, data = final_df)
internalize.female.age <- update(internalize.female, ~ . + female:age_lab, data = final_df)
anova(internalize.nofemale, internalize.female, REML = F)
anova(internalize.female, internalize.female.age, REML = F)
confint(internalize.female, method = 'boot')
# plot(internalize.female, resid(., type = 'n') ~ fitted(.)|sesid, abline=0)

internalize.mods <- tab_model(internalize.nofemale, internalize.female, internalize.female.age)

################################################################################
### 3) CORRELATION OF VIOLENCE AND INTERNALIZING SYMPTOM SEVERITY ##############
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
quantile(rm, probs = c(0.025, 0.975)) # (0.033, 0.217)
quantile(rf, probs = c(0.025, 0.975)) # (0.206, 0.325)
mean(ra) # 0.160
mean(rm) # 0.124
mean(rf) # 0.269

# Exploratory
sexviol_df <- final_df[!is.na(final_df$age_lab) & !is.na(final_df$female) &
                       !is.na(final_df$num_pastyear) & !is.na(final_df$RCADS_sum), ]
internalize.sexviol <- lmer(RCADS_sum ~ female*num_pastyear + (1|subid),
                            data = sexviol_df)
sexviol.mods <- tab_model(internalize.sexviol)

sexviol_df$fit <- predict(internalize.sexviol)
sexviol_df$female <- as.factor(sexviol_df$female)
sexviol_df$Sex <- recode(sexviol_df$female, `1`='Female', `0`='Male')

fslope <- summary(internalize.sexviol)$coefficients['num_pastyear', 'Estimate'] +
            summary(internalize.sexviol)$coefficients['female:num_pastyear', 'Estimate']
fintercept <- summary(internalize.sexviol)$coefficients['(Intercept)', 'Estimate'] +
                summary(internalize.sexviol)$coefficients['female', 'Estimate']
mslope <- summary(internalize.sexviol)$coefficients['num_pastyear', 'Estimate']
mintercept <- summary(internalize.sexviol)$coefficients['(Intercept)', 'Estimate']

sexviol_plot <- ggplot(sexviol_df, aes(num_pastyear, RCADS_sum, color=Sex)) +
      geom_abline(slope=fslope, intercept=fintercept, color='#FF333F') +
      geom_abline(slope=mslope, intercept=mintercept, color='#334FFF') +
      geom_point(alpha = 0.3) + theme_linedraw() +
      ylab('Internalizing Symptom Severity') +
      xlab('Number of Violent Events in the Past Year') +
      scale_color_manual(values=c('#334FFF', '#FF333F'))

pdf(paste0(base_dir, 'plots/sexviol.pdf'), width=6, height=4)
sexviol_plot
dev.off()

################################################################################
### 4) CONTROLLING FOR PEER STRESSOR EVENTS, SEX DIFF IN IS NO LONGER SIGNIF ###
### control.stress is the main effect only model (female signif at p = 0.0156)
# ^ ... THIS DOESN'T EXIST. Do you mean peer.female?
### control.stress.female is the model with interaction (interact not signif)
# ^ ... THIS DOESN'T EXIST. Do you mean peer.female.stress?

int2.nofemale <- lmer(RCADS_sum ~ age_lab + peer_conflict + (1|subid), data=final_df, na.action = na.exclude)
int2.female <- update(int2.nofemale, ~. + female)
int2.female.peer <- update(int2.female, ~. + female:peer_conflict)

anova(int2.nofemale, int2.female, REML = F)
anova(int2.female, int2.female.peer, REML = F)

confint(int2.female.peer, method = 'boot')

int2.mods <- tab_model(int2.nofemale, int2.female, int2.female.peer)

################################################################################
### 5) Amygdala connectivity
# nothing significant... hm, no
#https://stats.stackexchange.com/questions/242109/model-failed-to-converge-warning-in-lmer

region2 <- lmer(region2 ~ age_lab + female + num_pastyear + (1|subid), data = final_df, na.action = na.exclude)
region2.interact <- update(region2, ~. + female:num_pastyear)
region14 <- lmer(region14 ~ age_lab + female + num_pastyear + (1|subid),
                            data = final_df, na.action = na.exclude)
region14.interact <- update(region14, ~. + female:num_pastyear)
region237 <- lmer(region237 ~ age_lab + female + num_pastyear + (1|subid), data = final_df, na.action = na.exclude)
region237.interact <- update(region237, ~. + female:num_pastyear)
region261 <- lmer(region261 ~ age_lab + female + num_pastyear + (1|subid), data = final_df, na.action = na.exclude)
region261.interact <- update(region261, ~. + female:num_pastyear)
region281 <- lmer(region281 ~ age_lab + female + num_pastyear + (1|subid), data = final_df, na.action = na.exclude)
region281.interact <- update(region281, ~. + female:num_pastyear)

amygconn_main_mods <- tab_model(region2, region14, region237, region261, region281,
                      digits=3)

amygconn_int_mods <- tab_model(region2.interact, region14.interact,
                     region237.interact, region261.interact, region281.interact,
                     digits=3)

# Adjusting for multiple comparisons using FDR
# age
p.adjust(c(0.211, 0.472, 0.009, 0.608, 0.659), method='fdr')

# female
p.adjust(c(0.311, 0.045, 0.266, 0.223, 0.106), method='fdr')

# violence
p.adjust(c(0.656, 0.447, 0.526, 0.840, 0.342), method='fdr')

# female x violence
p.adjust(c(0.037, 0.538, 0.083, 0.117, 0.221), method='fdr')


################################################################################
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
