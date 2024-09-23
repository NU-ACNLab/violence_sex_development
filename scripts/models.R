### This code focuses on session 2 for the analyses
###
### Ellyn Butler
### September 10, 2024 - September 23, 2024

set.seed(2000)

library(reshape2)
library(MASS)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(ggpubr)
#library(lmerTest) #confint

d <- read.csv('~/Documents/Northwestern/projects/violence_sex_development/data/combined_data_2024-09-23.csv')
dall <- d
d <- d[!is.na(d$exp_b_pos) & !is.na(d$FC_b_pos) & !is.na(d$RCADS_sum) & !is.na(d$num_pastyear), ]
d1 <- d[d$sesid == 1, ]
d2 <- d[d$sesid == 2, ]

dim(d2) # N = 220
range(d2$age_mri) # 14 - 18
mean(d2$ever) # 55.45%
mean(d2$num_pastyear > 0) # 45.45%
mean(d2[d2$female == 1, 'num_pastyear']) # 1.16
mean(d2[d2$female == 0, 'num_pastyear']) # 2.95
t.test(num_pastyear ~ female, data = d2)

################### Plot
# Violence rates
viol_df <- read.csv('~/Documents/Northwestern/studies/mwmh/data/processed/violence/violence_2022-10-06.csv')

viol_df <- merge(viol_df, d2)
nf <- nrow(viol_df[viol_df$female == 1, ])
nm <- nrow(viol_df[viol_df$female == 0, ])

pastyear_df <- data.frame(Variable=paste0('ETV', 1:7),
                      Violence=c('Family Hurt or Killed', 'Friends Hurt or Killed',
                        'Saw Attacked Knife', 'Saw Shot', 'Shoved Kicked Punched',
                        'Attacked Knife', 'Shot At'),
                      Sex=c(rep('Female', 7), rep('Male', 7)),
                      Sum=c(sum(viol_df[viol_df$female == 1, 'etv1_pastyear'])/nf,
                            sum(viol_df[viol_df$female == 1, 'etv2_pastyear'])/nf,
                            sum(viol_df[viol_df$female == 1, 'etv3_pastyear'])/nf,
                            sum(viol_df[viol_df$female == 1, 'etv4_pastyear'])/nf,
                            sum(viol_df[viol_df$female == 1, 'etv5_pastyear'])/nf,
                            sum(viol_df[viol_df$female == 1, 'etv6_pastyear'])/nf,
                            sum(viol_df[viol_df$female == 1, 'etv7_pastyear'])/nf,
                            sum(viol_df[viol_df$female == 0, 'etv1_pastyear'])/nm,
                            sum(viol_df[viol_df$female == 0, 'etv2_pastyear'])/nm,
                            sum(viol_df[viol_df$female == 0, 'etv3_pastyear'])/nm,
                            sum(viol_df[viol_df$female == 0, 'etv4_pastyear'])/nm,
                            sum(viol_df[viol_df$female == 0, 'etv5_pastyear'])/nm,
                            sum(viol_df[viol_df$female == 0, 'etv6_pastyear'])/nm,
                            sum(viol_df[viol_df$female == 0, 'etv7_pastyear'])/nm)
                      )


pastyear_df$Violence <- ordered(pastyear_df$Violence, c('Family Hurt or Killed', 'Friends Hurt or Killed',
  'Saw Attacked Knife', 'Saw Shot',
  'Attacked Knife', 'Shot At', 'Shoved Kicked Punched'))

prop_pastyear_plot <- ggplot(pastyear_df, aes(x=Violence, y=Sum, fill=Violence)) +
  facet_grid(. ~ Sex) + theme_linedraw() + geom_bar(stat='identity', position='dodge') +
  theme(legend.position='none', axis.title.x=element_blank(), axis.title.y=element_text(size=7),
		panel.spacing=unit(.1, 'lines'), axis.text.y=element_text(size=6),
    axis.text.x = element_text(angle=45, hjust=1, size=6)) +
  ylab('Average number of times in the past year') +
  scale_fill_manual(values=c('deepskyblue3', 'steelblue1', 'springgreen3',
  'palegreen1', 'pink1', 'violetred1', 'firebrick2'))

# Export
jpeg('~/Documents/Northwestern/projects/violence_sex_development/plots/pastyear_violence_ses-2.jpg', res=300, units='mm', width=120, height=75)
prop_pastyear_plot
dev.off() 

# Sex effects
viol_df$Sex <- recode(viol_df$female, `1`='Female', `0`='Male')
viol_df$Sex <- ordered(viol_df$Sex, c('Male', 'Female'))

int_plot <- ggplot(viol_df, aes(num_pastyear, depression, color=Sex)) +
      geom_smooth(method = 'lm') +
      geom_point(alpha = 0.3, ) + theme_linedraw() +
      ylab('Depression') +
      xlab('Number of Violent Events in the Past Year') +
      scale_color_manual(values=c('#334FFF', '#FF333F'))

exp_plot <- ggplot(viol_df, aes(exp_b_pos, depression, color=Sex)) +
      geom_smooth(method = 'lm') +
      geom_point(alpha = 0.3) + theme_linedraw() +
      ylab('Depression') +
      xlab('Salience Network Expansion') +
      scale_color_manual(values=c('#334FFF', '#FF333F'))

fc_plot <- ggplot(viol_df, aes(FC_b_pos, depression, color=Sex)) +
      geom_smooth(method = 'lm') +
      geom_point(alpha = 0.3) + theme_linedraw() +
      ylab('Depression') +
      xlab('Salience Network Connectivity') +
      scale_color_manual(values=c('#334FFF', '#FF333F'))

blank_plot <- ggplot() + theme_void()
combined <- ggarrange(
  int_plot, blank_plot, exp_plot, fc_plot, 
  nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom"
  ) 

# Export
base_dir <- '~/Documents/Northwestern/projects/violence_sex_development/plots/'
png(paste0(base_dir, 'sn_interaction_plots.png'), width=2000, height=2000, res=300)
combined 
dev.off()

################### Model
d1 <- d[d$sesid == 1, ]
d2 <- d[d$sesid == 2, ]
d2[, c('depression', 'num_pastyear', 'age_mri', 'exp_b_pos', 'FC_b_pos')] <- scale(d2[, c('depression', 'num_pastyear', 'age_mri', 'exp_b_pos', 'FC_b_pos')])
d2[, c('BMIperc', 'PubCat')] <- scale(d2[, c('BMIperc', 'PubCat')])

# use these for poster
boo1 <- lm(depression ~ num_pastyear*female, d=d2) #sig: female, and femaleXnum_pastyear
boo2 <- lm(exp_b_pos ~ num_pastyear*female, d=d2) #sig: none
boo2.2 <- lm(FC_b_pos ~ num_pastyear*female, d=d2) #sig: female
boo3 <- lm(depression ~ exp_b_pos*female, d=d2) #sig: exp_b_pos, female
boo3.2 <- lm(depression ~ FC_b_pos*female, d=d2) #sig: FC_b_pos, female, FC_b_posXfemale
tab_model(boo1, boo2, boo2.2, show.ci = FALSE)
tab_model(boo3, boo3.2, show.ci = FALSE)

# sensitivity
goo1 <- lm(depression ~ age_mri + black + white + otherrace + BMIperc + PubCat + num_pastyear*female, d=d2) #sig: female, and femaleXnum_pastyear
goo2 <- lm(exp_b_pos ~ age_mri + black + white + otherrace + BMIperc + PubCat + num_pastyear*female, d=d2) #sig: black
goo2.2 <- lm(FC_b_pos ~ age_mri + black + white + otherrace + BMIperc + PubCat + num_pastyear*female, d=d2) #sig: black, female
goo3 <- lm(depression ~ age_mri + black + white + otherrace + BMIperc + PubCat + exp_b_pos*female, d=d2) #sig: none
goo3.2 <- lm(depression ~ age_mri + black + white + otherrace + BMIperc + PubCat + FC_b_pos*female, d=d2) #sig: female, FC_b_posXfemale

##### Control for T1

# Impute brain values for subjects that have T2 but not T1 data
d1b <- dall[dall$sesid == 1 & dall$subid %in% d2$subid, ]
d2b <- d2
same <- c('subid', 'black', 'white', 'otherrace', 'female')
names(d1b)[!(names(d1b) %in% same)] <- paste0(names(d1b)[!(names(d1b) %in% same)], '_1')
names(d2b)[!(names(d2b) %in% same)] <- paste0(names(d2b)[!(names(d2b) %in% same)], '_2')
d1b <- d1b[, c(1, 3:length(names(d1b)))]
d2b <- d2b[, c(1, 3:length(names(d2b)))]

# How many missing values for each variable?
# Three subjects did not complete the MRI visit; 5 did not indicate how many
# times they had been exposed to violence in the past year; and 14 did not 
# have usable resting state data (look more into this)
sum(is.na(d1b$age_mri_1)) #3
sum(is.na(d1b$num_pastyear_1)) #5
sum(is.na(d1b$depression_1)) #0
sum(is.na(d1b$exp_b_pos_1)) #17
sum(is.na(d1b$FC_b_pos_1)) #17
d1b[is.na(d1b$exp_b_pos_1) & !is.na(d1b$age_mri_1), 'subid']
#"MWMH169" - missing resting state data, but has task data
#"MWMH178" - missing resting state data, but has task data
#"MWMH215" - missing resting state data, but has task data
#"MWMH233" - missing resting state data, but has task data
#"MWMH235" - missing resting state data, but has task data
#"MWMH242" - missing all functional data
#"MWMH247" - missing resting state data and faces task, but has avoid task
#"MWMH253" - missing resting state data and faces task, but has avoid task
#"MWMH267" - missing resting state data, but has task data
#"MWMH271" - missing resting state data, but has task data
#"MWMH278" - missing resting state data, but has task data (missing tsvs)
#"MWMH279" - missing all functional data
#"MWMH299" - missing resting state data, but has task data
#"MWMH358" - has resting state, but only 31 TRs, so excluded from processing

# Assume age_mri_1 would have been two years before age_mri_2
subids <- d1b[is.na(d1b$age_mri_1), 'subid']
d1b[is.na(d1b$age_mri_1), 'age_mri_1'] <- d2b[d2b$subid %in% subids, 'age_mri_2'] - 2

# Predict exp_b_pos_1
t.test(exp_b_pos_1 ~ black, data = d1b) #sig... lower p-val than white
t.test(exp_b_pos_1 ~ white, data = d1b) #sig
t.test(exp_b_pos_1 ~ otherrace, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_b_pos_1), 'exp_b_pos_1'], d1b[!is.na(d1b$exp_b_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_b_pos_1), 'exp_b_pos_1'], d1b[!is.na(d1b$exp_b_pos_1), 'PubCat_1']) #sig
t.test(exp_b_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$exp_b_pos_1), 'exp_b_pos_1'], d1b[!is.na(d1b$exp_b_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_b_pos_1), 'exp_b_pos_1'], d1b[!is.na(d1b$exp_b_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_b_pos_1 ~ black + PubCat_1 + female, data = d1b) #PubCat_1 not over female
exp_mod <- lm(exp_b_pos_1 ~ black + female, data = d1b) 

d1b[is.na(d1b$exp_b_pos_1), 'exp_b_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_b_pos_1), ])

# Predict FC_b_pos_1
t.test(FC_b_pos_1 ~ black, data = d1b) #not
t.test(FC_b_pos_1 ~ white, data = d1b) #not
t.test(FC_b_pos_1 ~ otherrace, data = d1b) # not
cor.test(d1b[!is.na(d1b$FC_b_pos_1), 'FC_b_pos_1'], d1b[!is.na(d1b$FC_b_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_b_pos_1), 'FC_b_pos_1'], d1b[!is.na(d1b$FC_b_pos_1), 'PubCat_1']) #not
t.test(FC_b_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_b_pos_1), 'FC_b_pos_1'], d1b[!is.na(d1b$FC_b_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_b_pos_1), 'FC_b_pos_1'], d1b[!is.na(d1b$FC_b_pos_1), 'depression_1']) #not
fc_mod <- lm(FC_b_pos_1 ~ female, data = d1b)
d1b[is.na(d1b$FC_b_pos_1), 'FC_b_pos_1'] <- predict(fc_mod, d1b[is.na(d1b$FC_b_pos_1), ])

df <- merge(d1b, d2b) #keep the subjects that have complete session 2 data

# Controling for T1 data
df[, c('depression_2', 'num_pastyear_2', 'age_mri_2', 'depression_1')]
foo1 <- lm(depression_2 ~ num_pastyear_2*female + depression_1, d=df) #sig: female, depression_1
foo2 <- lm(exp_b_pos_2 ~ num_pastyear_2*female + exp_b_pos_1, d=df) #sig: exp_b_pos_1
foo2.2 <- lm(FC_b_pos_2 ~ num_pastyear_2*female + FC_b_pos_1, d=df) #sig: female, FC_b_pos_1
foo3 <- lm(depression_2 ~ exp_b_pos_2*female + depression_1, d=df) #sig: exp_b_pos_2 (interesting!), depression_1
foo3.2 <- lm(depression_2 ~ FC_b_pos_2*female + depression_1, d=df) #sig: FC_b_pos_2 (interesting!), female, depression_1, FC_b_pos_2:female (interesting!)

# sensitivity
coo1 <- lm(depression_2 ~ age_mri_2 + black + white + otherrace + BMIperc_2 + PubCat_2 + num_pastyear_2*female + depression_1, d=df) #sig: depression_1
coo2 <- lm(exp_b_pos_2 ~ age_mri_2 + black + white + otherrace + BMIperc_2 + PubCat_2 + num_pastyear_2*female + exp_b_pos_1, d=df) #sig: black, exp_b_pos_1
coo2.2 <- lm(FC_b_pos_2 ~ age_mri_2 + black + white + otherrace + BMIperc_2 + PubCat_2 + num_pastyear_2*female + FC_b_pos_1, d=df) #sig: black, female, FC_b_pos_1
coo3 <- lm(depression_2 ~ age_mri_2 + black + white + otherrace + BMIperc_2 + PubCat_2 + exp_b_pos_2*female + depression_1, d=df) #sig: depression_1
coo3.2 <- lm(depression_2 ~ age_mri_2 + black + white + otherrace + BMIperc_2 + PubCat_2 + FC_b_pos_2*female + depression_1, d=df) #sig: FC_b_pos_2, depression_1, FC_b_pos_2:female
