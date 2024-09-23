### This script plots the average number of violence exposures by sex and
### category of violence exposure
###
### Ellyn Butler
### July 4, 2023

library('ggplot2')

###### Load the data
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
             mediators, paste0('etv', 1:7, '_pastyear'))]

# remove rows with NAs
df <- df[complete.cases(df), ]

# remove extreme outlier
df <- df[df$num_pastyear < 100, ]

# use only the first time point (get to multilevel later: p16 of `mediation` pdf)
viol_df <- df[df$sesid == 1, ]

nf <- nrow(viol_df[viol_df$female == 1, ])
nm <- nrow(viol_df[viol_df$female == 0, ])

# plot
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

jpeg('/projects/b1108/projects/violence_sex_development/plots/pastyear_violence_ses-1.jpg', res=300, units='mm', width=120, height=90)
prop_pastyear_plot
dev.off()
