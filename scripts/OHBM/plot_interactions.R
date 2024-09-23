### This script tests if females have experienced fewer violence exposures in
### the past year than males
###
### Ellyn Butler
### July 4, 2023 - July 7, 2023

library('ggplot2')
library('patchwork')
library('dplyr')

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

df <- df[, c('subid', 'sesid', 'female', 'ever', 'num_pastyear', 'RCADS_sum',
             mediators, paste0('etv', 1:7, '_pastyear'))]

# remove rows with NAs
df <- df[complete.cases(df), ]

# remove extreme outlier
df <- df[df$num_pastyear < 100, ]

# use only the first time point
viol_df <- df[df$sesid == 1, ]


##################################### Plots ####################################

##### Internalizing
viol_df$Sex <- recode(viol_df$female, `1`='Female', `0`='Male')
viol_df$Sex <- ordered(viol_df$Sex, c('Male', 'Female'))

int_mod <- lm(RCADS_sum ~ female*num_pastyear, data = viol_df)
# ^ July 10, 2023: still significant if filter for fewer than 40 events

int_plot <- ggplot(viol_df, aes(num_pastyear, RCADS_sum, color=Sex)) +
      geom_smooth(method = 'lm') +
      geom_point(alpha = 0.3) + theme_linedraw() +
      ylab('Internalizing Symptom Severity') +
      xlab('Number of Violent Events in the Past Year') +
      scale_color_manual(values=c('#334FFF', '#FF333F'))

##### Ventral Attention

net_mod <- lm(VentralAttention ~ female*num_pastyear, data = viol_df)
# ^ July 10, 2023: still significant if filter for fewer than 40 events

net_plot <- ggplot(viol_df, aes(num_pastyear, VentralAttention, color=Sex)) +
      geom_smooth(method = 'lm') +
      geom_point(alpha = 0.3) + theme_linedraw() +
      ylab('Ventral Attention Connectivity') +
      xlab('Number of Violent Events in the Past Year') +
      scale_color_manual(values=c('#334FFF', '#FF333F'))

combined <- int_plot + net_plot & theme(legend.position = 'bottom')
##### Export
base_dir <- '/projects/b1108/projects/violence_sex_development/plots/'
png(paste0(base_dir, 'interaction_plots.png'), width=3000, height=1500, res=300)
combined + plot_layout(guides = 'collect')
dev.off()
