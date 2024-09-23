### This script tests if females have experienced fewer violence exposures in
### the past year than males
###
### Ellyn Butler
### July 4, 2023 - July 6, 2023

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

df <- df[, c('subid', 'sesid', 'female', 'ever', 'num_pastyear', 'RCADS_sum',
             mediators, paste0('etv', 1:7, '_pastyear'))]

# remove rows with NAs
df <- df[complete.cases(df), ]

# remove extreme outlier
df <- df[df$num_pastyear < 100, ]

# use only the first time point (get to multilevel later: p16 of `mediation` pdf)
viol_df <- df[df$sesid == 1, ]


################################## Statistics ##################################

# Males experienced twice as many violence exposures in the past year as females,
# on average
t.test(num_pastyear ~ female, data=viol_df)

# Percent of people who had ever experienced
sum(viol_df$ever)/nrow(viol_df) # 54.2%

# Percent of people who had experienced in the past year
sum(viol_df$ever > 0)/nrow(viol_df) # 54.2%
