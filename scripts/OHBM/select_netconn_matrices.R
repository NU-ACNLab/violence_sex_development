### This script identifies female subjects who typify the interaction observed
### for ventral attention network connectivity
###
### Ellyn Butler
### July 7, 2023

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

# low violence, low network connectivity
viol_df[viol_df$female == 1 & viol_df$VentralAttention < 0.01 & viol_df$num_pastyear == 0, 'subid']
#[1] "MWMH108" "MWMH150" **"MWMH191"** "MWMH245" "MWMH254" "MWMH377"

# high violence, high network connectivity
viol_df[viol_df$female == 1 & viol_df$VentralAttention > 0.1 & viol_df$num_pastyear > 10, 'subid']
#[1] "MWMH109" **"MWMH267"** "MWMH312" "MWMH318" "MWMH329"
