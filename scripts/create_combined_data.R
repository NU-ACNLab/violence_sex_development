### Merge all data types for final sample
###
### Ellyn Butler
### August 7, 2024


# load data
basedir <- '/projects/b1108/studies/mwmh/data/processed/'
demo_df <- read.csv(paste0(basedir, 'demographic/demographics_2022-11-07.csv'))
viol_df <- read.csv(paste0(basedir, 'violence/violence_2022-10-06.csv'))
dep_df <- read.csv(paste0(basedir, 'clinical/depanx_2022-10-04.csv'))
net_df <- read.csv(paste0(basedir, 'neuroimaging/tabulated/surf_network_metrics_2024-09-13.csv'))

# Merge
final_df <- merge(viol_df, demo_df, by=c('subid', 'sesid'), all=TRUE)
final_df <- merge(final_df, dep_df, by=c('subid', 'sesid'), all=TRUE)
final_df <- merge(final_df, net_df, by=c('subid', 'sesid'), all=TRUE)

final_df$depression <- rowSums(final_df[, paste0('RCADS_', c(1, 4, 8, 10, 13, 16, 15, 18, 19, 21, 24), 'r')])

final_df <- final_df[, c('subid', 'sesid', 'black', 'white', 'otherrace', 
                         'BMIperc', 'PubCat', 'female', 'age_lab', 'age_mri',
                         'days_mri_minus_lab', 'ever', 'num_pastyear',
                         'depression', 'RCADS_sum', 'exp_b_pos', 'FC_b_pos')] #BC_b_pos

# Remove subjects who were pilots
final_df <- final_df[which(!(final_df$subid %in% c('MWMH001', 'MWMH102'))), ]
dim(final_df) # 560

# Remove sessions that are missing an age for both mri and lab (means they weren't
# a member of that time point)
final_df <- final_df[which(!(is.na(final_df$age_lab) & is.na(final_df$age_mri))), ]
dim(final_df) # 534

########### Descriptive - before removing NAs

# Number of participants
length(unique(final_df$subid)) # 277

first_subids <- final_df[final_df$sesid == 1, 'subid']
second_subids <- final_df[final_df$sesid == 2, 'subid']

# Both time points
both_tps <- second_subids[second_subids %in% first_subids]
length(both_tps) # 257

# Just first time point
first_tp <- first_subids[!(first_subids %in% second_subids)]
length(first_tp) # 20

# Just second time point
second_tp <- second_subids[!(second_subids %in% first_subids)]
length(second_tp) # 0

table(final_df[final_df$sesid == 1, 'female']) # 177 (100)
table(final_df[final_df$sesid == 2, 'female']) # 168 (89)
summary(final_df[final_df$sesid == 1, 'age_lab']) # min=11.88, median=13.95, mean=13.96, max=15.34
summary(final_df[final_df$sesid == 2, 'age_lab']) # min=13.93, median=15.98, mean=16.00, max=17.54, numna=2
table(final_df$ever) # 0->237, 1->296

summary(final_df$num_pastyear)
table(final_df$num_pastyear)


########### Descriptive - before after removing NAs

sum(is.na(final_df$female)) #0
sum(is.na(final_df$age_lab)) #2
sum(is.na(final_df$age_mri)) #40
sum(is.na(final_df$num_pastyear)) #6... lab day
sum(is.na(final_df$RCADS_sum)) #0... lab day
sum(is.na(final_df$region46)) #52
sum(is.na(final_df$region69)) #52
sum(is.na(final_df$region129)) #52
sum(is.na(final_df$region139)) #52
sum(is.na(final_df$region142)) #52
sum(is.na(final_df$region174)) #52
sum(is.na(final_df$region237)) #52
sum(is.na(final_df$region261)) #52
sum(is.na(final_df$region281)) #54


##### Survey
survey_df <- na.omit(final_df[, c('subid', 'sesid', 'female', 'age_lab',
                                  'num_pastyear', 'RCADS_sum')])

# Number of participants
length(unique(survey_df$subid))

first_subids <- survey_df[survey_df$sesid == 1, 'subid']
second_subids <- survey_df[survey_df$sesid == 2, 'subid']

# Both time points
both_tps <- second_subids[second_subids %in% first_subids]
length(both_tps)

# Just first time point
first_tp <- first_subids[!(first_subids %in% second_subids)]
length(first_tp)

# Just second time point
second_tp <- second_subids[!(second_subids %in% first_subids)]
length(second_tp)

table(survey_df[survey_df$sesid == 1, 'female'])
table(survey_df[survey_df$sesid == 2, 'female'])
summary(survey_df[survey_df$sesid == 1, 'age_lab'])
summary(survey_df[survey_df$sesid == 2, 'age_lab'])
table(survey_df$ever)

summary(survey_df$num_pastyear)
table(survey_df$num_pastyear)


##### Survey + MRI
mri_df <- na.omit(final_df)

length(unique(mri_df$subid)) # 263

first_subids <- mri_df[mri_df$sesid == 1, 'subid']
second_subids <- mri_df[mri_df$sesid == 2, 'subid']

# Both time points
both_tps <- second_subids[second_subids %in% first_subids]
length(both_tps)

# Just first time point
first_tp <- first_subids[!(first_subids %in% second_subids)]
length(first_tp)

# Just second time point
second_tp <- second_subids[!(second_subids %in% first_subids)]
length(second_tp)

table(mri_df[mri_df$sesid == 1, 'female'])
table(mri_df[mri_df$sesid == 2, 'female'])
summary(mri_df[mri_df$sesid == 1, 'age_lab'])
summary(mri_df[mri_df$sesid == 2, 'age_lab'])
summary(mri_df[mri_df$sesid == 1, 'age_mri'])
summary(mri_df[mri_df$sesid == 2, 'age_mri'])
table(mri_df$ever)

summary(mri_df$num_pastyear)
table(mri_df$num_pastyear)

summary(mri_df$days_mri_minus_lab)


########### netort data

write.csv(final_df, '/projects/b1108/projects/violence_sex_development/data/combined_data_2024-09-23.csv', row.names=FALSE)






#
