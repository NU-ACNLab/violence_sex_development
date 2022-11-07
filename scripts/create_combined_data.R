### Merge all data types for final sample
###
### Ellyn Butler
### November 7, 2022


# load data
basedir <- '/projects/b1108/studies/mwmh/data/processed/'
demo_df <- read.csv(paste0(basedir, 'demographic/demographics_2022-11-07.csv'))
viol_df <- read.csv(paste0(basedir, 'violence/violence_2022-10-06.csv'))
dep_df <- read.csv(paste0(basedir, 'clinical/depanx_2022-10-04.csv'))
amyg_df <- read.csv(paste0(basedir, 'neuroimaging/tabulated/amygconn_2022-11-03.csv'))

# Merge
final_df <- merge(viol_df, demo_df, by=c('subid', 'sesid'), all=TRUE)
final_df <- merge(final_df, dep_df, by=c('subid', 'sesid'), all=TRUE)
final_df <- merge(final_df, amyg_df, by=c('subid', 'sesid'), all=TRUE)

regs <- c('region46', 'region69', 'region129', 'region139', 'region142',
          'region174', 'region237', 'region261', 'region281')

final_df <- final_df[, c('subid', 'sesid', 'female', 'age_lab', 'age_mri',
                         'ever', 'num_pastyear', 'RCADS_sum', regs)]

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

#table(final_df$pastyear)


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

final_df2 <- na.omit(final_df)

# Number of participants
length(unique(final_df2$subid))

first_subids <- final_df2[final_df2$sesid == 1, 'subid']
second_subids <- final_df2[final_df2$sesid == 2, 'subid']

# Both time points
both_tps <- second_subids[second_subids %in% first_subids]
length(both_tps)

# Just first time point
first_tp <- first_subids[!(first_subids %in% second_subids)]
length(first_tp)

# Just second time point
second_tp <- second_subids[!(second_subids %in% first_subids)]
length(second_tp)

table(final_df2[final_df2$sesid == 1, 'female'])
table(final_df2[final_df2$sesid == 2, 'female'])
summary(final_df2[final_df2$sesid == 1, 'age_lab'])
summary(final_df2[final_df2$sesid == 2, 'age_lab'])
table(final_df2$ever)

summary(final_df2$num_pastyear)
table(final_df2$num_pastyear)

#table(final_df$pastyear)


########### Export data

write.csv(final_df, '/projects/b1108/projects/violence_sex_development/data/combined_data.csv', row.names=FALSE)






#
