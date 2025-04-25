### Merge all data types for final sample
###
### Ellyn Butler
### April 18, 2025


# load data
basedir <- '/projects/b1108/studies/mwmh/data/processed/'
projdir <- '/projects/b1108/projects/violence_sex_development/data/'
net2_df <- read.csv(paste0(basedir, 'neuroimaging/tabulated/surf_network_metrics_2_2024-10-31.csv'))
net2_df <- net2_df[, c('subid', 'sesid', 'exp_a_pos', 'FC_a_pos', 'exp_b_pos', 'FC_b_pos')]
names(net2_df) <- c('subid', 'sesid', 'exp_saliencea_pos', 'FC_saliencea_pos', 'exp_salienceb_pos', 'FC_salienceb_pos')
net15_df <- read.csv(paste0(basedir, 'neuroimaging/tabulated/surf_network_metrics_15_2025-04-03.csv'))
first_df <- read.csv(paste0(projdir, 'combined_data_2024-11-01.csv'))
first_df <- first_df[, 1:17]

# Merge
final_df <- merge(first_df, net15_df, by=c('subid', 'sesid'), all=TRUE)
final_df <- merge(final_df, net2_df, by=c('subid', 'sesid'), all=TRUE)

# Remove subjects who were pilots
final_df <- final_df[which(!(final_df$subid %in% c('MWMH001', 'MWMH102'))), ]
dim(final_df) # 534

write.csv(final_df, paste0(projdir, 'combined_data_2025-04-18.csv'), row.names=FALSE)

#
