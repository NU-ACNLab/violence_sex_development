### This script creates a plot of the percentage of participants
### that have a given vertex designated as a member of the salience
### network, splitting the participants at the median for depression
###
### Ellyn Butler
### September 11, 2024

library(ciftiTools)
library(dplyr)
#ciftiTools.setOption('wb_path', '/Applications/workbench')
ciftiTools.setOption('wb_path', '/projects/b1108/software/workbench')

##### Load files
file_pattern <- '/projects/b1108/studies/mwmh/data/processed/neuroimaging/surfnet/sub-MWMH*/ses-*/*network_membership_pos.rds'
file_list <- Sys.glob(file_pattern)
rds_list <- lapply(file_list, readRDS)

##### Load data
d <- read.csv('/projects/b1108/projects/violence_sex_development/data/combined_data_2024-09-09.csv')
d <- d[!is.na(d$exp_b_pos) & !is.na(d$FC_b_pos) & !is.na(d$RCADS_sum) & !is.na(d$num_pastyear), ]
d1 <- d[d$sesid == 1, ]
d2 <- d[d$sesid == 2, ]

##### Identify the subjects above and below the median
depmean <- mean(d2$depression)
low_dep_indices <- c()
high_dep_indices <- c()
for (i in 1:length(rds_list)) { 
    subid <- strsplit(strsplit(file_list[i], '/')[[1]][10], '-')[[1]][2]
    if (subid %in% d2$subid) {
        if (d2[d2$subid == subid, 'depression'] < depmean) {
            low_dep_indices <- c(low_dep_indices, i)
        } else {
            high_dep_indices <- c(high_dep_indices, i)
        }
    }
}

low_list <- rds_list[low_dep_indices]
high_list <- rds_list[high_dep_indices]

##### Plot low dep
for (i in 1:length(low_list)) {
    if (i == 1) {
        left <- low_list[[i]]$active$data$cortex_left[,8]
        right <- low_list[[i]]$active$data$cortex_right[,8]
    } else {
        left <- left + low_list[[i]]$active$data$cortex_left[,8]
        right <- right + low_list[[i]]$active$data$cortex_right[,8]
    }
}
left <- left/length(low_list) 
right <- right/length(low_list) 

lowrds <- rds_list[[1]]

#Q: not a valid xifti after these next two... how to make valid?
lowrds$active$data$cortex_left[,8] <- left 
lowrds$active$data$cortex_right[,8] <- right

view_xifti_surface(lowrds, zlim = c(0, 1), colors = '3-class PuRd', idx = 8)

##### Plot high dep
for (i in 1:length(high_list)) {
    if (i == 1) {
        left <- high_list[[i]]$active$data$cortex_left[,8]
        right <- high_list[[i]]$active$data$cortex_right[,8]
    } else {
        left <- left + high_list[[i]]$active$data$cortex_left[,8]
        right <- right + high_list[[i]]$active$data$cortex_right[,8]
    }
}
left <- left/length(high_list) 
right <- right/length(high_list) 

highrds <- rds_list[[1]]

#Q: not a valid xifti after these next two... how to make valid?
highrds$active$data$cortex_left[,8] <- left 
highrds$active$data$cortex_right[,8] <- right

view_xifti_surface(highrds, zlim = c(0, 1), colors = '3-class PuRd', idx = 8)
