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
low_dep_indices <- c()
high_dep_indices <- c()
for (i in 1:length(rds_list)) { 
    subid <- strsplit(strsplit(file_list[i], '/')[[1]][10], '-')[[1]][2]
    if (subid %in% d2$subid) {
        if (d2[d2$subid == subid, 'depression'] < 6) {
            low_dep_indices <- c(low_dep_indices, i)
        } else if (d2[d2$subid == subid, 'depression'] > 11) {
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

probmap <- read_cifti('/projects/b1108/studies/mwmh/data/processed/neuroimaging/surf/sub-MWMH200/ses-2/func/sub-MWMH200_ses-2_task-rest_space-fsLR_desc-medpostproc_meanmed.dscalar.nii')

# move back the mwall
probmap <- move_from_mwall(probmap)

probmap$data$cortex_left[,1] <- left
probmap$data$cortex_right[,1] <- right

# move out the mwall
probmap <- move_to_mwall(probmap, values = NA) #this doesn't seem to be working
#probmap$data$cortex_left[,1][probmap$data$cortex_left[,1] == 1] <- 0 #gets rid of dots...
#probmap$data$cortex_right[,1][probmap$data$cortex_right[,1] == 1] <- 0 #gets rid of dots...

saveRDS(probmap, '/projects/b1108/studies/mwmh/data/processed/neuroimaging/group/lowdep_propsn.rds')
lowmap <- probmap
view_xifti_surface(lowmap, zlim = c(0, 1), colors = 'BuPu') #What is up with the dots?

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

probmap <- read_cifti('/projects/b1108/studies/mwmh/data/processed/neuroimaging/surf/sub-MWMH200/ses-2/func/sub-MWMH200_ses-2_task-rest_space-fsLR_desc-medpostproc_meanmed.dscalar.nii')

# move back the mwall
probmap <- move_from_mwall(probmap)

probmap$data$cortex_left[,1] <- left
probmap$data$cortex_right[,1] <- right

# move out the mwall
probmap <- move_to_mwall(probmap, values = NA) #this doesn't seem to be working
probmap$data$cortex_left[,1][probmap$data$cortex_left[,1] == 1] <- 0 #gets rid of dots...
probmap$data$cortex_right[,1][probmap$data$cortex_right[,1] == 1] <- 0 #gets rid of dots...

saveRDS(probmap, '/projects/b1108/studies/mwmh/data/processed/neuroimaging/group/highdep_propsn.rds')
highmap <- probmap 
view_xifti_surface(highmap, zlim = c(0, 1), colors = 'BuPu') #What is up with the dots?

######### Illustration with single male subjects

# high depression, high expansion - MWMH379
d2[d2$female == 0 & d2$depression > 20 & d2$exp_b_pos > .3, ]
highdep <- readRDS('/projects/b1108/studies/mwmh/data/processed/neuroimaging/surfnet/sub-MWMH379/ses-2/network_membership_pos.rds')
view_xifti_surface(highdep$active, idx = 8)

# low depression, low expansion - MWMH271
d2[d2$female == 0 & d2$depression < 15 & d2$exp_b_pos < .2, ]
lowdep <- readRDS('/projects/b1108/studies/mwmh/data/processed/neuroimaging/surfnet/sub-MWMH271/ses-2/network_membership_pos.rds')
view_xifti_surface(lowdep$active, idx = 8)