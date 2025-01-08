### This script plots the ICs for the 17 networks, 
### with the limits set by the network, not by
### the processing method
###
### Ellyn Butler
### October 25, 2024 - December 9, 2024

# Load libraries
library(templateICAr)
library(ciftiTools)
library(png)
library(grid)
library(gridExtra)
ciftiTools.setOption('wb_path', '/projects/b1108/software/workbench')
#ciftiTools.setOption('wb_path', '/Applications/workbench')

# Set paths
#neurodir <- '~/Documents/Northwestern/studies/mwmh/data/processed/neuroimaging/'
neurodir <- '/projects/b1108/studies/mwmh/data/processed/neuroimaging/'
#plotdir <- '~/Documents/Northwestern/studies/mwmh/plots/'
plotdir <- '/projects/b1108/studies/mwmh/plots/'

# Load templates
max <- readRDS(paste0(neurodir, 'template/temp_sub-ses2_ses-rand_task-rest_desc-maxpostproc.rds'))
export_template(max, out_fname = paste0(neurodir, 'template/temp_sub-ses2_ses-rand_task-rest_desc-maxpostproc'))

mean_temp <- read_cifti(paste0(neurodir, 'template/temp_sub-ses2_ses-rand_task-rest_desc-maxpostproc_mean.dscalar.nii'))
var_temp <- read_cifti(paste0(neurodir, 'template/temp_sub-ses2_ses-rand_task-rest_desc-maxpostproc_var.dscalar.nii'))


# Define ICs
for (i in 1:17) {
    mt <- select_xifti(mean_temp, seq(i, ncol(mean_temp), 17))
    vt <- select_xifti(var_temp, seq(i, ncol(mean_temp), 17))
    assign(paste0('IC', i, '_mean'), mt) 
    assign(paste0('IC', i, '_var'), vt) 
}
 
# Export plots
for (i in 1:17) {
    plot(get(paste0('IC', i, '_mean')), fname=paste0(plotdir, 'IC', i, '_mean'))
    plot(get(paste0('IC', i, '_var')), fname=paste0(plotdir, 'IC', i, '_var'))
}
