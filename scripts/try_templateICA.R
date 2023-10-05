### This script is a first stab at creating a single subject template using
### templateICAr
### https://github.com/mandymejia/templateICAr
### Yeo networks: https://www.researchgate.net/figure/Network-parcellation-of-Yeos-17-networks-The-17-networks-include-the-following-regions_fig1_352966687#:~:text=The%2017%2Dnetworks%20include%20the%20following%20regions%3A%20N1%3A%20VisCent,N7%3A%20SalVentAttnA%20%2DSalience%2FVentral
###
### Ellyn Butler
### August 31, 2023 - September 7, 2023

library(templateICAr)
library(INLA)
library(ciftiTools)
ciftiTools.setOption("wb_path", "/Applications/workbench")

rest_path <- '/Users/flutist4129/Documents/Northwestern/studies/mwmh/data/processed/neuroimaging/amygconn/sub-MWMH209/ses-1/sub-MWMH209_ses-1_task-rest_final.nii.gz'

bold_scans <- # vector of paths to all of the bold scans

# Ideally would have already...
estimate_template(bold_scans) #

export_template()

# Single subject template estimation
templateICA(, spatial_model-TRUE)

# Identify areas of engagement and deviation
activations()
