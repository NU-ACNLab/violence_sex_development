### This script is my first stab at getting single subject network topography
###
### Ellyn Butler
### August 14, 2023 - August 21, 2023

import os
import numpy as np #1.19.1
import nibabel as nib
from nilearn.input_data import NiftiLabelsMasker #0.8.1
from nilearn import plotting, signal, image, datasets, surface
import sys, getopt
import argparse

fsaverage = datasets.fetch_surf_fsaverage("fsaverage5")

subprepdir = '/Users/flutist4129/Documents/Northwestern/studies/mwmh/data/processed/neuroimaging/fmriprep/sub-MWMH209/'
sesprocdir = '/Users/flutist4129/Documents/Northwestern/studies/mwmh/data/processed/neuroimaging/amygconn/sub-MWMH209/ses-1/'
sessurfdir = '/Users/flutist4129/Documents/Northwestern/studies/mwmh/data/processed/neuroimaging/surf/sub-MWMH209/ses-1/'

anatdir = subprepdir+'anat/'

#what MNI are they using?
img = sesprocdir + 'sub-MWMH209_ses-1_task-rest_final.nii.gz'
mask_img = anatdir + 'sub-MWMH209_desc-brain_mask.nii.gz'


surf_data = surface.vol_to_surf(img,
                                surf_mesh = fsaverage["pial_left"],
                                inner_mesh = fsaverage["white_left"]) #mask_img = mask_img

nib.to_filename(sessurfdir+'sub-MWMH209_ses-1_', surf_data)

# make a connectivity matrix for each node

# load a prior with 17 networks - Yeo 17 (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3174820/)
# https://surfer.nmr.mgh.harvard.edu/fswiki/CorticalParcellation_Yeo2011


#https://nilearn.github.io/stable/modules/generated/nilearn.surface.vol_to_surf.html


#https://nilearn.github.io/stable/auto_examples/01_plotting/plot_3d_map_to_surface_projection.html#sphx-glr-auto-examples-01-plotting-plot-3d-map-to-surface-projection-py


#https://link.springer.com/article/10.1007/s00429-021-02435-0
