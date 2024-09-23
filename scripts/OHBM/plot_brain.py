### This script generates a figure of the brain where regions of the ventral
### attention network are plotted, as well as their connections getting stronger
### https://nilearn.github.io/dev/auto_examples/03_connectivity/plot_probabilistic_atlas_extraction.html#sphx-glr-auto-examples-03-connectivity-plot-probabilistic-atlas-extraction-py
###
### Ellyn Butler
### July 10, 2023

from nilearn import plotting
import numpy as np

base_dir = '/Users/flutist4129/Documents/Northwestern/studies/mwmh/data/processed/neuroimaging/amygconn/'

# low violence
coords = [(51.28, -28.52, -4.30),
              (-56.47, -50.48, 9.92),
              (-55.30, -39.89, 13.51),
              (51.52, -32.52, 7.55),
              (55.75, -46.07, 11.42),
              (-49.07, 25.13, -0.98),
              (-9.88, 10.95, 66.61),
              (52.68, 32.58, 0.57),
              (53.90, -42.76, 21.83),
              (25.26, 1.75, -1.26),
              (-28.26, -0.97, -3.09)]

rows = [210, 211, 212, 213, 214, 215, 216, 217, 218, 255, 256] # minus 1 indices

low_corr_mat = np.loadtxt(base_dir+'sub-MWMH191/ses-1/sub-MWMH191_ses-1_atlas-seitz_corrmat.csv',
                          delimiter=',')

low_corr_mat = low_corr_mat[rows, :][:, rows]

lowviol_plot = plotting.plot_connectome(low_corr_mat,
    coords, edge_vmin=-0.6, edge_vmax=0.6, colorbar=True) #max not 1 = 0.446

plotting.show()

# high violence
high_corr_mat = np.loadtxt(base_dir+'sub-MWMH267/ses-1/sub-MWMH267_ses-1_atlas-seitz_corrmat.csv',
                          delimiter=',')

high_corr_mat = high_corr_mat[rows, :][:, rows]

highviol_plot = plotting.plot_connectome(high_corr_mat,
    coords, edge_vmin=-0.6, edge_vmax=0.6, colorbar=True)

plotting.show()
