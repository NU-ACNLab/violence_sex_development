### This script generates a figure of the brain where regions of the ventral
### attention network are plotted, as well as their connections getting stronger
### https://nilearn.github.io/dev/auto_examples/03_connectivity/plot_probabilistic_atlas_extraction.html#sphx-glr-auto-examples-03-connectivity-plot-probabilistic-atlas-extraction-py
###
### Ellyn Butler
### September 10, 2024

from nilearn import plotting
import numpy as np

base_dir = '/Users/flutist4129/Documents/Northwestern/studies/mwmh/data/processed/neuroimaging/amygconn/'

############## low depression
coords = [(40.28, 64.47, -9.67),
          (67.31, -25.47, 6.65),
          (-35.77, 67.72, -4.02),
          (-65.94, -24.22, 6.65),
          (38.4, 39.31, -20.33),
          (-34.51, 37.43, -20.33),
          (-1.20, 36.17, 23.59),
          (5.70, 36.17, 39.27),
          (38.31, 36.17, -23.47),
          (-33.17, 39.31, -16.57),
          (-2.45, 51.41, 4.14),
          (-26.9, 53.29, 28.61),
          (11.97, 33.23, 47.43),
          (-13.11, 26.33, 48.69)]

# Define the size of the matrix
size = len(coords)

# Create an empty matrix
low_corr_matrix = np.zeros((size, size))

# Fill the off-diagonal with random values from a normal distribution
low_off_diag_values = np.random.normal(loc=0.15, scale=0.05, size=(size, size))

# Since we want a symmetric matrix, we need to fill the lower triangle
# and mirror it to the upper triangle, while setting diagonal elements to 1
for i in range(size):
    for j in range(i, size):
        if i == j:
            low_corr_matrix[i, j] = 1  # Diagonal elements
        else:
            value = low_off_diag_values[i, j]
            low_corr_matrix[i, j] = value
            low_corr_matrix[j, i] = value  # Symmetric part

#print(corr_matrix)


lowdep_plot = plotting.plot_connectome(low_corr_matrix,
    coords, edge_vmin=0, edge_vmax=0.4, colorbar=True) 

plotting.show()

############## high depression

# Create an empty matrix
high_corr_matrix = np.zeros((size, size))

# Fill the off-diagonal with random values from a normal distribution
high_off_diag_values = np.random.normal(loc=0.3, scale=0.05, size=(size, size))

# Since we want a symmetric matrix, we need to fill the lower triangle
# and mirror it to the upper triangle, while setting diagonal elements to 1
for i in range(size):
    for j in range(i, size):
        if i == j:
            high_corr_matrix[i, j] = 1  # Diagonal elements
        else:
            value = high_off_diag_values[i, j]
            high_corr_matrix[i, j] = value
            high_corr_matrix[j, i] = value  # Symmetric part

#print(corr_matrix)


highdep_plot = plotting.plot_connectome(high_corr_matrix,
    coords, edge_vmin=0, edge_vmax=0.4, colorbar=True) 

plotting.show()

