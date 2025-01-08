### This script plots time series from two vertices
### in the salience network for sub-MWMH212_ses-2
### for purposes of illustration
### 
### Ellyn Butler
### October 29, 2024

library(ciftiTools)
ciftiTools.setOption('wb_path', '/Applications/workbench/')
library(ggplot2)

# Read the CIFTI file
cifti_img <- read_cifti('sub-MWMH212_ses-2_task-rest_space-fsLR_desc-maxpostproc_bold.dscalar.nii')

# Specify the vertex index (e.g., vertex 1000)
vertex_index <- 1000

# Extract the time series for the specified vertex
vertex_time_series <- as.numeric(cifti_img$data$cortex_left[vertex_index, ])

# Create a data frame for plotting
time_series_df <- data.frame(
  Time = 1:length(vertex_time_series),
  Value = vertex_time_series
)

# Plot the time series using ggplot2
ggplot(time_series_df, aes(x = Time, y = Value)) +
  geom_line() +
  labs(
    title = paste("Time Series for Vertex", vertex_index),
    x = "Time Point",
    y = "Activation"
  ) +
  theme_minimal()