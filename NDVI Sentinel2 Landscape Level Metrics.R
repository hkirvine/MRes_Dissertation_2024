# This code calculates Landscape Level Landscape Metrics from Classified Sentinel 2 time series data (2018-2023)
# This code also includes metric change over time for transparency in addition to what was included in the dissertation
# Sentinel 2 NDVI data is created from Google Earth Engine Code, which is available on Github: https://github.com/hkirvine

# Set-up ####

# Install and load necessary libraries
if (!requireNamespace("landscapemetrics", quietly = TRUE)) {
  install.packages("landscapemetrics", dependencies = TRUE)
}
if (!requireNamespace("raster", quietly = TRUE)) {
  install.packages("raster", dependencies = TRUE)
}
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf", dependencies = TRUE)
}
if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra", dependencies = TRUE)
}

library(landscapemetrics)
library(raster)
library(sf)
library(terra)

# Set the working directory to the directory containing the classified NDVI GeoTIFF files
setwd("C:/Users/hopek/OneDrive - Newcastle University/Working File/Newcastle Uni/MRes Geospatial Data Science/MRes Diss/Data/Classified Data/Sentinel2_NDVI_Class")

# Landscape Level Landscape Metric Calculations ####

# Define the exact filenames of the classified NDVI GeoTIFF files in the working directory
filenames <- c(
  "Sentinel2_NDVI_Class_2018.tif",
  "Sentinel2_NDVI_Class_2019.tif",
  "Sentinel2_NDVI_Class_2020.tif",
  "Sentinel2_NDVI_Class_2021.tif",
  "Sentinel2_NDVI_Class_2022.tif",
  "Sentinel2_NDVI_Class_2023.tif"
)

print("Files to be processed:")
print(filenames)

# Function to load each raster file
load_raster <- function(file_path) {
  rast(file_path)
}

# Load all classified NDVI files
classified_ndvi_list <- lapply(filenames, load_raster)

# Check if the rasters were loaded correctly
if (length(classified_ndvi_list) == 0) {
  stop("No raster files were loaded.")
}

# Example: Print the first raster to verify
if (!is.null(classified_ndvi_list[[1]])) {
  print("First raster:")
  print(classified_ndvi_list[[1]])
} else {
  stop("First raster is NULL. Check if the file exists and is readable.")
}

# Use terra's native functions to stack the rasters if needed
# Here we assume they have the same extent and resolution
classified_ndvi_stack <- rast(classified_ndvi_list)

# Print the stack to verify
print("Classified NDVI stack:")
print(classified_ndvi_stack)

# Convert the SpatRaster to a RasterLayer for compatibility with landscapemetrics
classified_ndvi_raster <- as(classified_ndvi_stack, "Raster")

# Inspect unique values before cleaning
for (i in 1:nlayers(classified_ndvi_raster)) {
  unique_values <- unique(values(classified_ndvi_raster[[i]]))
  print(paste("Unique values in original layer", i, ":", toString(unique_values)))
}

# Assuming valid classification values are 1, 2, 3 (adjust if needed based on the inspection above)
valid_classes <- c(1, 2, 3)

# Clean the raster values to ensure only valid classes are kept
cleaned_ndvi_raster <- calc(classified_ndvi_raster, fun = function(x) {
  x[!x %in% valid_classes] <- NA
  return(x)
})

# Inspect unique values after cleaning
for (i in 1:nlayers(cleaned_ndvi_raster)) {
  unique_values <- unique(values(cleaned_ndvi_raster[[i]]))
  print(paste("Unique values in cleaned layer", i, ":", toString(unique_values)))
}

# Function to calculate landscape metrics for each layer
calculate_landscape_metrics <- function(raster_layer) {
  list(
    SHAPE_MN = lsm_l_shape_mn(raster_layer),
    SHAPE_SD = lsm_l_shape_sd(raster_layer),
    IJI = lsm_l_iji(raster_layer),
    CONTAG = lsm_l_contag(raster_layer),
    SIEI = lsm_l_siei(raster_layer)
  )
}

# Calculate landscape metrics for each layer
landscape_metrics_by_layer <- lapply(1:nlayers(cleaned_ndvi_raster), function(i) {
  calculate_landscape_metrics(cleaned_ndvi_raster[[i]])
})

# Function to extract metric values and convert to a data frame
extract_landscape_metrics <- function(metric_list, metric_name) {
  sapply(metric_list, function(layer_metrics) {
    metric_value <- layer_metrics[[metric_name]]$value
    if (is.null(metric_value)) NA else metric_value
  })
}

# Extract and organize metric values
landscape_metrics_df_list <- list()
metric_names <- c("SHAPE_MN", "SHAPE_SD", "IJI", "CONTAG", "SIEI")

for (metric_name in metric_names) {
  metric_values <- extract_landscape_metrics(landscape_metrics_by_layer, metric_name)
  df <- as.data.frame(t(metric_values))
  colnames(df) <- paste("Year", 2018:2023, sep = "_")
  landscape_metrics_df_list[[metric_name]] <- df
}

# Print the landscape metrics data frames for debugging
for (metric_name in names(landscape_metrics_df_list)) {
  print(paste(metric_name, "values:"))
  print(landscape_metrics_df_list[[metric_name]])
}

# Calculate the change in each metric over time
change_in_landscape_metrics_list <- lapply(landscape_metrics_df_list, function(df) {
  change_df <- apply(df, 1, function(x) diff(x))
  change_df <- as.data.frame(t(change_df))
  colnames(change_df) <- paste("Change_from", 2018:2022, "to", 2019:2023, sep = "_")
  return(change_df)
})

# Print the change in metrics over time
for (metric_name in names(change_in_landscape_metrics_list)) {
  change_df <- change_in_landscape_metrics_list[[metric_name]]
  print(paste("Change in", metric_name, "over time:"))
  print(change_df)
}


# Calculate the overall change from 2018 to 2023 for each metric
overall_change_in_landscape_metrics <- lapply(landscape_metrics_df_list, function(df) {
  start_values <- df[, 1]  # Values from the first year (2018)
  end_values <- df[, ncol(df)]  # Values from the last year (2023)
  overall_change <- end_values - start_values
  return(overall_change)
})

names(overall_change_in_landscape_metrics) <- names(landscape_metrics_df_list)

# Print the overall change from 2018 to 2023 for each metric
for (metric_name in names(overall_change_in_landscape_metrics)) {
  overall_change <- overall_change_in_landscape_metrics[[metric_name]]
  print(paste("Overall change in", metric_name, "from 2018 to 2023:"))
  print(overall_change)
}


# SHAPE_MN Plot ####

# Extract SHAPE_MN values for plotting
shape_mn_values <- landscape_metrics_df_list[["SHAPE_MN"]]

# Ensure the years and columns match the length of the SHAPE_MN values
years <- 2018:2023

# Create a data frame for ggplot
shape_mn_df <- data.frame(
  Year = years,
  SHAPE_MN = as.numeric(shape_mn_values)
)

# Plot the SHAPE_MN metrics
shape_mn_plot <- ggplot(shape_mn_df, aes(x = Year, y = SHAPE_MN)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue", size = 3) +
  labs(title = "SHAPE_MN Metrics from 2018-2023",
       x = "Year",
       y = "Mean Shape Index (SHAPE_MN)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

# Display the plot
print(shape_mn_plot)

# Save the plot to the working directory
ggsave(filename = "SHAPE_MN_Metrics_2018_2023.png", plot = shape_mn_plot, width = 10, height = 6)




# SHAPE_SD Plot ####

# Extract SHAPE_SD values for plotting
shape_sd_values <- landscape_metrics_df_list[["SHAPE_SD"]]

# Ensure the years and columns match the length of the SHAPE_SD values
years <- 2018:2023

# Create a data frame for ggplot
shape_sd_df <- data.frame(
  Year = years,
  SHAPE_SD = as.numeric(shape_sd_values)
)

# Plot the SHAPE_SD metrics
shape_sd_plot <- ggplot(shape_sd_df, aes(x = Year, y = SHAPE_SD)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue", size = 3) +
  labs(title = "SHAPE_SD Metrics from 2018-2023",
       x = "Year",
       y = "Standard Deviation of Shape Index (SHAPE_SD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

# Display the plot
print(shape_sd_plot)

# Save the plot to the working directory
ggsave(filename = "SHAPE_SD_Metrics_2018_2023.png", plot = shape_sd_plot, width = 10, height = 6)


# IJI Plot ####

# Extract IJI values for plotting
iji_values <- landscape_metrics_df_list[["IJI"]]

# Ensure the years and columns match the length of the IJI values
years <- 2018:2023

# Create a data frame for ggplot
iji_df <- data.frame(
  Year = years,
  IJI = as.numeric(iji_values)
)

# Plot the IJI metrics
iji_plot <- ggplot(iji_df, aes(x = Year, y = IJI)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue", size = 3) +
  labs(title = "IJI Metrics from 2018-2023",
       x = "Year",
       y = "Interspersion and Juxtaposition Index (IJI)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

# Display the plot
print(iji_plot)

# Save the plot to the working directory
ggsave(filename = "IJI_Metrics_2018_2023.png", plot = iji_plot, width = 10, height = 6)


# CONTAG Plot ####

# Extract CONTAG values for plotting
contag_values <- landscape_metrics_df_list[["CONTAG"]]

# Ensure the years and columns match the length of the CONTAG values
years <- 2018:2023

# Create a data frame for ggplot
contag_df <- data.frame(
  Year = years,
  CONTAG = as.numeric(contag_values)
)

# Plot the CONTAG metrics
contag_plot <- ggplot(contag_df, aes(x = Year, y = CONTAG)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue", size = 3) +
  labs(title = "CONTAG Metrics from 2018-2023",
       x = "Year",
       y = "Contagion Index (CONTAG)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

# Display the plot
print(contag_plot)

# Save the plot to the working directory
ggsave(filename = "CONTAG_Metrics_2018_2023.png", plot = contag_plot, width = 10, height = 6)




# SIEI ####

# Extract SIEI values for plotting
siei_values <- landscape_metrics_df_list[["SIEI"]]

# Ensure the years and columns match the length of the SIEI values
years <- 2018:2023

# Create a data frame for ggplot
siei_df <- data.frame(
  Year = years,
  SIEI = as.numeric(siei_values)
)

# Plot the SIEI metrics
siei_plot <- ggplot(siei_df, aes(x = Year, y = SIEI)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue", size = 3) +
  labs(title = "SIEI Metrics from 2018-2023",
       x = "Year",
       y = "Shannon's Evenness Index (SIEI)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

# Display the plot
print(siei_plot)

# Save the plot to the working directory
ggsave(filename = "SIEI_Metrics_2018_2023.png", plot = siei_plot, width = 10, height = 6)
