# This code calculates Class Level Landscape Metrics from Classified Sentinel 2 time series data (2018-2023)
# This code also includes metric change over time for transparency in addition to what was included in the dissertation
# Sentinel 2 NDVI data is created from Google Earth Engine Code, which is available on Github: https://github.com/hkirvine

# Set-up ####

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
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", dependencies = TRUE)
}

library(landscapemetrics)
library(raster)
library(sf)
library(terra)
library(ggplot2)

# Set the working directory to the directory containing the classified NDVI GeoTIFF files
setwd("C:/Users/hopek/OneDrive - Newcastle University/Working File/Newcastle Uni/MRes Geospatial Data Science/MRes Diss/Data/Classified Data/Sentinel2_NDVI_Class")

# Class Level Landscape Metric Calculations #### 

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

# Function to calculate landscape metrics for a specific class
calculate_metric <- function(raster_layer, class_val, metric_function) {
  # Create a binary raster where the class of interest is 1 and everything else is NA
  class_raster <- calc(raster_layer, fun = function(x) {
    ifelse(x == class_val, 1, NA)
  })
  # Calculate the specified metric for the binary raster
  metric_function(class_raster)
}

# Calculate a specific metric for each layer and each class
calculate_metrics_by_layer <- function(metric_function) {
  lapply(1:nlayers(cleaned_ndvi_raster), function(i) {
    layer_metrics <- lapply(valid_classes, function(class_val) {
      calculate_metric(cleaned_ndvi_raster[[i]], class_val, metric_function)
    })
    names(layer_metrics) <- valid_classes
    return(layer_metrics)
  })
}

# Define the metric functions
metric_functions <- list(
  PD = lsm_c_pd,
  AREA = lsm_c_area_mn,
  NP = lsm_c_np,
  LPI = lsm_c_lpi,
  CA = lsm_c_ca,
  AREA_MN = lsm_c_area_mn
)

# Calculate each metric separately
metric_results <- list()
for (metric_name in names(metric_functions)) {
  metric_results[[metric_name]] <- calculate_metrics_by_layer(metric_functions[[metric_name]])
  print(paste("Calculated", metric_name))
}

# Function to extract total values for a specific metric, class, and year
extract_metric_values <- function(metric_list, metric_name) {
  sapply(valid_classes, function(class_val) {
    sapply(metric_list, function(layer_metrics) {
      if (!is.null(layer_metrics[[as.character(class_val)]])) {
        sum(layer_metrics[[as.character(class_val)]]$value, na.rm = TRUE)
      } else {
        NA
      }
    })
  })
}

# Extract and organize metric values
metric_values_list <- lapply(names(metric_results), function(metric_name) {
  extract_metric_values(metric_results[[metric_name]], metric_name)
})
names(metric_values_list) <- names(metric_results)

# Convert each metric values matrix to a data frame for easier handling
metric_values_df_list <- lapply(metric_values_list, function(metric_values) {
  df <- as.data.frame(metric_values)
  colnames(df) <- as.character(valid_classes)  # Rename columns to match valid_classes
  rownames(df) <- paste("Year", 2018:2023, sep = "_")
  return(df)
})

# Calculate the change in each metric over time for each class
change_in_metrics_list <- lapply(metric_values_df_list, function(df) {
  change_df <- apply(df, 2, function(x) diff(x))
  change_df <- as.data.frame(change_df)
  rownames(change_df) <- paste("Change_from", 2018:2022, "to", 2019:2023, sep = "_")
  return(change_df)
})

# Print the change in metrics over time for each class
for (metric_name in names(metric_results)) {
  change_df <- change_in_metrics_list[[metric_name]]
  print(paste("Change in", metric_name, "over time for each class:"))
  for (j in 1:length(valid_classes)) {
    class_val <- valid_classes[j]
    print(paste("Class", class_val, ":"))
    for (i in 1:nrow(change_df)) {
      print(paste(rownames(change_df)[i], ":", change_df[i, j]))
    }
  }
}

# Print the metric values data frames for debugging
for (metric_name in names(metric_results)) {
  print(paste(metric_name, "values:"))
  print(metric_values_df_list[[metric_name]])
}

# Calculate the overall change from 2018 to 2023 for each class and each metric
overall_change_in_metrics <- lapply(names(metric_results), function(metric_name) {
  df <- metric_values_df_list[[metric_name]]
  sapply(valid_classes, function(class_val) {
    start_value <- as.numeric(df[1, as.character(class_val)])
    end_value <- as.numeric(df[nrow(df), as.character(class_val)])
    print(paste("Start value for class", class_val, "in", metric_name, ":", start_value))
    print(paste("End value for class", class_val, "in", metric_name, ":", end_value))
    end_value - start_value
  })
})

names(overall_change_in_metrics) <- names(metric_results)

# Print the overall change from 2018 to 2023 for each class and each metric
for (metric_name in names(metric_results)) {
  overall_change <- overall_change_in_metrics[[metric_name]]
  print(paste("Overall change in", metric_name, "from 2018 to 2023 for each class:"))
  for (j in 1:length(valid_classes)) {
    class_val <- valid_classes[j]
    print(paste("Class", class_val, "overall change from 2018 to 2023:", overall_change[j]))
  }
}

# PD Plot ####

# Convert the PD values data frame to long format for ggplot
pd_values_long <- reshape2::melt(pd_values_df_list[["PD"]], id.vars = NULL)
colnames(pd_values_long) <- c("Class", "PD")
pd_values_long$Year <- rep(2018:2023, times = 3)
pd_values_long$Class <- factor(pd_values_long$Class, levels = c("1", "2", "3"),
                               labels = c("No Vegetation", "Low Vegetation", "High Vegetation"))

# Define the output file path
output_file <- file.path(getwd(), "PD_Over_Years_by_Class_ggplot.png")

# Create the plot using ggplot2
pd_plot <- ggplot(pd_values_long, aes(x = Year, y = PD, color = Class, group = Class)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("No Vegetation" = "blue", "Low Vegetation" = "green", "High Vegetation" = "red")) +
  labs(title = "Patch Density (PD) Over Years by Class",
       x = "Year",
       y = "Patch Density (PD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.title = element_blank(),
    legend.text = element_text(size = 18)
  )

# Save the plot to a file
ggsave(output_file, plot = pd_plot, width = 10, height = 7)

# Confirm the file was saved
print(paste("Graph saved to:", output_file))

# AREA Plot ####

# Convert the AREA values data frame to long format for ggplot
area_values_long <- reshape2::melt(metric_values_df_list[["AREA"]], id.vars = NULL)
colnames(area_values_long) <- c("Class", "AREA")
area_values_long$Year <- rep(2018:2023, times = 3)
area_values_long$Class <- factor(area_values_long$Class, levels = c("1", "2", "3"),
                                 labels = c("No Vegetation", "Low Vegetation", "High Vegetation"))

# Define the output file path
output_file_area <- file.path(getwd(), "AREA_Over_Years_by_Class_ggplot.png")

# Create the plot using ggplot2
area_plot <- ggplot(area_values_long, aes(x = Year, y = AREA, color = Class, group = Class)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("No Vegetation" = "blue", "Low Vegetation" = "green", "High Vegetation" = "red")) +
  labs(title = "Mean Patch Area (AREA) Over Years by Class",
       x = "Year",
       y = "Mean Patch Area (AREA)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.title = element_blank(),
    legend.text = element_text(size = 18)
  )

# Save the plot to a file
ggsave(output_file_area, plot = area_plot, width = 10, height = 7)

# Confirm the file was saved
print(paste("Graph saved to:", output_file_area))

# NP Plot ####

# Convert the NP values data frame to long format for ggplot
np_values_long <- reshape2::melt(metric_values_df_list[["NP"]], id.vars = NULL)
colnames(np_values_long) <- c("Class", "NP")
np_values_long$Year <- rep(2018:2023, times = 3)
np_values_long$Class <- factor(np_values_long$Class, levels = c("1", "2", "3"),
                               labels = c("No Vegetation", "Low Vegetation", "High Vegetation"))

# Define the output file path
output_file_np <- file.path(getwd(), "NP_Over_Years_by_Class_ggplot.png")

# Create the plot using ggplot2
np_plot <- ggplot(np_values_long, aes(x = Year, y = NP, color = Class, group = Class)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("No Vegetation" = "blue", "Low Vegetation" = "green", "High Vegetation" = "red")) +
  labs(title = "Number of Patches (NP) Over Years by Class",
       x = "Year",
       y = "Number of Patches (NP)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.title = element_blank(),
    legend.text = element_text(size = 18)
  )

# Save the plot to a file
ggsave(output_file_np, plot = np_plot, width = 10, height = 7)

# Confirm the file was saved
print(paste("Graph saved to:", output_file_np))

# LPI plot ####

# Convert the LPI values data frame to long format for ggplot
lpi_values_long <- reshape2::melt(metric_values_df_list[["LPI"]], id.vars = NULL)
colnames(lpi_values_long) <- c("Class", "LPI")
lpi_values_long$Year <- rep(2018:2023, times = 3)
lpi_values_long$Class <- factor(lpi_values_long$Class, levels = c("1", "2", "3"),
                                labels = c("No Vegetation", "Low Vegetation", "High Vegetation"))

# Define the output file path
output_file_lpi <- file.path(getwd(), "LPI_Over_Years_by_Class_ggplot.png")

# Create the plot using ggplot2
lpi_plot <- ggplot(lpi_values_long, aes(x = Year, y = LPI, color = Class, group = Class)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("No Vegetation" = "blue", "Low Vegetation" = "green", "High Vegetation" = "red")) +
  labs(title = "Largest Patch Index (LPI) Over Years by Class",
       x = "Year",
       y = "Largest Patch Index (LPI)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.title = element_blank(),
    legend.text = element_text(size = 18)
  )

# Save the plot to a file
ggsave(output_file_lpi, plot = lpi_plot, width = 10, height = 7)

# Confirm the file was saved
print(paste("Graph saved to:", output_file_lpi))

# CA plot ####

# Convert the CA values data frame to long format for ggplot
ca_values_long <- reshape2::melt(metric_values_df_list[["CA"]], id.vars = NULL)
colnames(ca_values_long) <- c("Class", "CA")
ca_values_long$Year <- rep(2018:2023, times = 3)
ca_values_long$Class <- factor(ca_values_long$Class, levels = c("1", "2", "3"),
                               labels = c("No Vegetation", "Low Vegetation", "High Vegetation"))

# Define the output file path
output_file_ca <- file.path(getwd(), "CA_Over_Years_by_Class_ggplot.png")

# Create the plot using ggplot2
ca_plot <- ggplot(ca_values_long, aes(x = Year, y = CA, color = Class, group = Class)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("No Vegetation" = "blue", "Low Vegetation" = "green", "High Vegetation" = "red")) +
  labs(title = "Class Area (CA) Over Years by Class",
       x = "Year",
       y = "Class Area (CA)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.title = element_blank(),
    legend.text = element_text(size = 18)
  )

# Save the plot to a file
ggsave(output_file_ca, plot = ca_plot, width = 10, height = 7)

# Confirm the file was saved
print(paste("Graph saved to:", output_file_ca))



