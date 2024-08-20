# After calculating field distances in qgis, and adding an attirbute with their width, use this code.
# This code calculates minimum, 25th percentile, median, 75th percentile and max field widths in a box plot.

# Install and load the necessary package
# Uncomment the line below if you don't have the sf package installed
# install.packages("sf")

library(sf)

# Set the working directory
setwd("add your working directory here")

# Read the shapefile (use the .shp file)
shapefile_data <- st_read("Field_Distances_2024_07_22_2.shp")

# Inspect the attribute table to find the correct column name
print(names(shapefile_data))

# Assuming the column you want to plot is named "Distance" (replace if different)
# Extract the relevant column
distance_data <- shapefile_data$Distance

# Set the file name for the plot
output_file <- "Field_Distances_2024_07_22_2_BoxPlot.png"

# Open a graphics device to save the plot
png(filename = output_file)

# Create the horizontal box plot with grey color
boxplot(distance_data, 
        main="Box Plot of Field Distances", 
        xlab="Distance", 
        col="grey", 
        horizontal=TRUE)

# Close the graphics device
dev.off()

# Print a message confirming that the file has been saved
cat("Box plot saved as", output_file, "\n")
