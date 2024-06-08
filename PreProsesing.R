library(raster)
library(terra)

# Define the input and output directories
input_dir <- "F:\\Modeling\\CECI Update\\ENV Rast Cliped\\Catagorical"
output_dir <- "F:\\Modeling\\CECI Update\\RF Ready Modles"  

# Define the DEM raster
dem_path <- "F:\\Modeling\\CECI Update\\ENV Rast Cliped\\DEM.tif" 
dem <- raster(dem_path)

# Get a list of all .tif files in the input directory
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)


# Loop through each .tif file
for (file in tif_files) {
  # Load the raster
  r <- raster(file)
  
  # Clip and resample the raster to match the DEM
  r_clipped <- crop(r, dem)
  r_resampled <- resample(r_clipped, dem, method = "ngb")  # Use "ngb" for categorical data
  
  # Create output filename
  output_filename <- file.path(output_dir, basename(file))
  
  # Save the processed raster
  writeRaster(r_resampled, filename = output_filename, format = "GTiff", overwrite = TRUE)
  
  # Print status
  cat("Processed:", basename(file), "\n")
}

cat("Processing completed. All rasters saved to", output_dir, "\n")
