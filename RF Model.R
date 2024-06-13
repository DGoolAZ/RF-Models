# Libraries --------------------------------------------------------------------
library(party)        # For conditional inference trees
library(terra)        # For raster and vector data manipulation
library(sf)           # For spatial data handling
library(caret)        # For data splitting and downsampling
library(pROC)         # For ROC curve and AUC calculation
library(ggplot2)      # For plotting
library(dplyr)        # For data manipulation
library(data.table)   # For efficient data handling

# Step 1: Read and Clean Presence Data -----------------------------------------
presence_data <- fread("F:\\Modeling\\CECI Update\\Presance\\PresMain_CECI.csv")

# Inspect the data
head(presence_data)
str(presence_data)

# Adjust column names based on actual data structure
latitude_col <- "latitude"
longitude_col <- "longitude"

# Remove extraneous columns and NA values
presence_data_clean <- presence_data[, .(latitude = get(latitude_col), longitude = get(longitude_col))]
presence_data_clean <- na.omit(presence_data_clean)

# Convert to sf object
presence_data_sf <- st_as_sf(presence_data_clean, coords = c("longitude", "latitude"), crs = 4326)

# Step 2: Read in Raster Data --------------------------------------------------
# Step 2: Read in Raster Data --------------------------------------------------
raster_directory <- "F:\\Modeling\\CECI Update\\ENV Rast Cliped"

# Identify the DEM file and the categorical variable file
dem_file <- file.path(raster_directory, "DEM.tif")
vegtype_file <- file.path(raster_directory, "VegType.tif")

# Load the DEM raster as a template for resampling
template_raster <- rast(dem_file)

# List all .tif raster files in the directory except the DEM and VegType
raster_files <- list.files(path = raster_directory, pattern = "\\.tif$", full.names = TRUE)
raster_files <- raster_files[!grepl("DEM.tif|VegType.tif", raster_files)]

# Extract original names of rasters
raster_names <- tools::file_path_sans_ext(basename(raster_files))

# Function to align rasters to the DEM template
align_raster <- function(raster_path, template) {
  raster <- rast(raster_path)
  if (nrow(raster) == 0 || ncol(raster) == 0) {
    stop(paste("Raster is empty or invalid:", raster_path))
  }
  resampled_raster <- resample(raster, template, method = "bilinear")
  return(resampled_raster)
}

# Resample all rasters to align with the DEM raster
aligned_rasters <- lapply(raster_files, align_raster, template = template_raster)
names(aligned_rasters) <- raster_names

# Add the DEM raster and VegType raster to the list
aligned_rasters <- c(aligned_rasters, list(DEM = template_raster))
vegtype_raster <- rast(vegtype_file)
vegtype_raster <- resample(vegtype_raster, template_raster, method = "near")
aligned_rasters <- c(aligned_rasters, list(VegType = vegtype_raster))

# Combine aligned rasters into a RasterStack
raster_stack <- rast(aligned_rasters)

# Ensure rasters are correctly stacked
if (is.null(raster_stack) || length(raster_stack) == 0) {
  stop("Raster stack is NULL or empty. Check the rasters and their alignment.")
}

# Print details about the raster stack
print(raster_stack)

################################################################################
###                      Step 2 Explanation:
###  1. Loaded all .tif rasters
###  2. Assigned DEM and VegType and made DEM reference raster
###  3. Aligned all continuous rasters, using DEM as reference raster
###  4. Aligned VegType to DEM, using method suitable for categorical variables
###  5. Check to assure all names of variables are present in final stack
################################################################################

# Step 3: Extract Raster Values for Presence Data ------------------------------
presence_values <- terra::extract(raster_stack, presence_data_sf)
presence_values <- presence_values[, -1]  # Remove ID column
presence_df <- as.data.table(presence_data_sf)
presence_df <- cbind(presence_df, presence_values)
presence_df$VegType <- as.factor(presence_df$VegType)
presence_df$response <- 1

################################################################################
###                      Step 3 Explanation:
###  1. Extract raster values at presence points
###  2. Combine presence data with extracted values
###  3. Adds a response column with a value of 1 for presence
################################################################################

# Step 4: Generate Background Data ---------------------------------------------
set.seed(17172)
background_points <- spatSample(raster_stack, size = nrow(presence_data_clean), xy = TRUE, as.df = TRUE)
background_sf <- st_as_sf(background_points, coords = c("x", "y"), crs = 4326)
background_values <- terra::extract(raster_stack, background_sf)
background_values <- background_values[, -1]
background_df <- as.data.table(background_sf)
background_df <- cbind(background_df, background_values)
background_df <- na.omit(background_df)
background_df$VegType <- as.factor(background_df$VegType)
background_df$response <- 0

# Remove unnecessary columns and clean
background_df_clean <- background_df %>% select(-(1:9))

# Inspect the cleaned background dataframe
head(background_df_clean)

################################################################################
###                      Step 4 Explanation:
###  1. Set seed for reproducibility
###  2. Generate the same number of background points as presence points
###  3. Extract raster values using terra
###  4. Creates response column set to 0 for pseudo absence points
###  5. Removes NA's from data frame
###  6. Sets VegType to factor
################################################################################

# Combine presence and background data
combined_data <- rbind(presence_df, background_df_clean)

# Remove rows with any remaining NA values
combined_data_clean <- na.omit(combined_data)

# Ensure response variable is a factor
combined_data_clean$response <- as.factor(combined_data_clean$response)

# Check the structure of the cleaned combined data
print(head(combined_data_clean))
str(combined_data_clean)

# Prepare predictor data and response variable for downSampling
x_data <- combined_data_clean[, !names(combined_data_clean) %in% c("response", "geometry"), with = FALSE]
y_data <- combined_data_clean$response

# Debug: Print dimensions to verify alignment
cat("Dimensions of x_data before downsampling: ", dim(x_data), "\n")
cat("Dimensions of y_data before downsampling: ", length(y_data), "\n")

# Downsample to balance classes
set.seed(123)
balanced_data <- downSample(
  x = x_data,
  y = y_data,
  yname = "response"
)

# Inspect the balanced data
print(head(balanced_data))
str(balanced_data)

# Remove the geometry column if it still exists
balanced_data_no_geom <- balanced_data %>% select(-geometry, -contains("geometry"))

# Check the final structure of the balanced data
str(balanced_data_no_geom)

# Debug: Check for NAs and dimensions
cat("Number of rows in balanced_data_no_geom: ", nrow(balanced_data_no_geom), "\n")
cat("Number of columns in balanced_data_no_geom: ", ncol(balanced_data_no_geom), "\n")


# Define the control parameters for cforest
control <- cforest_unbiased(mtry = 2, ntree = 500)  # Adjust mtry and ntree as needed

# Train the model
set.seed(17172)
rf_model <- cforest(response ~ ., data = balanced_data_no_geom, controls = control)

# Inspect the model
print(rf_model)

################################################################################
###                      Step 5 Explanation:
###  1. Combine presence and cleaned background data
###  2. Remove rows with any remaining NA values
###  3. Convert the response variable to a factor
###  4. Downsample the majority class (background) to balance the dataset
###  5. Inspect the first few rows of the balanced data
################################################################################
# Libraries required for this step
library(terra)
library(data.table)
library(pbapply)

# Step 6: Predict Using Direct Raster Processing in Tiles --------------------------------

# Paths for input and output
output_raster_path <- "F:\\Modeling\\CECI Update\\Predictions\\Predicted_Presence.tif"

# Function to predict raster values directly in chunks
predict_in_chunks <- function(model, raster_stack, tile_size) {
  # Get raster extent and tile layout
  ext <- terra::ext(raster_stack)
  n_cols <- ceiling((ext[2] - ext[1]) / tile_size)
  n_rows <- ceiling((ext[4] - ext[3]) / tile_size)
  
  # Initialize an empty raster for the output
  prediction_raster <- terra::rast(raster_stack, nlyr = 1)
  terra::values(prediction_raster) <- NA
  
  # Progress bar setup
  pb <- txtProgressBar(min = 0, max = n_cols * n_rows, style = 3)
  progress_index <- 0
  
  # Process each tile
  for (i in seq_len(n_cols)) {
    for (j in seq_len(n_rows)) {
      # Define the extent of the current tile
      xmin <- ext[1] + (i - 1) * tile_size
      xmax <- min(xmin + tile_size, ext[2])
      ymin <- ext[3] + (j - 1) * tile_size
      ymax <- min(ymin + tile_size, ext[4])
      tile_extent <- terra::ext(c(xmin, xmax, ymin, ymax))
      
      # Crop the raster to the current tile extent
      tile_raster <- terra::crop(raster_stack, tile_extent)
      
      # Extract data from the tile raster
      tile_values <- as.data.table(terra::as.data.frame(tile_raster, xy = TRUE, na.rm = FALSE))
      
      # Skip empty tiles
      if (nrow(tile_values) == 0) next
      
      # Align factor levels
      factor_columns <- names(balanced_data_no_geom)[sapply(balanced_data_no_geom, is.factor)]
      for (col in factor_columns) {
        if (col %in% names(tile_values)) {
          tile_values[[col]] <- factor(tile_values[[col]], levels = levels(balanced_data_no_geom[[col]]))
        }
      }
      
      # Predict probabilities
      predictions <- predict(model, newdata = tile_values, type = "prob")
      positive_class_probs <- predictions[, 2]
      
      # Assign predicted probabilities to tile
      tile_prediction_raster <- terra::rast(tile_raster[[1]])
      terra::values(tile_prediction_raster) <- NA
      cell_indices <- terra::cellFromXY(tile_prediction_raster, tile_values[, .(x, y)])
      tile_prediction_raster[cell_indices] <- positive_class_probs
      
      # Merge the tile predictions back into the full prediction raster
      prediction_raster <- terra::mosaic(prediction_raster, tile_prediction_raster, fun = "mean", overwrite = TRUE)
      
      # Clear memory
      rm(tile_raster, tile_values, predictions, tile_prediction_raster)
      gc()
      
      # Update progress bar
      progress_index <- progress_index + 1
      setTxtProgressBar(pb, progress_index)
    }
  }
  close(pb)
  
  return(prediction_raster)
}

# Define the tile size (e.g., 500 x 500 pixels)
tile_size <- 100  # Reduced to fit memory constraints

# Predict presence probabilities
predicted_presence_raster <- predict_in_chunks(rf_model, raster_stack, tile_size)

# Save the final predicted raster
writeRaster(predicted_presence_raster, filename = output_raster_path, filetype = "GTiff", overwrite = TRUE)

# Plot the predicted presence probability raster
plot(predicted_presence_raster, main = "Predicted Presence Probability")

# Calculate and plot ROC curve
roc_curve <- roc(balanced_data_no_geom$response, values(predicted_presence_raster, mat = TRUE))
auc_value <- auc(roc_curve)
print(paste("AUC Value:", auc_value))

################################################################################
###                      Step 6 Explanation:
###  1. Splits the raster into smaller tiles to manage memory usage.
###  2. Predicts presence probabilities for each tile using the random forest model.
###  3. Merges the tile predictions into the full raster incrementally.
###  4. Clears memory after processing each tile.
###  5. Saves the final predicted raster and plots it.
###  6. Calculates and prints the AUC value.
################################################################################

