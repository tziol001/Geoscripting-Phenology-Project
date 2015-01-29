# Authors: Nikoula Team, Latifah and Nikos
# Date: 27/1/2015

# Function to get a table of values representing all layers with known selected classes

# Description: A mask operation is performed in order to keep only raster values representing selected class pixels. The function results a data frame.

# parameter (type, description)

# raster (raster brick or stack)
# ROI (vector, A vector with the representing class)  

getROI <-
  function(raster, ROI)
  {

  # reproject vector layer to projection of the raster layer
  ROI_proj2raster <- spTransform(ROI, CRS(proj4string(raster)))

  # keep only the representing class pixels by using mask
  masked_ROI <- mask(raster, ROI_proj2raster)
  
  # extract all values into a matrix and convert it a new data frame
  valuetable <- getValues(masked_ROI)
  valuetable <- as.data.frame(masked_ROI)

  return (valuetable)
}