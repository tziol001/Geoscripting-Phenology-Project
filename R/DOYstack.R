# Authors: Nikoula Team, Latifah and Nikos
# Date: 27/1/2015

# Function to connect DOY with the corresponding layer of the stack

# Description: Give in each layer of the stack the corresponding observation DOY and it returns a georefernced raster stack with appropriate layer names.

# parameter (type, description)

# stack (Stack layer, A georefernced stack layer)
# date (data frame, The nrow of data frame should be equal to the number of layers in the stack)  

DOYstack <- function(stack, date) {
  
  # transponse the data frame and transform it into a list
  date <- as.list(t(date))
  
  # give the DOY as name in each layer
  for (i in 1:length(date) ) {
    names(stack)[i]=date[i]
  }
  return(stack)
}
