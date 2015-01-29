# Authors: Nikoula Team, Latifah and Nikos
# Date: 27/1/2015

# Function to get the DOYs of the satellite observations

# Description: Get the dates of MODIS observations and convert them into DOYs for further analysis. Function returns a data frame

# parameter (type, description)

# site (Character, It can be one of the flux towers from around the world)
# band (Character, It can be one of the two vegetation indices "EVI" or "NDVI")  
# year (Numeric, All data which are different from this year will be removed)

getDate <-
  function(site, band, year)
  {
    Sites_info <- read.csv("data/MODIS_Subset_Sites_Information.csv")
    pos <- which(Sites_info$Site_Name == site)
    fluxtower<- paste0(Sites_info$Site_ID[pos],".txt")
    modis <- read.csv(fluxtower, colClasses = "character")
      
    # convert the date format
    modis$Date <- as.Date(modis$Date,"A%Y%j")
 
 date <- modis[modis$Band == paste0("250m_16_days_",band) & as.numeric(format(modis$Date, "%Y")) == year,3:4] # for a strange reason it is not possible to select only column 3 so line 22 and 23 are written to make this simples selection!
  date$Site <- NULL
  date$Date<-as.numeric(strftime(date$Date, format="%j",tz="EST"))
 return(date)
}