# Authors: Nikoula Team, Latifah and Nikos
# Date: 27/1/2015

# Function to download KNMI data and prepared for preproces

# Description: Automatically download KNMI data and returns a data frame 

# parameter (type, description)

# ID (Numeric, The ID of the meteorological station)


getKNMIdata <- function(ID){
  
  url<-paste0("http://www.knmi.nl/climatology/daily_data/datafiles3/",ID,"/etmgeg_",ID,".zip")
  download.file(url=url, destfile='data/url.zip', method='auto')
  unzip('data/url.zip', exdir="data/url")
  unlink('data/url.zip', recursive = TRUE)

# read the meteorogical data, the first 46 lines contain only the explanation of the data, so we start to read the data from line 47 (by defining "skip"parameter)
meteo <-read.table(file = paste0("data/url/etmgeg_",ID,".txt"), sep = ",", skip = 47, header = TRUE)

# add the column head of the table
headlist <- c("STN", "YYYYMMDD", "DDVEC", "FHVEC", "FG", "FHX", "FHXH", "FHN", "FHNH", "FXX", "FXXH", "TG", "TN", "TNH", "TX", "TXH", "T10N", "T10NH", "SQ", "SP", "Q", "DR", "RH", "RHX", "RHXH", "PG", "PX", "PXH", "PN", "PNH", "VVN", "VVNH", "VVX", "VVXH", "NG", "UG", "UX", "UXH", "UN", "UNH", "EV24")

colnames(meteo) <- headlist # add to data frame as a column name

# change the date format
meteo$YYYYMMDD <- as.Date(format(meteo$YYYYMMDD),'%Y%m%d')

return(meteo)
}
