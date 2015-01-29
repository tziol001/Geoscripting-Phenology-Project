# Authors: Nikoula Team, Latifah and Nikos
# Date: 27/1/2015

# The objective of this script is to perform an analysis with R in order to investigate if the phenological metrics are affected by the climatic changes. Particularly, is this project the greenup dates for a deciduous forest in Lelystad Netherlands calculated for two different years. Several studies indicated that plant phenology reactes to warmer and wet climate hence, leaf emergence happens earlier, and leaf fall happens later due to milder winters. Based on the above acceptance we will compare two different years expecting that the warmer year results in earliers greenup dates. The end product is in a format that is amenable to simple statistical analysis (histograms and boxplots). Simulteanously a visual comparison could be done among the Vegetation index maps among the several observations.This project is organized by division made between several major tasks: 1) meteorogical KNMI data analysis 2) MODIS remotely sensed data 3) specification of the area of interest 4) calculation of phenological metrics 5) simple statistical analysis and visualization of some results.

# import the required packages
rm(list=ls()) # clear the workspace

getwd() # make sure the data directory


#install.packages("gridExtra")
require(gridExtra) # also loads grid
require(lattice)
#install.packages("spocc")
library(googleVis)

# load required packages and functions
library(phenex)
library(sp)
library(raster)
library(rgdal)
library(ggplot2)
library(googleVis)


source('R/modisSubset.R')
source('R/getDate.R')
source('R/modisRaster.R')
source('R/df2raster.R')
source('R/DOYstack.R')
source('R/getROI.R')
source('R/getPhenology.R')
source('R/getKNMIdata.R')
source('R/interactiveMAP.R')

#-----------------------------------------------------------------------------------------
# 1) meteorogical KNMI data analysis
#-----------------------------------------------------------------------------------------

# download meteorological data of Leleystad station (ID = 269). source: http://www.knmi.nl/klimatologie/daggegevens/download.html
meteo <- getKNMIdata(269)

# subset daily mean temperature (TG) and daily precipitation (RH) data from the year of 2013 and 2014
met13 <- meteo[as.numeric(format(meteo$YYYYMMDD, "%Y")) == 2013, c("YYYYMMDD", "TG", "RH")]
met14 <- meteo[as.numeric(format(meteo$YYYYMMDD, "%Y")) == 2014, c("YYYYMMDD", "TG", "RH")]

# multiply the TG and RH with 0.1 (TG is in 0.1 deg Celcius, RH is in 0.1mm)
met13$TG <- met13$TG*0.1
met13$RH <- met13$RH*0.1
met14$TG <- met14$TG*0.1
met14$RH <- met14$RH*0.1

# evaluation of the difference in temperature and precipitation between 2013 and 2014
## apply cumulative summary for the table
head(met13) # inspect the data
meteo13 <- apply(met13[,2:3],2,cumsum) # TG and RH are in column 2 and 3 
meteo14 <- apply(met14[,2:3],2,cumsum)
meteo13 <- as.data.frame(meteo13) # coerce to data frame
meteo14 <- as.data.frame(meteo14)

# temperature (TG) as data frame
temp <- cbind(meteo13$TG, meteo14$TG)
temp <- as.data.frame(temp)
colnames(temp) <- c("TG2013", "TG2014") # give the proper name of column
temp$day <- seq.int(nrow(temp)) # add column: day-of-year

# precipitation (RH) as data frame
precip <- cbind(meteo13["RH"], meteo14["RH"])
precip <- as.data.frame(precip)
colnames(precip) <- c("RH2013", "RH2014") # give different name of column
precip$day <- seq.int(nrow(precip)) # add column: day-of-year

#----------------------------------------------------------------------------------------
# 2) MODIS remotely sensed data
#----------------------------------------------------------------------------------------

# download for Lelystad flux tower the NDVI band for 2013 and 2014 by using the  ModisSubset function. The reliability is selected as TRUE in order to perform a cloud  cleaning.
ndvi_clear13 <- modisSubset("Lelystad", "NDVI", 2013, rel = TRUE) 
ndvi_clear14 <- modisSubset("Lelystad", "NDVI", 2014, rel = TRUE) 

# create raster stacks from the subset MODIS data
stack_2013 <- modisRaster(ndvi_clear13,"Lelystad")
stack_2014 <- modisRaster(ndvi_clear14,"Lelystad")
stack_2013<-stack_2013/10000
stack_2014<-stack_2014/10000
# get observation dates for the selected subsets
DOY2013<- getDate("Lelystad", "NDVI", 2013)
DOY2014<- getDate("Lelystad", "NDVI", 2014)

# gives DOY as name in stack's layers
stack2013 <- DOYstack(stack_2013, DOY2013)
stack2014 <- DOYstack(stack_2014, DOY2014)

#----------------------------------------------------------------------------------------
# 3) specification of the area of interest
#----------------------------------------------------------------------------------------

# url <- "https://github.com/tziol001/Project-Geoscripting/blob/master/data/clc2012.zip"
# download.file(url=url, destfile='data/corine.zip', method='auto')
unzip('data/clc2012.zip')

# import the corine landcover 2012, in this project a preprocessing of corine data is performed in order to achieve faster computation time. For this reason only the corine for The Netherlands is selected
corine <- list.files ('clc2012/', pattern = glob2rx('*.shp'), full.names = TRUE)
layerName <- ogrListLayers(corine)
clc2012 <- readOGR(corine, layer = layerName)

# select the deciduous forest clc_code: 311
clc2012_forest <- clc2012[clc2012$CODE_12 == 311,]

# extract all the values from the layers of the rasterstack by using the selected forest area as a mask. These data.frames will be used as an input into the getPhenology function.   
df2013<- getROI(stack2013, clc2012_forest)
df2014<- getROI(stack2014, clc2012_forest)

#----------------------------------------------------------------------------------------
# 4) calculation of phenological metrics
#---------------------------------------------------------------------------------------- 

# prepare the data for pheno analysis and extract the greenup date for each pixel by using an assymetric Gaussian("Gauss") function from the phenex package. Method could be also specified as "SavGol". 
greenup2013<-getPhenology(df2013, DOY2013, "SavGol", 2013)
greenup2014<-getPhenology(df2014, DOY2014,"SavGol", 2014)

# calculate the differences in the greenup dates between the two years
diff <- greenup2014 - greenup2013
#----------------------------------------------------------------------------------------
# 5.1) simple statistical analysis
#----------------------------------------------------------------------------------------
# simple histograms
hist(as.numeric(greenup2013))
hist(as.numeric(greenup2014))
hist(as.numeric(diff))
# create a raster and a KML of the difference between the estimated greeup dates of 2013 and 2014 
greenup_diff <- modisRaster(diff,"Lelystad")
KML(greenup_diff, filename = "data/diff.kml", overwrite=TRUE)
  
# boxplots and histogram statistic results
diff_table<-as.data.frame(t(diff))
boxplot(diff_table,col=rainbow(10),main="Boxplots of difference in greenup estimation", ylab ="green-up date (DOY)",ylim=c(-200, 200), xlab ="greenup")
hist(diff_table$V1, breaks=200, main="Comparison of greenup dates ", xlab="DOY")

# create a histogram in which is indicated the differences in the greeen up DOYS  
Hist <- gvisHistogram(diff_table, options=list(
  legend="{ position: 'top', maxLines: 1 }",
  colors="['#5C3292']",
  width=400, height=360))
plot(Hist)
#----------------------------------------------------------------------------------------
# 5.2) visualization of results
#----------------------------------------------------------------------------------------
# i) visualize meteorological data
#****************************************************************************************
# cummulative temperature
ggplot(temp, aes(day)) + 
  geom_line(aes(y = TG2013, colour = "2013"),size = 1.5) + 
  geom_line(aes(y = TG2014, colour = "2014"), size = 1.5) +
  xlab('Day of year') +
  ylab('Temperature in °C (cummulative summary)') +
  labs(colour = 'Year') +
  theme_bw()

# cummulative precipitation
ggplot(precip, aes(day)) + 
  geom_line(aes(y = RH2013, colour = "2013"), size = 1.5) + 
  geom_line(aes(y = RH2014, colour = "2014"), size = 1.5) +
  xlab('Day of year') +
  ylab('Precipitation in mm (cummulative summary)') +
  labs(colour = 'Year') +
  theme_bw()

# interactiveplot (googleVis) cummulative temperature
Line <- gvisLineChart(temp, "day", c("TG2013","TG2014"),
                      options=list(
                        vAxis="{title:'cummulative temperature'}"
                      ))
plot(Line)

#****************************************************************************************
# ii) visualize ndvi maps of Lelystad subset for specific DOY with ggplot and spplot
#****************************************************************************************
names(stack2013) 

# ggplot demands a data frame as input, so the raster should be convert into data frame
map.points <- rasterToPoints(stack2013$X1)
df <- data.frame(map.points)

# create the appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "Value")

# crate map with ggplot
ggplot(aes(x = Longitude, y = Latitude, fill = Value), data = df) + 
  geom_raster() + coord_equal() + 
  ggtitle("NDVI 1st of January, 2013") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  scale_fill_continuous(low="brown", high="green", limits=c(-0,1)) +
  labs(fill = "NDVI")  +
  theme_bw()

# plot some DOYs of 2013 together by using the gridextra package
p1 = spplot(stack2013, zcol = 'X1',main=paste("NDVI, DOY 1"), col.regions = colorRampPalette(c("white","darkkhaki","darkgreen"))(50))
p2 = spplot(stack2013, zcol = 'X65',main=paste("NDVI, DOY 65"), col.regions = colorRampPalette(c("white","darkkhaki","darkgreen"))(50))
p3 = spplot(stack2013, zcol = 'X129',main=paste("NDVI, DOY 129"), col.regions = colorRampPalette(c("white","darkkhaki","darkgreen"))(50))
p4 = spplot(stack2013, zcol = 'X161',main=paste("NDVI, DOY 161"), col.regions = colorRampPalette(c("white","darkkhaki","darkgreen"))(50))
p5 = spplot(stack2013, zcol = 'X225',main=paste("NDVI, DOY 225"), col.regions = colorRampPalette(c("white","darkkhaki","darkgreen"))(50))
p6 = spplot(stack2013, zcol = 'X353',main=paste("NDVI, DOY 353"), col.regions = colorRampPalette(c("white","darkkhaki","darkgreen"))(50))

# put all together and save it as png
png(filename="NDVI2013.png")
grid.arrange(p1,p2,p3,p4,p5,p6, nrow=3, ncol=2)
dev.off()

#****************************************************************************************
# iii) visualize DOY map of 2013 using an interactive map (googleVis)
#****************************************************************************************

inter.map.2013 <- modisRaster(greenup2013,"Lelystad")
interactiveMAP(inter.map.2013)
#----------------------------------------------------------------------------------------