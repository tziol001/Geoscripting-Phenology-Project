# Authors: Nikoula Team, Latifah and Nikos
# Date: 27/1/2015

# Function to calculate phenology metrics

# Description: This function implements the several method for extracting phenology. For further information you can search in the phenex package.

# parameter (type, description)

# df (data.frame, A data frame containing VI observations)
# DOYdf (data.frame, DOYs of the satellite observations)
# method (Character, Determines which model will be fitted to the corrected VI values)
# year (Numeric, Observation year)

getPhenology <-
  function(df, DOYdf, method, year )
  {
    if(method == "Gauss" | method == "SavGol") { 
      
    t.df<-as.data.frame(t(df))
    
    analysis<-cbind(DOYdf, t.df)
    
    x = DOYdf$Date
    ndvi <- matrix( nrow=366, ncol=length(t.df), byrow=FALSE)
    for(i in 1:length(t.df)){
      ndvi[x,i] <- analysis[,i]
    }
      #Fit an assymmetric Gausian
      ndvi.mod <- modelNDVI(ndvi.values=ndvi, year.int=year, 
                            correction="bise", method=method,asym=TRUE,slidingperiod=40)
      
      #estimate phenological parameters
      metrics <- matrix( nrow=1, ncol=length(ndvi.mod), byrow=FALSE)
      metrics<-as.data.frame(metrics)
      
      for(i in 1:length(metrics)){
        #greenup day  
        metrics[1,i] <- round((phenoPhase(ndvi.mod[[i]], phase="greenup", method="global", threshold=0.35)
                               + phenoPhase(ndvi.mod[[i]], phase="greenup", method="global", threshold=0.40)
                               + phenoPhase(ndvi.mod[[i]], phase="greenup", method="global", threshold=0.45))/3)
        
      }
  
  
      return (metrics)
    }
    else {
      stop('The method is not supported; should be "Gauss" or "SavGol".')
    }

  }

