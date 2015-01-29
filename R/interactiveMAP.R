interactiveMAP<- function (raster){
  raster.points<-rasterToPoints(raster)
  raster.points<-as.data.frame(raster.points)


  raster.points$LatLong<- paste0(raster.points$y,":",raster.points$x)
  
  
  map <- gvisMap(raster.points, "LatLong" , "layer",
                 options=list( 
                   enableScrollWheel=TRUE,
                   mapType='terrain',
                   useMapTypeControl=TRUE,
                   icons=paste0("{",
                                "'default': {'normal': 'https://cdn4.iconfinder.com/data/icons/free-game-icons/64/Tree.png'",
                                "}}")))
  #https://cdn4.iconfinder.com/data/icons/free-game-icons/64/Tree.png
  #http://www.collectorspoint.se/files/css/ShopStatus/InStock.png
  #https://cdn4.iconfinder.com/data/icons/Boolean/Nursery/Tree.png 
  plot(map)
}