#If a shapefile data column doesn't have unique IDs per column
#Re-shape the shapefile so that each feature (each row)
#Is a single (possibly multi-part) polygon on its own row
#So that data can be attached correctly to it.
#Note: any data that came with the shapefile is chucked.
#This function just keeps the ID column in question and attaches to polygons
#If you want to do something with the original data, keep it
#summarise as you see fit then re-attach afterwards.
geolibs <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat")
lapply(geolibs, require, character.only = TRUE)

#shp: shapefile
#IDindex: column to check is unique ID
#Or to reshape to unique ID
IDtoSingleRow <- function(shp, IDindex) {
  
  #check if ID isn't unique per row. If it is, we have nothing to do here!
  #(Note: logically can't be *more* unique IDs than there are rows)
  if ( (unique(shp@data[,IDindex]) %>% length) != nrow(shp@data) ) {
    
    print("Unique IDs in more than one row: combining into single row/feature.", quote = F)
  
    #Dissolve down to individual polygons by ID
    #ID goes into the row names. Loses any attached data
    #cf. http://www.inside-r.org/packages/cran/maptools/docs/unionSpatialPolygons
    dissolve <- gUnaryUnion(shp, id = shp@data[,IDindex])
    
    #Convert back to spatialpolygonDF with ID as zone ID
    #To get polygon IDs: https://stat.ethz.ch/pipermail/r-help/2005-December/085175.html
    #So to be clear on what's happening:
    #Set zone ID to polygon ID when dissolving
    #Then use that same ID to rebuild as DF for saving.
    #(Which wouldn't work if we cared about the data, but if there *was data
    #We could e.g. do some summarising first)
    ids <- as.data.frame(sapply(slot(dissolve, "polygons"), function(x) slot(x, "ID")))
    row.names(ids) <- ids[,1]
    
    #It's got the right projection from the spatialPolygons object already
    dissolve_SPDF <- SpatialPolygonsDataFrame(
      dissolve,
      data=ids
    )
    
    names(dissolve_SPDF@data) <- names(shp@data)[IDindex]
    
    return(dissolve_SPDF)
    
  } else {
   
    print("IDs are already unique per row.", quote = F)
    return(FALSE)
    
  }
  
  
}