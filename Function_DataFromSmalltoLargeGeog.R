#Function: takes in a QGIS/python generated intersection shapefile
#A data CSV matching the smaller geography
#Re-assigns that data by area
geolibs <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat")
lapply(geolibs, require, character.only = TRUE)

#its: intersect SpatialPolygonsDataframe
#dta: data to re-assign from small to large (small zone IDs should match those in intersect)
#lrg: large-zone shapefile to attach data to.
#gIDs: column for geo-ID for small zones
#gIDb: column for geo-ID for large zones
#ID: column for ID in data
#datacols: columns of data to re-assign zone
moveData <- function(its,dta,lrg,gIDs,gIDb,ID,datacols){
  
  #Get names of data columns to re-assign
  namez <- names(dta)[datacols]
  
  #Check whether either has more small-zone IDs than the other
  #The merge will only keep matching ones - but worth flagging up any difference here.
  
  #How many small-zone IDs in the intersect?
  #(some can e.g. be lost if not intersecting at all)
  print(
    paste0("Number of small-zone IDs in the intersect shapefile: ",
    length(unique(its@data[,gIDs]))
    ),quote = F
  )
  #How many in the data?
  print(
    paste0("Number of small-zone IDs in the data: ",
           length(unique(dta[,ID]))
    ),quote = F
  )
  
  #Area of small-zone chunks - can find small zone area by summing by group
  #Doing that here, rather than in original, also gets rid of e.g. any coastal edging
  #via the intersect already done
  its@data$area <- gArea(its, byid=T)
  
  #Work with dataframe to save faff, add to large zones later
  its_df <- data.frame(its)
  
  #Area of small zones by summing area of their slices
  its_df <- its_df %>% group_by_(names(its_df)[gIDs]) %>% 
    mutate(area_smallZone = sum(area))
  
  #fraction of the slices
  its_df$fraction <- its_df$area/its_df$area_smallZone
  
  #Merge fractions into data
  #Row for each fraction added by merge
  #(Will lose any non-matching IDs at this point)
  mrg <- merge(dta,its_df[,c(names(its_df)[gIDs],names(its_df)[gIDb],'fraction')],
               by.x = names(dta)[ID],
               by.y= names(its_df)[gIDs])
  
  #list of columns that are *not* data (to keep and not multiply)
  #notDataCols <- seq(seq(1:ncol(mrg)))[!(seq(1:ncol(mrg)) %in% datacols)]
  
  #Only need to keep new large zone ID and data to be multiplied by fraction
  #(Having previously verified this function works!)
  mrg2 <- cbind(mrg[,names(its_df)[gIDb]],mrg[,namez] * mrg$fraction)
  
  #Sum fractions to large zone totals
  #Zone to sum by will be in first column, via above cbind assignment
  izResult <- aggregate(mrg2[,namez],by=list(mrg2[,1]),sum)
  
  izData <- merge(lrg,izResult,by.x = "interzone", by.y = "Group.1")
  
  #Return the resulting re-assigned census values in shapefile form
  return(izData)
  
}