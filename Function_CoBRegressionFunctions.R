#CoB regression functions
geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat","data.table",'readr','pryr','combinat')
lapply(geolibs, require, character.only = TRUE)


#~~~~~~~~~~~~~~~~~~~~
CoBtoRegressionReadyData <- function(listOfSHPS, zonesToKeep, CoBsToKeep, UKcobs, year4weightsNmigshare, censusYear){
  
  #totalPop calculated just below
  CoBsToKeep <- c('label',CoBsToKeep,'totalPop')
  
  #Before subsetting CoBs, use them to get total population per zone
  #keep separate, attach to full sheet below
  totalPop <- list()
  
  for(i in 1:length(listOfSHPS)){
    #listOfSHPS[[i]]@data$totalPop <- apply(listOfSHPS[[i]]@data[,2:ncol(listOfSHPS[[i]]@data)],1,sum)
    totalPop[[i]] <- apply(listOfSHPS[[i]]@data[,2:ncol(listOfSHPS[[i]]@data)],1,sum)
  }
  
  #subset zones
  subz <- lapply(listOfSHPS, function(x) x[x$label %in% zonesToKeep,])
  
    #subset cobs. Assumes they're the same in each so uses first as match
  subz <- lapply(subz, function(x) x[,(names(x) %in% CoBsToKeep)])
  
  for(i in 1:length(subz)) subz[[i]]$censusYear <- censusYear[i]
  
  #Combine into one
  allCoB <- lapply(subz,data.frame)
  allCoB <- do.call(rbind,allCoB)
  
  #return(allCoB)
  #break
  
  #Do props
  cobprops <- allCoB %>% 
    dplyr::select(2:ncol(allCoB)) %>% 
    group_by(censusYear) %>% 
    mutate_each(  funs(  ((.)/sum(.))*100  ) )
  
  #Did that work? Yup!
  # apply(cobprops[cobprops$censusYear==2011,c(1:ncol(cobprops))],2,sum)
  
  #This'll need changing
  #Then: each decade in it own column. Each long.
  # long91 <- cobprops[cobprops$censusYear==1991,] %>%
  #   gather(cob,xij91,1:(ncol(cobprops)-1))
  # long01 <- cobprops[cobprops$censusYear==2001,] %>%
  #   gather(cob,xij01,1:(ncol(cobprops)-1))
  # long11 <- cobprops[cobprops$censusYear==2011,] %>%
  #   gather(cob,xij11,1:(ncol(cobprops)-1))
  # 
  # 
  # #CoB will match for each. Label vector will repeat.
  # sheet <- data.frame(cob = long91$cob,xij91 = long91$xij91, xij01 = long01$xij01, xij11 = long11$xij11)

  #Generic version good for 3 and 5 census
  longz <- list()

  for(i in 1:length(censusYear)){

    longz[[i]] <- cobprops[cobprops$censusYear==censusYear[i],!(names(cobprops) =='censusYear')] %>%
      #gather_('cob',paste0('xij',censusYear[i]),'1:(ncol(cobprops)-1)')
      #https://stackoverflow.com/questions/29537848/gather-does-not-work-shouldnt-quoting-and-ing-have-the-same-effect-in-standa
      gather_('cob',paste0('xij',censusYear[i]),names(.))

  }

  sheet <- longz[[1]]

  #add other xij columns
  for(i in 2:length(censusYear)){
    sheet <- cbind(sheet,longz[[i]][,2])
  }
  
  #Just one set of labels... will repeat
  sheet$label <- allCoB$label[allCoB$censusYear=='1991']
  
  #return(sheet)
  #break
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #MIGSHARE MINUS OWN----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #Migrant share is actually per zone, not across zones
  #And it's "non-UK born" as a percent of the population in that zone
  #And we need to discount the CoB in question too. So.
  #Need to subset again
  #Pick year. So none of these are now just 91. Change!
  
  #Do for each supplied year
  for(year in year4weightsNmigshare){

    subformig <- listOfSHPS[[which(censusYear==year)]]
    subformig <- subformig[subformig$label %in% zonesToKeep,]

    #migshareCalc91 <- data.frame(listOfSHPS[[1]])
    migshareCalc91 <- data.frame(subformig)

    #migshareCalc91 <- tibble(subformig)

    migshareCalc91$totalPop <- apply(migshareCalc91[,2:ncol(migshareCalc91)],1,sum)

    #UK total
    # migshareCalc91$UK <- (migshareCalc91$Channel_Is +
    #                         migshareCalc91$UK_part_no +
    #                         migshareCalc91$England +
    #                         migshareCalc91$Wales +
    #                         migshareCalc91$Northern_I +
    #                         migshareCalc91$Scotland)

    #UK total
    #migshareCalc91$UK <- apply(migshareCalc91[,UKcobs],1,sum)

    #cos the dataframe conversion messes with the column names, need to faff about
    #Will see if this is robust or not
    migshareCalc91$UK <- apply(migshareCalc91[,gsub(' ','.',UKcobs)],1,sum)

    #make long so we can work out migshare minus own
    #Keeping total pop and UK in own cols repeated
    #remove label
    #And keep only the CoBs we're using (will be correct order, checked)
    namesKeepinz <- names(listOfSHPS[[1]])[2:length(names(listOfSHPS[[1]]))]

    migshareLong <- migshareCalc91 %>%
      dplyr::select(2:ncol(migshareCalc91)) %>%
      #gather(cob, count, Channel_Is:Iran) %>%
      #gather_('cob', 'count', namesKeepinz) %>%
      gather_('cob', 'count', gsub(' ','.',namesKeepinz)) %>%
      filter(cob %in% CoBsToKeep)

    #share of migrants, not including my own group
    #Is zone total pop minus all-UK minus my own CoB group
    #E.g. 100 total pop 80 UK is 20 non-UK
    #5 India means 15 non-UK discounting Indian born
    #As proportion of total pop
    migshareLong$migShareMinusOwn91 <- ((migshareLong$totalPop - migshareLong$UK - migshareLong$count)/migshareLong$totalPop)*100

    #Add to sheet
    sheet$x <- migshareLong$migShareMinusOwn91
    names(sheet)[names(sheet)=='x'] <- paste0('migshareMinusOwn',year)

  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #WEIGHTS MATRICES----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
    #Random shp to get the polygons
  zones <- listOfSHPS[[1]] 
  #Need to reduce zones to those we've used above in the sheet
  
  zonesub <- zones[zones@data$label %in% zonesToKeep,]
  
  #spdep
  #https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
  contig <- poly2nb(zonesub, row.names = zonesub@data$label)
  
  #Row-normalised
  mx <- nb2mat(contig,zero.policy = T)
  
  #OK. Weights add to 1 over rows.
  apply(mx,1,sum)
  
  #save
  # mxdf <- data.frame(mx)
  # names(mxdf) <- zonesub@data$label
  # row.names(mxdf) <- zonesub@data$label
  # 
  # write.csv(mxdf,'R_data/londonWeightsMatrix_queenContigRowNormalised.csv')
  # 
  # #Not row-normalised
  # mx <- nb2mat(contig,zero.policy = T, style = 'B')
  # mxdf <- data.frame(mx)
  # names(mxdf) <- zonesub@data$label
  # row.names(mxdf) <- zonesub@data$label
  # 
  # write.csv(mxdf,'R_data/londonWeightsMatrix_queenContigBinary.csv')
  # 
  
  
  
  #~~~~~~~~~~~~~~~~~
  #Apply to CoBs in each census year
  #One column per CoB (cobprops minus censusyear)
  
  for(year in year4weightsNmigshare){
  
  #Subset to correct CoBs here to save faff later
    cobpropsub <- cobprops[,names(cobprops) %in% c(as.character(unique(sheet$cob)),'censusYear')]
    
    weightsMatrix <- matrix(nrow = nrow(zonesub),ncol = (length(names(cobpropsub))-1))
    
    for(val in seq(1:(length(names(cobpropsub))-1))){
      weightsMatrix[,val] <- mx %*% as.matrix(cobpropsub[cobpropsub$censusYear==year,(val)])
    }
    
    #And they should now all be in the right order for melting and sticking in the thingyo.
    sheet$x <- weightsMatrix %>% data.frame() %>% 
      gather(cob,value) %>%
      dplyr::select(value) %>% 
      unlist(use.names = F)
    
    names(sheet)[names(sheet)=='x'] <- paste0('w',year,'q')
  
  }
  
  return(sheet)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#OUTPUT REGRESSION TO FILE----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

outputTofile <- function(inputdata, title, lmfunction, splitByFactor = NULL, myAppend = F,...){
  
  if(myAppend){
  capture.output(noquote(paste0(title,', formula: ', lmfunction)), ..., append = T)
  } else {
  capture.output(noquote(paste0(title,', formula: ', lmfunction)), ...)
    }
  
  
  #Append is always T for the second output. Does this append override dots? Newp...
  
  if(is.null(splitByFactor)){
    
    capture.output(
    summary(lm(data = inputdata, formula = lmfunction)),
    ...,
    append = T
    )
    
  } else {
    
    capture.output(
      lapply(split(inputdata, 
                   inputdata[,splitByFactor]), function(x) summary(lm(data=x, formula = lmfunction))),
      ...,
      append = T
    )
    
  }
  
}

# outputRegressionSplitByFactorToFile <- function(title, dataframe, ...){
#   
#   capture.output(noquote(title), ...)
#   
#   capture.output(
#     lapply(split(dataframe, 
#                  dataframe[,splitvar]), function(x) summary(lmfunction)),
#     append = T
#   )
#   
# }


#THIS IS VERY SPECIFIC TO 3 CENSUS SCOTLAND CURRENTLY!
#AND TO SOME VARS I HAVE LOADED IN REGRESSIONS2. HACKING FOR ENGLAND NOW.
compileSheetDiffZones <- function(zonesToKeep){
  
  #sheet <- CoBtoRegressionReadyData(cobScot,zonesToKeep,geoffsOrigList,c(1991,2001,2011))
  
  sheet <- CoBtoRegressionReadyData(cobScot,zonesToKeep,geoffsOrigList,
                                    c('Channel_Is','UK_part_no','England','Wales','Northern_I','Scotland'),
                                    c(1991,2001),c(1991,2001,2011))
  
  
  #Can use all vars from previous sheet that aren't specific to CoBs and apply only to zones
  #pop91s will be fine, it's only 2001 that changed
  
  #Merge in other bits
  sheet <- merge(sheet,eViewsFile,by.x='label',by.y='code', all.x=T)
  
  #Rich/poor would be good. We also need cities.
  sheet <- merge(sheet,richpoor,by.x = 'cob',by.y='geoffsOrigList',all.x=T)
  
  #make rich/poor a little more readable
  sheet$rich[sheet$rich==1] <- 'rich'
  sheet$rich[sheet$rich==0] <- 'poor'
  
  #Europe
  europeCoBs <- data.frame(cob = c(
    "Irish_Repu",
    "Europe_oth",
    "France",
    "Germany",
    "Italy",
    "Netherland",
    "Spain"),
    europe = rep(1,7)
  )
  
  sheet <- merge(sheet,europeCoBs,by = 'cob',all.x=T)
  sheet$europe[is.na(sheet$europe)] <- 0
  
  #Last: housing density. So just number of houses per acre. Which we can get from the shapefiles loaded.
  #get areas from one of em
  areaz <- gArea(cobScot[[1]],byid = T)
  #For merging cos sheet isn't in same order now
  areaz <- data.frame(code = cobScot[[1]]$label, area = areaz)
  #Convert from m2 to acres
  areaz$acres <-  areaz$area * 0.000247105381
  
  #Number of houses from above
  areaz <- merge(areaz,eViewsFile[,c('code','hs91')],by='code')
  
  #Density! Houses per acre
  areaz$hsperacre91 <- areaz$hs91/areaz$acres
  
  #merge into sheet
  sheet <- merge(sheet,areaz[,c('code','hsperacre91')],by.x='label',by.y='code',all.x=T)
  
}





#AGAIN, TANGLED WITH ALREADY LOADED DATA, HACKED
compileSheetDiffZones5census <- function(zonesToKeep, cobz = c("Irish_Repu","Old_Common","Africa__Ne","India","Pakistan","Other_Euro","SE_Asia_Ne","Caribbean_","New_Common","Rest_of_Wo")
){
  
  #Label rich/poor
  richpoor5 <- data.frame(
    cob = names(cobScot5[[1]])[2:15],
    rich = c(1,1,1,1,1,1,0,0,0,1,0,0,0,0)
  )
  
  richpoor5$rich[richpoor5$rich==1] <- 'rich'
  richpoor5$rich[richpoor5$rich==0] <- 'poor'
  
  zonez <- unique(cobScot5[[1]]$label)
  #cobz <- names(cobScot5[[1]][2:ncol(cobScot5[[1]])])
  
  sheet5 <- CoBtoRegressionReadyData(cobScot5,zonesToKeep,cobz,
                                     c('Rest_of_UK','England','Wales','Scotland'),
                                     c(1971,1981,1991,2001),c(1971,1981,1991,2001,2011))
  
  #Look reasonable? Well, it sums correctly
  #hist(sheet5$xij2011[sheet5$cob=='Pakistan'],breaks=20)
  #sum(sheet5$xij2011[sheet5$cob=='Pakistan'])
  
  #old eviewsready file. What's it look like?
  eViewsFile5 <- read_csv('R_data/5Census_estimation_basic.csv')
  
  #Keep only single set of zone values, pop71 and ea71
  eViews71 <- eViewsFile5[!duplicated(eViewsFile5$code),] %>% 
    dplyr::select(code,ea1971,ea1981,ea1991,ea2001)
  
  #Work out people per acre using pop1971 from that sheet for years we want the data for
  #Or, err, work it out from scratch
  
  popDecades <- c(1971,1981,1991,2001)
  
  areaz <- data.frame(label = cobScot5[[1]]$label, acres = gArea(cobScot5[[1]], byid = T) * 0.000247105381)
  
  for(i in 1:4){
  
    totalPop <- apply(data.frame(cobScot5[[i]])[2:15],1,sum)
    areaz$x <- totalPop/areaz$acres
    
    names(areaz)[names(areaz)=='x'] <- paste0('popPerAcre',popDecades[i])
    
    #totalPop91 <- apply(data.frame(cobScot5[[3]])[2:15],1,sum)
    #areaz <- data.frame(label = cobScot5[[1]]$label, acres = gArea(cobScot5[[1]], byid = T) * 0.000247105381,totalPop91 = totalPop91)
    #areaz$popPerAcre91 <- totalPop91/areaz$acres
    
  }
  

  
  #Merge in the various bits
  sheet5a <- merge(sheet5,richpoor5,by = 'cob', all.x = T)
  
  sheet5a <- merge(sheet5a,eViews71,by.x = 'label',by.y = 'code', all.x=T)
  
  sheet5a <- merge(sheet5a, areaz[,c(1,3:6)], by = 'label', all.x=T)
  
  eViewsFile <- read_csv('R_data/estimation_aggEmploymentToLargerZonesDeltaEmpUrbanRuralTTWA.csv')
  
  #Steal urban 50% plus from 3 census. Oh and TTWA names!
  urban50 <- eViewsFile[,c('code',"urbanFiftyPercentPlus","name")]
  
  #One unique set
  urban50 <- urban50[!duplicated(urban50$code),]
  
  sheet5a <- merge(sheet5a, urban50, by.x = 'label', by.y = 'code', all.x=T)

}














