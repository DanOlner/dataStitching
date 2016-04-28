#Postcode sector zone / data checking.
#The base geography for the whole thing (in Scotland at any rate - wards down south)
#See http://www.nrscotland.gov.uk/files/geography/products/1991-census-bkgrd.pdf
geolibs <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat")
lapply(geolibs, require, character.only = TRUE)

pseudo <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_census_pseudoPostcodeSectors_1991", 
                         layer="scotland_oas_1991")

#processed actual postcode sectors to compare to
actual <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_postcodesectors_1991", 
                         layer="scotland_pcs_1991_uniqueIDsperRow_LochsRemoved")

#filter out Lochs straight away
pseudo_df <- data.frame(pseudo)
pseudo_df %>% filter(name == "Loch") %>% nrow

actual_df <- data.frame(actual)

#There's an empty NA row we need to drop first...
table(0 + is.na(pseudo@data$name))

#then drop Lochs
pseudo <- pseudo[!is.na(pseudo@data$name),]

#then drop Lochs
pseudo <- pseudo[pseudo@data$name != "Loch",]

#~~~~~~

#I think we might have correct non-postcode matching IDs now?
cob91 <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/91.csv")

#cob91 <- cob91[order(0 + rowMeans(cob91[,3:16])!=0),]
#Remove rows that contain only zeroes
#1003 to 822
cob91 <- cob91[rowMeans(cob91[,3:16])!=0,]

head(pseudo@data$label)
head(cob91$Zone.ID)

#Yes, they are looking similar.
#Less census than zone IDs. Huh.
zoneIDs <- unique(pseudo@data$label)
censusIDs <- unique(cob91$Zone.ID)

#All none-zero census IDs have a zone to attach to
table (censusIDs %in% zoneIDs)
#179 pseudo-postcode sectors with no data
table (zoneIDs %in% censusIDs)

#I would like to look at those plz!
noData <- pseudo[!(pseudo@data$label %in% censusIDs),]

#save and looksee
writeOGR(noData, "C:/Data/temp/MigrationIntersectTests/random",
         "pseudoPCS_with_NoData", driver="ESRI Shapefile", overwrite_layer = T)

#Oh dear, that's a lot. But we've now accounted for all '91 census CoB zones
#that actually *contain* data. Do they, by any chance, match any zero columns?
keepzeros <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/91.csv")

censusIDs2 <- unique(keepzeros$Zone.ID)

#Every zone has a matching census ID if we don't drop the zero rows. OK. 
#So some of the zero rows? Hmm. Bet they're not zero in the other datasets!
table (zoneIDs %in% censusIDs2)

#Two census IDs don't match. 
table (censusIDs2 %in% zoneIDs)

#I think that's one dud row and 'shipping'


#~~~~~~~~~~~~
#CUTTINZ-----

#table (as.character(censusIDs) %in% as.character(zoneIDs))
#FALSE  TRUE 
#2  1001

#What are those two in the data? 
#zoneIDs[!(censusIDs %in% zoneIDs)]
# censusIDs[!(censusIDs %in% zoneIDs)]
# cob91[cob91$Zone.ID %in% censusIDs[!(censusIDs %in% zoneIDs)],]
# 
# #Ah ha. One is shipping. (Zeroes)
# 
# #Are those postcodes in the 'pseudo' zone data?
# #Yup. 
# pseudo_df[pseudo_df$name == 'EH259',]
# pseudo_df[pseudo_df$name == 'G73 2',]
# cob91[cob91$Zone.ID == '6230AR',1:4]
# cob91[cob91$Zone.ID == '6341EX',1:4]
# pseudo_df[pseudo_df$label == '6230AR',]
# pseudo_df[pseudo_df$label == '6341EX',]
# 
# #As are the zone IDs. What exactly the ***k.
# zoneIDs[zoneIDs == '6230AR']
# zoneIDs[zoneIDs == '6341EX']
# censusIDs[censusIDs == '6230AR']
# censusIDs[censusIDs == '6341EX']









