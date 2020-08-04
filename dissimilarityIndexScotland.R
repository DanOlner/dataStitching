#Dissimilarity index using Duncan Lee's code
#First job: need to get zone proportions of non-UK born
geolibs <- c("spdep","dplyr", "tidyr","assertthat","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster","readr","purrr","CARBayes")
lapply(geolibs, require, character.only = TRUE)

#Use 5 census for this
fz <- c(list.files("StitchOutputs/Scotland/LBS_postcodeSector_5Census_csvs/",pattern="CoB",recursive = F,full.names = T),list.files("StitchOutputs/Scotland/LBS_postcodeSector_5Census_csvs/",pattern="Birth",recursive = F,full.names = T))

#correct year order
fz <- fz[c(1,2,5,3,4)]

cob <- lapply(fz,read_csv)

View(cob[[3]])

#Can't remember, are there any column issues to fix?
#Yup! Actually, 91 names are more readable
lapply(cob,names)

#Drop spid col
cob[[3]] <- cob[[3]][,-1]

for(i in c(1,2,4,5)) names(cob[[i]]) <- names(cob[[3]])

#Right ee ho. Non-UK proportions per zone = sum both UK/non-UK then proportion per row
#lapply(cob, function(x) x$UKtotal = sum(x$England,x$Scotland,x$Wales,x$Rest_of_UK))

#https://stackoverflow.com/questions/27027347/mutate-each-summarise-each-in-dplyr-how-do-i-select-certain-columns-and-give
#Well that's pretty
cob <- cob %>% 
  map(mutate, UKtot = England + Scotland + Wales + Rest_of_UK,
      totalPop = England+Scotland+Wales+Rest_of_UK+Irish_Repu+Old_Common+Africa__Ne+India+Pakistan+Other_Euro+SE_Asia_Ne+Caribbean_+New_Common+Rest_of_Wo,
      nonUKprop = (totalPop-UKtot)/totalPop) 

#Can't get this to work despite it working on one of them
#map(mutate_at, .vars = vars(UKtot), .funs = funs(totalPop = sum(.)))
#Also, that's the wrong thing anyway! Move on...  

View(cob[[1]])
  
#Right! So we have proportions.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Duncan has each decades proportion in its own column. Let's do that.
props <- data.frame(label = cob[[1]]$label,
                    uktot71 = cob[[1]]$UKtot,
                    uktot81 = cob[[2]]$UKtot,
                    uktot91 = cob[[3]]$UKtot,
                    uktot01 = cob[[4]]$UKtot,
                    uktot11 = cob[[5]]$UKtot,
                    
                    nonuktot71 = cob[[1]]$totalPop - cob[[1]]$UKtot,
                    nonuktot81 = cob[[2]]$totalPop - cob[[2]]$UKtot,
                    nonuktot91 = cob[[3]]$totalPop - cob[[3]]$UKtot,
                    nonuktot01 = cob[[4]]$totalPop - cob[[4]]$UKtot,
                    nonuktot11 = cob[[5]]$totalPop - cob[[5]]$UKtot,
                    
                    totpop71 = cob[[1]]$totalPop,
                    totpop81 = cob[[2]]$totalPop,
                    totpop91 = cob[[3]]$totalPop,
                    totpop01 = cob[[4]]$totalPop,
                    totpop11 = cob[[5]]$totalPop,
                    
                    nonUKprop71 = cob[[1]]$nonUKprop,
                    nonUKprop81 = cob[[2]]$nonUKprop,
                    nonUKprop91 = cob[[3]]$nonUKprop,
                    nonUKprop01 = cob[[4]]$nonUKprop,
                    nonUKprop11 = cob[[5]]$nonUKprop
                    )

#Load shapefile for 91 zones to attach to 
pcs91shp <- readOGR('C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount','pseudoPCS_aggregated4CorrectCount')

pcs91shp <- pcs91shp[,1]

#Link
pcs91shp <- merge(pcs91shp,props,by = 'label')

#Going to need urban and city markers ... err, about now actually.
sheet <- read_csv('R_data/estimation_aggEmploymentToLargerZonesDeltaEmpUrbanRuralTTWA.csv')

#Only need the one zone
sheet <- sheet[!duplicated(sheet$code),c('code','urbanFiftyPercentPlus','name')]

pcs91shp <- merge(pcs91shp,sheet,by.x='label',by.y='code')

saveRDS(pcs91shp,'R_data/scots91pcs4dissimilarity.rds')

#~~~~~~~~~~~~~~~~~~~~~~~~~~
#Subset to cities then stick in carbayes----
#~~~~~~~~~~~~~~~~~~~~~~~~~~

#Test with Glasgow
glas <- pcs91shp[pcs91shp$name=='Glasgow',]
#Some of which have no neighbours. I wonder what Duncan's def of urban is?
glas <- pcs91shp[pcs91shp$name=='Glasgow' & pcs91shp$urbanFiftyPercentPlus == 1,]

#Get weights matrix
W.nb <- poly2nb(glas, row.names = rownames(glas@data))
W.nb <- poly2nb(glas, row.names = rownames(glas@data))

#drop no-link regions... oh, stops it being nb object. Err.
#Presume it's the same order...
glasNoIslands <- glas[!row.names(glas@data) %in% c('424','740','742'),]

W.nb <- poly2nb(glasNoIslands, row.names = rownames(glasNoIslands@data))
W.nb

W <- nb2mat(W.nb, style = "B")
W.list <- nb2listw(W.nb, style = "B")

#Better. Right then. There's still one isolated cluster. Will that be a problem?

#From Duncan's session 6:
#Aaaaah fine. It doesn't use proportions, it uses the raw numbers.
#Integer values required so... 
Y.mat <- cbind(glasNoIslands@data$nonuktot91, round(glasNoIslands@data$nonuktot11,0))
Y <- as.numeric(t(Y.mat))
N.mat <- cbind(glasNoIslands@data$totpop91, round(glasNoIslands@data$totpop11,0))
N <- as.numeric(t(N.mat))

model1 <- MVS.CARleroux(formula=Y~1, family="binomial", trials=N, W=W,
                        burnin=10000, n.sample=60000, thin=10, verbose=FALSE)

saveRDS(model1,'R_data/carbayesrun1.rds')

print(model1)
#spatial cor in rho. Temporal correlation:
Sigma.estimated <- apply(model1$samples$Sigma, c(2, 3), median)
Sigma.estimated[1,2] / sqrt(Sigma.estimated[1,1] * Sigma.estimated[2,2])

#Get some segregation indices ... 
source('SegregationIndicesDuncanLee.R')

#Start with 1000, up to full sample number (10K? Err)
#model1$samples$fitted %>% length
#went out of bounds at 5000. Err.
#Oh that's quick, use them all
K <- nrow(glasNoIslands)
D.dist <- array(NA, c(5000,2))
for(i in 1:5000)
{
  fitted.values <- matrix(model1$samples$fitted[i, ], nrow=K, byrow=TRUE)
  D.dist[i,1] <- D.compute(Y=fitted.values[ ,1], N=glasNoIslands@data$totpop91)
  D.dist[i,2] <- D.compute(Y=fitted.values[ ,2], N=glasNoIslands@data$totpop91)
  print(i)
}

#Too fast. Surely hasn't worked.
D.dist.df <- data.frame(D=c(D.dist[ ,1], D.dist[ ,2]),
                        year=factor(c(rep(1991, 5000), rep(2011, 5000))))

ggplot(D.dist.df, aes(x=year, y=D)) +
  geom_boxplot(fill="red") + xlab("Year") +
  ylab("Dissimilarity index") +
  ggtitle("Distribution of D for each year") +
  theme(text=element_text(face="bold", size=14))

#Credible interval
#1991
quantile(D.dist[ ,1], c(0.5, 0.025, 0.975))
#2011
quantile(D.dist[ ,2], c(0.5, 0.025, 0.975))
#Diff
quantile(D.dist[ ,2] - D.dist[ ,1], c(0.5, 0.025, 0.975))

ggplot(D.dist.df, aes(x=D)) +
  geom_density(fill="red") +
  ylab("Dissimilarity index") +
  ggtitle("density of D for each year") +
  theme(text=element_text(face="bold", size=14)) +
  facet_wrap(~year,nrow = 2)





