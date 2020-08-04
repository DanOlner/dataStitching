o#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Test regressions on the estimation data----
#Created in eViewsReady.R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(reshape2)

eViewsFile <- read_csv('R_data/estimation_aggEmploymentToLargerZones.csv')

#~~~~~~~~~~~~~~~
#Correlations for different CoBs on the employment data----
#~~~~~~~~~~~~~~~

#Probably easier just to reload the CoBs so I can do a correlation matrix
cob <- read_csv('StitchOutputs/Scotland/LBS_3censusCombinedData/countryOfBirth.csv')

cob91 <- cob[cob$censusYear==1991,c(4:42)]

#add employment columns
co91plusEmp <- cbind(eViewsFile[1:822,c(11:13)],cob91)

#~~~~~~~~~~~
#Oh wait. I'm supposed to be correlating on 2011. Silly person

cob11 <- cob[cob$censusYear==2011,c(4:42)]

#add employment columns
cob11plusEmp <- cbind(eViewsFile[1:822,c(11:13)],cob11)

corz <- data.frame(cor(cob11plusEmp))

#table of just those we want
corz <- corz[4:nrow(corz),1:3]

corz$cob <- row.names(corz)

corz$cob <- factor(corz$cob)
#corz$cob <- reorder(corz$cob, corz$ea91)
corz$cob <- reorder(corz$cob, corz$ea_ttwa91)

corzlong <- gather(corz,type,correlationToShare,ea91:ea_nuts3_91)

ggplot(corzlong,aes(x = cob, y = correlationToShare,colour = type)) +
  geom_point(size = 4, alpha = .7) +
  theme(axis.text.x = element_text(angle = 270, hjust =0,vjust=0.2)) +
  coord_flip()

#REPEAT FOR DIFFERENT VARS
#BUT NOT THE CONTIGUITY ONES! They're different for each CoB
#var <- 'w91q'
#var <- 'w91nn8'
var <- 'popsh91'
#var <- 'pophs91'
cob11plusPopShare <- cbind(eViewsFile[1:822,c(var)],cob[cob$censusYear==2011,c(4:42)])

corzvar <- data.frame(cor(cob11plusPopShare))
corzvar <- corzvar[2:nrow(corz),1,drop = F]

corzvar$cob <- row.names(corzvar)

corzvar$cob <- factor(corzvar$cob)
#corz$cob <- reorder(corz$cob, corz$ea91)
corzvar$cob <- reorder(corzvar$cob, corzvar[,var])

#corzlong <- gather(corzvar,type,correlationToShare,ea91:ea_nuts3_91)

ggplot(corzvar,aes_string(x = 'cob', y = var)) +
  geom_point(size = 4, alpha = .7) +
  theme(axis.text.x = element_text(angle = 270, hjust =0,vjust=0.2)) +
  coord_flip()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Pooled OLS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Just testing pooled OLS based on Geoff's different groups
#Reload the rich/poor one from above.
#Can work Europe out

#ALL----
#Try all first
#Geoff's original
allz <- lm(data = eViewsFile, xij2011 ~ xij1991 + lph91 + lpophs91 + migsh91)
summary(allz)

capture.output(c(noquote("ALL")), file = "regressionOutputs/3census/allz.txt")
capture.output(summary(allz), file = "regressionOutputs/3census/allz.txt", append = T)

#EUROPE (INC IRELAND)----
europeCoBs <- c(
  "Irish_Repu",
  "Europe_oth",
  "France",
  "Germany",
  "Italy",
  "Netherland",
  "Spain"
)

europe <- eViewsFile[eViewsFile$CoB %in% europeCoBs,]

eurRegression <- lm(data = europe, xij2011 ~ xij1991 + lph91 + lpophs91 + migsh91)
summary(eurRegression)

capture.output(c(noquote("EUROPE INC. IRELAND")), file = "regressionOutputs/3census/allz.txt", append = T)
capture.output(summary(eurRegression), file = "regressionOutputs/3census/allz.txt", append = T)

#ACCCESSION VS NOT EU----

accession <- c(
    
)

other <- c(
  
)

#RICH v POOR----
#(May be different from eViewsReady.R)
subz <- c(
  'Irish_Repu',
  'China',
  'South_Afri',
  'Africa_oth',
  'South_Amer',
  'Other_Midd',
  'Other_East',
  'Europe_oth',
  'Australia',
  'Canada',
  'Nigeria',
  'India',
  'Pakistan',
  'Hong_Kong',
  'Malaysia',
  'France',
  'Germany',
  'Italy',
  'Netherland',
  'Spain',
  'Poland',
  'United_Sta'
)

#rich/poor split
richpoor <- data.frame(subz,rich = c(1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,1,1,1))
#richpoor <- data.frame(subz,rich = c(1,0,0,0,0,0,0,0,1,1,0,0,0,1,0,1,1,1,1,1,0,1))

rich <- eViewsFile[eViewsFile$CoB %in% richpoor$subz[richpoor$rich==1],]
poor <- eViewsFile[eViewsFile$CoB %in% richpoor$subz[richpoor$rich==0],]

richregression <- lm(data = rich, xij2011 ~ xij1991 + lph91 + lpophs91 + migsh91)
summary(richregression)

capture.output(c(noquote("RICH")), file = "regressionOutputs/3census/allz.txt", append = T)
capture.output(summary(richregression), file = "regressionOutputs/3census/allz.txt", append = T)

#poor
poorregression <- lm(data = poor, xij2011 ~ xij1991 + lph91 + lpophs91 + migsh91)
summary(poorregression)

capture.output(c(noquote("POOR")), file = "regressionOutputs/3census/allz.txt", append = T)
capture.output(summary(poorregression), file = "regressionOutputs/3census/allz.txt", append = T)



#ALL ADDITIONS----
allz2 <- lm(data = eViewsFile, xij2011 ~ xij1991 + lph91 + lpophs91 + migsh91minusown + w91q)
summary(allz2)

capture.output(noquote("ALL PLUS MIGSHARE-MINUS-OWN AND QUEEN CONTIG"), file = "regressionOutputs/3census/allz.txt", append = T)
capture.output(summary(allz2), file = "regressionOutputs/3census/allz.txt", append = T)


#EUROPE ADDITIONS----
eurRegression <- lm(data = europe, xij2011 ~ xij1991 + lph91 + lpophs91 + migsh91minusown + w91q)

capture.output(noquote("EUROPE PLUS MIGSHARE-MINUS-OWN AND QUEEN CONTIG"), file = "regressionOutputs/3census/allz.txt", append = T)
capture.output(summary(eurRegression), file = "regressionOutputs/3census/allz.txt", append = T)


#RICHPOOR ADDITIONS---
richregressionAll <- lm(data = rich, xij2011 ~ xij1991 + lph91 + lpophs91 + migsh91minusown + w91q)
#summary(richregressionAll)

capture.output(noquote("RICH PLUS MIGSHARE-MINUS-OWN AND QUEEN CONTIG"), file = "regressionOutputs/3census/allz.txt", append = T)
capture.output(summary(richregressionAll), file = "regressionOutputs/3census/allz.txt", append = T)

#
poorregressionAll <- lm(data = poor, xij2011 ~ xij1991 + lph91 + lpophs91 + migsh91minusown + w91q)
#summary(poorregressionAll)

capture.output(noquote("POOR PLUS MIGSHARE-MINUS-OWN AND QUEEN CONTIG"), file = "regressionOutputs/3census/allz.txt", append = T)
capture.output(summary(poorregressionAll), file = "regressionOutputs/3census/allz.txt", append = T)


#ALL ADDITIONS INC. TTWA-LEVEL ECON ACTIVE----
allz2 <- lm(data = eViewsFile, xij2011 ~ xij1991 + lph91 + lpophs91 + migsh91minusown + w91q + ea_ttwa91)
summary(allz2)

capture.output(noquote("ALL PLUS MIGSHARE-MINUS-OWN, CONTIG & TTWA-LEVEL ECON ACTIVE"), file = "regressionOutputs/3census/allz.txt", append = T)
capture.output(summary(allz2), file = "regressionOutputs/3census/allz.txt", append = T)


eurRegression <- lm(data = europe, xij2011 ~ xij1991 + lph91 + lpophs91 + migsh91minusown + w91q + ea_ttwa91)

capture.output(noquote("EUROPE PLUS PLUS MIGSHARE-MINUS-OWN, CONTIG & TTWA-LEVEL ECON ACTIVE"), file = "regressionOutputs/3census/allz.txt", append = T)
capture.output(summary(eurRegression), file = "regressionOutputs/3census/allz.txt", append = T)


richregressionAll <- lm(data = rich, xij2011 ~ xij1991 + lph91 + lpophs91 + migsh91minusown + w91q + ea_ttwa91)

capture.output(noquote("RICH PLUS PLUS MIGSHARE-MINUS-OWN, CONTIG & TTWA-LEVEL ECON ACTIVE"), file = "regressionOutputs/3census/allz.txt", append = T)
capture.output(summary(richregressionAll), file = "regressionOutputs/3census/allz.txt", append = T)

poorregressionAll <- lm(data = poor, xij2011 ~ xij1991 + lph91 + lpophs91 + migsh91minusown + w91q + ea_ttwa91)

capture.output(noquote("POOR PLUS PLUS MIGSHARE-MINUS-OWN, CONTIG & TTWA-LEVEL ECON ACTIVE"), file = "regressionOutputs/3census/allz.txt", append = T)
capture.output(summary(poorregressionAll), file = "regressionOutputs/3census/allz.txt", append = T)


#Slight improvement, probably not valid
# eViewsFileLog <- eViewsFile
# eViewsFileLog[eViewsFileLog == 0] <- 0.000001
# corzlog <- data.frame(cor(eViewsFileLog[,c(2:15)] %>% log))

#Silly person, this won't work. Would need to compare ranks for different groups
#corz2 <- data.frame(cor(eViewsFile[,c(2:15)],method = 'spearman'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#LOOKING AT R-SQUARED VALUES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Added in delta emp 91/01
eViewsFile <- read_csv('R_data/estimation_aggEmploymentToLargerZonesDeltaEmp.csv')

#Added in delta employment 91/01
corz <- data.frame(cor(eViewsFile[,c(2:17,19)]))
corzrich <- data.frame(cor(rich[,c(2:17,19)]))
corzpoor <- data.frame(cor(poor[,c(2:17,19)]))

rz <- data.frame(var = row.names(corz),all = corz[,1],rich=corzrich[,1],poor=corzpoor[,1])

#don't need first row
rz <- rz[2:nrow(rz),]

#long then order then plot
rzlong <- gather(rz,subset,val,all:poor)

rzlong$var <- factor(rzlong$var)
rzlong$var <- reorder(rzlong$var,rzlong$val)

ggplot(rzlong,aes(x = var,y =val,colour=subset)) +
  geom_point(size = 3, alpha =0.7) +
  theme(axis.text.x = element_text(angle = 270, hjust =0,vjust=0.2)) +
  coord_flip()
  

#SOME PLOTS----

#plot(eViewsFile$xij2011,eViewsFile$ea_ttwa91)

plot(rich$xij2011,rich$ea_ttwa91)
plot(poor$xij2011,poor$ea_ttwa91)

plot(rich$xij2011,rich$deltaEmp9101)
plot(poor$xij2011,poor$deltaEmp9101)

# plot(log(rich$xij2011),rich$ea_ttwa91)
# plot(log(poor$xij2011),poor$ea_ttwa91)

#~~~~~~~~~~~~~~~~~~~~~~~
#URBAN/RURAL----

eViewsFile <- read_csv('R_data/estimation_aggEmploymentToLargerZonesDeltaEmpUrbanRural.csv')

urb <- lm(data = eViewsFile[eViewsFile$urbanFiftyPercentPlus ==1,], xij2011 ~ xij1991 + w91q)
summary(urb)

rur <- lm(data = eViewsFile[eViewsFile$urbanFiftyPercentPlus ==0,], xij2011 ~ xij1991 + w91q)
summary(rur)

#Using dummy: "zone is 50% or more urban" = 1. Reference then = mostly or completely rural.
allz <- lm(data = eViewsFile, xij2011 ~ xij1991 + factor(urbanFiftyPercentPlus))
summary(allz)

richz <- lm(data = rich, xij2011 ~ xij1991 + factor(urbanFiftyPercentPlus))
summary(richz)

poorz <- lm(data = poor, xij2011 ~ xij1991 + factor(urbanFiftyPercentPlus))
summary(poorz)

plot(poor$xij2011~poor$xij1991)
plot(log(poor$xij2011)~log(poor$xij1991),ylim=c(-10,2))
plot(logrich$xij2011~rich$xij1991)
plot(log(rich$xij2011)~log(rich$xij1991),ylim=c(-10,2))

#Which is just showing that more people live in urban areas
#Given we're talking about shares as the underlying data
ggplot(poor, aes(x = xij1991, y = xij2011, colour = factor(urbanFiftyPercentPlus))) +
  geom_point(alpha = 0.5, size = 3) +
  scale_y_log10() +
  scale_x_log10()

ggplot(rich, aes(x = xij1991, y = xij2011, colour = factor(urbanFiftyPercentPlus))) +
  geom_point(alpha = 0.5, size = 3) +
  scale_y_log10() +
  scale_x_log10()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Nature of the CoB changes----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#So: using shares - problems? What's happening when we're regressing?
#Isn't there an issue with the zone pops that needs addressing?
#Or does the difference in time periods deal with that?

#pick one example to think through.
cobz <- eViewsFile[eViewsFile$CoB=='Poland',]
cobz <- eViewsFile[eViewsFile$CoB=='Pakistan',]

cobz$logxij1991 <- ifelse(cobz$xij1991==0,0,log(cobz$xij1991))
cobz$logxij2011 <- ifelse(cobz$xij2011==0,0,log(cobz$xij2011))

plot(cobz$xij2011~cobz$xij1991)
abline(lm(cobz$xij2011~cobz$xij1991))

plot(cobz$logxij2011~cobz$logxij1991)
abline(lm(cobz$logxij2011~cobz$logxij1991))

ggplot(cobz,aes(y=xij2011,x=xij1991)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

ggplot(cobz,aes(y=xij2011,x=popsh91,colour=CoB)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

more7 <- cobz[cobz$xij1991 > 5,]

cor(cobz$xij2011,cobz$popsh91,method = 'pearson')
cor(cobz$xij2011,cobz$popsh91,method = 'spearman')

cor(eViewsFile$xij2011,eViewsFile$popsh91,method = 'pearson')
cor(eViewsFile$xij2011,eViewsFile$popsh91,method = 'spearman')


#cumulative frequencies of overall pop share vs cobzistani
cumulcobz <- cobz$xij2011[order(cobz$xij2011)] %>% cumsum()
plot(cumulcobz)

#Same main pop for each CoB so can use this
cumulpop <- cobz$popsh91[order(cobz$popsh91)] %>% cumsum()
points(cumulpop,col='red')

#Use this as cumulpop for all comparisons (just cobz cos I just used it - same for all CoBs)
#Note: different size zones so this is a bit suspect
#But for comparison, it's OK.
cumulpop <- cobz$popsh91[order(cobz$popsh91)] %>% cumsum()

for(cob in unique(eViewsFile$CoB)){
#for(cob in 'cobzistan'){
  
  cumulcob <- eViewsFile[eViewsFile$CoB==cob,]
  
  #Order CoB by popShare so comparing same zones...
  cumulcob <- cumulcob$xij1991[order(cumulcob$popsh91)] %>% cumsum()
  #cumulcob <- cumulcob$xij2011[order(cumulcob$xij2011)] %>% cumsum()
  
  df <- data.frame(pop = cumulpop, cob = cumulcob) %>% gather(countryOrPop,percent)
  df$index <- c(1:822)#repeats
  
  df$countryOrPop[df$countryOrPop=='cob'] <- cob
  
  plotz <- ggplot(df,aes(y = percent, x = index, colour = countryOrPop)) +
    geom_point() +
    ggtitle(cob) + 
    theme(plot.title = element_text(lineheight=.8, face="bold",hjust=0.5))
  
  #plotz
  ggsave(paste0('R_outputs/shareComparisons/',cob,'.png'), plotz, dpi = 150, width = 7, height = 5)
  
}

#Pick on one example again

cobz <- eViewsFile[eViewsFile$CoB=='cobzistan',]

cobz$logxij1991 <- ifelse(cobz$xij1991==0,0,log(cobz$xij1991))
cobz$logxij2011 <- ifelse(cobz$xij2011==0,0,log(cobz$xij2011))

plot(cobz$xij2011~cobz$xij1991)
abline(lm(cobz$xij2011~cobz$xij1991), col='GREEN')

plot(cobz$logxij2011~cobz$logxij1991)
abline(lm(cobz$logxij2011~cobz$logxij1991), col='GREEN')

#So from log plotting:
#1991 zeroes - many got eensy slivers of count in 2011
#So redo - keeping only those above zero counts in 1991
#(Which is dubious regression wise but good for looking
#and might not be dubious anyway.)
cobzlog <- eViewsFile[eViewsFile$CoB=='cobzistan' & eViewsFile$xij1991!=0,]

cobzlog$logxij1991 <- ifelse(cobzlog$xij1991==0,0,log(cobzlog$xij1991))
cobzlog$logxij2011 <- ifelse(cobzlog$xij2011==0,0,log(cobzlog$xij2011))

plot(cobzlog$logxij2011~cobzlog$logxij1991, ylim = c(-10,2))
abline(lm(cobzlog$logxij2011~cobzlog$logxij1991), col='GREEN')

#What's the change in cobzistan, based on where the most were in 1991?
cobzchange <- cobz[,c(2:3)]
cobzchange <- cobzchange[order(-cobzchange$xij1991),]

#Compare cumulative shares between 1991 and 2011

for(cob in unique(eViewsFile$CoB)){
# for(cob in 'cobzistan'){

  cobzchange <- eViewsFile[eViewsFile$CoB==cob,c(2:3)]
  
  #What's that cumulatively between each decade...?
  cobzchangeCumul <- data.frame(
    twoEleven = cobzchange$xij2011[order(cobzchange$xij2011)] %>% cumsum(),
    ninetyOne = cobzchange$xij1991[order(cobzchange$xij1991)] %>% cumsum()
  ) %>% gather(year,cumulsum)
  
  cobzchangeCumul$index <- 1:822
  
  plotz <- ggplot(cobzchangeCumul,aes(x = index, y = cumulsum, colour = year)) +
    geom_point() +
    ggtitle(cob) + 
    theme(plot.title = element_text(lineheight=.8, face="bold",hjust=0.5))
  
  ggsave(paste0('R_outputs/shareComparisons91to11/',cob,'.png'), plotz, dpi = 150, width = 7, height = 5)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5-CENSUS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#We've got various decades to look at here
eViewsFile5 <- read_csv('R_data/5Census_estimation_basic.csv')

#Reduce to some different countries. Exclude UK, compare Europe / other.
europe <- eViewsFile5[eViewsFile5$cob %in% c('OtherEurope','IrishRepublic'),]

rich <- eViewsFile5[eViewsFile5$cob %in% 
                     c('OtherEurope',
                       'IrishRepublic',
                       'OldCommonwealth'),]

poor <- eViewsFile5[eViewsFile5$cob %in% 
                     c("Africa(New-C)",
                       "India",
                       "Pakistan",
                       "SEAsia(New-C)",
                       "Caribbean(New-C)",
                       "NewCommonwealthother",
                       "RestofWorld")
                       ,]

capture.output(c(noquote("EUROPE INC. IRELAND")), file = "regressionOutputs/5census/allz.txt")

eurRegression <- lm(data = europe, xij2011 ~ xij1971 + ea1971 + w71nn8)
summary(eurRegression)
capture.output(summary(eurRegression), file = "regressionOutputs/5census/allz.txt", append = T)

eurRegression <- lm(data = europe, xij2011 ~ xij1981 + ea1981 + w81nn8)
summary(eurRegression)
capture.output(summary(eurRegression), file = "regressionOutputs/5census/allz.txt", append = T)

eurRegression <- lm(data = europe, xij2011 ~ xij1991 + ea1991 + w91nn8)
summary(eurRegression)
capture.output(summary(eurRegression), file = "regressionOutputs/5census/allz.txt", append = T)

eurRegression <- lm(data = europe, xij2011 ~ xij2001 + ea2001 + w01nn8)
summary(eurRegression)
capture.output(summary(eurRegression), file = "regressionOutputs/5census/allz.txt", append = T)


capture.output(c(noquote("RICH")), file = "regressionOutputs/5census/allz.txt", append = T)

richReg <- lm(data = rich, xij2011 ~ xij1971 + ea1971 + w71nn8)
summary(richReg)
capture.output(summary(richReg), file = "regressionOutputs/5census/allz.txt", append = T)

richReg <- lm(data = rich, xij2011 ~ xij1981 + ea1981 + w81nn8)
summary(richReg)
capture.output(summary(richReg), file = "regressionOutputs/5census/allz.txt", append = T)

richReg <- lm(data = rich, xij2011 ~ xij1991 + ea1991 + w91nn8)
summary(richReg)
capture.output(summary(richReg), file = "regressionOutputs/5census/allz.txt", append = T)

richReg <- lm(data = rich, xij2011 ~ xij2001 + ea2001 + w01nn8)
summary(richReg)
capture.output(summary(richReg), file = "regressionOutputs/5census/allz.txt", append = T)
capture.output(summary(richReg), file = "regressionOutputs/5census/allz.txt", append = T)

#pairs(rich[,c('xij2011','xij2001','xij1991','xij1981','xij1971')])
capture.output(c(noquote("POOR")), file = "regressionOutputs/5census/allz.txt", append = T)

poorReg <- lm(data = poor, xij2011 ~ xij1971 + ea1971 + w71nn8)
summary(poorReg)
capture.output(summary(poorReg), file = "regressionOutputs/5census/allz.txt", append = T)

poorReg <- lm(data = poor, xij2011 ~ xij1981 + ea1981 + w81nn8)
summary(poorReg)
capture.output(summary(poorReg), file = "regressionOutputs/5census/allz.txt", append = T)

poorReg <- lm(data = poor, xij2011 ~ xij1991 + ea1991 + w91nn8)
summary(poorReg)
capture.output(summary(poorReg), file = "regressionOutputs/5census/allz.txt", append = T)

poorReg <- lm(data = poor, xij2011 ~ xij2001 + ea2001 + w01nn8)
summary(poorReg)
capture.output(summary(poorReg), file = "regressionOutputs/5census/allz.txt", append = T)


#facet the four
four <- gather(rich %>% dplyr::select(xij1971:xij2001),
               decade,share)

#stick repeating 2011 next to it
four$xij2011 <- rich$xij2011

#Yup
table(four$xij2011)

output <- ggplot(four,aes(y = xij2011, x = share)) +
  geom_point(alpha = 0.75) +
  facet_wrap(~decade,ncol=1)

ggsave('R_outputs/facet_5censusRich.png', output,dpi=150,width = 3,height = 10)

#facet the four
four <- gather(poor %>% dplyr::select(xij1971:xij2001),
               decade,share)

#stick repeating 2011 next to it
four$xij2011 <- poor$xij2011

#Yup
table(four$xij2011)

output <- ggplot(four,aes(y = xij2011, x = share)) +
  geom_point(alpha = 0.75) +
  facet_wrap(~decade,ncol=1)

ggsave('R_outputs/facet_5censusPoor.png', output,dpi=150,width = 3,height = 10)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3-CENSUS for range of Scots TTWAs, urban only----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

eViewsFile <- read_csv('R_data/estimation_aggEmploymentToLargerZonesDeltaEmpUrbanRuralTTWA.csv')

#Add in rich flag (having run code above to get the rich list)
eViewsFile$rich <- 0 + eViewsFile$CoB %in% richpoor$subz[richpoor$rich==1]

#Some var checks
chkPopsh <- subset(eViewsFile, !duplicated(eViewsFile$code))

#Tick
sum(chkPopsh$popsh91)

#look in map
write_csv(chkPopsh[,c('code','popsh91')],'R_data/checkPopShares.csv')

#Check if CoB shares across TTWAs generally correlate to pop shares there
#(Might be better than small scale zone shares)

#sums per CoB per TTwa
cobSharesTTWA <- eViewsFile %>% 
  dplyr::select(name, CoB, xij1991) %>% 
  group_by(CoB,name) %>% 
  summarise(ttwaShareSumCOB = sum(xij1991))

#Same for overall pop
#(pop is repeated, just need one value per PCS zone)
popSharesTTWA <- subset(eViewsFile, !duplicated(eViewsFile$code)) %>% 
  dplyr::select(name,popsh91) %>% 
  group_by(name) %>% 
  summarise(ttwaShareSumPOP = sum(popsh91))

#should be 100
sum(popSharesTTWA$ttwaShareSumPOP)
  
checkShares <- merge(cobSharesTTWA, popSharesTTWA, by = 'name', all.x = T)

cobnamez = unique(checkShares$CoB)

for(cobname in cobnamez){
  jpeg(paste0('R_outputs/urbanPlots/CobVsPopShareAcrossTTWAs/',cobname,'.jpg'))
  plot(checkShares$ttwaShareSumCOB[checkShares$CoB == cobname]~
         checkShares$ttwaShareSumPOP[checkShares$CoB == cobname])
  dev.off()
}

lapply(split(checkShares, checkShares$CoB), function(x) summary(lm(data = x, shareSum.x ~ shareSum.y)))

#Just saving to check the TTWA match actually worked... yup.
#chk <- eViewsFile %>% dplyr::select(code,name,urbanFiftyPercentPlus) %>% distinct()
#write_csv(chk,'R_data/pcs_ttwaOverlap.csv')

# rur <- lm(data = eViewsFile[eViewsFile$urbanFiftyPercentPlus ==0,], xij2011 ~ xij1991 + w91q)
# summary(rur)

#Just looking at urban
#urb <- lm(data = eViewsFile[eViewsFile$urbanFiftyPercentPlus ==1,], xij2011 ~ xij1991 + w91q)
#summary(urb)

urb <- eViewsFile[eViewsFile$urbanFiftyPercentPlus ==1,]

#What are the coeffs for each CoB
coeffs <- lapply(split(urb, urb$CoB), function(x) summary(lm(data = x, xij2011 ~ xij1991 + w91q))$coefficients[2])

#Look at each CoB for urban as a whole
cobnamez = unique(urb$CoB)

#reorder so same order as coefficients
cobnamez <- cobnamez[order(cobnamez)]

#Add in colour indicating polarity of change
#(Which should be obvious from what side of identity line they are, but let's see.)
urb$increased <- 0 + ((urb$xij2011 - urb$xij1991) > 0)

#Plot using coeffs in names, including slope 1 line for comparison
for(cobname in cobnamez){
  jpeg(paste0('R_outputs/urbanPlots/urbanCoB/',cobname,'_',coeffs[[cobname]] %>% round(digits = 4),'.jpg'))
  
  plot(urb$xij2011[urb$CoB == cobname] ~ urb$xij1991[urb$CoB == cobname],
       col = ifelse(urb$increased[urb$CoB == cobname]==1,'blue','black'),
       main = paste0('beta=',coeffs[[cobname]] %>% round(digits = 4)))
  
  abline(lm(urb$xij2011[urb$CoB == cobname] ~ urb$xij1991[urb$CoB == cobname]),col='green')
  abline(a = 0, b = 1,col='red', lty = 2, lwd = 3)
  dev.off()
}

#Versus weights matrix
coeffs <- lapply(split(urb, urb$CoB), function(x) summary(lm(data = x, xij2011 ~ xij1991 + w91q))$coefficients[3])

for(cobname in cobnamez){
  jpeg(paste0('R_outputs/urbanPlots/urbanCoBvsW/',cobname,'_',coeffs[[cobname]] %>% round(digits = 4),'.jpg'))
  plot(urb$xij2011[urb$CoB == cobname] ~ urb$w91q[urb$CoB == cobname])
  abline(lm(urb$xij2011[urb$CoB == cobname] ~ urb$w91q[urb$CoB == cobname]),col='green')
  abline(a = 0, b = 1,col='red', lty = 2, lwd = 3)
  dev.off()
}

#Break down by TTWA - pick top ones (by some criteria?)
#Well, maybe just names:
#Glasgow, Edinburgh, Dundee, Aberdeen
#The latter will probably not have many urban zones - wonder how many the rest have?


#top four cities
urb4 <- urb[urb$name %in% c('Glasgow','Edinburgh','Dundee','Aberdeen'),]

#Number of zones per city?
table(urb4 %>% dplyr::select(code,name) %>% 
        distinct() %>% 
        group_by(name) %>% 
        summarise(countZones = n())
)
      
#Glasgow: 127; Edinburgh: 64; Dundee: 19; Aberdeen: 15 
lapply(split(urb4, urb4$name), function(x) summary(lm(data = x, xij2011 ~ xij1991 + w91q)))

lapply(split(urb4, urb4$name), function(x) summary(lm(data = x, xij2011 ~ xij1991)))
lapply(split(urb4, urb4$name), function(x) summary(lm(data = x, xij1991 ~ xij2011)))


#Break down by rich/poor
capture.output(noquote("RICH"),file = 'regressionOutputs/urbanRichPoorCities.txt')

capture.output(
lapply(split(urb4[urb4$rich==1,], 
             urb4$name[urb4$rich==1]), function(x) summary(lm(data = x, xij2011 ~ xij1991 + w91q))),
file = 'regressionOutputs/urbanRichPoorCities.txt',
append = T
)

capture.output(noquote("POOR"),file = 'regressionOutputs/urbanRichPoorCities.txt',append = T)

capture.output(
lapply(split(urb4[urb4$rich==0,], 
             urb4$name[urb4$rich==0]), function(x) summary(lm(data = x, xij2011 ~ xij1991 + w91q))),
file = 'regressionOutputs/urbanRichPoorCities.txt',
append = T
)

#~~~~~~~~~~~~~~~~~~~~~

#plots for those
#Assuming cobnamez is still in alphabetical order
#lapply(split(urb4, urb4$name), function(x) summary(lm(data = x, xij2011 ~ xij1991 + w91q)))

rich <- urb4[urb4$rich==1,]

coeffs <- lapply(split(rich, rich$name), function(x) summary(lm(data = x, xij2011 ~ xij1991 + w91q))$coefficients[2])
intz <- lapply(split(rich, rich$name), function(x) summary(lm(data = x, xij2011 ~ xij1991 + w91q))$coefficients[1])

for(name in unique(rich$name)[order(unique(rich$name))]){
  
  jpeg(paste0('R_outputs/urbanPlots/urbanCoB_citiesRichPoor/rich_',name,'_',coeffs[[name]] %>% round(digits = 4),'.jpg'))
  plot(rich$xij2011[rich$name == name] ~ rich$xij1991[rich$name == name])
  #abline(lm(rich$xij2011[rich$name == name] ~ rich$xij1991[rich$name == name]),col='green')
  abline(a = intz[[name]], b = coeffs[[name]],col='green')
  abline(a = 0, b = 1,col='red', lty = 2, lwd = 3)
  dev.off()
  
}

poor <- urb4[urb4$rich==0,]

coeffs <- lapply(split(poor, poor$name), function(x) summary(lm(data = x, xij2011 ~ xij1991 + w91q))$coefficients[2])
intz <- lapply(split(poor, poor$name), function(x) summary(lm(data = x, xij2011 ~ xij1991 + w91q))$coefficients[1])

for(name in unique(poor$name)[order(unique(poor$name))]){
  
  jpeg(paste0('R_outputs/urbanPlots/urbanCoB_citiesRichPoor/poor_',name,'_',coeffs[[name]] %>% round(digits = 4),'.jpg'))
  plot(poor$xij2011[poor$name == name] ~ poor$xij1991[poor$name == name])
  #abline(lm(poor$xij2011[poor$name == name] ~ poor$xij1991[poor$name == name]),col='green')
  abline(a = intz[[name]], b = coeffs[[name]],col='green')
  abline(a = 0, b = 1,col='red', lty = 2, lwd = 3)
  dev.off()
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Let's take a look at some of those
city <- urb4[urb4$name == 'Glasgow',]
#city <- urb4[urb4$name == 'Dundee',]

plot(city$xij2011 ~ city$xij1991)
abline(lm(city$xij2011 ~ city$xij1991))

plot(city$xij1991 ~ city$xij2011)
abline(lm(city$xij1991 ~ city$xij2011))

plot(log(city$xij2011) ~ log(city$xij1991), ylim = c(-5,1))

#is this just a population thing?
lapply(split(urb4, urb4$name), function(x) summary(lm(data = x, xij2011 ~ xij1991 + popsh91)))

#Err.
plot(city$xij2011 ~ city$popsh91)
abline(lm(city$xij2011 ~ city$popsh91))

#Oh: that makes no sense to regress, surely: it's a repeating value. Would need to look at each separate CoB?
lapply(split(city, city$CoB), function(x) summary(lm(data = x, xij2011 ~ xij1991 + popsh91)))

#Oh wait - that's why it's logged. Ah. Or it's not logged but anything it's used with is.
plot(city$xij2011 ~ log(city$popsh91))
abline(lm(city$xij2011 ~ log(city$popsh91)))

#Does it look as odd with house prices?
plot(city$xij2011 ~ city$hs91)
abline(lm(city$xij2011 ~ city$hs91))

plot(city$xij2011 ~ log(city$hs91))
abline(lm(city$xij2011 ~ log(city$hs91)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Checking on dynamics of shares. One country, urban.
AmericaUrb <- urb[urb$CoB == 'United_Sta',]

mean(AmericaUrb$xij2011- AmericaUrb$xij1991)

#Oh right: DON'T TEST ON URBAN! The shares are not 100%
lapply(split(urb, urb$CoB), function (x) mean(x$xij2011-x$xij1991))
lapply(split(urb, urb$CoB), function (x) sum(x$xij2011))

#Better: as close to zero as makes no odds
lapply(split(eViewsFile, eViewsFile$CoB), function (x) all.equal (mean(x$xij2011-x$xij1991),0))
#And yes, these ones sum to 100
lapply(split(eViewsFile, eViewsFile$CoB), function (x) sum(x$xij2011))

America <- eViewsFile[eViewsFile$CoB == 'United_Sta',]

#So I'm looking at the America graph... 
#Ooooh. Of course, that's urban only. I need to repeat for all to see how it compares.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#What are the coeffs for each CoB
# coeffs <- lapply(split(eViewsFile, eViewsFile$CoB), function(x) summary(lm(data = x, xij2011 ~ xij1991 + w91q))$coefficients[2])
coeffs <- lapply(split(eViewsFile, eViewsFile$CoB), function(x) summary(lm(data = x, xij2011 ~ xij1991))$coefficients[2])

#Look at each CoB for eViewsFilean as a whole
cobnamez = unique(eViewsFile$CoB)

#reorder so same order as coefficients
cobnamez <- cobnamez[order(cobnamez)]

#Add in colour indicating polarity of change
#(Which should be obvious from what side of identity line they are, but let's see.)
eViewsFile$increased <- 0 + ((eViewsFile$xij2011 - eViewsFile$xij1991) > 0)

#Plot using coeffs in names, including slope 1 line for comparison
for(cobname in cobnamez){
  jpeg(paste0('R_outputs/urbanPlots/CoBall/',cobname,'_',coeffs[[cobname]] %>% round(digits = 4),'.jpg'))
  
  plot(eViewsFile$xij2011[eViewsFile$CoB == cobname] ~ eViewsFile$xij1991[eViewsFile$CoB == cobname],
       col = ifelse(eViewsFile$increased[eViewsFile$CoB == cobname]==1,'blue','black'),
       main = paste0('beta=',coeffs[[cobname]] %>% round(digits = 4)))
  
  abline(lm(eViewsFile$xij2011[eViewsFile$CoB == cobname] ~ eViewsFile$xij1991[eViewsFile$CoB == cobname]),col='green')
  abline(a = 0, b = 1,col='red', lty = 2, lwd = 3)
  dev.off()
}

#So are we looking at US military bases previously in Scotland where numbers reduced?
#Would explain why they're out in the sticks. Let's look at an example.
#Actually, in the original data would be better - higher res.
#Oh no, wait: we don't *have* higher-res for 91.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SYNTHETIC TESTING ON THE DATA----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Because I want to know what I should really expect.
#So using actual data, look at some examples of what might follow in 2011 based on 1991 patterns
#Just for zone-to-zone correlations.
#And then what comparisons might mean.
eViewsFile2 <- read_csv('R_data/estimation_aggEmploymentToLargerZonesDeltaEmpUrbanRuralTTWA.csv')

#So let's pick on one city and one CoB
# test <- eViewsFile2 %>% 
#   filter(CoB=='Pakistan',name=='Glasgow',urbanFiftyPercentPlus == 1)

#Little update: 2011 values via intersections: non-integers are being troublesome for logging
#Especially those less than one.
#I could use each remainder as a probably of assigning...
#Ah no, that would need doing earlier - it's shares now.

#Actually, no, don't subset by city: I need the shares to sum to 100 to be able to think sensibly about this.
test <- eViewsFile2 %>% 
  filter(CoB=='Pakistan')


#OK, the actual 91 and 11:
plot(test$xij2011~test$xij1991)
abline(lm(test$xij2011~test$xij1991))
#identity line
abline(a = 0, b = 1, col = "green")

plot(log(test$xij2011)~log(test$xij1991), ylim=c(-5,1))
abline(lm(log(test$xij2011)~log(test$xij1991)))
#identity line
abline(a = 0, b = 1, col = "green")

#pop vs cob?
cobz <- unique(eViewsFile2$CoB)

for(cob in cobz){

  jpeg(paste0('R_outputs/miscPlots/',cob,'.jpg'))
  
  test <- eViewsFile2 %>% 
    filter(CoB==cob)
  
  plot(test$xij1991~test$popsh91)
  abline(lm(test$xij1991~test$popsh91))
  dev.off()

}

#So if it's percentages, do the diffs have to be constrained to something?
#Well, mean of zero
#Somewhat obviously given they both sum to 100...
diff <- test$xij2011-test$xij1991
summary(diff)

#So first point: if you're breaking down by city
#You probably need to re-do shares at the city level
#Otherwise the result is warped by shifts elsewhere
#(So hierarchical approach better?)

#So we have zones that were 6%, 10% of pakistanis in 1991
#Now dropped to ~5.5%
plot(test$xij2011~test$w91q)
abline(lm(test$xij2011~test$w91q))

#And! Those Ws with similar averages are likely to be near each other
#If the numbers are like this
#Because they're all overlapping their averages with those high values

#OK, so experimental data
exp <- data.frame(xij1991 = test$xij1991)

#If the shares don't change, regression's gonna be 1
exp$xij2011 <- exp$xij1991 + (exp$xij1991 * rnorm(nrow(exp),0,0.05))

plot(exp$xij2011~exp$xij1991)
abline(lm(exp$xij2011~exp$xij1991))
summary(lm(exp$xij2011~exp$xij1991))

#Which actually looks more or less the same as when I looked at glasgow
#Because most of this group are there and the tiny numbers have little impact
#on the regression

#~~~~~~~~~~~~~~~~~
#Constraining

#So: make sure the resulting numbers also still add up to the same.
#Various ways of doing this, but do I want to try saying something about the underlying numbers
#While doing it?
#Point is: you can't get a large set of % increases without decreases elsewhere.
#In terms of the regression line, I think it's likely only the top few values that are making 
#Any difference to the outcomes
#Those bunched very low values are going to keep the line's origin constrained, in reality.
#Though in theory they shouldn't need to if it were random...
#Which is probably what I should be testing.

#If you presume a thousand people with each person having equal odds of ending up in each zone
#Or odds weighted by that zones existing population would make more sense wouldn't it?

#Then that gives you some kind of baseline.
#That baseline can then be played with assuming some things about how many people from a CoB
#stayed in that zone since the last census
#(Leaving aside internal movement just for now.)

#The result is obviously boring: the mean's just going to be the zone's pop size.
#but what would the variance be? Actually not sure. It shouldn't depend on numbers, right?
#OK then, let's look!

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#This is our baseline pop from 1991: shares
#Just picking one country: pop shares are repeated for each, we only need it once
baselinePop91 <- eViewsFile2$popsh91[eViewsFile2$CoB == 'Pakistan']
sum(baselinePop91)
summary(baselinePop91)

#Assume population of 1000000
#So this kind of works - it doesn't quite constrain the population
#But it does give the distribution
newPop <- rbinom(822,10000000,baselinePop91/100)
sum(newPop)
plot(newPop,baselinePop91)

#So yes: the variability is of course reliant on the trials.
#For much larger numbers, it'll tend towards the underlying distribution.
#So variability will depend on the change in numbers. Uh oh.

#Which looks like what? What kind of numbers do we actually have?
#Let's pick the largest number: 53000 poles since 2001
#And do some repeat trials
newPop <- rbinom(822,53000,baselinePop91/100)
plot(newPop,baselinePop91)

#Or Ukraine
newPop <- rbinom(822,1984,baselinePop91/100)
plot(newPop,baselinePop91)

#dfProp <- data.frame(popShareProb = baselinePop91/100)


#Just to get it to a sum-to-one probability
popProbability <- baselinePop91/100


#I can't think of a quicker way of doing this
#Question: does re-assigning an exact number of people
#End up with the same distribution as doing it based on binom probability above?
#i.e. where we have x number of trials - so each one may not assign a value at all
#So that's actually "for each zone, what's the probability of a person turning up here?"
#Wherease I want "what zone will this person move to given the probability of moving to each?"
#So no-one may turn up. I assume it's the same but would like to check.

#Good lord - you know, I think there's no one answer here.
#You *either* have that person try and assess each at the same time
#In which case you're back to where you were with nothing but a prob distribution
#Or you randomly assess a zone in turn ... and I'm not sure that could do anything different
#To the binom approach above.

#There is a way, similar to what I did with the migration model:
#If the CPD is a line, just need to pick a point on that line 
#In proportion to each probability.
#And there *must* be a better way of doing this.
#plot(cumsum(popProbability[order(popProbability)]))

#http://stats.stackexchange.com/questions/26858/how-to-generate-numbers-based-on-an-arbitrary-discrete-distribution
#Includes my "make a vector and pick" approach, but does this work too?

#This is looking rather Poisson distributiony
result <- sample(x=1:822, size=50000, replace=TRUE, prob=popProbability)

#Oh nearly: if some zones get zero it has no count. 
#So we need to make sure the zeroes are included.
#as.is doesn't seem to work.
freqz <- table(result) %>% data.frame()
#So de-factor
freqz$result <- as.integer(as.character(freqz$result))

#Where we'll match so zeroes get included
freqzAllZones <- data.frame(result = 1:822)
freqz2 <- merge(freqz,freqzAllZones,by = 'result', all.y = T)
freqz2$Freq[is.na(freqz2$Freq)] <- 0

#Yup: the correct number of people
sum(freqz2$Freq)

plot(freqz2$Freq,popProbability, xlim=c(0,220))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Poking into this further: 
#I'd now like to know what the distributions look like when this is repeated
#What's the kind of spread of values in each zone for repeated trials given x number of people? 
#Should that be easily predictable?

#So that requires:
#For each zone, I need to keep the resulting value from above
#From a number of trials.
#Could do with finding a quicker way to store the initial dataframe?
#Naaaah
getDist <- function(inputSize){

  result <- sample(x=1:822, size=inputSize, replace=TRUE, prob=popProbability)
  
  freqz <- table(result) %>% data.frame()
  freqz$result <- as.integer(as.character(freqz$result))
  
  freqzAllZones <- data.frame(result = 1:822)
  freqz2 <- merge(freqz,freqzAllZones,by = 'result', all.y = T)
  freqz2$Freq[is.na(freqz2$Freq)] <- 0
  
  return(freqz2$Freq)

}

trialz <- replicate(100,getDist(inputSize = 500000)) 

# for(i in 1:ncol(trialz)){
#   jpeg(paste0('R_outputs/testRandomPeopleAssignment/',i,'.jpg'))
#   hist(trialz[,i],breaks = 20)
#   dev.off()
# }

#Might be better to check the shares
#And see how those differ
#Maybe. Or not. Err.
trialzprops <- (prop.table(trialz,2) * 100) %>% 
  data.frame %>% 
  gather() 
#apply(trialzprops,2,sum)

ggplot(trialzprops,aes(y = value, x = key)) +
  geom_boxplot()
  
#Well nothing very exciting to report there! Though worth noting. Onwards

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#More checks: synt 100% shares, what do coeffs mean?----

#My one example of coeff above one was a mistake: based on urban only for US
#But then the shares don't sum to 100
#For those that do, nothing so far above 1. Is that built in?

#So let's pick some examples
#Oz <- eViewsFile[eViewsFile$CoB == 'Australia',]
#make up changes from 1991 to 2011

#This has to be easier with a much smaller set of pretend numbers!
#And actually massively randomising it to work out the space.

#So 10 zones should do it. Numbers don't matter, just shares.
#Two periods. Compare change.

#And as well as randomising, some examples. Hmm, maybe start with those
props <- data.frame(
  xij91 = c(1:10) %>% prop.table,
  xij11 = c(1:10) %>% prop.table
)

# props <- data.frame(
#   xij91 = c(rnorm(100,1,0.2),20) %>% prop.table,
#   xij11 = c(rnorm(100,1,0.2),40) %>% prop.table
# )


#OK, let's try randomising this shizzle
randomProps <- function(){

  props <- data.frame(
    xij91 = runif(n = 822,min = 0,max=10000) %>% prop.table,
    xij11 = runif(n = 822,min = 0,max=10000) %>% prop.table
  )
  
  #sum to 1?
  #apply(props,2,sum)
  
  coeff <- summary(lm(data = props, xij11 ~ xij91))$coefficients[2]
  
}

coeffz <- replicate(10000,randomProps())

#Let's save that, took a while to run!
#write_csv(coeffz %>% data.frame(),'R_data/coeffz_synth_random100kiterations.csv')

summary(coeffz)
hist(coeffz,breaks = 100)

sd(coeffz)

#And how does it come out if we compare to an actual result?
#So 1991 is real data but 2011 is randomised
#I'm presuming the same but let's see.
#Needs to be data that actually sums to 1
#(so divide by 100)
eViewsFile <- read_csv('R_data/estimation_aggEmploymentToLargerZonesDeltaEmpUrbanRuralTTWA.csv')

#pick one country, convert to [0,1] range
xij91cob = eViewsFile$xij1991[eViewsFile$CoB=='Pakistan']/100
xij91cob = eViewsFile$xij1991[eViewsFile$CoB=='United_Sta']/100

randomProps2 <- function(){
  
  props <- data.frame(
    xij91 = xij91cob,
    xij11 = runif(n = length(xij91cob),min = 0,max=1000) %>% prop.table
    
  )
  
  #sum to 1?
  #apply(props,2,sum)
  
  coeff <- summary(lm(data = props, xij11 ~ xij91))$coefficients[2]
  
}

coeffz2 <- replicate(10000,randomProps2())
hist(coeffz2,breaks = 40)
sd(coeffz2)



#~~~~~~~~~~~~~~~~~~~~

#Some scenarios

#Half stay the same(ish), other half double their numbers
#And are very large to start with
props <- data.frame(
  xij91 = c(rnorm(100,1,0.2),rnorm(10,40,0.2)) %>% prop.table,
  xij11 = c(rnorm(100,1,0.2),rnorm(10,20,0.2)) %>% prop.table
)

props <- data.frame(
  xij91 = c(1:10 * rnorm(100,0,0.2),rnorm(10,40,0.2)) %>% prop.table,
  xij11 = c(1:10 * rnorm(100,0,0.2),rnorm(10,20,0.2)) %>% prop.table
)

props <- data.frame(
  xij91 = c(1:10,rnorm(10,40,0.2)) %>% prop.table,
  xij11 = c(1:10,rnorm(10,20,0.2)) %>% prop.table
)

props <- data.frame(
  xij91 = c(1:10,rnorm(10,40,0.2)) %>% prop.table,
  xij11 = c(11:20,rnorm(10,40,0.2)) %>% prop.table
)


# props <- data.frame(
#   xij91 = c(11:20,rnorm(10,40,0.2)) %>% prop.table,
#   xij11 = c(1:10,rnorm(10,40,0.2)) %>% prop.table
# )

# props <- data.frame(
#   xij91 = c(1:10000 * rnorm(10000,10,0.2)) %>% prop.table,
#   xij11 = c(1:10000 * rnorm(10000,10,0.2) * 2) %>% prop.table
# )
# 
# props <- data.frame(
#   xij91 = c(rnorm(10000,10,0.2)) %>% prop.table,
#   xij11 = c(rnorm(10000,10,0.2) + 10) %>% prop.table
# )

plot(props$xij11~props$xij91)
abline(lm(props$xij11~props$xij91), col='green')
abline(a = 0, b = 1,col='red', lty = 2, lwd = 3)
summary(lm(data = props,xij11~xij91))
summary(lm(data = props,xij11~xij91))$coefficient[2]


propsl <- props %>% gather()

ggplot(propsl,aes(x = value, fill = key)) +
  geom_histogram(position = 'identity', bins = 30, alpha = 0.5)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ACTUAL COB ZONE POP CHANGES-----

cob <- read_csv('StitchOutputs/Scotland/LBS_3censusCombinedData/countryOfBirth.csv')

#We want census years in their own columns and CoB long
#Exactly opposite...
cob91 <- cob[cob$censusYear==1991,] %>% dplyr::select(4:42)
cob11 <- cob[cob$censusYear==2011,] %>% dplyr::select(4:42)

cob91 <- gather(cob91)
cob11 <- gather(cob11)

#Columns all in same order
# names(cob91) <- c('cob','xij91')
# names(cob11) <- c('cob','xij11')

cobw <- data.frame(cob = cob91$key, xij91 = cob91$value, xij11 = cob11$value)

cobw$increased <- 0 + (cobw$xij11 - cobw$xij91 > 0)

#plot each
for(cobname in unique(cobw$cob)){
  
  jpeg(paste0('R_outputs/CoB91vs11actual/',cobname,'.jpg'))
  # jpeg(paste0('R_outputs/CoB91vs11actual/log/',cobname,'.jpg'))
  
  plot(cobw$xij11[cobw$cob == cobname] ~ cobw$xij91[cobw$cob == cobname],
       col = ifelse(cobw$increased[cobw$cob == cobname]==1,'blue','red'))
 
  # plot(cobw$xij11[cobw$cob == cobname] %>% log10 ~ cobw$xij91[cobw$cob == cobname] %>% log10,
  #      col = ifelse(cobw$increased[cobw$cob == cobname]==1,'blue','red'),
  #      ylim = c(-1,max( log10(cobw$xij11[cobw$cob == cobname]) )))
  # 
  abline(a = 0, b = 1,col='red', lty = 2, lwd = 3)
  dev.off()
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Bar charts of coefficients / comparing to Geoff's----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Just going to grab those numbers manually.
scot <- data.frame(
  place = rep('Scotland',4),
  type = c('All','Europe','Richer','Poorer'), 
  coeff = c(0.433,0.469,0.295,0.551))

#These are IV estimations though...
#Make negative so mirrored
geoffs <- data.frame(
  place = rep('London',4),
  type = c('All','Europe','Richer','Poorer'), 
  coeff = -c(0.812,0.68,0.651,0.828))

both <- rbind(scot,geoffs)

#separete all/europe and poor/rich into their own facets
both$facet <- c(1,1,0,0,1,1,0,0)

#~~~~~~~~~~~~~
#London vs Scotland as a whole
#https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
output <- ggplot() +
  geom_bar(data = both, aes(x = type, y = coeff, fill = place),
           stat='identity', colour = 'white') +
  geom_text(data = both, aes(x = type, 
                             y = ifelse(coeff < 0, coeff + 0.1, coeff - 0.1), 
                             label = abs(coeff)), vjust=0) +
  scale_y_continuous(breaks = c(0.5,0,-0.5), labels = c(0.5,0,0.5)) +
  ylab('spatial persistence') +
  xlab('') +
  theme(legend.title=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  facet_wrap(~facet, scales = 'free_x') 
  #scale_fill_brewer(palette = 'Set2')
  # scale_y_continuous(breaks = c(0.5,0,-0.5), labels, limits)

output

ggsave('R_outputs/graphs4Glasgow/scot_v_london.png',output,dpi=500,width = 6,height = 4)

ggsave()

#~~~~~~~~~~~~~~~~~~
#Scots city rich/poor - overlay with weights
#So again let's just manually enter what we've got.
#Just for three cities
cities <- data.frame(
  city = c(rep('Aberdeen',2),rep('Edinburgh',2),rep('Glasgow',2)),
  type = c(rep(c('richer','poorer'),3)),
  xij91 = c(0.242,0.412,0.735,0.486,0.383,0.4),
  wij91 = c(0.15,0.81,0.754,0.480,0.29,0.41)
)

#Oh, don't need to overlay, just facet, maybe?
#Though overlay will make more sense probably?
# output <- ggplot() +
#   
#   geom_bar(data = cities, aes(x = type, y = xij91),
#            stat='identity', colour = 'white', fill = '#F8766D') +
#   geom_bar(data = cities, aes(x = type, y = wij91),
#            stat='identity', colour = 'black', fill = NA, linetype='dashed',size = 1) +
#   # geom_text(data = both, aes(x = type, 
#   #                            y = ifelse(coeff < 0, coeff + 0.1, coeff - 0.1), 
#   #                            label = abs(coeff)), vjust=0) 
#   # # scale_y_continuous(breaks = c(0.5,0,-0.5), labels = c(0.5,0,0.5))
#   # scale_y_continuous(breaks = c(0.5,0,-0.5), labels, limits)
#   facet_wrap(~city,nrow=1) +
#   theme(strip.text.x = element_text(face="bold")) 
#   #scale_fill_brewer(palette = 'Set2')
# 
# output

#Attempting to manually define styles so I can get a legend
#Requires melting a little...
citiesm <- melt(cities,id.vars=c('city','type'))

output <- ggplot(citiesm) +
  geom_bar(aes(x = type, y = value,fill = variable, linetype = variable, colour = variable),
           stat='identity', position = 'identity',size = 1) +
  scale_fill_manual(values = c('#F8766D',NA)) +
  scale_colour_manual(values = c('white','black')) +
  scale_linetype_manual(values = c('solid','dashed')) +
  facet_wrap(~city,nrow=1) +
  ylab('spatial persistence') +
  xlab('country of birth poorer/richer') +
  theme(legend.title=element_blank(),
        strip.text.x = element_text(face="bold")) 

output

ggsave('R_outputs/graphs4Glasgow/scotsCitiesSP.png',output,dpi=500,width = 6,height = 4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Some new graphs for another presentation from latest data----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#manually copy numbers again

#from regressionOutputs/3census/regressionsJuly2017/cityScotlandnationalCoBproportion_vs_cityLevel.docx
cities <- data.frame(
  city = c(rep('Aberdeen',2),rep('Edinburgh',2),rep('Glasgow',2)),
  type = c(rep(c('richer','poorer'),3)),
  #xij91 = c(0.242,0.412,0.735,0.486,0.383,0.4),
  #wij91 = c(0.15,0.81,0.754,0.480,0.29,0.41)
  xij = c(0.2124,0.2933,0.60415,0.49654,0.372871,0.413189),
  wij = c(0.1824,0.631,0.47850,0.50022,0.158315,0.384903)
)


#Requires melting a little...
citiesm <- melt(cities,id.vars=c('city','type'))

output <- ggplot(citiesm[citiesm$city %in% c('Glasgow','Edinburgh'),]) +
#output <- ggplot(citiesm) +
  geom_bar(aes(x = type, y = value,fill = variable, linetype = variable, colour = variable),
           stat='identity', position = 'identity',size = 1) +
  scale_fill_manual(values = c('#F8766D',NA)) +
  scale_colour_manual(values = c('white','black')) +
  scale_linetype_manual(values = c('solid','dashed')) +
  facet_wrap(~city,nrow=1) +
  ylab('spatial persistence') +
  xlab('country of birth poorer/richer') +
  ggtitle("3 CENSUS:\nSpatial persistence between 1991 & 2011\nEdinburgh and Glagow\npoorer vs richer in each city") +
  theme(legend.title=element_blank(),
        strip.text.x = element_text(face="bold"),
        plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) 

output

ggsave('R_outputs/graphs4g/3censusTwoCitiesRichPoor.png',output,dpi=500,width = 6,height = 4)


#~~~~~~~~~~~~~~~~~~~~~~~
#5 CENSUS TWO CITIES----


#from regressionOutputs/3census/regressionsJuly2017/cityScotlandnationalCoBproportion_vs_cityLevel.docx
cities <- data.frame(
  city = c(rep('Edinburgh',2),rep('Glasgow',2)),
  type = c(rep(c('richer','poorer'),2)),
  xij = c(0.359348,0.150446,0.299639,0.121441),
  wij = c(0.00741,0.252910,0.123358,0.287726)
)


#Requires melting a little...
citiesm <- melt(cities,id.vars=c('city','type'))

output <- ggplot(citiesm) +
#output <- ggplot(citiesm) +
  geom_bar(aes(x = type, y = value,fill = variable, linetype = variable, colour = variable),
           stat='identity', position = 'identity',size = 1) +
  scale_fill_manual(values = c('#F8766D',NA)) +
  scale_colour_manual(values = c('white','black')) +
  scale_linetype_manual(values = c('solid','dashed')) +
  facet_wrap(~city,nrow=1) +
  ylab('spatial persistence') +
  xlab('country of birth poorer/richer') +
  ggtitle("5 CENSUS:\nSpatial persistence between 1971 & 2011\nEdinburgh and Glagow\npoorer vs richer in each city") +
  theme(legend.title=element_blank(),
        strip.text.x = element_text(face="bold"),
        plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) 

output

ggsave('R_outputs/graphs4g/5censusTwoCitiesRichPoor.png',output,dpi=500,width = 6,height = 4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#REPEAT FOR EACH DECADE FOR THE TWO CITIES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cities <- data.frame(
  city = c(rep('Edinburgh',4),rep('Glasgow',4)),
  type = c(rep(c('1971','1981','1991','2001'),2)),
  xij = c(0.1537987,0.366589,0.414352,0.7156745,0.1156286,0.2527456,0.4244031,0.4985077),
  wij = c(0.1905084,0.214906,0.202166,0.10520094,0.2687198,0.2587457,0.2408373,0.2194093)
)


#Requires melting a little...
citiesm <- melt(cities,id.vars=c('city','type'))

output <- ggplot(citiesm) +
#output <- ggplot(citiesm) +
  geom_bar(aes(x = type, y = value,fill = variable, linetype = variable, colour = variable),
           stat='identity', position = 'identity',size = 1) +
  scale_fill_manual(values = c('#F8766D',NA)) +
  scale_colour_manual(values = c('white','black')) +
  scale_linetype_manual(values = c('solid','dashed')) +
  facet_wrap(~city,nrow=1) +
  ylab('spatial persistence') +
  xlab('country of birth poorer/richer') +
  ggtitle("2011 ~ 1971/81/91 AND 2001\n(lag closing from 40 to 10 years)\nSpatial persistence\nfor Edinburgh and Glagow") +
  theme(legend.title=element_blank(),
        strip.text.x = element_text(face="bold"),
        plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) 

output

ggsave('R_outputs/graphs4g/5censusLagtwoCities.png',output,dpi=500,width = 6,height = 4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Looking for employment shocks, 5 census----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#CoBs not yet updated post April-2017
eViewsFile5 <- read_csv('R_data/5Census_estimation_basic.csv')

#Reduce to some different countries. Exclude UK, compare Europe / other.
europe <- eViewsFile5[eViewsFile5$cob %in% c('OtherEurope','IrishRepublic'),]

rich <- eViewsFile5[eViewsFile5$cob %in% 
                      c('OtherEurope',
                        'IrishRepublic',
                        'OldCommonwealth'),]

poor <- eViewsFile5[eViewsFile5$cob %in% 
                      c("Africa(New-C)",
                        "India",
                        "Pakistan",
                        "SEAsia(New-C)",
                        "Caribbean(New-C)",
                        "NewCommonwealthother",
                        "RestofWorld")
                    ,]

#Basic employment stats first please. Think I've done this before. Oh well.
#Boxplot of EA for the five censuses

ea5 <- gather(eViewsFile5[,c(12:16)],year,percent)

ggplot(ea5,aes(x = factor(year), y = percent)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE)

#Differences between decades.
ea_diffs <- data.frame(diff7181 = eViewsFile5$ea1981-eViewsFile5$ea1971,
                       diff8191 = eViewsFile5$ea1991-eViewsFile5$ea1981,
                       diff9101 = eViewsFile5$ea2001-eViewsFile5$ea1991,
                       diff0111 = eViewsFile5$ea2011-eViewsFile5$ea2001,
                       diff7191 = eViewsFile5$ea1991-eViewsFile5$ea1971
                       )

#Add zone names and save
ea_diffs$code <- eViewsFile5$code

#Composite string column to view raw numbers for a zone
#So we can see if e.g. a high employment zone got particularly hit
ea_diffs$all5EAs <- paste0(
  round(eViewsFile5$ea1971,digits=2),',',
  round(eViewsFile5$ea1981,digits=2),',',
  round(eViewsFile5$ea1991,digits=2),',',
  round(eViewsFile5$ea2001,digits=2),',',
  round(eViewsFile5$ea2011,digits=2),','
  )

ea_diffs$dropped4twoDecades71to91 <- 0 + ((eViewsFile5$ea1971 > eViewsFile5$ea1981) & (eViewsFile5$ea1981 > eViewsFile5$ea1991))

# ea_diffs$dropped4threeDecades71to01 <- 0 + ((eViewsFile5$ea1971 > eViewsFile5$ea1981) & (eViewsFile5$ea1981 > eViewsFile5$ea1991) & (eViewsFile5$ea1991 > eViewsFile5$ea2001))

table(ea_diffs$dropped4twoDecades71to91)
#Zero. Remove.
table(ea_diffs$dropped4threeDecades71to01)

write_csv(ea_diffs,'R_data/5census_EconActive_10yearDiffs2.csv')

#And what's that look like before doing some mapping?
ea5diff <- gather(ea_diffs,year,percent)

ea5diff$gap <- 'one decade'
ea5diff$gap[ea5diff$year=='diff7191'] <- 'two decades'

#Which now only works if not including the extra columns that confuse the gather...
ggplot(ea5diff,aes(x = factor(year, levels = c('diff7181','diff8191','diff9101','diff0111','diff7191')), y = percent, fill = factor(gap))) +
  geom_boxplot() +
  geom_hline(yintercept = 0, alpha = .5) +
  #facet_wrap(~gap, scales = 'free_x') +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE)

















