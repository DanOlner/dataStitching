geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat","data.table",'readr','pryr','combinat','purrr','forcats','ggfortify')
lapply(geolibs, require, character.only = TRUE)

#INVESTIGATING MEANINGS FOR THE NUMBERS
#Reload some recent data
#5 census seems easier to get hold of
sheet5 <- readRDS('R_data/sheet5.rds')
citySheets5 <- readRDS('R_data/citySheets5.rds')#list

#Though I would prefer 3 census as it has house prices in
sheet3 <- readRDS('R_data/sheet3census.rds')
citySheets3 <- readRDS('R_data/citySheetsList3census.rds')#list

sheet3urban <- sheet3 %>% filter(urbanFiftyPercentPlus == 1)

#Need the original numbers too
#Just loading 1991 for now to compare to other 1991 numbers
ninetyOne <- readShapeSpatial('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp') %>% data.frame()

#Find mig share - just raw, don't need to subtract owt
#So this is just non-UK born per zone
uksum <- apply(ninetyOne %>% dplyr::select(Channel_Is,UK_part_no,England,Scotland,Wales,Northern_I),1,sum)
restsum <- apply(ninetyOne %>% dplyr::select(-one_of('Channel_Is','UK_part_no','England','Scotland','Wales','Northern_I','SP_ID','label','fnl_rsL')),1,sum)

ninetyOne$nonUKbornProportion <- (restsum/uksum)*100

#NOW SOME BASIC PLOTS TO THINK ABOUT SHIZZLE

#Keep only one set of econ active and price data
data91 <- sheet3[!duplicated(sheet3$label),]

data91 <- merge(data91,ninetyOne %>% dplyr::select(label,nonUKbornProportion),by = 'label')

#plots
plot(data91$ph91 ~ data91$ea91)
plot(data91$ph91 ~ data91$nonUKbornProportion)

plot(data91$ea91 ~ data91$nonUKbornProportion)

#coeffs
summary(lm(data91$ph91 ~ data91$ea91))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Sim - check what shares does----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#via R_stats_learning / regression101.R
modelz <- function(obnumber){

  wage  <- rnorm(obnumber,0,1)
  hist(wage)
  
  numimmigrants   <- 2.0+(0.5*wage)
  
  plot(numimmigrants~wage)
  
  #Keep raw number version
  rawz <- lm(numimmigrants~wage)
  
  #Values as shares adding to 100
  shares <- numimmigrants/(sum(numimmigrants)) * 100
  sum(shares)
  
  plot(shares~wage)
  
  sharez <- lm(shares~wage)
  
  return(list(rawz,sharez))

}

#~~~~~~~~~~~~~~
options("scipen"=100, "digits"=4)

#Change number of obs. First is raw, second shares
lapply(modelz(10000),summary)
lapply(modelz(1000),summary)
lapply(modelz(500),summary)

lapply(modelz(100),summary)
lapply(modelz(10),summary)

#~~~~~~~~~~~~~~~~~~
#Looking for ways to make concentration share be comparable----

#set up two lines (proxy for zone number)
#Use same underlying numbers

pop <- data.frame(vals = rnorm(100000,10,1))

pop$smallnumberofzones <- cut_number(pop$vals,100) %>% as.numeric
pop$largenumberofzones <- cut_number(pop$vals,500) %>% as.numeric

#values for each. Should be same count but will end up different for shares
popsummarysmall <- pop %>% group_by(smallnumberofzones) %>% 
  summarise(countAveragePerZone = mean(vals))

popsummarylarge <- pop %>% group_by(largenumberofzones) %>% 
  summarise(countAveragePerZone = mean(vals))

hist(popsummarysmall$countAveragePerZone)
hist(popsummarylarge$countAveragePerZone)
mean(popsummarysmall$countAveragePerZone)
mean(popsummarylarge$countAveragePerZone)
sd(popsummarysmall$countAveragePerZone)
sd(popsummarylarge$countAveragePerZone)

#Good good.
#Shares across zones
popsummarysmall <- popsummarysmall %>% 
  mutate(shares = (countAveragePerZone/sum(countAveragePerZone)) * 100)

popsummarylarge <- popsummarylarge %>% 
  mutate(shares = (countAveragePerZone/sum(countAveragePerZone)) * 100)

#Same underlying values
mean(popsummarysmall$countAveragePerZone)
mean(popsummarylarge$countAveragePerZone)

#Different shares cos of zone number (perfectly linear)
mean(popsummarysmall$shares)
#[1] 1
mean(popsummarylarge$shares)
#[1] 0.2

#So: five times increase in area = average value becomes a fifth.
#Which is easy enough to imagine.

#So what we're doing is coming back round to the share being in proportion to area again


#Test normalising by area
#Which in this case with equal-sized zones is just by number of obs.
popsummarysmall$normalisedshares <- popsummarysmall$shares/nrow(popsummarysmall)
popsummarylarge$normalisedshares <- popsummarylarge$shares/nrow(popsummarylarge)


#~~~~~~~~~~~~~~~~~~~~
#Checking per-km^2 method----
#~~~~~~~~~~~~~~~~~~~~

#We will need areas so:
ninetyOneshp <- readShapeSpatial('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp')

#km^2
ninetyOneshp$zonearea <- gArea(ninetyOneshp, byid = T)/1000000

#Whole area
areaScotland <- gArea(ninetyOneshp)/1000000

#Tick, same.
sum(ninetyOneshp$zonearea)

#OK so if this is right. Test.
#Pakistani per km^2
pak <- ninetyOneshp[,c('Pakistan','zonearea')]

pak$PakistanPerKmSquare <- pak$Pakistan/pak$zonearea

#We have count of pakistani per km2 within each zone
#To work out proportion per KM2 for the whole area:
#If e.g. there are 1.6 people per Km2 in this zone.
#That as a proportion for the whole area is just that over the number of people.
pak$PakistanSHAREPerKmSquare <- (pak$PakistanPerKmSquare/sum(pak$Pakistan)) * 100

#So should sum to 100 if you multiply each by the zone size then sum them
#That is, it's the average proportion per km2 in that zone
#This makes my head hurt

#Oh good!
pak$chk = pak$zonearea * pak$PakistanSHAREPerKmSquare
sum(pak$chk)

#So share of migrants per square km (summing to 100 for all km in a given zoneset) should now make everything comparable for different zone sets, right?
#Note that as you increase area size, the count goes up too. So ... it should actually just give you a consistent ... ?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~COMPARE 3 AND 5 FOR 2011 VS 1991----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Which I've already done at least once 
#but I want to see everything again 
#and really see what the actual eff

#So I should look at the sheets used for running the actual regressions.
#Let's set up a general sanity check output folder as well.
sheet3 <- readRDS('R_data/sheet3census.rds')
sheet5 <- readRDS('R_data/sheet5.rds')

#They the right data? Look like the right number to me
unique(sheet3$cob) %>% length
unique(sheet5$cob) %>% length

#Yup. Right proportions?
sheet3 %>% group_by(cob) %>% summarise(sum(xij1991))
sheet3 %>% group_by(cob) %>% summarise(sum(xij2011))
sheet5 %>% group_by(cob) %>% summarise(sum(xij1991))
sheet5 %>% group_by(cob) %>% summarise(sum(xij2011))

#So: this is 100% shares at all-scotland level. 
#I can load the 100% share cities to compare innabit. Basics first.
names(sheet3)
names(sheet5)

#~~~~~~~~~~~~~~~~
#1. Look again at all CoB 2011~1991 (and other year) plots----
#~~~~~~~~~~~~~~~~

#ggplot this time.
for(cobz in unique(sheet3$cob)){
  
  subz <- sheet3 %>% filter(cob == cobz)
  
  ggplot(subz, aes(x = xij1991, y = xij2011)) +
    geom_point() +
    geom_abline(aes(intercept = 0, slope = 1),colour='blue') +
    coord_fixed() +
    ggtitle(cobz) +
    theme(plot.title = element_text(face="bold",hjust = 0.5))
  
  ggsave(paste0('R_outputs/CoBsharePlots/3census2011_1991/',cobz,'.png'), dpi = 150, width = 6, height = 6)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~
#TEST DOUBLING SHARE----
#~~~~~~~~~~~~~~~~~~~~~~~

glaspakistan <- citySheets5[[1]] %>% dplyr::filter(cob=='Pakistan')

#We just need to regress one of them. Looking at original data...
pakistan91 <- data.frame(cobScot5[[3]])[,c('label','Pakistan')]
pakistan11 <- data.frame(cobScot5[[5]])[,c('label','Pakistan')]

#So what I want here: 
#Two identical regressions of spatial persistence
#With something like unemployment added
#Then double up the zone number
#Which will halve the cob count in each

#Wait now. Err.

#Stand in for "same % unemp regardless of zone count"
x1 = rnorm(n = 500, mean = 2, sd = 1)
x2 = rnorm(n = 10000, mean = 1, sd = 1)
sum(x1)
sum(x2)#double sum, obv

#If total is 100.
y1 = rnorm(n = 500, mean = 2, sd = 1) + x1
y2 = rnorm(n = 10000, mean = 1, sd = 1) + x2

#Will be about the same sum
sum(y1)
sum(y2)

plot(y1 ~ x1)
plot(y2 ~ x2)

summary(lm(y1~x1))
summary(lm(y2~x2))

#~~~~~~~~~~~~~~~~~~~~~~~
#SIMPLE SHARE TEST----
#~~~~~~~~~~~~~~~~~~~~~~~

#Think through an actual geography. Come on.
#If we imagine a single vector world, that will make this easier.


#Employment. Random.
emply <- runif(10000,20,100)

#So stock of people in each initial zone
#More concentrated at the beginning
#More likely to be concentrated if high employment
people <- rnorm(10000,1000,50) + (10000:1) + (emply * 0.2)
plot(people)

#So...
summary(lm(people ~ emply))

#dataframe it up before getting summaries
datz <- data.frame(people,emply)

#Break into two different size groups, 100 zones and 50 zones.
datz$fiftyzones <- cut_number(datz$people,20) %>% as.numeric
datz$hundredzones <- cut_number(datz$people,100) %>% as.numeric

table(datz$fiftyzones)
table(datz$hundredzones)

#Two separate summaries. Shares for the people, averages for the employment level.
#(Which is fine cos they're equal sized...)
fiftysummary <- datz %>% group_by(fiftyzones) %>% 
  summarise(share = (sum(people)/sum(datz$people)*100), emply = mean(emply))

#Tick
sum(fiftysummary$share)

hundredsummary <- datz %>% group_by(hundredzones) %>% 
  summarise(share = (sum(people)/sum(datz$people)*100), emply = mean(emply))

#Tick
sum(hundredsummary$share)

options(scipen=2)

summary(lm(data = fiftysummary, share ~ emply))
summary(lm(data = hundredsummary, share ~ emply))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#COMPARING BINOM TO SAMPLING AS WAY TO MOVE PEOPLE----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Taking bits from regressionsOnEviewsReady.R/SYNTHETIC TESTING ON DATA
eViewsFile2 <- read_csv('R_data/estimation_aggEmploymentToLargerZonesDeltaEmpUrbanRuralTTWA.csv')

baselinePop91 <- eViewsFile2$popsh91[eViewsFile2$CoB == 'Pakistan']
sum(baselinePop91)
summary(baselinePop91)
hist(baselinePop91)

#the variability is reliant on the number of trials.
#For much larger numbers, it'll tend towards the underlying distribution.
#So variability will depend on the change in numbers. Uh oh.
newPop <- rbinom(822,10000000,baselinePop91/100)
sum(newPop)
plot(newPop,baselinePop91)

newPop <- rbinom(822,53000,baselinePop91/100)
plot(newPop,baselinePop91)

#Or Ukraine
newPop <- rbinom(822,1984,baselinePop91/100)
plot(newPop,baselinePop91)

#Toy example to illustrate difference. With an empty world just adding individuals.
#Each one not dependent at all on who's there so no path dependency.

#The difference:
#Binom: "for each zone, what's the probability of a person turning up here?"
#But we might want a constraint: that is, we know a thousand people are turning up:
#What zone will each person move to given the probability of moving to each zone?


#So imagine we have just ten zones.
#Binomial distribution. If we assume an even probability of ending up in each zone
newPop <- rbinom(10,1000,0.1)
sum(newPop)

#So it's never quite a thousand, though always close.
#If we were imagining the scenario: 
#There are an arbitrary number of people waiting at customs to get in to two zones.
#A customs person in each zone flips a coin. Heads, you get in, tails you don't.
newPop <- rbinom(2,1000,0.5)
sum(newPop)


#~~~~~~~~~~~~~~~~~~~~~~
#DOING FROM SCRATCH----
#~~~~~~~~~~~~~~~~~~~~~~

#Get the original data so it's clear what we're doing
cob91 <- read_csv('StitchOutputs/Scotland/LBS_postcodeSector_3Census_csvs/1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.csv')

#Then share proportions across all 822 zones for each country.
#Leaving as a proportion of 1
#Don't need zone labels
cob91props <- data.frame(prop.table(as.matrix(cob91[,c(4:42)]),2))

#Double-check all sum to one across all 822 zones...
apply(cob91props,2,sum)


#Rather than a binomial, which won't constrain to a set population
#We can use sample and pass in the probability vector for each zone
#http://stats.stackexchange.com/questions/26858/how-to-generate-numbers-based-on-an-arbitrary-discrete-distribution
#So picking a random one from our zone proportions...
result <- sample(x=1:822, size=50000, replace=TRUE, prob=cob91props$Pakistan)

#Although presumably distribution-wise, the result will be the same
#This gives us a little more control when thinking about exact counts

#So all we're doing:
#Sample from a vector index of zones 1:822
#Here, 50,000 times. Replacing each zone when sampled.
#Results is 50000 values telling us the zone each person ended up in
#Tabling that gives the zone counts
table(result)
#Though we need to use tabulate to make sure it includes zones with zero counts
#Include nbins in case any of the last few are zero
tabulate(result,822)
#Check it's constrained to 50000 people
sum(tabulate(result))

#Store as a proportion
newcounts <- tabulate(result,822)
newcounts.prop <- newcounts/sum(newcounts)

#So if we assume this is the *new* population
#i.e. that everyone's location in 2011 was determined this way
#What's the coefficient? Well, this is going to be close to one...
plot(newcounts.prop~cob91props$Pakistan)

summary(lm(newcounts.prop~cob91props$Pakistan))



#REPEATING
getCoeff <- function(inputSize,popProbability,origProportions){
  
  result <- sample(x=1:822, size=inputSize, replace=TRUE, prob=popProbability)
  newcounts <- tabulate(result,822)
  newcounts.prop <- newcounts/sum(newcounts)
  
  coeff <- summary(lm(newcounts.prop ~ origProportions))$coefficients[2]
  
}


coeffz <- replicate(100,getCoeff(inputSize = 500, 
                                 popProbability = cob91props$Pakistan,
                                 origProportions = cob91props$Pakistan)) 
autoplot(density(coeffz))

#Variance drops for higher numbers of course...
coeffz <- replicate(100,getCoeff(inputSize = 50000, 
                                 popProbability = cob91props$Pakistan,
                                 origProportions = cob91props$Pakistan)) 
autoplot(density(coeffz))


#AND IF IT'S RANDOM?
#Then it's going to be either side of zero innit?
#randomproportionsumsto1 <- abs(runif(822))
randomproportionsumsto1 <- abs(runif(822))^2#some larger values
randomproportionsumsto1 <- randomproportionsumsto1/sum(randomproportionsumsto1)
#autoplot(density(randomproportionsumsto1))

coeffz <- replicate(100,getCoeff(inputSize = 50000, 
                                 popProbability = randomproportionsumsto1,
                                 origProportions = cob91props$Pakistan)) 
autoplot(density(coeffz))






