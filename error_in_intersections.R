#Error rate in intersection of geographies
source("Function_DataFromSmalltoLargeGeog.R")

#For Scotland only currently
#Aggregated PCSs 822 zones
lrg <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount", 
               layer="pseudoPCS_aggregated4CorrectCount")

#~~~~~~~
#Use 1981 as an example----

#As 1971 is synthetic/voronoi zones.
its <- readOGR(dsn="Intersects/Scots_5_census_91_PCSzeroCounts_targetGeog", 
               layer="81EDsTo91PCSzeroCount")

dta <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/81.csv")

#Some checks/reminders about what we're looking at.
#Add an area column
#Round so View doesn't use sci notation
its$area <- gArea(its, byid = T) %>% round(2)

its_df <- as_tibble(its)

#To check which smaller zones are fully inside one zone
#First need each smaller zones' full area
#Could merge in from originals or just ...
its_df <- its_df %>% 
  group_by(label) %>% 
  mutate(smallZoneArea = sum(area))

#Note: should also be able to do it by counting:
#If there's only one small zone entry it must be fully contained.

#A lot of these are going to be border slivers.



rbinom(500, 6, 0.5) %>% hist

rbinom(50, 1, 0.5) %>% table
rbinom(10000, 1, 0.5) %>% table
rbinom(50, 1, 0.5) %>% table

rbinom(n = 10000, size = 50, prob = 0.5) %>% table %>% barplot

#https://www.tutorialspoint.com/r/r_binomial_distribution.htm
dbinom(1:100,100,0.5)
#dbinom(1:100,100,0.5) %>% barplot()
#or
plot(y= dbinom(1:50,50,0.5),x=1:50)

n = 50
plot(y= dbinom(1:n,n,0.5),x=1:n)










its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result <- moveData(its,dta,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

