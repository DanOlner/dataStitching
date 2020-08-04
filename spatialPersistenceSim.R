#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SPATIAL PERSISTENCE SIM----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Get the original data so it's clear what we're doing
#This has counts for each country of birth in their own columns
#For 822 Scottish zones.
cob91 <- read_csv('1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.csv')

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
#There are 822 zones in the above data. Hardcoding that...
result <- sample(x=1:822, size=50000, replace=TRUE, prob=cob91props$Pakistan)

#Although distribution-wise, the result will be more or less the same as a binomial
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



#Function for replicating the simulation
getCoeff <- function(inputSize,popProbability,origProportions){
  
  result <- sample(x=1:822, size=inputSize, replace=TRUE, prob=popProbability)
  newcounts <- tabulate(result,822)
  newcounts.prop <- newcounts/sum(newcounts)
  
  #Return coefficient
  coeff <- summary(lm(newcounts.prop ~ origProportions))$coefficients[2]
  
}

#Replicate, pull out coeffients
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