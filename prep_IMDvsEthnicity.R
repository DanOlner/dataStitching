library(tidyverse)
#In an attempt to get it embedded in me brain.
#IMD 2015 (to compare to 2011 Census data on ethnicity)
#Data is at LSOA level (32844 obs)
imd2015 <- read_csv('C:/Users/admin/Dropbox/imd2019/data/File_7_ID_2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv')

#Rank has 1 as most deprived. Actual score is the other way round, right? Yup.
plot(imd2015$`Index of Multiple Deprivation (IMD) Score` ~ imd2015$`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`)

#English ethnicity data
#KS201UK for English LSOAs
eth <- read_csv('data/KS201UK_ethnicgroup2011_England_LSOA.csv')

#How many unique LSOAs there...? One per row - more LSOAs than in the IMD data
length(unique(eth$`geography code`))

#Well, we have all the English ones it appears
table(imd2015$`LSOA code (2011)` %in% eth$`geography code`, useNA = 'always')

#Proportion ethnic group non-white
#Sort of tempted to get rural/urban breakdown but let's stick to this for now
# eth <- eth %>% 
#   mutate(prop_nonwhite = rowSums(.[5:14]))

#We already have a sum column there, can use that
eth <- eth %>%
  mutate(
    percent_nonwhite = 
      (
        ((`Ethnic Group: All categories: Ethnic group; measures: Value`-`Ethnic Group: White; measures: Value`)/
           `Ethnic Group: All categories: Ethnic group; measures: Value`) * 100
      ),
    percent_BAC = (
      ((`Ethnic Group: Black / African / Caribbean / Black British; measures: Value`)/
         `Ethnic Group: All categories: Ethnic group; measures: Value`) * 100
    )
  )

# dens(eth$percent_nonwhite)
# hist(eth$percent_nonwhite,breaks=50)


#Deprivation and ethnicity data linked
#Just sticking with full IMD for now
# eth <- eth %>% 
#   select(LSOA = `geography code`,percent_nonwhite)
# 
# imd <- imd2015 %>% 
#   select(LSOA=`LSOA code (2011)`,IMD=`Index of Multiple Deprivation (IMD) Score`)
# 
# imd <- imd %>% 
#   left_join(eth, by = 'LSOA')
# 
# plot(imd$IMD ~ imd$percent_nonwhite)
# imd.sample <- sample_n(imd,1000)
# plot(imd.sample$IMD ~ imd.sample$percent_nonwhite)
#Hmm. Perhaps do have to filter down to urban, though that will still be most


#Or can I get just London? Bit fiddly, it's LA names
#I did actually get that list from somewhere once
library(rvest)

#https://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/
url <- "https://en.wikipedia.org/wiki/List_of_London_boroughs"
LAs <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table()
LAs <- LAs[[1]]

LAs$`Local authority` <- gsub(x = LAs$`Local authority`, pattern = ' London Borough Council', replacement = '')
#"Westminster City Council", should be "Westminster"
LAs$`Local authority` <- gsub(x = LAs$`Local authority`, pattern = ' City Council', replacement = '')
#Tick
table(LAs$`Local authority` %in% unique(imd2015$`Local Authority District name (2013)`))

#Keep just London
imd.london <- imd2015 %>%
  filter(`Local Authority District name (2013)` %in% LAs$`Local authority`) %>% 
  select(LSOA=`LSOA code (2011)`,
         IMD=`Index of Multiple Deprivation (IMD) Score`
         # IMD=`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`
         )

eth <- eth %>% 
    select(LSOA = `geography code`,percent_nonwhite, percent_BAC)

imd.london <- imd.london %>% 
  left_join(eth, by = 'LSOA')

#Yup, that's a lot more clear
plot(imd.london$IMD ~ imd.london$percent_nonwhite)
plot(imd.london$IMD ~ imd.london$percent_BAC)

ggplot(imd.london, aes(x = percent_nonwhite, y = IMD)) +
# ggplot(imd.london, aes(x = percent_BAC, y = IMD)) +
  geom_point(alpha = 0.5) +
  geom_smooth()
  # geom_smooth(method='lm')

saveRDS(imd.london,'data/imd_ethnicity_london.rds')




