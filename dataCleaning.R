library(tidyverse)
library(devtools)
library(reshape2)
library(codyn) # species synchrony
library(lubridate)
library(ncdf4) # package for netcdf manipulation
library(rgdal) # package for geospatial analysis
library(raster)
library(maptools)
library(geodata) # cmip6
library(gstat)
library(sp)


setwd('C:/Users/alexi/OneDrive/Documents/01_GradSchool/_Dissertation work/Chapter4/03_code')
euram <- read.csv("euramv2.csv", header = T, encoding = "UTF-8")
germany <- read.csv("germanyv2.csv", header = T, encoding = "UTF-8")
uk <- read.csv("ukv2.csv", header = T, encoding = "UTF-8")
spain <- read.csv("spainv2.csv", header = T, encoding = "UTF-8")
belgium <- read.csv("belgium.csv", header = T, encoding = "UTF-8")

## combine dataframes
prev <- rbind(belgium, euram, germany, uk)

## remove empty columns & delete irrelevant columns
data.frame(colnames(prev))
prev <- Filter(function(x)!all(is.na(x)), prev)
prev <- prev[-c(3, 21, 24, 27, 30:31, 36, 38:42)]

## Rename countries in the "countries" column based on lat/long coords
prev$country <- gsub(prev$country, pattern = "USA",
                     replacement = "United States")
prev$country <- gsub(prev$country, pattern = "Italy", 
                     replacement = "Switzerland")


## Add empty columns for climactic variables/clean up the df a bit
newcols <- c("ADM0", "ADM1", "ADM2", "susceptibility", "scientific", "nativeStatus",
             "soilMoisture", "tmin", "tmax", "prec", "bio1", "bio2", "bio3", "bio4", 
             "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", 
             "bio13", "bio14", "bio15","bio16", "bio17", "bio18", "bio19")
prev[newcols] <- NA 
names(prev)[names(prev) == 'specificEpithet'] <- 'species' # change name of this column
prev$scientific <- paste(prev$genus, prev$species) # combine genus & species 
data.frame(colnames(prev)) # returns indexed data frame 
prev <- prev[, c(2, 34:36, 4:5, 21:23, 40, 6, 3, 18:19, 8:9, 38, 37, 39,  # reorder columns to make more intuitive
                 26:30, 20, 10:17, 7, 24, 41:62, 25, 1, 31:33)] 

## Add missing columns  
spain_cols <- c("locationRemarks", "occurrenceRemarks", "principalInvestigator",
                "eventRemarks", "expeditionCode", "projectId")
spain[spain_cols] <- NA

## Rearrange columns to match main df
spain <- spain[, c(2:7, 59, 8:9, 32, 1, 10:15, 17, 16, 30:31, 60, 28:29, 27, 
                   19, 21, 20, 22:26, 18, 62, 33:55, 61, 56, 57:58)]

## Add Spain
prev <- rbind(prev, spain)

prev <- prev %>%
  # make sure individualCount is numeric
  mutate(individualCount = as.numeric(individualCount)) %>% 
  # assume all NA values are observations for a single individual
  replace_na(list(individualCount = 1)) %>%
  # replace NA values in diseaseTested with appropriate test
  replace_na(list(diseaseTested = "Bsal")) %>%
  # remove rows with no data
  dplyr::filter(!(materialSampleID=="")) %>%
  # drop rows that include sampling from Peru or the US (imported with euram df)
  dplyr::filter(!(country == "Peru")) %>%
  dplyr::filter(!(country == "United States")) %>%
    # rename elevation column
  rename(Elevation = minimumElevationInMeters)

## Export data frame to work with in QGIS
#write.csv(prev, 'C:/Users/alexi/OneDrive/Documents/01_GradSchool/_Dissertation work/Chapter4/03_code/locations.csv',
#          row.names = FALSE)

## Read in attribute table with ADM data and case match ADM levels in prev
a <- read.csv("location_ADMs.csv", header = T, encoding = "UTF-8")

a2 <- a %>%
  dplyr::select(COUNTRY_2, GID_0, NAME_1, NAME_2, materialSampleID)
colnames(a2) <- c("country", "ADM0", "ADM1", "ADM2", "materialSampleID")

prev$ADM0 = a2$ADM0[base::match(paste(prev$country), 
                                paste(a2$country))]
prev$ADM1 = a2$ADM1[base::match(paste(prev$ADM0, prev$materialSampleID), 
                                paste(a2$ADM0, a2$materialSampleID))]
prev$ADM2 = a2$ADM2[base::match(paste(prev$ADM1, prev$materialSampleID), 
                                paste(a2$ADM1, a2$materialSampleID))]
prev$ADM1 <- toupper(prev$ADM1)
prev$ADM2 <- toupper(prev$ADM2)

prev %>%
  group_by(country) %>%
  summarise(n = n())

## Group sites by unique lat/long combos and assign site #s to them, for all countries excluding Spain
temp <- prev%>%
  dplyr::select(materialSampleID, decimalLatitude, decimalLongitude) %>%
  dplyr::group_by(decimalLatitude, decimalLongitude) %>%
  mutate(Site = cur_group_id()) %>% 
  ungroup()
temp <- temp %>%
  dplyr::select(materialSampleID, Site) 

prev$Site <- NA

prev$Site = temp$Site[base::match(paste(prev$materialSampleID), 
                                      paste(temp$materialSampleID))]



## Add data to the susceptibility column in prev df
## Susceptibility codes (based on coding system from Bosch et al. 2021)
## 1 = resistant
## 2 = tolerant/susceptible
## 3 = lethal
s <- read.csv("susceptibility.csv", header = T, encoding = "UTF-8")
data.frame(colnames(s))
names(s) <- c("order", "family", "genus", "species", "scientific", 
              "susceptibility", "citation")

prev$susceptibility <- s$susceptibility[base::match(prev$scientific, s$scientific)]
## double check there are no NAs
plyr::count(prev, "susceptibility") 
#sus <- prev %>% dplyr::filter(is.na(susceptibility))

#### Calculate abundance, richness, and diversity ####
prev <- unite(prev, c("yearCollected", "monthCollected", "dayCollected"), sep = "-", col = "date", remove = F)

## calculate relative spp richness
spr <- prev %>%
  dplyr::select(Site, date, scientific) %>%
  group_by(Site, date, scientific) %>%
  slice(1) %>% # remove duplicate rows
  ungroup() %>% # ungroup dataframe by aforementioned variables
  mutate(presence = 1) %>% # insert a 1 for presence, 0 for absence
  pivot_wider(names_from = scientific, # convert df to matrix
              values_from = presence, values_fill = 0) %>% 
  group_by(Site, date) %>% 
  summarise_all(sum) %>%
  ungroup() %>%
  mutate(Richness = apply(.[,3:(ncol(.)-1)] > 0, 1, sum))


## Calculate abundance of individual spp at a site during each sampling event
spa <- prev %>%
  dplyr::select(Site, date, scientific, individualCount) # subset relevant data
spa <- aggregate(individualCount ~ scientific+Site+date, spa, sum) # aggregate by Site, date, spp. & summarise
names(spa)[names(spa) == 'individualCount'] <- 'sppAbun'

## Calculate abundance of total spp at a site during each sampling event
SAb <- aggregate(sppAbun ~ Site + date, spa, sum)
names(SAb)[names(SAb) == 'sppAbun'] <- 'siteAbun'
t <- prev
## Add abundance and richness back into prev df
prev <- prev %>%
  # species richness
  left_join(spr[,c(1:2,25)], by = c("Site", "date")) %>%
  # species abundance
  left_join(spa, by = c("scientific", "Site", "date")) %>%
  # site abundance
  left_join(SAb[,c(1:2,3)], by = c("Site", "date")) 


## Calculate community diversity
diversity <- community_diversity(prev, time.var = "date",
                                 abundance.var = "sppAbun",
                                 replicate.var = "Site",
                                 metric = "Shannon")
names(diversity)[names(diversity) == 'Shannon'] <- 'Diversity'

## Add community diversity (Shannon's Index) back into prev df 
prev <- prev %>%
  left_join(diversity, by = c("Site", "date")) %>%
  mutate(alphadiv = Diversity)


## Make sure columns that have categorical data are uniform in coding
prev <- prev %>%
  # code all NA values as 'False'
  mutate(BdDetected = as.factor(BdDetected)) %>%
  replace_na(list(BdDetected = "FALSE")) %>%
  mutate(BsalDetected = as.factor(BsalDetected)) %>%
  replace_na(list(BsalDetected = "FALSE")) %>%
  mutate(fatal = as.factor(fatal))


prev$fatal <- toupper(prev$fatal)
prev$specimenDisposition <- toupper(prev$specimenDisposition)

## Convert factors with two levels to binary integers
levels(prev$BdDetected) <- c(0,1) #0 = F, 1 = T
levels(prev$BsalDetected) <- c(0,1) #0 = F, 1 = T
prev$fatal <- as.factor(prev$fatal)
levels(prev$fatal) <- c(0,1) #0 = F, 1 = T


## Obtain unique lat/long/date combinations to extract weather data
Sys.setenv(TZ = "UTC")
weather <- prev %>%
  dplyr::select(decimalLatitude, decimalLongitude, date, 
                yearCollected, monthCollected, dayCollected) %>%
  dplyr::mutate(date_t1 = NA, date_t2 = NA) %>%
  relocate(c(date_t1, date_t2), .after = date) %>%
  unite(decimalLatitude, decimalLongitude, sep = ", ", col = "LatLon", remove = F) %>%
  relocate(LatLon, .after = decimalLongitude) %>%
  dplyr::filter(!(dayCollected == "NA")) %>%
  mutate(t0_temp = NA, t0_soilMoisture = NA, t0_precip = NA,
         t1_temp = NA, t1_soilMoisture = NA, t1_precip = NA,
         t2_temp = NA, t2_soilMoisture = NA, t2_precip = NA) %>%
  group_by(LatLon, date)

weather <- unique(weather)
weather$date <- as.Date(weather$date, format = "%Y-%m-%d")

## get dates for 1 month prior to sample date
for(i in 1:nrow(weather)){
  weather[i,5] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(30)

## get dates for 2 months prior to sample date
  weather[i,6] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(60)
}


sum(is.na(weather$date)) # 0
sum(is.na(weather$date_t1)) # 0
sum(is.na(weather$date_t2)) # 0

weather <- weather %>%
  group_by(decimalLatitude, decimalLongitude) %>%
  pivot_longer(cols = c(date, date_t1, date_t2),
               names_to = "timepoint",
               values_to = "sampledate") %>%
  ungroup() %>%
  dplyr::select(-c("LatLon", "yearCollected", "monthCollected", "dayCollected")) %>%
  relocate(c("timepoint", "sampledate"), .after = "decimalLongitude") %>%
  separate(sampledate, sep="-", into = c("yearCollected", "monthCollected", "dayCollected"))


## Export for weather data
#write.csv(weather, 'C:/Users/alexi/OneDrive/Documents/01_GradSchool/_Dissertation work/Chapter4/03_code/weather.csv',
#          row.names = FALSE)

## Python 3.9.4 used to download .nc4 files from NASA's EarthData data repository for each date and location.

## Import temperature & soil moisture data from NASA's EarthData website (citation below)
## Li, B., H. Beaudoing, and M. Rodell, NASA/GSFC/HSL (2020), GLDAS Catchment Land Surface Model L4 daily 0.25 x 0.25 degree GRACE-DA1 V2.2, 
## Greenbelt, Maryland, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: 2022-09-08.






## Import precip data from NASA's EarthData website (citation below)
## Huffman, G.J., E.F. Stocker, D.T. Bolvin, E.J. Nelkin, Jackson Tan (2019), GPM IMERG Late Precipitation L3 1 day 0.1 degree x 0.1 degree V06, 
## Edited by Andrey Savtchenko, Greenbelt, MD, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: 2022-09-08, 
##10.5067/GPM/IMERGDL/DAY/06












#### cbind model df
## Create two new columns for Bsal detection successes/failures at each site, 
## for each species, during each sampling event
#prev$genus <- gsub("[[:space:]]", "", prev$genus) # get rid of weird spaces in this column

#disease <- prev %>%
#  dplyr::select(country, decimalLatitude, decimalLongitude, Site, sppAbun, siteAbun,
#                richness, alphadiv, date, yearCollected, monthCollected, dayCollected,
#                genus, species, scientific, susceptibility, diseaseDetected, tmin, tmax, 
#                prec, bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, bio11, 
#                bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19) %>%
#  group_by(Site, date, scientific) %>%
#  mutate(NoBsal = sum(diseaseDetected == 0)) %>%
#  mutate(YesBsal = sum(diseaseDetected == 1)) %>%
#  distinct()


#data.frame(colnames(disease))
#disease <- disease[,c(1:17, 40:41, 18:39)]

## File for cbind model:
#write.csv(disease, 'C:/Users/alexi/OneDrive/Documents/01_GradSchool/_Dissertation work/Chapter4/03_code/bsal_analysis.csv',
#          row.names = FALSE)




