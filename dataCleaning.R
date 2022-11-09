library(tidyverse)
library(devtools)
library(reshape2)
library(janitor) # for reviewing duplicates
library(codyn) # species synchrony
library(lubridate)
library(ncdf4) # package for netcdf manipulation
library(rgdal) # package for geospatial analysis
library(raster) # package for raster manipulation
library(maptools) # package to create maps
library(geodata) # cmip6; projected climate data
library(gstat) 
library(sp)
library(fs) # construct relative paths to files/directories


setwd('C:/Users/alexi/OneDrive/Documents/01_GradSchool/_DissertationWork/Chapter4/03_code')
euram <- read.csv("euram.csv", header = T, encoding = "UTF-8")
germany <- read.csv("germany.csv", header = T, encoding = "UTF-8")
uk <- read.csv("uk.csv", header = T, encoding = "UTF-8")
spain <- read.csv("spain.csv", header = T, encoding = "UTF-8")
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
             "tmin", "tmax", "prec", "bio1", "bio2", "bio3", "bio4", 
             "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", 
             "bio13", "bio14", "bio15","bio16", "bio17", "bio18", "bio19")
prev[newcols] <- NA 
names(prev)[names(prev) == 'specificEpithet'] <- 'species' # change name of this column
prev$scientific <- paste(prev$genus, prev$species) # combine genus & species 
data.frame(colnames(prev)) # returns indexed data frame 
prev <- prev[, c(2, 34:36, 4:5, 21:23, 6, 3, 18:19, 8:9, 38, 37, 39,  # reorder columns to make more intuitive
                 26:30, 20, 10:17, 7, 24, 40:61, 25, 1, 31:33)] 

## Add missing columns  
spain_cols <- c("locationRemarks", "occurrenceRemarks", "principalInvestigator",
                "eventRemarks", "expeditionCode", "projectId")
spain[spain_cols] <- NA

## Rearrange columns to match main df
spain <- spain[, c(2:7, 59, 8:9, 32, 1, 10:15, 17, 16, 30:31, 60, 28:29, 27, 
                   19, 21, 20, 22:26, 18, 62, 33:55, 61, 56, 57:58)]
spain <- spain[, -10]
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
a <- read.csv("locations_with_ADMs.csv", header = T, encoding = "UTF-8")

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
  mutate(richness = apply(.[,3:(ncol(.)-1)] > 0, 1, sum))


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
  rename(alphadiv = "Diversity")


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
  mutate(temp = NA, soilMoisture = NA) %>%
  group_by(LatLon, date) %>%
  unique() %>%
  ungroup()

weather$date <- as.Date(weather$date, format = "%Y-%m-%d")

## get dates for 1 month prior to sample date
for(i in 1:nrow(weather)){
  weather[i,5] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(30)

## get dates for 2 months prior to sample date
  weather[i,6] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(60)
}

# Add dates t-1 and t-2 back into prev dataframe
prev <- prev %>%
  mutate(date = base::as.Date(date, "%Y-%m-%d")) %>%
  left_join(weather[, c(1:2, 4:6)], by = c("decimalLatitude", "decimalLongitude", "date")) %>%
  relocate(c("date_t1", "date_t2"), .after = date)


weather2 <- weather %>%
  group_by(decimalLatitude, decimalLongitude) %>%
  pivot_longer(cols = c(date, date_t1, date_t2),
               names_to = "timepoint",
               values_to = "date") %>%
  ungroup() %>%
  dplyr::select(-c("LatLon", "yearCollected", "monthCollected", "dayCollected")) %>%
  relocate(c("timepoint", "date"), .after = "decimalLongitude") %>%
  unite(decimalLatitude, decimalLongitude, sep = ", ", col = "LatLon", remove = F)


## Export to use in Python and PyQGIS to obtain weather data
#write.csv(weather2, 'C:/Users/alexi/OneDrive/Documents/01_GradSchool/_Dissertation work/Chapter4/03_code/weather.csv',
#          row.names = FALSE)

## Python 3.9.4 used to download .nc4 files from NASA's EarthData data repository for each date and location.

## Import temperature & soil moisture data from NASA's EarthData website (citation below)
   ## Li, B., H. Beaudoing, and M. Rodell, NASA/GSFC/HSL (2020), GLDAS Catchment Land Surface Model L4 daily 0.25 x 0.25 degree GRACE-DA1 V2.2, 
   ## Greenbelt, Maryland, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: 2022-09-08.
gldas <- read.csv("weather_merged.csv", header = T, encoding = "UTF-8")

gldas <- gldas %>%
  unite(c("yearCollected", "monthCollected", "dayCollected"), sep = "-", col = "date", remove = F) %>%
  rename(soilMoisture = "SOILMOIST_kgm.21") %>%
  rename(temp = "SURFTEMP_K1") %>%
  dplyr::select(decimalLatitude, decimalLongitude, timepoint, date, soilMoisture, temp) %>%
  unite(decimalLatitude, decimalLongitude, sep = ", ", col = "LatLon", remove = F) 


# Copy separate temp and soilMoisture
gldas2 <- gldas
gldas2 <- gldas[!(is.na(gldas2$temp)|gldas$temp ==""),]

# Add temp data back into gldas df
gldas$temp = gldas2$temp[base::match(paste(gldas$decimalLatitude, gldas$decimalLongitude, gldas$date),
                                     paste(gldas2$decimalLatitude, gldas2$decimalLongitude, gldas2$date))]

gldas <- gldas %>%
  na.omit() %>%
  mutate(row = row_number()) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::group_by(decimalLatitude, decimalLongitude, timepoint) %>%
  pivot_wider(names_from = timepoint, values_from = date)
  
  
# separate data by timepoint
gldas_date <- gldas %>% # 809 entries -- some missing
  dplyr::select(LatLon, decimalLatitude, decimalLongitude, soilMoisture, temp, date) %>%
  na.omit
gldas_date_t1 <- gldas %>% # 804 entries -- some missing
  dplyr::select(LatLon, decimalLatitude, decimalLongitude, soilMoisture, temp, date_t1) %>%
  na.omit
gldas_date_t2 <- gldas %>% # 805 entries -- some missing
  dplyr::select(LatLon, decimalLatitude, decimalLongitude, soilMoisture, temp, date_t2) %>%
  na.omit

## Add back into weather df to prepare to left_join back into prev df
weather <- weather[, -c(7:11)]
weather <- weather %>%
  dplyr::mutate(soilMoisture_date = NA, soilMoisture_date_t1 = NA, soilMoisture_date_t2 = NA,
                temp_date = NA, temp_date_t1 = NA, temp_date_t2 = NA)

# Soil Moisture (kg/m^2)
weather$soilMoisture_date = gldas_date$soilMoisture[base::match(paste(weather$LatLon, weather$date),
                                                           paste(gldas_date$LatLon, gldas_date$date))]
weather$soilMoisture_date_t1 = gldas_date_t1$soilMoisture[base::match(paste(weather$LatLon, weather$date_t1),
                                                              paste(gldas_date_t1$LatLon, gldas_date_t1$date_t1))]
weather$soilMoisture_date_t2 = gldas_date_t2$soilMoisture[base::match(paste(weather$LatLon, weather$date_t2),
                                                                      paste(gldas_date_t2$LatLon, gldas_date_t2$date_t2))]

# Temperature (K)
weather$temp_date = gldas_date$temp[base::match(paste(weather$LatLon, weather$date),
                                      paste(gldas_date$LatLon, gldas_date$date))]
weather$temp_date_t1 = gldas_date_t1$temp[base::match(paste(weather$LatLon, weather$date_t1),
                                      paste(gldas_date_t1$LatLon, gldas_date_t1$date_t1))]
weather$temp_date_t2 = gldas_date_t2$temp[base::match(paste(weather$LatLon, weather$date_t2),
                                      paste(gldas_date_t2$LatLon, gldas_date_t2$date_t2))]


## gldas missing timepoints w/ same lat/lon/date -- issue with datasetFetcher.py?
## Subset missing data
#SM_date <- weather[is.na(c(weather$soilMoisture_date)),]
#SM_date_t1 <- weather[is.na(c(weather$soilMoisture_date_t1)),]
#SM_date_t2 <- weather[is.na(c(weather$soilMoisture_date_t2)),]

#T_date <- weather[is.na(c(weather$temp_date)),]
#T_date_t1 <- weather[is.na(c(weather$temp_date_t1)),]
#T_date_t2 <- weather[is.na(c(weather$temp_date_t2)),]

#View(SM_date)
#View(SM_date_t1)
#View(SM_date_t2)
#View(T_date)
#View(T_date_t1)
#View(T_date_t2)

## Import precip data from NASA's EarthData website (citation below)
## Huffman, G.J., E.F. Stocker, D.T. Bolvin, E.J. Nelkin, Jackson Tan (2019), GPM IMERG Late Precipitation L3 1 day 0.1 degree x 0.1 degree V06, 
## Edited by Andrey Savtchenko, Greenbelt, MD, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: 2022-09-08, 
##10.5067/GPM/IMERGDL/DAY/06

# Create file path to where nc4 files are at (Work in progress)
#imerg_path <- path('E:/', '01_GradSchool', '_DissertationWork', 'Chapter4', '03_code', 'Weather','sample_data')

# 


#imerg <- nc_open('gimms3g_ndvi_1982-2012.nc4')
# Save the print(nc) dump to a text file
#{
#  sink('gimms3g_ndvi_1982-2012_metadata.txt')
#  print(nc_data)
#  sink()
#}

## Add weather data to main dataframe
prev$soilMoisture_date = weather$soilMoisture_date[base::match(paste(prev$decimalLatitude, prev$decimalLongitude, prev$date),
                                                               paste(weather$decimalLatitude, weather$decimalLongitude, weather$date))]
prev$soilMoisture_date_t1 = weather$soilMoisture_date_t1[base::match(paste(prev$decimalLatitude, prev$decimalLongitude, prev$date_t1),
                                                                     paste(weather$decimalLatitude, weather$decimalLongitude, weather$date_t1))]
prev$soilMoisture_date_t2 = weather$soilMoisture_date_t2[base::match(paste(prev$decimalLatitude, prev$decimalLongitude, prev$date_t2),
                                                                     paste(weather$decimalLatitude, weather$decimalLongitude, weather$date_t2))]

prev$temp_date = weather$temp_date[base::match(paste(prev$decimalLatitude, prev$decimalLongitude, prev$date),
                                                               paste(weather$decimalLatitude, weather$decimalLongitude, weather$date))]
prev$temp_date_t1 = weather$temp_date_t1[base::match(paste(prev$decimalLatitude, prev$decimalLongitude, prev$date_t1),
                                                                     paste(weather$decimalLatitude, weather$decimalLongitude, weather$date_t1))]
prev$temp_date_t2 = weather$temp_date_t2[base::match(paste(prev$decimalLatitude, prev$decimalLongitude, prev$date_t2),
                                                                     paste(weather$decimalLatitude, weather$decimalLongitude, weather$date_t2))]


prev <- prev[, -c(7:9, 24, 37)]
prev <- prev[, c(1:6, 60, 11:13, 8:10, 7, 14:32, 61:70, 33:59)]

# Convert temp from K to C
prev$temp_date <- prev$temp_date - 273.15
prev$temp_date_t1 <- prev$temp_date_t1 - 273.15
prev$temp_date_t2 <- prev$temp_date_t2 - 273.15


# Get rid of climate data for now
#prev <- prev[, -c(46:66)]

#### cbind model df
## Create two new columns for Bsal detection successes/failures at each site, 
## for each species, during each sampling event
prev$genus <- gsub("[[:space:]]", "", prev$genus) # get rid of weird spaces in this column

disease <- prev %>%
  dplyr::select(country, decimalLatitude, decimalLongitude, Site, yearCollected, 
                monthCollected, dayCollected, date, date_t1, date_t2,  
                genus, species, scientific, susceptibility, BdDetected, BsalDetected, fatal, 
                sppAbun, siteAbun, richness, alphadiv, 
                soilMoisture_date, soilMoisture_date_t1, soilMoisture_date_t2, 
                temp_date, temp_date_t1, temp_date_t2) %>%
  group_by(Site, date, scientific) %>%
  distinct()


data.frame(colnames(disease))

## File for final prev dataframe:
#write.csv(prev, 'C:/Users/alexi/OneDrive/Documents/01_GradSchool/_DissertationWork/Chapter4/03_code/bsalData_clean.csv',
#          row.names = FALSE)

## File for cbind model:
#write.csv(disease, 'C:/Users/alexi/OneDrive/Documents/01_GradSchool/_DissertationWork/Chapter4/03_code/bsalData_cbind.csv',
#          row.names = FALSE)












