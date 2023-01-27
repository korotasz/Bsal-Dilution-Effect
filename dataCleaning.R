library(tidyverse)
library(devtools)
library(reshape2)
library(codyn) # species synchrony
library(rgdal) # package for geospatial analysis
library(lubridate)
library(raster) # package for raster manipulation
library(ncdf4) # for IMERG satellite data
library(maptools) # package to create maps
library(geodata) # cmip6; projected climate data
library(gstat)
library(sp)
library(sf)
library(fs) # construct relative paths to files/directories



dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
csvpath <- (path.expand("/csvFiles"))
setwd(file.path(dir, csvpath))

euram <- read.csv("euram.csv", header = T, encoding = "UTF-8")
germany <- read.csv("germany.csv", header = T, encoding = "UTF-8")
uk <- read.csv("uk.csv", header = T, encoding = "UTF-8")
spain <- read.csv("spain.csv", header = T, encoding = "UTF-8")
belgium <- read.csv("belgium.csv", header = T, encoding = "UTF-8")

## Combine dataframes
prev <- rbind(belgium, euram, germany, uk)

## Remove empty columns
prev <- Filter(function(x)!all(is.na(x)), prev)

prev <- prev %>%
  mutate_all(na_if, "") %>%
  # fieldNumber and lifeStage both have life stage data -- combine
  unite(., col = "lifeStage_merged", c("fieldNumber", "lifeStage"), 
        sep = "/", remove = TRUE, na.rm = TRUE) %>%
  rename(lifeStage = lifeStage_merged) %>%
  # taxonRemarks and sex both have life stage data -- combine
  unite(., col = "sex_merged", c("taxonRemarks", "sex"),
        sep = "/", remove = TRUE, na.rm = TRUE) %>%
  rename(sex = sex_merged) %>%
  # Rename countries in the "countries" column based on lat/long coords
  mutate(country = dplyr::recode(country,
                          USA = "United States",
                          Italy = "Switzerland")) %>%
  # Change name of these columns
  rename(species = specificEpithet,
         sampleRemarks = eventRemarks,
         specimenFate = specimenDisposition) %>%
  # Combine genus & species into one new column 
  unite(., col = "scientific", c("genus", "species"),
        sep = " ", remove = FALSE) %>%
  relocate(scientific, .after = species) %>%
  # Add empty columns for new variables 
  mutate(ADM0 = NA, ADM1 = NA, ADM2 = NA, susceptibility = NA, nativeStatus = NA)

## Delete irrelevant columns
data.frame(colnames(prev)) # returns indexed data frame 
prev <- prev[-c(3, 22, 24:25, 26:28, 33, 35, 37:42)]

## Rearrange dataframe
data.frame(colnames(prev)) 
prev <- prev[, c(2, 30:32, 4:5, 3, 19:20, 6, 8:10, 33:34, 24:26, 
                 13:18, 27, 7, 11:12, 21:22, 1, 23, 28:29)]


## Remove columns from Spain
data.frame(colnames(spain)) 
spain <- spain[, -c(8:9, 32:54, 56)]


## Rearrange & Add missing columns
spain <- spain[, c(2, 3:10, 1, 11:13, 15, 14, 27:29, 26, 18, 
                   20:24, 16, 17, 19, 25, 30:32)]
spain <- spain %>%
  mutate_all(na_if, "") %>%
  rename(specimenFate = specimenDisposition) %>%
  mutate(sampleRemarks = NA, principalInvestigator = "An Martel") %>%
  relocate(c("sampleRemarks", "principalInvestigator"), .after = "diagnosticLab")
  
## Join Spain to main df
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
  dplyr::filter(!(country == "United States"))

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
  mutate(richness = apply(.[,3:(ncol(.))] > 0, 1, sum))

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
prev$specimenFate <- toupper(prev$specimenFate)

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

  
weather <- unique(weather)
weather$date <- base::as.Date(weather$date, format = "%Y-%m-%d")

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


## Import precip data from NASA's EarthData website (citation below)
## Huffman, G.J., E.F. Stocker, D.T. Bolvin, E.J. Nelkin, Jackson Tan (2019), GPM IMERG Late Precipitation L3 1 day 0.1 degree x 0.1 degree V06, 
## Edited by Andrey Savtchenko, Greenbelt, MD, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: 2022-09-08, 
##10.5067/GPM/IMERGDL/DAY/06
# Write function for processing multiple .nc4 files





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

prev <- prev[, c(1:6, 38, 10:12, 7:9, 13:29, 39:48, 30:37)]

# Convert temp from K to C
prev$temp_date <- prev$temp_date - 273.15
prev$temp_date_t1 <- prev$temp_date_t1 - 273.15
prev$temp_date_t2 <- prev$temp_date_t2 - 273.15

# Combine BdDetected and BsalDetected into one "diseaseDetected" column
prev <- prev %>%
  unite(., col = "diseaseDetected", c("BdDetected", "BsalDetected"),
        sep = "/", remove = FALSE, na.rm = TRUE) %>%
  relocate(diseaseDetected, .before = fatal) %>%
  drop_na(diseaseDetected) %>%
  mutate(whichDisease = diseaseDetected,
         whichDisease = dplyr::recode(whichDisease,
                                      "0/0" = "None",
                                      "0/1" = "Bsal",
                                      "1/0" = "Bd",
                                      "1/1" = "Both"),
         diseaseDetected = dplyr::recode(diseaseDetected,
                                         "0/0" = "0",
                                         "0/1" = "1",
                                         "1/0" = "1",
                                         "1/1" = "1"),
         diseaseDetected = as.factor(diseaseDetected)) %>%
  relocate(whichDisease, .after = diseaseDetected)


## Convert characters to factors with two levels to binary integers
prev$diseaseDetected <- as.factor(prev$diseaseDetected)
#levels(prev$diseaseDetected) <- c(0,1) #0 = F, 1 = T


## Climate data from geodata package
#### Obtain borders of each ADM2 as a geometry ####
## Subset unique values for ADM0, ADM1, and ADM2 levels from prev df
temp <- prev %>%
  dplyr::select(ADM0, ADM1, ADM2)
temp <- unique(temp)
temp <- with(temp, temp[order(ADM0, ADM1, ADM2) , ])

CHE <- temp %>% # Switzerland
  dplyr::group_by(ADM0) %>% 
  filter(ADM0 == "CHE")
DEU <- temp %>% # Germany
  dplyr::group_by(ADM0) %>% 
  filter(ADM0 == "DEU")
GBR <- temp %>% # UK
  dplyr::group_by(ADM0) %>% 
  filter(ADM0 == "GBR")
ESP <- temp %>% # Spain
  dplyr::group_by(ADM0) %>%
  filter(ADM0 == "ESP")
BEL <- temp %>% # Belgium
  dplyr::group_by(ADM0) %>%
  filter(ADM0 == "BEL")


## Construct file path to store WorldClim Data
wclim_path <- (path.expand("/WorldClim"))
setwd(file.path(dir, wclim_path))

CHEadm2 <- geodata::gadm(country = CHE$ADM0, path = file.path(dir, wclim_path), level = 2, version = "latest")
CHEadm2 <- sf::st_as_sf(CHEadm2)
  
DEUadm2 <- geodata::gadm(country = DEU$ADM0, path = file.path(dir, wclim_path), level = 2, version = "latest")
DEUadm2 <- sf::st_as_sf(DEUadm2)
  
GBRadm2 <- geodata::gadm(country = GBR$ADM0, path = file.path(dir, wclim_path), level = 2, version = "latest")
GBRadm2 <- sf::st_as_sf(GBRadm2)
  
ESPadm2 <- geodata::gadm(country = ESP$ADM0, path = file.path(dir, wclim_path), level = 2, version = "latest")
ESPadm2 <- sf::st_as_sf(ESPadm2)
  
BELadm2 <- geodata::gadm(country = BEL$ADM0, path = file.path(dir, wclim_path), level = 2, version = "latest")
BELadm2 <- sf::st_as_sf(BELadm2)

#### Obtain WorldClim data as SpatVector layers ####
tmin <- geodata::worldclim_global(var = 'tmin', path = file.path(dir, wclim_path), res = 2.5, version = "2.1")
tmin <- as(tmin, "Raster") # convert SpatRaster to Rasterstack

tmax <- geodata::worldclim_global(var = 'tmax', path = file.path(dir, wclim_path), res = 2.5, version = "2.1")
tmax <- as(tmax, "Raster") # convert SpatRaster to Rasterstack

tavg <- geodata::worldclim_global(var = 'tavg', path = file.path(dir, wclim_path), res = 2.5, version = "2.1")
tavg <- as(tavg, "Raster") # convert SpatRaster to Rasterstack

prec <- geodata::worldclim_global(var = 'prec', path = file.path(dir, wclim_path), res = 2.5, version = "2.1")
prec <- as(prec, "Raster") # convert SpatRaster to Rasterstack
gain(prec) = 0.1 # convert to cm

bio <- geodata::worldclim_global(var = 'bio', path = file.path(dir, wclim_path), res = 2.5, version = "2.1")
bio <- as(bio, "Raster") # convert SpatRaster to Rasterstack


###### Use ADM geometries to sample WorldClim rasters 
#### tmin ####
### CHE
r1 <- crop(tmin, CHEadm2) 
r_tmin1 <- mask(r1, CHEadm2)
rsp_tmin1 <- as.data.frame(raster::extract(x = r_tmin1, y = CHEadm2, fun = mean, sp = T))
rsp_tmin1 <- rsp_tmin1 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_tmin_01, wc2.1_2.5m_tmin_02,
                wc2.1_2.5m_tmin_03, wc2.1_2.5m_tmin_04, wc2.1_2.5m_tmin_05, wc2.1_2.5m_tmin_06,
                wc2.1_2.5m_tmin_07, wc2.1_2.5m_tmin_08, wc2.1_2.5m_tmin_09, wc2.1_2.5m_tmin_10,
                wc2.1_2.5m_tmin_11,wc2.1_2.5m_tmin_12)
colnames(rsp_tmin1) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar", 
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
                         "Dec")
rsp_tmin1 <- reshape(rsp_tmin1, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "tmin",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_tmin1$ADM1 <- toupper(rsp_tmin1$ADM1)
rsp_tmin1$ADM2 <- toupper(rsp_tmin1$ADM2)

### DEU
r2 <- crop(tmin, DEUadm2) 
r_tmin2 <- mask(r2, DEUadm2)
rsp_tmin2 <- as.data.frame(raster::extract(x = r_tmin2, y = DEUadm2, fun = mean, sp = T))
rsp_tmin2 <- rsp_tmin2 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_tmin_01, wc2.1_2.5m_tmin_02,
                wc2.1_2.5m_tmin_03, wc2.1_2.5m_tmin_04, wc2.1_2.5m_tmin_05, wc2.1_2.5m_tmin_06,
                wc2.1_2.5m_tmin_07, wc2.1_2.5m_tmin_08, wc2.1_2.5m_tmin_09, wc2.1_2.5m_tmin_10,
                wc2.1_2.5m_tmin_11,wc2.1_2.5m_tmin_12)
colnames(rsp_tmin2) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_tmin2 <- reshape(rsp_tmin2, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "tmin",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_tmin2$ADM1 <- toupper(rsp_tmin2$ADM1)
rsp_tmin2$ADM2 <- toupper(rsp_tmin2$ADM2)

### GBR
r3 <- crop(tmin, GBRadm2) 
r_tmin3 <- mask(r3, GBRadm2)
rsp_tmin3 <- as.data.frame(raster::extract(x = r_tmin3, y = GBRadm2, fun = mean, sp = T))
rsp_tmin3 <- rsp_tmin3 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_tmin_01, wc2.1_2.5m_tmin_02,
                wc2.1_2.5m_tmin_03, wc2.1_2.5m_tmin_04, wc2.1_2.5m_tmin_05, wc2.1_2.5m_tmin_06,
                wc2.1_2.5m_tmin_07, wc2.1_2.5m_tmin_08, wc2.1_2.5m_tmin_09, wc2.1_2.5m_tmin_10,
                wc2.1_2.5m_tmin_11,wc2.1_2.5m_tmin_12)
colnames(rsp_tmin3) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_tmin3 <- reshape(rsp_tmin3, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "tmin",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_tmin3$ADM1 <- toupper(rsp_tmin3$ADM1)
rsp_tmin3$ADM2 <- toupper(rsp_tmin3$ADM2)

### ESP
r4 <- crop(tmin, ESPadm2) 
r_tmin4 <- mask(r4, ESPadm2)
rsp_tmin4 <- as.data.frame(raster::extract(x = r_tmin4, y = ESPadm2, fun = mean, sp = T))
rsp_tmin4 <- rsp_tmin4 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_tmin_01, wc2.1_2.5m_tmin_02,
                wc2.1_2.5m_tmin_03, wc2.1_2.5m_tmin_04, wc2.1_2.5m_tmin_05, wc2.1_2.5m_tmin_06,
                wc2.1_2.5m_tmin_07, wc2.1_2.5m_tmin_08, wc2.1_2.5m_tmin_09, wc2.1_2.5m_tmin_10,
                wc2.1_2.5m_tmin_11,wc2.1_2.5m_tmin_12)
colnames(rsp_tmin4) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_tmin4 <- reshape(rsp_tmin4, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "tmin",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_tmin4$ADM1 <- toupper(rsp_tmin4$ADM1)
rsp_tmin4$ADM2 <- toupper(rsp_tmin4$ADM2)

### BEL
r5 <- crop(tmin, BELadm2) 
r_tmin5 <- mask(r5, BELadm2)
rsp_tmin5 <- as.data.frame(raster::extract(x = r_tmin5, y = BELadm2, fun = mean, sp = T))
rsp_tmin5 <- rsp_tmin5 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_tmin_01, wc2.1_2.5m_tmin_02,
                wc2.1_2.5m_tmin_03, wc2.1_2.5m_tmin_04, wc2.1_2.5m_tmin_05, wc2.1_2.5m_tmin_06,
                wc2.1_2.5m_tmin_07, wc2.1_2.5m_tmin_08, wc2.1_2.5m_tmin_09, wc2.1_2.5m_tmin_10,
                wc2.1_2.5m_tmin_11,wc2.1_2.5m_tmin_12)
colnames(rsp_tmin5) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_tmin5 <- reshape(rsp_tmin5, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "tmin",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_tmin5$ADM1 <- toupper(rsp_tmin5$ADM1)
rsp_tmin5$ADM2 <- toupper(rsp_tmin5$ADM2)


rsp_tmin <- rbind(rsp_tmin1, rsp_tmin2, rsp_tmin3, rsp_tmin4, rsp_tmin5)
rsp_tmin$monthCollected <- match(rsp_tmin$monthCollected, month.abb)

#### tmax ####
### CHE
r1 <- crop(tmax, CHEadm2) 
r_tmax1 <- mask(r1, CHEadm2)
rsp_tmax1 <- as.data.frame(raster::extract(x = r_tmax1, y = CHEadm2, fun = mean, sp = T))
rsp_tmax1 <- rsp_tmax1 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_tmax_01, wc2.1_2.5m_tmax_02,
                wc2.1_2.5m_tmax_03, wc2.1_2.5m_tmax_04, wc2.1_2.5m_tmax_05, wc2.1_2.5m_tmax_06,
                wc2.1_2.5m_tmax_07, wc2.1_2.5m_tmax_08, wc2.1_2.5m_tmax_09, wc2.1_2.5m_tmax_10,
                wc2.1_2.5m_tmax_11,wc2.1_2.5m_tmax_12)
colnames(rsp_tmax1) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar", 
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
                         "Dec")
rsp_tmax1 <- reshape(rsp_tmax1, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "tmax",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_tmax1$ADM1 <- toupper(rsp_tmax1$ADM1)
rsp_tmax1$ADM2 <- toupper(rsp_tmax1$ADM2)

### DEU
r2 <- crop(tmax, DEUadm2) 
r_tmax2 <- mask(r2, DEUadm2)
rsp_tmax2 <- as.data.frame(raster::extract(x = r_tmax2, y = DEUadm2, fun = mean, sp = T))
rsp_tmax2 <- rsp_tmax2 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_tmax_01, wc2.1_2.5m_tmax_02,
                wc2.1_2.5m_tmax_03, wc2.1_2.5m_tmax_04, wc2.1_2.5m_tmax_05, wc2.1_2.5m_tmax_06,
                wc2.1_2.5m_tmax_07, wc2.1_2.5m_tmax_08, wc2.1_2.5m_tmax_09, wc2.1_2.5m_tmax_10,
                wc2.1_2.5m_tmax_11,wc2.1_2.5m_tmax_12)
colnames(rsp_tmax2) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_tmax2 <- reshape(rsp_tmax2, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "tmax",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_tmax2$ADM1 <- toupper(rsp_tmax2$ADM1)
rsp_tmax2$ADM2 <- toupper(rsp_tmax2$ADM2)

### GBR
r3 <- crop(tmax, GBRadm2) 
r_tmax3 <- mask(r3, GBRadm2)
rsp_tmax3 <- as.data.frame(raster::extract(x = r_tmax3, y = GBRadm2, fun = mean, sp = T))
rsp_tmax3 <- rsp_tmax3 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_tmax_01, wc2.1_2.5m_tmax_02,
                wc2.1_2.5m_tmax_03, wc2.1_2.5m_tmax_04, wc2.1_2.5m_tmax_05, wc2.1_2.5m_tmax_06,
                wc2.1_2.5m_tmax_07, wc2.1_2.5m_tmax_08, wc2.1_2.5m_tmax_09, wc2.1_2.5m_tmax_10,
                wc2.1_2.5m_tmax_11,wc2.1_2.5m_tmax_12)
colnames(rsp_tmax3) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_tmax3 <- reshape(rsp_tmax3, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "tmax",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_tmax3$ADM1 <- toupper(rsp_tmax3$ADM1)
rsp_tmax3$ADM2 <- toupper(rsp_tmax3$ADM2)


### ESP
r4 <- crop(tmax, ESPadm2) 
r_tmax4 <- mask(r4, ESPadm2)
rsp_tmax4 <- as.data.frame(raster::extract(x = r_tmax4, y = ESPadm2, fun = mean, sp = T))
rsp_tmax4 <- rsp_tmax4 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_tmax_01, wc2.1_2.5m_tmax_02,
                wc2.1_2.5m_tmax_03, wc2.1_2.5m_tmax_04, wc2.1_2.5m_tmax_05, wc2.1_2.5m_tmax_06,
                wc2.1_2.5m_tmax_07, wc2.1_2.5m_tmax_08, wc2.1_2.5m_tmax_09, wc2.1_2.5m_tmax_10,
                wc2.1_2.5m_tmax_11,wc2.1_2.5m_tmax_12)
colnames(rsp_tmax4) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_tmax4 <- reshape(rsp_tmax4, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "tmax",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_tmax4$ADM1 <- toupper(rsp_tmax4$ADM1)
rsp_tmax4$ADM2 <- toupper(rsp_tmax4$ADM2)

### BEL
r5 <- crop(tmax, BELadm2) 
r_tmax5 <- mask(r5, BELadm2)
rsp_tmax5 <- as.data.frame(raster::extract(x = r_tmax5, y = BELadm2, fun = mean, sp = T))
rsp_tmax5 <- rsp_tmax5 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_tmax_01, wc2.1_2.5m_tmax_02,
                wc2.1_2.5m_tmax_03, wc2.1_2.5m_tmax_04, wc2.1_2.5m_tmax_05, wc2.1_2.5m_tmax_06,
                wc2.1_2.5m_tmax_07, wc2.1_2.5m_tmax_08, wc2.1_2.5m_tmax_09, wc2.1_2.5m_tmax_10,
                wc2.1_2.5m_tmax_11,wc2.1_2.5m_tmax_12)
colnames(rsp_tmax5) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_tmax5 <- reshape(rsp_tmax5, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "tmax",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_tmax5$ADM1 <- toupper(rsp_tmax5$ADM1)
rsp_tmax5$ADM2 <- toupper(rsp_tmax5$ADM2)


rsp_tmax <- rbind(rsp_tmax1, rsp_tmax2, rsp_tmax3, rsp_tmax4, rsp_tmax5)
rsp_tmax$monthCollected <- match(rsp_tmax$monthCollected, month.abb)



#### tavg ####
### CHE
r1 <- crop(tavg, CHEadm2) 
r_tavg1 <- mask(r1, CHEadm2)
rsp_tavg1 <- as.data.frame(raster::extract(x = r_tavg1, y = CHEadm2, fun = mean, sp = T))
rsp_tavg1 <- rsp_tavg1 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_tavg_01, wc2.1_2.5m_tavg_02,
                wc2.1_2.5m_tavg_03, wc2.1_2.5m_tavg_04, wc2.1_2.5m_tavg_05, wc2.1_2.5m_tavg_06,
                wc2.1_2.5m_tavg_07, wc2.1_2.5m_tavg_08, wc2.1_2.5m_tavg_09, wc2.1_2.5m_tavg_10,
                wc2.1_2.5m_tavg_11,wc2.1_2.5m_tavg_12)
colnames(rsp_tavg1) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar", 
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
                         "Dec")
rsp_tavg1 <- reshape(rsp_tavg1, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "tavg",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_tavg1$ADM1 <- toupper(rsp_tavg1$ADM1)
rsp_tavg1$ADM2 <- toupper(rsp_tavg1$ADM2)

### DEU
r2 <- crop(tavg, DEUadm2)
r_tavg2 <- mask(r2, DEUadm2)
rsp_tavg2 <- as.data.frame(raster::extract(x = r_tavg2, y = DEUadm2, fun = mean, sp = T))
rsp_tavg2 <- rsp_tavg2 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_tavg_01, wc2.1_2.5m_tavg_02,
                wc2.1_2.5m_tavg_03, wc2.1_2.5m_tavg_04, wc2.1_2.5m_tavg_05, wc2.1_2.5m_tavg_06,
                wc2.1_2.5m_tavg_07, wc2.1_2.5m_tavg_08, wc2.1_2.5m_tavg_09, wc2.1_2.5m_tavg_10,
                wc2.1_2.5m_tavg_11,wc2.1_2.5m_tavg_12)
colnames(rsp_tavg2) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_tavg2 <- reshape(rsp_tavg2, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "tavg",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_tavg2$ADM1 <- toupper(rsp_tavg2$ADM1)
rsp_tavg2$ADM2 <- toupper(rsp_tavg2$ADM2)

### GBR
r3 <- crop(tavg, GBRadm2) 
r_tavg3 <- mask(r3, GBRadm2)
rsp_tavg3 <- as.data.frame(raster::extract(x = r_tavg3, y = GBRadm2, fun = mean, sp = T))
rsp_tavg3 <- rsp_tavg3 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_tavg_01, wc2.1_2.5m_tavg_02,
                wc2.1_2.5m_tavg_03, wc2.1_2.5m_tavg_04, wc2.1_2.5m_tavg_05, wc2.1_2.5m_tavg_06,
                wc2.1_2.5m_tavg_07, wc2.1_2.5m_tavg_08, wc2.1_2.5m_tavg_09, wc2.1_2.5m_tavg_10,
                wc2.1_2.5m_tavg_11,wc2.1_2.5m_tavg_12)
colnames(rsp_tavg3) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_tavg3 <- reshape(rsp_tavg3, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "tavg",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_tavg3$ADM1 <- toupper(rsp_tavg3$ADM1)
rsp_tavg3$ADM2 <- toupper(rsp_tavg3$ADM2)

### ESP
r4 <- crop(tavg, ESPadm2)
r_tavg4 <- mask(r4, ESPadm2)
rsp_tavg4 <- as.data.frame(raster::extract(x = r_tavg4, y = ESPadm2, fun = mean, sp = T))
rsp_tavg4 <- rsp_tavg4 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_tavg_01, wc2.1_2.5m_tavg_02,
                wc2.1_2.5m_tavg_03, wc2.1_2.5m_tavg_04, wc2.1_2.5m_tavg_05, wc2.1_2.5m_tavg_06,
                wc2.1_2.5m_tavg_07, wc2.1_2.5m_tavg_08, wc2.1_2.5m_tavg_09, wc2.1_2.5m_tavg_10,
                wc2.1_2.5m_tavg_11,wc2.1_2.5m_tavg_12)
colnames(rsp_tavg4) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_tavg4 <- reshape(rsp_tavg4, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "tavg",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_tavg4$ADM1 <- toupper(rsp_tavg4$ADM1)
rsp_tavg4$ADM2 <- toupper(rsp_tavg4$ADM2)

### BEL
r5 <- crop(tavg, BELadm2) 
r_tavg5 <- mask(r5, BELadm2)
rsp_tavg5 <- as.data.frame(raster::extract(x = r_tavg5, y = BELadm2, fun = mean, sp = T))
rsp_tavg5 <- rsp_tavg5 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_tavg_01, wc2.1_2.5m_tavg_02,
                wc2.1_2.5m_tavg_03, wc2.1_2.5m_tavg_04, wc2.1_2.5m_tavg_05, wc2.1_2.5m_tavg_06,
                wc2.1_2.5m_tavg_07, wc2.1_2.5m_tavg_08, wc2.1_2.5m_tavg_09, wc2.1_2.5m_tavg_10,
                wc2.1_2.5m_tavg_11,wc2.1_2.5m_tavg_12)
colnames(rsp_tavg5) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_tavg5 <- reshape(rsp_tavg5, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "tavg",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_tavg5$ADM1 <- toupper(rsp_tavg5$ADM1)
rsp_tavg5$ADM2 <- toupper(rsp_tavg5$ADM2)


rsp_tavg <- rbind(rsp_tavg1, rsp_tavg2, rsp_tavg3, rsp_tavg4, rsp_tavg5)
rsp_tavg$monthCollected <- match(rsp_tavg$monthCollected, month.abb)


#### precip ####
### CHE
r1 <- crop(prec, CHEadm2)
r_prec1 <- mask(r1, CHEadm2)
rsp_prec1 <- as.data.frame(raster::extract(x = r_prec1, y = CHEadm2, fun = mean, sp = T))
rsp_prec1 <- rsp_prec1 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_prec_01, wc2.1_2.5m_prec_02,
                wc2.1_2.5m_prec_03, wc2.1_2.5m_prec_04, wc2.1_2.5m_prec_05, wc2.1_2.5m_prec_06,
                wc2.1_2.5m_prec_07, wc2.1_2.5m_prec_08, wc2.1_2.5m_prec_09, wc2.1_2.5m_prec_10,
                wc2.1_2.5m_prec_11,wc2.1_2.5m_prec_12)
colnames(rsp_prec1) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar", 
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
                         "Dec")
rsp_prec1 <- reshape(rsp_prec1, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "prec",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_prec1$ADM1 <- toupper(rsp_prec1$ADM1)
rsp_prec1$ADM2 <- toupper(rsp_prec1$ADM2)

### DEU
r2 <- crop(prec, DEUadm2) 
r_prec2 <- mask(r2, DEUadm2)
rsp_prec2 <- as.data.frame(raster::extract(x = r_prec2, y = DEUadm2, fun = mean, sp = T))
rsp_prec2 <- rsp_prec2 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_prec_01, wc2.1_2.5m_prec_02,
                wc2.1_2.5m_prec_03, wc2.1_2.5m_prec_04, wc2.1_2.5m_prec_05, wc2.1_2.5m_prec_06,
                wc2.1_2.5m_prec_07, wc2.1_2.5m_prec_08, wc2.1_2.5m_prec_09, wc2.1_2.5m_prec_10,
                wc2.1_2.5m_prec_11,wc2.1_2.5m_prec_12)
colnames(rsp_prec2) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_prec2 <- reshape(rsp_prec2, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "prec",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_prec2$ADM1 <- toupper(rsp_prec2$ADM1)
rsp_prec2$ADM2 <- toupper(rsp_prec2$ADM2)

### GBR
r3 <- crop(prec, GBRadm2)
r_prec3 <- mask(r3, GBRadm2)
rsp_prec3 <- as.data.frame(raster::extract(x = r_prec3, y = GBRadm2, fun = mean, sp = T))
rsp_prec3 <- rsp_prec3 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_prec_01, wc2.1_2.5m_prec_02,
                wc2.1_2.5m_prec_03, wc2.1_2.5m_prec_04, wc2.1_2.5m_prec_05, wc2.1_2.5m_prec_06,
                wc2.1_2.5m_prec_07, wc2.1_2.5m_prec_08, wc2.1_2.5m_prec_09, wc2.1_2.5m_prec_10,
                wc2.1_2.5m_prec_11,wc2.1_2.5m_prec_12)
colnames(rsp_prec3) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_prec3 <- reshape(rsp_prec3, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "prec",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_prec3$ADM1 <- toupper(rsp_prec3$ADM1)
rsp_prec3$ADM2 <- toupper(rsp_prec3$ADM2)


### ESP
r4 <- crop(prec, ESPadm2)
r_prec4 <- mask(r4, ESPadm2)
rsp_prec4 <- as.data.frame(raster::extract(x = r_prec4, y = ESPadm2, fun = mean, sp = T))
rsp_prec4 <- rsp_prec4 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_prec_01, wc2.1_2.5m_prec_02,
                wc2.1_2.5m_prec_03, wc2.1_2.5m_prec_04, wc2.1_2.5m_prec_05, wc2.1_2.5m_prec_06,
                wc2.1_2.5m_prec_07, wc2.1_2.5m_prec_08, wc2.1_2.5m_prec_09, wc2.1_2.5m_prec_10,
                wc2.1_2.5m_prec_11,wc2.1_2.5m_prec_12)
colnames(rsp_prec4) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_prec4 <- reshape(rsp_prec4, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "prec",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_prec4$ADM1 <- toupper(rsp_prec4$ADM1)
rsp_prec4$ADM2 <- toupper(rsp_prec4$ADM2)

### BEL
r5 <- crop(prec, BELadm2) 
r_prec5 <- mask(r5, BELadm2)
rsp_prec5 <- as.data.frame(raster::extract(x = r_prec5, y = BELadm2, fun = mean, sp = T))
rsp_prec5 <- rsp_prec5 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_prec_01, wc2.1_2.5m_prec_02,
                wc2.1_2.5m_prec_03, wc2.1_2.5m_prec_04, wc2.1_2.5m_prec_05, wc2.1_2.5m_prec_06,
                wc2.1_2.5m_prec_07, wc2.1_2.5m_prec_08, wc2.1_2.5m_prec_09, wc2.1_2.5m_prec_10,
                wc2.1_2.5m_prec_11,wc2.1_2.5m_prec_12)
colnames(rsp_prec5) <- c("ADM0", "country", "ADM1", "ADM2", "Jan", "Feb", "Mar",
                         "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                         "Dec")
rsp_prec5 <- reshape(rsp_prec5, varying = c("Jan", "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep", "Oct",
                                            "Nov", "Dec"),
                     v.names = "prec",
                     timevar = "monthCollected",
                     times = month.abb,
                     direction = "long")
rsp_prec5$ADM1 <- toupper(rsp_prec5$ADM1)
rsp_prec5$ADM2 <- toupper(rsp_prec5$ADM2)

rsp_prec <- rbind(rsp_prec1, rsp_prec2, rsp_prec3, rsp_prec4, rsp_prec5)
rsp_prec$monthCollected <- match(rsp_prec$monthCollected, month.abb)


#### bio; these are annual trends, not monthly ####
### CHE
r1 <- crop(bio, CHEadm2)
r_bio1 <- mask(r1, CHEadm2)
rsp_bio1 <- as.data.frame(raster::extract(x = r_bio1, y = CHEadm2, fun = mean, sp = T))
head(rsp_bio1)
rsp_bio1 <- rsp_bio1 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_bio_1, wc2.1_2.5m_bio_2, wc2.1_2.5m_bio_3,
                wc2.1_2.5m_bio_4, wc2.1_2.5m_bio_5, wc2.1_2.5m_bio_6, wc2.1_2.5m_bio_7, wc2.1_2.5m_bio_8,
                wc2.1_2.5m_bio_9, wc2.1_2.5m_bio_10, wc2.1_2.5m_bio_11, wc2.1_2.5m_bio_12, wc2.1_2.5m_bio_13,
                wc2.1_2.5m_bio_14,wc2.1_2.5m_bio_15, wc2.1_2.5m_bio_16, wc2.1_2.5m_bio_17, wc2.1_2.5m_bio_18,
                wc2.1_2.5m_bio_19)
colnames(rsp_bio1) <- c("ADM0", "Country", "ADM1", "ADM2", "bio1", "bio2", "bio3",
                        "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10",
                        "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17",
                        "bio18", "bio19")
rsp_bio1$ADM1 <- toupper(rsp_bio1$ADM1)
rsp_bio1$ADM2 <- toupper(rsp_bio1$ADM2)
head(rsp_bio1)

### DEU
r2 <- crop(bio, DEUadm2)
r_bio2 <- mask(r2, DEUadm2)
rsp_bio2 <- as.data.frame(raster::extract(x = r_bio2, y = DEUadm2, fun = mean, sp = T))
head(rsp_bio2)
rsp_bio2 <- rsp_bio2 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_bio_1, wc2.1_2.5m_bio_2, wc2.1_2.5m_bio_3,
                wc2.1_2.5m_bio_4, wc2.1_2.5m_bio_5, wc2.1_2.5m_bio_6, wc2.1_2.5m_bio_7, wc2.1_2.5m_bio_8,
                wc2.1_2.5m_bio_9, wc2.1_2.5m_bio_10, wc2.1_2.5m_bio_11, wc2.1_2.5m_bio_12, wc2.1_2.5m_bio_13,
                wc2.1_2.5m_bio_14,wc2.1_2.5m_bio_15, wc2.1_2.5m_bio_16, wc2.1_2.5m_bio_17, wc2.1_2.5m_bio_18,
                wc2.1_2.5m_bio_19)
colnames(rsp_bio2) <- c("ADM0", "Country", "ADM1", "ADM2", "bio1", "bio2", "bio3",
                        "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", 
                        "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", 
                        "bio18", "bio19")
rsp_bio2$ADM1 <- toupper(rsp_bio2$ADM1)
rsp_bio2$ADM2 <- toupper(rsp_bio2$ADM2)
head(rsp_bio2)

### GBR
r3 <- crop(bio, GBRadm2) 
r_bio3 <- mask(r3, GBRadm2)
rsp_bio3 <- as.data.frame(raster::extract(x = r_bio3, y = GBRadm2, fun = mean, sp = T))
head(rsp_bio3)
rsp_bio3 <- rsp_bio3 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_bio_1, wc2.1_2.5m_bio_2, wc2.1_2.5m_bio_3,
                wc2.1_2.5m_bio_4, wc2.1_2.5m_bio_5, wc2.1_2.5m_bio_6, wc2.1_2.5m_bio_7, wc2.1_2.5m_bio_8,
                wc2.1_2.5m_bio_9, wc2.1_2.5m_bio_10, wc2.1_2.5m_bio_11, wc2.1_2.5m_bio_12, wc2.1_2.5m_bio_13,
                wc2.1_2.5m_bio_14,wc2.1_2.5m_bio_15, wc2.1_2.5m_bio_16, wc2.1_2.5m_bio_17, wc2.1_2.5m_bio_18,
                wc2.1_2.5m_bio_19)
colnames(rsp_bio3) <- c("ADM0", "Country", "ADM1", "ADM2", "bio1", "bio2", "bio3",
                        "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", 
                        "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", 
                        "bio18", "bio19")
rsp_bio3$ADM1 <- toupper(rsp_bio3$ADM1)
rsp_bio3$ADM2 <- toupper(rsp_bio3$ADM2)
head(rsp_bio3)

### ESP
r4 <- crop(bio, ESPadm2)
r_bio4 <- mask(r4, ESPadm2)
rsp_bio4 <- as.data.frame(raster::extract(x = r_bio4, y = ESPadm2, fun = mean, sp = T))
head(rsp_bio4)
rsp_bio4 <- rsp_bio4 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_bio_1, wc2.1_2.5m_bio_2, wc2.1_2.5m_bio_3,
                wc2.1_2.5m_bio_4, wc2.1_2.5m_bio_5, wc2.1_2.5m_bio_6, wc2.1_2.5m_bio_7, wc2.1_2.5m_bio_8,
                wc2.1_2.5m_bio_9, wc2.1_2.5m_bio_10, wc2.1_2.5m_bio_11, wc2.1_2.5m_bio_12, wc2.1_2.5m_bio_13,
                wc2.1_2.5m_bio_14,wc2.1_2.5m_bio_15, wc2.1_2.5m_bio_16, wc2.1_2.5m_bio_17, wc2.1_2.5m_bio_18,
                wc2.1_2.5m_bio_19)
colnames(rsp_bio4) <- c("ADM0", "Country", "ADM1", "ADM2", "bio1", "bio2", "bio3",
                        "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", 
                        "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", 
                        "bio18", "bio19")
rsp_bio4$ADM1 <- toupper(rsp_bio4$ADM1)
rsp_bio4$ADM2 <- toupper(rsp_bio4$ADM2)
head(rsp_bio4)

### BEL
r5 <- crop(bio, BELadm2)
r_bio5 <- mask(r5, BELadm2)
rsp_bio5 <- as.data.frame(raster::extract(x = r_bio5, y = BELadm2, fun = mean, sp = T))
head(rsp_bio5)
rsp_bio5 <- rsp_bio5 %>% 
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, wc2.1_2.5m_bio_1, wc2.1_2.5m_bio_2, wc2.1_2.5m_bio_3,
                wc2.1_2.5m_bio_4, wc2.1_2.5m_bio_5, wc2.1_2.5m_bio_6, wc2.1_2.5m_bio_7, wc2.1_2.5m_bio_8,
                wc2.1_2.5m_bio_9, wc2.1_2.5m_bio_10, wc2.1_2.5m_bio_11, wc2.1_2.5m_bio_12, wc2.1_2.5m_bio_13,
                wc2.1_2.5m_bio_14,wc2.1_2.5m_bio_15, wc2.1_2.5m_bio_16, wc2.1_2.5m_bio_17, wc2.1_2.5m_bio_18,
                wc2.1_2.5m_bio_19)
colnames(rsp_bio5) <- c("ADM0", "Country", "ADM1", "ADM2", "bio1", "bio2", "bio3",
                        "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", 
                        "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", 
                        "bio18", "bio19")
rsp_bio5$ADM1 <- toupper(rsp_bio5$ADM1)
rsp_bio5$ADM2 <- toupper(rsp_bio5$ADM2)
head(rsp_bio5)

rsp_bio <- rbind(rsp_bio1, rsp_bio2, rsp_bio3, rsp_bio4, rsp_bio5)
rsp_bio[, c(16:18, 20:23)] = rsp_bio[, c(16:18, 20:23)]*0.1


#### Merge extracted data to original dataframe ####
prev$tmin = rsp_tmin$tmin[base::match(paste(prev$ADM2, prev$monthCollected), 
                                      paste(rsp_tmin$ADM2, rsp_tmin$monthCollected))]
prev$tmax = rsp_tmax$tmax[base::match(paste(prev$ADM2, prev$monthCollected), 
                                      paste(rsp_tmax$ADM2, rsp_tmax$monthCollected))]
prev$tavg = rsp_tavg$tavg[base::match(paste(prev$ADM2, prev$monthCollected), 
                                      paste(rsp_tavg$ADM2, rsp_tavg$monthCollected))]
prev$prec = rsp_prec$prec[base::match(paste(prev$ADM2, prev$monthCollected), 
                                      paste(rsp_prec$ADM2, rsp_prec$monthCollected))]
prev$bio1 = rsp_bio$bio1[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio2 = rsp_bio$bio2[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio3 = rsp_bio$bio3[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio4 = rsp_bio$bio4[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio5 = rsp_bio$bio5[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio6 = rsp_bio$bio6[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio7 = rsp_bio$bio7[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio8 = rsp_bio$bio8[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio9 = rsp_bio$bio9[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio10 = rsp_bio$bio10[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio11 = rsp_bio$bio11[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio12 = rsp_bio$bio12[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio13 = rsp_bio$bio13[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio14 = rsp_bio$bio14[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio15 = rsp_bio$bio15[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio16 = rsp_bio$bio16[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio17 = rsp_bio$bio17[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio18 = rsp_bio$bio18[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]
prev$bio19 = rsp_bio$bio19[base::match(paste(prev$ADM2), paste(rsp_bio$ADM2))]


data.frame(colnames(prev))

prev <- prev[, c(1:42, 51:73, 43:50)]

#### cbind model df ####
prev$genus <- gsub("[[:space:]]", "", prev$genus) # get rid of weird spaces in this column

# Classify which sites are Bsal positive beginning at the date of the first positive Bsal observation
# A site may initially be Bsal negative and may later test positive, thus it is possible to have a site classified as both negative and positive
BsalPos_FS <- prev %>%
  tidyr::drop_na(., any_of(c("BsalDetected", "date", "soilMoisture_date", "temp_date"))) %>%
  group_by(Site, date) %>%
  mutate(scientific = gsub(pattern = "Pelophylax perezi", replacement = "Pelophylax sp.", scientific),
         cumulative_prev = ave(BsalDetected == 1, FUN = cumsum),
         prev_above_0 = NA) %>%
  ungroup()


# Populate all rows with 1 or 0 based on Bsal presence at a given Site
for(i in 1:nrow(BsalPos_FS)){
  if(BsalPos_FS[i, 74] != 0){
    BsalPos_FS[i, 75] = 1 # true; at least one animal at that site tested positive for Bsal at the time of the observation
  }
  else {
    BsalPos_FS[i, 75] <- 0 # false; Site either is Bsal negative, or Bsal had not been detected at the time of the observation
  }
}

# Return data frame containing only fire salamander observations from sites that have tested positive for Bsal (starting at the date the sites initially tested positive) 
dcbind <- BsalPos_FS %>%
  relocate(c(cumulative_prev, prev_above_0), .after = Site) %>%
  group_by(Site, date, scientific) %>%
  mutate(nPos_FS = sum(BsalDetected == 1 & scientific == "Salamandra salamandra"),
         nNeg_FS = sum(BsalDetected == 0 & scientific == "Salamandra salamandra"),
         nDead_FS = sum(fatal == 1 & scientific == "Salamandra salamandra", na.rm = T),
         nAlive_FS = sum(fatal == 0 & scientific == "Salamandra salamandra", na.rm = T),
         nFatalUnk_FS = sum(is.na(fatal) & scientific == "Salamandra salamandra"), 
         nPos_all = sum(BsalDetected == 1),
         nNeg_all = sum(BsalDetected == 0),
         nDead_all = sum(fatal == 1, na.rm = T),
         nAlive_all = sum(fatal == 0, na.rm = T),
         nFatalUnk_all = sum(is.na(fatal)),
         prev_above_0 = as.factor(prev_above_0)) %>%
  ungroup() %>%
  relocate(c(nPos_FS, nNeg_FS, nDead_FS, nAlive_FS, nFatalUnk_FS, nPos_all, nNeg_all, nDead_all, nAlive_all, nFatalUnk_all), .after = susceptibility) %>%
  dplyr::select(country, decimalLatitude, decimalLongitude, Site, prev_above_0, date, date_t1, date_t2, scientific, susceptibility, 
                nPos_FS, nNeg_FS, nDead_FS, nAlive_FS, nFatalUnk_FS, nPos_all, nNeg_all, nDead_all, nAlive_all, nFatalUnk_all,
                richness, sppAbun, siteAbun, alphadiv, temp_date, temp_date_t1, temp_date_t2, soilMoisture_date, soilMoisture_date_t1, soilMoisture_date_t2, 
                tmin, tmax, tavg, prec, bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19, collectorList) %>%
  group_by(Site, date) %>%
  distinct() 

dcbind <- with(dcbind, dcbind[order(Site, scientific), ])


setwd(file.path(dir, csvpath))
## File for final prev dataframe:
write.csv(prev, file = "bsalData_clean.csv", row.names = FALSE)

## File for cbind model:
write.csv(dcbind, file = "bsalData_cbind.csv", row.names = FALSE)



