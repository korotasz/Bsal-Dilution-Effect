require(renv)
require(pacman)

## Activate project (only need to do once upon first use of script)
#renv::activate() ## need to run renv::restore(packages = "renv")

#### Packages ####
pckgs <- c("tidyverse", # data wrangling/manipulation
            "reshape2", # data wrangling/manipulation
               "rgdal", # geospatial analyses
           "lubridate", # deals with dates
          "data.table", # 
              "raster", # raster manipulation/working with geospatial data
             "geodata", # get admin levels for each country
            "maptools", # package to create maps
               "gstat", # spatio-temporal geostatistical modelling
                  "sp", # working with geospatial data
                  "sf", # workin with geospatial data
                  "fs"  # construct relative paths to files/directories
)

## Load packages
pacman::p_load(pckgs, character.only = T)


dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
csvpath <- (path.expand("/csvFiles"))
shppath <- (path.expand("/csvFiles/shapefiles"))
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
  # replace empty string with NA
  replace(., . == "", NA) %>%
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
                                 USA = "United States")) %>%
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
prev <- prev[-c(3, 22, 24:28, 33, 35, 37:41)]

## Rearrange dataframe
data.frame(colnames(prev)) 
prev <- prev[, c(2, 31:33, 4:5, 3, 19:20, 6, 8:10, 34:35, 24:26, 
                 13:18, 27, 7, 11:12, 21:22, 1, 23, 28:30)]


## Remove columns from Spain
data.frame(colnames(spain)) 
spain <- spain[, -c(8:9, 32:54)]


## Rearrange columns in Spain df
spain <- spain[, c(2:10, 1, 11:13, 15, 14, 28:29, 26, 18, 
                   20:24, 27, 16:17, 19, 25, 30:33)]
spain <- spain %>%
  replace(., . == "", NA) %>%
  rename(specimenFate = specimenDisposition) %>%
  mutate(sampleRemarks = NA, principalInvestigator = "An Martel") %>%
  relocate(c("sampleRemarks", "principalInvestigator"), .after = "diagnosticLab")
  
## Join Spain to main df
prev <- rbind(prev, spain)

prev <- prev %>%
  # make sure individualCount is numeric
  mutate(individualCount = as.numeric(individualCount)) %>% 
  # assume all NA values are observations for a single individual
  mutate(individualCount = coalesce(NA, 1)) %>%
  # replace NA values in diseaseTested with appropriate test
  mutate(diseaseTested = coalesce(NA, "Bsal")) %>%
  # remove rows with no data
  dplyr::filter(!(materialSampleID=="")) %>%
  # drop rows that include sampling from Peru or the US (imported with euram df)
  dplyr::filter(!(country == "Peru")) %>%
  dplyr::filter(!(country == "United States")) %>%
  rename(year = yearCollected,
         month = monthCollected,
         day = dayCollected,
         Lat = decimalLatitude,
         Lon = decimalLongitude)


#### Obtaining climate data from geodata package -------------------------------
# Construct file path to store Euro country shapefiles
#dir.create(file.path(dir, shppath)) # Will give warning if path already exists
setwd(file.path(dir, shppath))

## 1. Use 'raster' pckg to get shapefiles for each country and epsg.io to find the best coordinate system for Europe (we will use EPSG:3035).
#        Using a CRS for the country/region we are looking at will yield more accurate results.
poly <- gadm(country = c('BEL', 'DEU', 'CHE', 'ITA', 'GBR', 'ESP'), level = 2, 
             path = file.path(dir, shppath), version = "latest", resolution = 1) %>%
  st_as_sf(.) %>%
  st_cast(., "MULTIPOLYGON") %>%
  st_transform(., crs = 3035) # native crs for gadm = 4326

points_transformed <- prev %>% 
  dplyr::select(Lon, Lat) %>%
  st_as_sf(x = ., coords = c("Lon", "Lat"), crs = 4326) %>%
  st_transform(., crs = 3035) %>%
  mutate(L1 = row_number())

points_original <- prev %>% 
  dplyr::select(Lon, Lat) %>%
  mutate(L1 = row_number())

joined <- left_join(points_original, points_transformed, by = "L1") %>%
  relocate(L1, .before = Lon)

## Intersect lat/lon coordinates with each raster to get the correct admin levels associated with our data
out <- st_intersection(points_transformed, poly) %>%
  left_join(., joined, by = c("L1", "geometry")) %>%
  st_transform(., crs = 4326)

## Get admin levels in a dataframe format
adminlvls <- data.frame(out) %>%
  mutate(L1 = row_number(),
         Lat = round(Lat, 6),
         Lon = round(Lon, 6))
geometry <- data.frame(st_coordinates(st_cast(out$geometry, "POINT"))) %>%
  mutate(L1 = row_number())

## Join lat/lon data to identifiers (ADM levels) 
adminlvls <- adminlvls %>%
  left_join(., geometry, by = "L1") %>% # X = LON, Y = LAT
  dplyr::select(GID_0, COUNTRY, NAME_1, NAME_2, Y, X) %>%
  plyr::mutate(country = COUNTRY,
               ADM0 = GID_0,
               ADM1 = NAME_1,
               ADM2 = NAME_2,
               Lat = Y,
               Lon = X) %>%
  dplyr::select(-c(GID_0, COUNTRY, NAME_1, NAME_2, Y, X)) %>%
  relocate(c("Lat", "Lon"), .after = ADM2) %>%
  mutate(Lat = round(Lat, 6),
         Lon = round(Lon, 6)) %>%
  unique()

## Check for missing admin levels!! 
# adminlvls %>%
#   filter(is.na(ADM2)) # only one location missing (ADM2 in GBR).

## Check & correct any rounding errors in transformed points
# test <- adminlvls
# 
# df1 <- test %>%
#   dplyr::select(Lat, Lon) %>%
#   st_transform(., crs = 4326) %>%
#   mutate(transLat = round(transLat, 6),
#          transLon = round(transLon, 6)) %>%
#   unite(., LatLon, c("Lat", "Lon"), sep = ", ") 
#
# df2 <- test %>%
#   mutate(Lat = round(Lat, 6),
#          Lon = round(Lon, 6)) %>%
#   dplyr::select(Lat, Lon) %>%
#   unite(., LatLon, c("Lat", "Lon"), sep = ", ")
# 
# compare <- anti_join(df1, df2, by = "LatLon")

## Add back to main dataframe 
prev <- prev %>%
  dplyr::select(!("country":"ADM2")) %>%
  mutate(Lat = round(Lat, 6),
         Lon = round(Lon, 6)) %>%
  left_join(., adminlvls, by = c("Lat", "Lon"), relationship = "many-to-one",
                  keep = F) %>%
  relocate(c(country, ADM0, ADM1, ADM2), .before = Lat) 
## Compared this method with sampling locations in QGIS and it is just as precise/accurate.


## Group sites by unique lat/long combos and assign site #s to them, for all countries
siteNumber <- prev %>%
  dplyr::select(materialSampleID, Lat, Lon) %>%
  dplyr::group_by(Lat, Lon) %>%
  mutate(Site = cur_group_id()) %>% 
  ungroup() 

prev$Site <- siteNumber$Site[base::match(paste(prev$materialSampleID), 
                                      paste(siteNumber$materialSampleID))]

prev <- relocate(prev, Site, .after = "day")

## Add data to the susceptibility column in prev df
## Susceptibility codes (based on coding system from Bosch et al. 2021)
## 1 = resistant
## 2 = tolerant/susceptible
## 3 = lethal
setwd(file.path(dir, csvpath))
s <- read.csv("susceptibility.csv", header = T, encoding = "UTF-8")
data.frame(colnames(s))
names(s) <- c("order", "family", "genus", "species", "scientific", 
              "susceptibility", "citation")

prev$susceptibility <- s$susceptibility[base::match(prev$scientific, s$scientific)]
## double check there are no NAs
plyr::count(prev, "susceptibility") 
#sus <- prev %>% dplyr::filter(is.na(susceptibility))

#### Calculate abundance, richness, and diversity ####
prev <- unite(prev, c("year", "month", "day"), sep = "-", col = "date", remove = F)

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

## Add abundance and richness back into prev df
prev <- prev %>%
  # species richness
  left_join(spr[,c(1:2,25)], by = c("Site", "date")) %>%
  # species abundance
  left_join(spa, by = c("scientific", "Site", "date")) %>%
  # site abundance
  left_join(SAb[,c(1:2,3)], by = c("Site", "date")) 

## Make sure columns that have categorical data are uniform in coding
prev <- prev %>%
  # code all NA values as 'False'
  mutate(BdDetected = as.factor(BdDetected),
         BsalDetected = as.factor(BsalDetected),
         fatal = as.factor(fatal)) %>%
  mutate(BdDetected = tidyr::replace_na(BdDetected, "FALSE"),
         BsalDetected = tidyr::replace_na(BsalDetected, "FALSE"))


prev$fatal <- toupper(prev$fatal)
prev$specimenFate <- toupper(prev$specimenFate)

## Convert factors with two levels to binary integers
prev$BdDetected <- as.factor(prev$BdDetected)
levels(prev$BdDetected) <- c(0,1) #0 = F, 1 = T
prev$BsalDetected <- as.factor(prev$BsalDetected)
levels(prev$BsalDetected) <- c(0,1) #0 = F, 1 = T
prev$fatal <- as.factor(prev$fatal)
levels(prev$fatal) <- c(0,1) #0 = F, 1 = T


## Obtain unique lat/long/date combinations to extract weather data
Sys.setenv(TZ = "UTC")
weather <- prev %>%
  dplyr::select(Lat, Lon, year, month, day, date) %>%
  unite(Lat, Lon, sep = ", ", col = "LatLon", remove = F) %>%
  relocate(LatLon, .after = Lon) %>%
  dplyr::filter(!(day == "NA")) %>%
  group_by(LatLon, date) %>%
  unique() %>%
  ungroup() %>%
  dplyr::select(!(LatLon)) %>%
  mutate(date = base::as.Date(date, format = "%Y-%m-%d"))
  
## get dates for 4 days prior to sample date
for(i in 1:nrow(weather)){
  weather[i,7] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(4)

## get dates for 1 week prior to sample date
  weather[i,8] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(7)

## get dates for 10 days prior to sample date
  weather[i,9] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(10)

## get dates for 2 weeks prior to sample date
  weather[i,10] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(14)

## get dates for 1 month prior to sample date
  weather[i,11] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(30)
  
}

weather <- weather %>%
  rename(t4 = ...7,
         t7 = ...8,
         t10 = ...9,
         t14 = ...10,
         t30 = ...11)

## Add date from timepoints back into prev
prev <- prev %>%
  mutate(date = base::as.Date(date, "%Y-%m-%d")) %>%
  left_join(weather[, c(1:2, 6:11)], by = c("Lat", "Lon", "date")) %>%
  relocate(c("t4", "t7", "t10", "t14", "t30"), .after = date) %>%
  rename(t0 = date)


weather <- weather %>%
  rename(t0 = date) %>%
  group_by(Lat, Lon) %>%
  pivot_longer(cols = c(t0, t4, t7, t10, t14, t30),
               names_to = "timepoint",
               values_to = "date") %>%
  ungroup() %>%
  relocate(c("timepoint", "date"), .after = "Lon") %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date)) %>%
  dplyr::select(-("date")) %>%
  filter(year != '1905') # likely a museum specimen --  not useful for our analyses


# weather_07_16 <- weather %>%
#   filter(between(year, '2007', '2016'))
# 
# weather_2017 <- weather %>%
#   filter(year == '2017')
# 
# weather_2018 <- weather %>%
#   filter(year == '2018')
# 
# weather_2019 <- weather %>%
#   filter(year == '2019')
# 
# weather_20_21 <- weather %>%
#   filter(between(year, '2020', '2021'))


## Export to use in Python and PyQGIS to obtain weather data
#write.csv(weather, 'weather.csv', row.names = F, fileEncoding = "UTF-8")


## Python 3.9.4 used to download .nc4 files from NASA's EarthData data repository for each date and location.

## Import DAILY temperature & soil moisture data from NASA's EarthData website (citation below)
   ## Li, B., H. Beaudoing, and M. Rodell, NASA/GSFC/HSL (2020), GLDAS Catchment Land Surface Model L4 daily 0.25 x 0.25 degree GRACE-DA1 V2.2, 
   ## Greenbelt, Maryland, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: 2022-09-08.
gldas_daily <- read.csv("weather_merged.csv", header = T, encoding = "UTF-8")

gldas_daily <- gldas_daily %>%
  unite(c("year", "month", "day"), sep = "-", col = "date", remove = F) %>%
  rename(soilMoisture = "SOILMOIST_kgm.21",
         temp = "SURFTEMP_K1") %>%
  dplyr::select(Lat, Lon, timepoint, date, soilMoisture, temp) %>%
  unite(Lat, Lon, sep = ", ", col = "LatLon", remove = F) 


# Copy & separate temp from soilMoisture
temp_d <- gldas_daily %>%
  # exclude rows with sM data, as well as any NAs in the temp data
  dplyr::select(Lat, Lon, date, timepoint, temp) %>%
  drop_na() %>%
  group_by(Lat, Lon, timepoint) %>%
  distinct_all() %>%
  # assign row # for matching purposes
  mutate(row = row_number(),
         date = as.Date(date, format = "%Y-%m-%d"),
         # Convert temp from K to C
         temp = as.numeric(temp - 273.15)) %>%
  dplyr::group_by(Lat, Lon, timepoint) %>%
  pivot_wider(names_from = timepoint, values_from = c(date, temp)) %>%
  subset(., select = -c(temp_date_t2, temp_date_t1)) %>%
  rename(date_t2 = "date_date_t2",
         date_t1 = "date_date_t1",
         date = "date_date",
         temp_d = "temp_date")


gldas_daily <- gldas_daily %>%
  dplyr::select(Lat, Lon, date, timepoint, soilMoisture) %>%
  drop_na() %>%
  group_by(Lat, Lon, timepoint) %>%
  distinct_all() %>%
  mutate(row = row_number(),
         date = as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::group_by(Lat, Lon, timepoint) %>%
  pivot_wider(names_from = timepoint, values_from = c(date, soilMoisture)) %>%
  subset(., select = -c(soilMoisture_date_t2, soilMoisture_date_t1)) %>%
  rename(date_t2 = "date_date_t2",
         date_t1 = "date_date_t1",
         date = "date_date",
         sMoist_d = "soilMoisture_date")
 
# Add temp data back into gldas_daily df in preparation to add it back to the main df.
gldas_daily <- left_join(gldas_daily, temp_d, by = c("Lat", "Lon", "date", "date_t1", "date_t2", "row")) %>%
  subset(., select = -c(row))



## Import MONTHLY temperature, precip, & soil moisture data from NASA's EarthData website (citation below)
## Beaudoing, H. and M. Rodell, NASA/GSFC/HSL (2020), GLDAS Noah Land Surface Model L4 monthly 0.25 x 0.25 degree V2.1,
## Greenbelt, Maryland, USA,Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/SXAVCZFAQLNO
gldas_monthly <- read.csv("monthly_weather.csv", header = T, encoding = "UTF-8")

gldas_monthly <- gldas_monthly %>%
  mutate(month = as.numeric(dplyr::recode(month,
                          "true" = "1")),
         day = as.numeric(dplyr::recode(day,
                               "true" = "1"))) %>%
  unite(c("year", "month", "day"), sep = "-", col = "date", remove = F) %>%
  rename(soilMoisture = "SOILMOIST_kgm.21",
         temp = "SURFTEMP_K1",
         precip = "PRECIP_kgm.2s.11") 

# Convert precipitation rate kg/m^2/s to mm/month
for(i in 1:nrow(gldas_monthly)){
  if(gldas_monthly[i, 8] == 31){
    gldas_monthly[i, 9] <- gldas_monthly[i, 9] * (31*24*60*60)}
  
  else if(gldas_monthly[i, 8] == 30){
    gldas_monthly[i, 9] <- gldas_monthly[i, 9] * (30*24*60*60)}
  
  else if(gldas_monthly[i, 8] == 28){
    gldas_monthly[i, 9] <- gldas_monthly[i, 9] * (28*24*60*60)}
  
  else if(gldas_monthly[i, 8] == 29){
    gldas_monthly[i, 9] <- gldas_monthly[i, 9] * (29*24*60*60)}
  
  else if(is.na(gldas_monthly[i, 8]) == TRUE){
    gldas_monthly[i, 9] <- NA}
}


# Copy separate temp from precip and soilMoisture
temp_m <- gldas_monthly %>%
  dplyr::select(Lat, Lon, date, timepoint, temp) %>%
  # exclude rows with sM and precip data, as well as any NAs in the temp data
  drop_na() %>%
  group_by(Lat, Lon, timepoint) %>%
  distinct_all() %>%
  # assign row # for matching purposes
  mutate(row = row_number(),
         date = as.Date(date, format = "%Y-%m-%d"),
         # Convert temp from K to C
         temp = as.numeric(temp - 273.15)) %>%
  dplyr::group_by(Lat, Lon, timepoint) %>%
  pivot_wider(names_from = timepoint, values_from = c(date, temp)) %>%
  rename(date_t2 = "date_date_t2",
         date_t1 = "date_date_t1",
         date = "date_date",
         temp_m_t2 = "temp_date_t2",
         temp_m_t1 = "temp_date_t1",
         temp_m = "temp_date")

precip_m <- gldas_monthly %>%
  dplyr::select(Lat, Lon, date, timepoint, precip) %>%
  drop_na() %>%
  group_by(Lat, Lon, timepoint) %>%
  distinct_all() %>%
  mutate(row = row_number(),
         date = as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::group_by(Lat, Lon, timepoint) %>%
  pivot_wider(names_from = timepoint, values_from = c(date, precip)) %>%
  rename(date_t2 = "date_date_t2",
         date_t1 = "date_date_t1",
         date = "date_date",
         precip_m_t2 = "precip_date_t2",
         precip_m_t1 = "precip_date_t1",
         precip_m = "precip_date")

gldas_monthly <- gldas_monthly %>%
  dplyr::select(Lat, Lon, date, timepoint, soilMoisture) %>%
  drop_na() %>%
  group_by(Lat, Lon, timepoint) %>%
  distinct_all() %>%
  mutate(row = row_number(),
         date = as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::group_by(Lat, Lon, timepoint) %>%
  pivot_wider(names_from = timepoint, values_from = c(date, soilMoisture)) %>%
  rename(date_t2 = "date_date_t2",
         date_t1 = "date_date_t1",
         date = "date_date",
         sMoist_m_t2 = "soilMoisture_date_t2",
         sMoist_m_t1 = "soilMoisture_date_t1",
         sMoist_m = "soilMoisture_date")


# Add temp and precip data back into gldas_monthly df in preparation to add it back to the main df.
gldas_monthly <- left_join(gldas_monthly, precip_m, by = c("Lat", "Lon", "date", "date_t1", "date_t2", "row"))
gldas_monthly <- left_join(gldas_monthly, temp_m, by = c("Lat", "Lon", "date", "date_t1", "date_t2", "row")) %>%
  subset(., select = -c(row))
  


## Add weather data to main dataframe
prev <- left_join(prev, gldas_monthly, by = c("Lat", "Lon", "date", "date_t1", "date_t2"))
prev <- left_join(prev, gldas_daily, by = c("Lat", "Lon", "date", "date_t1", "date_t2"))


prev <- prev[, c(1:6, 38, 10:12, 7:9, 13:31, 39:44, 51, 45:50, 52, 32:37)]



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
levels(prev$diseaseDetected) <- c(0,1) #0 = F, 1 = T


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
                     timevar = "month",
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
                     timevar = "month",
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
                     timevar = "month",
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
                     timevar = "month",
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
                     timevar = "month",
                     times = month.abb,
                     direction = "long")
rsp_tmin5$ADM1 <- toupper(rsp_tmin5$ADM1)
rsp_tmin5$ADM2 <- toupper(rsp_tmin5$ADM2)


rsp_tmin <- rbind(rsp_tmin1, rsp_tmin2, rsp_tmin3, rsp_tmin4, rsp_tmin5)
rsp_tmin$month <- match(rsp_tmin$month, month.abb)

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
                     timevar = "month",
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
                     timevar = "month",
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
                     timevar = "month",
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
                     timevar = "month",
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
                     timevar = "month",
                     times = month.abb,
                     direction = "long")
rsp_tmax5$ADM1 <- toupper(rsp_tmax5$ADM1)
rsp_tmax5$ADM2 <- toupper(rsp_tmax5$ADM2)


rsp_tmax <- rbind(rsp_tmax1, rsp_tmax2, rsp_tmax3, rsp_tmax4, rsp_tmax5)
rsp_tmax$month <- match(rsp_tmax$month, month.abb)



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
                     timevar = "month",
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
                     timevar = "month",
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
                     timevar = "month",
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
                     timevar = "month",
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
                     timevar = "month",
                     times = month.abb,
                     direction = "long")
rsp_tavg5$ADM1 <- toupper(rsp_tavg5$ADM1)
rsp_tavg5$ADM2 <- toupper(rsp_tavg5$ADM2)


rsp_tavg <- rbind(rsp_tavg1, rsp_tavg2, rsp_tavg3, rsp_tavg4, rsp_tavg5)
rsp_tavg$month <- match(rsp_tavg$month, month.abb)


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
                     timevar = "month",
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
                     timevar = "month",
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
                     timevar = "month",
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
                     timevar = "month",
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
                     timevar = "month",
                     times = month.abb,
                     direction = "long")
rsp_prec5$ADM1 <- toupper(rsp_prec5$ADM1)
rsp_prec5$ADM2 <- toupper(rsp_prec5$ADM2)

rsp_prec <- rbind(rsp_prec1, rsp_prec2, rsp_prec3, rsp_prec4, rsp_prec5)
rsp_prec$month <- match(rsp_prec$month, month.abb)


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
prev$tmin = rsp_tmin$tmin[base::match(paste(prev$ADM2, prev$month), 
                                      paste(rsp_tmin$ADM2, rsp_tmin$month))]
prev$tmax = rsp_tmax$tmax[base::match(paste(prev$ADM2, prev$month), 
                                      paste(rsp_tmax$ADM2, rsp_tmax$month))]
prev$tavg = rsp_tavg$tavg[base::match(paste(prev$ADM2, prev$month), 
                                      paste(rsp_tavg$ADM2, rsp_tavg$month))]
prev$prec = rsp_prec$prec[base::match(paste(prev$ADM2, prev$month), 
                                      paste(rsp_prec$ADM2, rsp_prec$month))]
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

prev <- prev[, c(1:48, 55:77, 49:54)]


#### cbind model df ####
prev$genus <- gsub("[[:space:]]", "", prev$genus) # get rid of weird spaces in this column

# Classify which sites are Bsal positive beginning at the date of the first positive Bsal observation
# A site may initially be Bsal negative and may later test positive, thus it is possible to have a site classified as both negative and positive
Bsalpos_FS <- prev %>%
  tidyr::drop_na(., any_of(c("BsalDetected", "date", "sMoist_d", "temp_d"))) %>%
  subset(scientific != "Calotriton asper" & # Only one observation with NA vals for date 
         scientific != "Lissotriton boscai" & 
         scientific != "Hyla meridionalis") %>%
  group_by(Site, date) %>%
  mutate(scientific = gsub(pattern = "Pelophylax perezi", replacement = "Pelophylax sp.", scientific),
         cumulative_prev = ave(BsalDetected == 1, FUN = cumsum),
         prev_above_0 = NA) %>%
  ungroup()


# Populate all rows with 1 or 0 based on Bsal presence at a given Site
for(i in 1:nrow(Bsalpos_FS)){
  if(Bsalpos_FS[i, 78] != 0){
    Bsalpos_FS[i, 79] = 1 # true; at least one animal at that site tested positive for Bsal at the time of the observation
  }
  else {
    Bsalpos_FS[i, 79] <- 0 # false; Site either is Bsal negative, or Bsal had not been detected at the time of the observation
  }
}



# Return data frame containing only observations from sites that have tested positive for Bsal (starting at the date the sites initially tested positive) 
prev <- Bsalpos_FS %>%
  relocate(c(cumulative_prev, prev_above_0), .after = Site) %>%
  dplyr::select(-cumulative_prev) %>%
  rename(tmin_wc = tmin, tmax_wc = tmax, tavg_wc = tavg, prec_wc = prec, bio1_wc = bio1, bio2_wc = bio2, bio3_wc = bio3, bio4_wc = bio4, bio5_wc = bio5, bio6_wc = bio6,
         bio7_wc = bio7, bio8_wc = bio8, bio9_wc = bio9, bio10_wc = bio10, bio11_wc = bio11, bio12_wc = bio12, bio13_wc = bio13, bio14_wc = bio14, bio15_wc = bio15, bio16_wc = bio16,
         bio17_wc = bio17, bio18_wc = bio18, bio19_wc = bio19) 


dcbind <- Bsalpos_FS %>%
  relocate(c(cumulative_prev, prev_above_0), .after = Site) %>%
  group_by(Site, date, scientific) %>%
  mutate(nPos_FS = sum(BsalDetected != 0 & scientific == "Salamandra salamandra"),
         nNeg_FS = sum(BsalDetected == 0 & scientific == "Salamandra salamandra"),
         nDead_FS = sum(fatal != 0 & scientific == "Salamandra salamandra", na.rm = T),
         nAlive_FS = sum(fatal == 0 & scientific == "Salamandra salamandra", na.rm = T),
         nFatalUnk_FS = sum(is.na(fatal) & scientific == "Salamandra salamandra"), 
         nPos_all = sum(BsalDetected != 0),
         nNeg_all = sum(BsalDetected == 0),
         nDead_all = sum(fatal != 0, na.rm = T),
         nAlive_all = sum(fatal == 0, na.rm = T),
         nFatalUnk_all = sum(is.na(fatal)),
         prev_above_0 = as.factor(prev_above_0)) %>%
  # slice(1) %>%
  ungroup() %>%
  relocate(c(nPos_FS, nNeg_FS, nDead_FS, nAlive_FS, nFatalUnk_FS, nPos_all, nNeg_all, nDead_all, nAlive_all, nFatalUnk_all), .after = susceptibility) %>%
  dplyr::select(country, Lat, Lon, Site, prev_above_0, date, date_t1, date_t2, scientific, susceptibility, 
                nPos_FS, nNeg_FS, nDead_FS, nAlive_FS, nFatalUnk_FS, nPos_all, nNeg_all, nDead_all, nAlive_all, nFatalUnk_all,
                richness, sppAbun, siteAbun, sMoist_m_t2, sMoist_m_t1, sMoist_m, sMoist_d, precip_m_t2, precip_m_t1, precip_m, temp_m_t2, temp_m_t1, temp_m, temp_d,
                tmin, tmax, tavg, prec, bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19, 
                diagnosticLab, principalInvestigator, collectorList) %>%
  rename(tmin_wc = tmin, tmax_wc = tmax, tavg_wc = tavg, prec_wc = prec, bio1_wc = bio1, bio2_wc = bio2, bio3_wc = bio3, bio4_wc = bio4, bio5_wc = bio5, bio6_wc = bio6,
         bio7_wc = bio7, bio8_wc = bio8, bio9_wc = bio9, bio10_wc = bio10, bio11_wc = bio11, bio12_wc = bio12, bio13_wc = bio13, bio14_wc = bio14, bio15_wc = bio15, bio16_wc = bio16,
         bio17_wc = bio17, bio18_wc = bio18, bio19_wc = bio19) 


dcbind <- with(dcbind, dcbind[order(Site, scientific), ])


## Double check everything matches
dcbind %>% dplyr::select(scientific, nPos_all, nNeg_all) %>%
  group_by(scientific) %>%
  summarise(nPos = sum(nPos_all != 0), nNeg = sum(nNeg_all != 0),
            n = sum(nPos_all, nNeg_all))

d %>% dplyr::select(scientific, individualCount, BsalDetected) %>%
  group_by(scientific) %>%
  summarise(nPos = sum(BsalDetected != 0), nNeg = sum(BsalDetected == 0),
            n = n())

setwd(file.path(dir, csvpath))
## File for final prev dataframe:
write.csv(prev, file = "bsalData_clean.csv", row.names = FALSE)

## File for cbind model:
write.csv(dcbind, file = "bsalData_cbind.csv", row.names = FALSE)



