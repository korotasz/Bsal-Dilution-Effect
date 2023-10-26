require(renv)
require(pacman)

## Activate project (only need to do once upon first use of script)
#renv::activate() ## need to run renv::restore(packages = "renv")

#### Packages ####
pckgs <- c("tidyverse", # data wrangling/manipulation
            "reshape2", # data wrangling/manipulation
               "rgdal", # geospatial analyses
           "lubridate", # deals with dates
                 "zoo", # deals with time series data/ordered obs
               "stats", # ts(); tsp()
          "data.table", # 
              "raster", # raster manipulation/working with geospatial data
               "terra", # supercedes raster package
             "geodata", # get admin levels for each country
            "maptools", # package to create maps
               "gstat", # spatio-temporal geostatistical modelling
                  "sp", # working with geospatial data
                  "sf", # working with geospatial data
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

## 1. Use 'raster' pckg to get shapefiles for each country. We are using EPSG:4326, as these are lat/lon data
#     that are presented in decimal degrees. Reprojecting to another coordinate system is not necessary, and 
#     may even introduce an additional source of error.
polygon <- gadm(country = c('BEL', 'DEU', 'CHE', 'ITA', 'GBR', 'ESP'), level = 2, 
             path = file.path(dir, shppath), version = "latest", resolution = 1) %>%
  sf::st_as_sf(., crs = 4326) %>%
  st_cast(., "MULTIPOLYGON") 

## Write multipolygon to .shp file for later use with WorldClim
# st_write(polygon, "europe.shp", append = F)

points <- prev %>% 
  dplyr::select(Lon, Lat) %>%
  st_as_sf(x = ., coords = c("Lon", "Lat"), crs = 4326) %>%
  mutate(L1 = row_number(),
         Lon = sf::st_coordinates(.)[,1],
         Lat = sf::st_coordinates(.)[,2]) 

# st_write(points, "locations.shp", append = F)

## Intersect lat/lon coordinates with each raster to get the correct admin levels associated with our data
out <- st_intersection(points, polygon) %>%
  mutate(Lon = sf::st_coordinates(.)[,1],
         Lat = sf::st_coordinates(.)[,2])
  
# st_write(out, "adm_info.shp", append = F)

## Get admin levels in a dataframe format
adminlvls <- data.frame(out) %>%
  dplyr::select(Lon, Lat, GID_0, COUNTRY, NAME_1, NAME_2) %>%
  mutate(Lat = round(Lat, 6),
         Lon = round(Lon, 6)) %>%
  rename(ADM0 = GID_0,
         ADM1 = NAME_1,
         ADM2 = NAME_2,
         country = COUNTRY) %>%
  unique(.)


## Check for missing admin levels!! 
# adminlvls %>%
#   filter(is.na(ADM2)) # only one location missing (ADM2 in GBR).
  

## Add back to main dataframe --------------------------------------------------
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

## get dates for 1 week prior to sample date (for avg temp/soil moisture)
  weather[i,8] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(8)
  weather[i,9] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(9)
  weather[i,10] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(10)
  weather[i,11] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(11)
  weather[i,12] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(12)
  weather[i,13] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(13)
  weather[i,14] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(14)
  
## get dates for 2 weeks prior to sample date (for avg temp/soil moisture)
  weather[i,15] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(15)
  weather[i,16] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(16)
  weather[i,17] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(17)
  weather[i,18] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(18)
  weather[i,19] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(19)
  weather[i,20] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(20)
  weather[i,21] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(21)

## get dates for 3 weeks prior to sample date (for avg temp/soil moisture)
  weather[i,22] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(22)
  weather[i,23] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(23)
  weather[i,24] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(24)
  weather[i,25] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(25)
  weather[i,26] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(26)
  weather[i,27] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(27)
  weather[i,28] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(28)  
    
}

weather <- weather %>%
  rename(t4 = ...7,
         t8 = ...8, t9 = ...9, t10 = ...10, t11 = ...11, t12 = ...12, t13 = ...13, t14 = ...14,
         t15 = ...15, t16 = ...16, t17 = ...17, t18 = ...18, t19 = ...19, t20 = ...20, t21 = ...21,
         t22 = ...22, t23 = ...23, t24 = ...24, t25 = ...25, t26 = ...26, t27 = ...27, t28 = ...28)

## Add date from timepoints back into prev
data.frame(colnames(weather))
test <- prev
test <- prev %>%
  mutate(date = base::as.Date(date, "%Y-%m-%d")) %>%
  left_join(weather[, c(1:2, 6:28)], by = c("Lat", "Lon", "date")) %>%
  relocate(c(t4, t8, t9, t10, t11, t12, t13, t14, 
             t15, t16, t17, t18, t19, t20, t21,
             t22, t23, t24, t25, t26, t27, t28), .after = date) #%>%
  rename(t0 = date)


weather <- weather %>%
  rename(t0 = date) %>%
  group_by(Lat, Lon) %>%
  pivot_longer(cols = c(t0, t4, t8, t9, t10, t11, t12, t13, t14, 
                        t15, t16, t17, t18, t19, t20, t21,
                        t22, t23, t24, t25, t26, t27, t28),
               names_to = "timepoint",
               values_to = "date") %>%
  ungroup() %>%
  relocate(c("timepoint", "date"), .after = "Lon") %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date)) %>%
  dplyr::select(-("date")) %>%
  filter(year != '1905') # likely a museum specimen --  not useful for our analyses

## Export to use in Python and PyQGIS to obtain weather data
# write.csv(weather, 'weather.csv', row.names = F, fileEncoding = "UTF-8")


## Python v3.12.0 used to download .nc4 files from NASA's EarthData data repository for each date and location.
## PyQGIS Python v3.9.5 used to process data in QGIS v3.28.3-Firenze.

## Import DAILY temperature & soil moisture data from NASA's EarthData website (citation below)
#  Li, B., H. Beaudoing, and M. Rodell, NASA/GSFC/HSL (2020), GLDAS Catchment Land Surface Model L4 daily 0.25 x 0.25 degree GRACE-DA1 V2.2, 
#     Greenbelt, Maryland, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: 2023-10-11. doi:10.5067/TXBMLX370XX8.
#  Li, B., M. Rodell, S. Kumar, H. Beaudoing, A. Getirana, B. F. Zaitchik, et al. (2019) Global GRACE data assimilation for groundwater and drought 
#     monitoring: Advances and challenges. Water Resources Research, 55, 7564-7586. doi:10.1029/2018wr024618.
gldas_daily <- read.csv("weather_all.csv", header = T, encoding = "UTF-8")

gldas_daily <- gldas_daily %>%
  mutate(day = as.integer(case_match(day, "true" ~ "1", .default = day)),
         month = as.integer(case_match(month, "true" ~ "1", .default = month))) %>%
  unite(c("year", "month", "day"), sep = "-", col = "date", remove = F) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  rename(soilMoisture = "SOILMOIST_kgm.21",
         temp = "SURFTEMP_K1") %>%
  filter(timepoint != 't30' & timepoint != 't7') # artifact from older 'datasetFetcher' python code when obtaining weather .nc4 files -- do not need here
  


# Copy & separate temp from soilMoisture
temp_d <- gldas_daily %>%
  # exclude rows with sM data, as well as any NAs in the temp data
  dplyr::select(Lat, Lon, timepoint, date, temp) %>%
  drop_na(.) %>%
  # expedite processing by only subsetting unique combinations of lat/lon/date
  group_by(Lat, Lon, timepoint) %>%
  distinct_all() %>%
  mutate(week = cut.Date(date, breaks = "1 week"),
    # assign row # for matching purposes
    #row = row_number(),
         # Convert temp from K to C
         temp = as.numeric(temp - 273.15)) %>%
  arrange(date)
  
  group_by(Lat, Lon, timepoint) %>%
  pivot_wider(names_from = timepoint, values_from = c(date, temp)) %>%
  rename(t28 = date_t28, t27 = date_t27, t26 = date_t26, t25 = date_t25, t24 = date_t24, t23 = date_t23, t22 = date_t22,
         t21 = date_t21, t20 = date_t20, t19 = date_t19, t18 = date_t18, t17 = date_t17, t16 = date_t16, t15 = date_t15,
         t14 = date_t14, t13 = date_t13, t12 = date_t12, t11 = date_t11, t10 = date_t10, t9 = date_t9, t8 = date_t8, 
         t4 = date_t4, t0 = date_t0)


  
## Process soil moisture data from the main weather dataframe
gldas_daily <- gldas_daily %>%
  dplyr::select(Lat, Lon, date, timepoint, soilMoisture) %>%
  drop_na() %>%
  group_by(Lat, Lon, timepoint) %>%
  distinct_all() %>%
  mutate(row = row_number(),
         date = as.Date(date, format = "%Y-%m-%d")) %>%
  group_by(Lat, Lon, timepoint) %>%
  pivot_wider(names_from = timepoint, values_from = c(date, soilMoisture)) %>%
  rename(t28 = date_t28, t27 = date_t27, t26 = date_t26, t25 = date_t25, t24 = date_t24, t23 = date_t23, t22 = date_t22,
         t21 = date_t21, t20 = date_t20, t19 = date_t19, t18 = date_t18, t17 = date_t17, t16 = date_t16, t15 = date_t15,
         t14 = date_t14, t13 = date_t13, t12 = date_t12, t_11 = date_t11, t10 = date_t10, t9 = date_t9, t8 = date_t8, 
         t4 = date_t4, t0 = date_t0)
 
# Add temp data back into gldas_daily df in preparation to add it back to the main df.
gldas_daily <- left_join(gldas_daily, temp_d, by = c("Lat", "Lon", "row", "t0", "t4", "t8", "t9", "t10", "t11", "t12", "t13", "t14", 
                                                     "t15", "t16", "t17", "t18", "t19", "t20", "t21",
                                                     "t22", "t23", "t24", "t25", "t26", "t27", "t28")) %>%
   dplyr::select(!(LatLon)) %>%


## Add weather data to main dataframe
prev <- left_join(prev, gldas_daily, by = c("Lat", "Lon", "t0", "t4", "t7", "t10", "t14", "t30")) %>% 
  relocate(c(richness, sppAbun, siteAbun), .after = Site) %>%
  relocate(c(soilMoisture_t30, soilMoisture_t14, soilMoisture_t10, soilMoisture_t7, soilMoisture_t4, soilMoisture_t0,
             temp_t30, temp_t14, temp_t10, temp_t7, temp_t4, temp_t0), .after = sampleRemarks) 


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
## Construct file path to store WorldClim data 
#dir.create(file.path(dir, shppath, "/WorldClim")) # Will give warning if path already exists
wclim_path <- path.expand("csvFiles/shapefiles/WorldClim")
setwd(file.path(dir, wclim_path))

## Obtain unique locations
unique_locations <- adminlvls %>%
  dplyr::select(Lon, Lat) %>%
  unique() %>%
  mutate(Lat = round(Lat, 6),
         Lon = round(Lon, 6))

latlon_id <- adminlvls %>%
  mutate(id = row_number())


## Obtain WorldClim data as SpatRasters and create SpatRaster collection and sample using the unique_locations
#     a. tmin | temporal scale: monthly (30yr avg)
tmin_BEL <- geodata::worldclim_country(var = 'tmin', country = "BEL", path = file.path(dir, wclim_path), res = 0.5, version = "2.1") 
tmin_CHE <- geodata::worldclim_country(var = 'tmin', country = "CHE", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")
tmin_DEU <- geodata::worldclim_country(var = 'tmin', country = "DEU", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")
tmin_ESP <- geodata::worldclim_country(var = 'tmin', country = "ESP", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")
tmin_GBR <- geodata::worldclim_country(var = 'tmin', country = "GBR", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")

rast_list<- list(tmin_BEL, tmin_CHE, tmin_DEU, tmin_ESP, tmin_GBR)
tmin_sprc <- terra::sprc(rast_list) 
tmin_mosaic <- terra::mosaic(tmin_sprc, fun = "mean")
names(tmin_mosaic) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
tmin_extract <- terra::extract(tmin_mosaic, unique_locations, xy = T, bind = T) 
tmin_df <- as.data.frame(tmin_extract) %>%
  reshape(., varying = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
          v.names = "tmin",
          timevar = "month",
          times = as.integer(seq_len(12)),
          direction = "long") 

tmin_id <- left_join(tmin_df, latlon_id, by = "id", keep = F) %>%
  dplyr::select(-c("x", "y")) %>%
  dplyr::select(-c("id"))

 rm(tmin_BEL, tmin_CHE, tmin_DEU, tmin_ESP, tmin_GBR, tmin_extract, tmin_mosaic, tmin_sprc, rast_list)


#     b. tmax | temporal scale: monthly (30yr avg)
tmax_BEL <- geodata::worldclim_country(var = 'tmax', country = "BEL", path = file.path(dir, wclim_path), res = 0.5, version = "2.1") 
tmax_CHE <- geodata::worldclim_country(var = 'tmax', country = "CHE", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")
tmax_DEU <- geodata::worldclim_country(var = 'tmax', country = "DEU", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")
tmax_ESP <- geodata::worldclim_country(var = 'tmax', country = "ESP", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")
tmax_GBR <- geodata::worldclim_country(var = 'tmax', country = "GBR", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")

rast_list<- list(tmax_BEL, tmax_CHE, tmax_DEU, tmax_ESP, tmax_GBR)
tmax_sprc <- terra::sprc(rast_list) 
tmax_mosaic <- terra::mosaic(tmax_sprc, fun = "mean")
names(tmax_mosaic) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
tmax_extract <- terra::extract(tmax_mosaic, unique_locations, xy = T, method = "simple", bind = T) 
tmax_df <- as.data.frame(tmax_extract) %>%
  reshape(., varying = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
          v.names = "tmax",
          timevar = "month",
          times = as.integer(seq_len(12)),
          direction = "long")  

tmax_id <- left_join(tmax_df, latlon_id, by = "id", keep = F) %>%
  dplyr::select(-c("x", "y")) %>%
  dplyr::select(-c("id"))

rm(tmax_BEL, tmax_CHE, tmax_DEU, tmax_ESP, tmax_GBR, tmax_extract, tmax_mosaic, tmax_sprc, rast_list)


#     c. tavg | temporal scale: monthly (30yr avg)
tavg_BEL <- geodata::worldclim_country(var = 'tavg', country = "BEL", path = file.path(dir, wclim_path), res = 0.5, version = "2.1") 
tavg_CHE <- geodata::worldclim_country(var = 'tavg', country = "CHE", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")
tavg_DEU <- geodata::worldclim_country(var = 'tavg', country = "DEU", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")
tavg_ESP <- geodata::worldclim_country(var = 'tavg', country = "ESP", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")
tavg_GBR <- geodata::worldclim_country(var = 'tavg', country = "GBR", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")

rast_list<- list(tavg_BEL, tavg_CHE, tavg_DEU, tavg_ESP, tavg_GBR)
tavg_sprc <- terra::sprc(rast_list) 
tavg_mosaic <- terra::mosaic(tavg_sprc, fun = "mean")
names(tavg_mosaic) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
tavg_extract <- terra::extract(tavg_mosaic, unique_locations, xy = T, method = "simple", bind = T) 
tavg_df <- as.data.frame(tavg_extract) %>%
  reshape(., varying = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
          v.names = "tavg",
          timevar = "month",
          times = as.integer(seq_len(12)),
          direction = "long") 

tavg_id <- left_join(tavg_df, latlon_id, by = "id", keep = F) %>%
  dplyr::select(-c("x", "y")) %>%
  dplyr::select(-c("id"))

rm(tavg_BEL, tavg_CHE, tavg_DEU, tavg_ESP, tavg_GBR, tavg_extract, tavg_mosaic, tavg_sprc, rast_list)

#     d. prec | temporal scale: monthly (30yr avg)
prec_BEL <- geodata::worldclim_country(var = 'prec', country = "BEL", path = file.path(dir, wclim_path), res = 0.5, version = "2.1") 
prec_CHE <- geodata::worldclim_country(var = 'prec', country = "CHE", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")
prec_DEU <- geodata::worldclim_country(var = 'prec', country = "DEU", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")
prec_ESP <- geodata::worldclim_country(var = 'prec', country = "ESP", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")
prec_GBR <- geodata::worldclim_country(var = 'prec', country = "GBR", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")

rast_list<- list(prec_BEL, prec_CHE, prec_DEU, prec_ESP, prec_GBR)
prec_sprc <- terra::sprc(rast_list) 
prec_mosaic <- terra::mosaic(prec_sprc, fun = "mean")
names(prec_mosaic) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
prec_extract <- terra::extract(prec_mosaic, unique_locations, xy = T, method = "simple", bind = T) 
prec_df <- as.data.frame(prec_extract) %>%
  reshape(., varying = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
          v.names = "prec",
          timevar = "month",
          times = as.integer(seq_len(12)),
          direction = "long") %>%
  mutate(prec = (prec * 0.1))

prec_id <- left_join(prec_df, latlon_id, by = "id", keep = F) %>%
  dplyr::select(-c("x", "y")) %>%
  dplyr::select(-c("id"))

rm(prec_BEL, prec_CHE, prec_DEU, prec_ESP, prec_GBR, prec_extract, prec_mosaic, prec_sprc, rast_list)


#     e. bio | temporal scale: annual (30yr avg)
bio_BEL <- geodata::worldclim_country(var = 'bio', country = "BEL", path = file.path(dir, wclim_path), res = 0.5, version = "2.1") 
bio_CHE <- geodata::worldclim_country(var = 'bio', country = "CHE", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")
bio_DEU <- geodata::worldclim_country(var = 'bio', country = "DEU", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")
bio_ESP <- geodata::worldclim_country(var = 'bio', country = "ESP", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")
bio_GBR <- geodata::worldclim_country(var = 'bio', country = "GBR", path = file.path(dir, wclim_path), res = 0.5, version = "2.1")

rast_list<- list(bio_BEL, bio_CHE, bio_DEU, bio_ESP, bio_GBR)
bio_sprc <- terra::sprc(rast_list) 
bio_mosaic <- terra::mosaic(bio_sprc, fun = "mean")
names(bio_mosaic) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12",
                       "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")
bio_extract <- terra::extract(bio_mosaic, unique_locations, xy = T, method = "simple", bind = T) 
bio_df <- as.data.frame(bio_extract) %>%
  mutate(bio12 = (0.1*bio12), # All bioclim precip vals are in mm. Convert to cm.
         bio13 = (0.1*bio13),
         bio14 = (0.1*bio14),
         bio15 = (0.1*bio15),
         bio16 = (0.1*bio16),
         bio17 = (0.1*bio17),
         bio18 = (0.1*bio18),
         bio19 = (0.1*bio19),
         id = row_number()) 

bio_id <- left_join(bio_df, latlon_id, by = "id", keep = F) %>%
  dplyr::select(-c("x", "y")) %>%
  dplyr::select(-c("id"))

rm(bio_BEL, bio_CHE, bio_DEU, bio_ESP, bio_GBR, bio_extract, bio_mosaic, bio_sprc, rast_list)

## 6. Merge WorldClim data with main data frame
prev <- prev %>%
  left_join(., tmin_id, by = c("Lat", "Lon", "month", "country", "ADM0", "ADM1", "ADM2"), keep = F) %>%
  left_join(., tavg_id, by = c("Lat", "Lon", "month", "country", "ADM0", "ADM1", "ADM2"), keep = F) %>%
  left_join(., tmax_id, by = c("Lat", "Lon", "month", "country", "ADM0", "ADM1", "ADM2"), keep = F) %>%
  left_join(., prec_id, by = c("Lat", "Lon", "month", "country", "ADM0", "ADM1", "ADM2"), keep = F) %>%
  left_join(., bio_id, by = c("Lat", "Lon", "country", "ADM0", "ADM1", "ADM2")) %>%
  relocate(c(tmin, tavg, tmax, prec, bio1:bio19), .after = temp_t0)


#### cbind model df ####
prev$genus <- gsub("[[:space:]]", "", prev$genus) # get rid of weird spaces in this column

# Classify which sites are Bsal positive beginning at the date of the first positive Bsal observation
# A site may initially be Bsal negative and may later test positive, thus it is possible to have a site classified as both negative and positive
Bsalpos_FS <- prev %>%
  tidyr::drop_na(., any_of(c("BsalDetected", "t0"))) %>%
  subset(scientific != "Calotriton asper" & # Only one observation with NA vals for date 
         scientific != "Lissotriton boscai" & 
         scientific != "Hyla meridionalis") %>%
  group_by(Site, t0) %>%
  mutate(scientific = gsub(pattern = "Pelophylax perezi", replacement = "Pelophylax sp.", scientific),
         cumulative_prev = ave(BsalDetected == 1, FUN = cumsum),
         prev_above_0 = NA) %>%
  ungroup()


# Populate all rows with 1 or 0 based on Bsal presence at a given Site
for(i in 1:nrow(Bsalpos_FS)){
  if(Bsalpos_FS[i, 83] != 0){
    Bsalpos_FS[i, 84] = 1 # true; at least one animal at that site tested positive for Bsal at the time of the observation
  }
  else {
    Bsalpos_FS[i, 84] <- 0 # false; Site either is Bsal negative, or Bsal had not been detected at the time of the observation
  }
}



# Return data frame containing only observations from sites that have tested positive for Bsal (starting at the date the sites initially tested positive) 
prev <- Bsalpos_FS %>%
  relocate(c(cumulative_prev, prev_above_0), .after = Site) %>%
  dplyr::select(-cumulative_prev) %>%
  rename(tmin_wc = tmin, tmax_wc = tmax, tavg_wc = tavg, prec_wc = prec, bio1_wc = bio1, bio2_wc = bio2, bio3_wc = bio3, bio4_wc = bio4, bio5_wc = bio5, bio6_wc = bio6,
         bio7_wc = bio7, bio8_wc = bio8, bio9_wc = bio9, bio10_wc = bio10, bio11_wc = bio11, bio12_wc = bio12, bio13_wc = bio13, bio14_wc = bio14, bio15_wc = bio15, bio16_wc = bio16,
         bio17_wc = bio17, bio18_wc = bio18, bio19_wc = bio19, sMoist_t30 = soilMoisture_t30, sMoist_t14 = soilMoisture_t14, sMoist_t10 = soilMoisture_t10,
         sMoist_t7 = soilMoisture_t7, sMoist_t4 = soilMoisture_t4, sMoist_t0 = soilMoisture_t0, date = t0) 


dcbind <- Bsalpos_FS %>%
  relocate(c(cumulative_prev, prev_above_0), .after = Site) %>%
  group_by(Site, t0, scientific) %>%
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
  dplyr::select(country, Lat, Lon, Site, prev_above_0, t0:t30, genus, scientific:nFatalUnk_all,richness, sppAbun, siteAbun, 
                soilMoisture_t30:bio19, diagnosticLab, principalInvestigator, Sample_bcid, collectorList) %>%
  rename(tmin_wc = tmin, tmax_wc = tmax, tavg_wc = tavg, prec_wc = prec, bio1_wc = bio1, bio2_wc = bio2, bio3_wc = bio3, bio4_wc = bio4, bio5_wc = bio5, bio6_wc = bio6,
         bio7_wc = bio7, bio8_wc = bio8, bio9_wc = bio9, bio10_wc = bio10, bio11_wc = bio11, bio12_wc = bio12, bio13_wc = bio13, bio14_wc = bio14, bio15_wc = bio15, bio16_wc = bio16,
         bio17_wc = bio17, bio18_wc = bio18, bio19_wc = bio19, sMoist_t30 = soilMoisture_t30, sMoist_t14 = soilMoisture_t14, sMoist_t10 = soilMoisture_t10,
         sMoist_t7 = soilMoisture_t7, sMoist_t4 = soilMoisture_t4, sMoist_t0 = soilMoisture_t0, date = t0) 


dcbind <- with(dcbind, dcbind[order(Site, scientific), ])


## Double check everything matches
dcbind %>% dplyr::select(scientific, nPos_all, nNeg_all) %>%
  group_by(scientific) %>%
  summarise(nPos = sum(nPos_all != 0), nNeg = sum(nNeg_all != 0),
            n = sum(nPos_all, nNeg_all))

prev %>% dplyr::select(scientific, individualCount, BsalDetected) %>%
  group_by(scientific) %>%
  summarise(nPos = sum(BsalDetected != 0), nNeg = sum(BsalDetected != 1),
            n = n())

setwd(file.path(dir, csvpath))
## File for final prev dataframe:
write.csv(prev, file = "bsalData_clean.csv", row.names = FALSE)

## File for cbind model:
write.csv(dcbind, file = "bsalData_cbind.csv", row.names = FALSE)



