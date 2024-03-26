require(renv)
require(pacman)

## May need to run:
# renv::init(repos = "https://packagemanager.posit.co/cran/2023-10-13") # will install the correct versions of maptools, rgdal, and sp
# renv::restore(packages = "renv")

#### Packages ####
pckgs <- c("tidyverse", # data wrangling/manipulation
            "reshape2", # data wrangling/manipulation
          "data.table", # data wrangling/manipulation (particularly weather data)
               "rgdal", # geospatial analyses
           "lubridate", # deals with dates
               "stats", # aggregate()
              "raster", # raster manipulation/working with geospatial data
               "terra", # supercedes raster package
       "rnaturalearth", # obtain spatial polygons that can be used with sf
             "geodata", # get admin levels for each country
               "rgbif", # obtain species occurrence data
   "CoordinateCleaner", # resolve geospatial issues
             "taxlist", # standardize/update taxonomic names
           "geosphere", # distGeo(); distm()
        "measurements", # convert coordinates from DMS to DD
            "maptools", # package to create maps
               "gstat", # spatio-temporal geostatistical modelling
               "stars", # interacting with rasters as sf objects
                  "sp", # working with geospatial data
                  "sf", # working with geospatial data
             "usethis", # edit R environ to access gbif data
                  "fs"  # construct relative paths to files/directories
)

## Load packages
#### IF RENV CANNOT INSTALL/LOAD PACKAGES, USE CODE BELOW TO NAVIGATE TO OTHER .libPaths() OUTSIDE OF PROJECT.
## Home computer:
# renv::hydrate(packages = c(pckgs, "pacman"), sources = c("C:/Users/alexi/AppData/Local/R/win-library/4.3", "C:/Program Files/R/R-4.3.1/library"))
## Work computer
# renv::hydrate(packages = c(pckgs, "pacman"), sources = c("C:/Users/Alexis/AppData/Local/R/win-library/4.3", "C:/Program Files/R/R-4.3.1/library"))

pacman::p_load(pckgs, character.only = T)

## Edit .Renviron to be able to access gbif data
# usethis::edit_r_environ() # need to restart R for changes to take effect

## Create file paths for each wd
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
csvpath <- (path.expand("/csvFiles"))
shppath <- (path.expand("/csvFiles/shapefiles"))

## Functions -------------------------------------------------------------------
assign_week <- function(df, timepoint, out_col){
  for(i in 1:nrow(df)){
    if(df[[timepoint]][i] == "t8" || df[[timepoint]][i] == "t9" || df[[timepoint]][i] == "t10" || df[[timepoint]][i] == "t11" || df[[timepoint]][i] == "t12" || df[[timepoint]][i] == "t13" || df[[timepoint]][i] =="t14"){
      df[[out_col]][i] <- "wk1"
    }
    else if(df[[timepoint]][i] == "t15" || df[[timepoint]][i] == "t16" || df[[timepoint]][i] == "t17" || df[[timepoint]][i] == "t18" || df[[timepoint]][i] == "t19" || df[[timepoint]][i] == "t20" || df[[timepoint]][i] == "t21"){
      df[[out_col]][i] <- "wk2"
    }
    else if(df[[timepoint]][i] == "t22" || df[[timepoint]][i] == "t23" || df[[timepoint]][i] == "t24" || df[[timepoint]][i] == "t25" || df[[timepoint]][i] == "t26" || df[[timepoint]][i] == "t27" || df[[timepoint]][i] == "t28"){
      df[[out_col]][i] <- "wk3"
    }
    else if(df[[timepoint]][i] == "t4"){
      df[[out_col]][i] <- "t4"
    }
    else if(df[[timepoint]][i] == "t0"){
      df[[out_col]][i] <- "t0"
    }
  }
  # Return result
  return(df)
}

assign_start_date <- function(df, timepoint, date, out_col){
  for(i in 1:nrow(df)){
    if(df[[timepoint]][i] == "t0" || df[[timepoint]][i] == "t4" || df[[timepoint]][i] == "t14" || df[[timepoint]][i] == "t21" || df[[timepoint]][i] == "t28"){
      df[[out_col]][i] <- paste(df[[date]][i])
    }
  }
  return(df)
}

assign_iucn_status <- function(df1, df2, country, scientific, nativeStatus, redListCat){
  for(i in 1:nrow(df1)){
    if(df1[[scientific]][i] == df2[[scientific]][i] & df1[[country]][i] == df2[[country]][i]){
      df1[[nativeStatus]][i] <- paste(df2[[nativeStatus]][i])
      df1[[redListCat]][i] <- paste(df2[[redListCat]][i])

    }
    else{
      df1[[nativeStatus]][i] <- "Unk"
      df1[[redListCat]][i] <- "Unk"
    }

    return(df1)
  }

}

## Read in .csv files-----------------------------------------------------------
setwd(file.path(dir, csvpath))

germany <- read.csv("germany.csv", header = T, encoding = "UTF-8")
euram <- read.csv("euram.csv", header = T, encoding = "UTF-8")
belgium <- read.csv("belgium.csv", header = T, encoding = "UTF-8")
uk <- read.csv("uk.csv", header = T, encoding = "UTF-8")

spain <- read.csv("spain.csv", header = T, encoding = "UTF-8")
china <- read.csv("china.csv", header = T, encoding = "UTF-8")
vietnam <- read.csv("vietnam.csv", header = T, encoding = "UTF-8")



## Belgium, Germany, UK, and Europe/North America combined data ----------------
df <- rbind(belgium, euram, germany, uk)

## Remove empty columns
df <- Filter(function(x)!all(is.na(x)), df)

df <- df %>%
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
data.frame(colnames(df)) # returns indexed data frame
df <- df %>%
  subset(., select = -c(locality, cycleTimeFirstDetection, locationRemarks:locationID,
                        occurrenceRemarks, diseaseLineage, zeScore:DiagFALSEstics_bcid))

## Rearrange df
data.frame(colnames(df))
df <- df %>%
  relocate(country, .before = principalInvestigator) %>%
  relocate(c(ADM0:ADM2, decimalLatitude, decimalLongitude, yearCollected, monthCollected, dayCollected,
             materialSampleID, genus:scientific, susceptibility, nativeStatus, lifeStage:individualCount,
             diseaseTested:fatal, specimenFate, basisOfRecord,sampleType, testMethod, diagnosticLab, sampleRemarks,
             principalInvestigator, collectorList, Sample_bcid:projectId), .after = country) %>%
  rename(year = yearCollected,
         month = monthCollected,
         day = dayCollected,
         Lat = decimalLatitude,
         Lon = decimalLongitude)


## Spain -----------------------------------------------------------------------
data.frame(colnames(spain))
spain <- spain %>%
  subset(., select = c(country, ADM0:ADM2, Lat:Lon, yearCollected:dayCollected,
                       materialSampleID, genus:scientific, susceptibility, nativeStatus,
                       lifeStage:sex, individualCount, diseaseTested, BdDetected:fatal,
                       specimenDisposition, basisOfRecord, sampleType, testMethod,
                       diagnosticLab, location, collectorList, Sample_bcid, expeditionCode, projectId)) %>%
  replace(., . == "", NA) %>%
  rename(specimenFate = specimenDisposition,
         year = yearCollected,
         month = monthCollected,
         day = dayCollected,
         sampleRemarks = location) %>%
  mutate(principalInvestigator = "An Martel") %>%
  relocate(c(sampleRemarks, principalInvestigator), .after = diagnosticLab)


## China -----------------------------------------------------------------------
data.frame(colnames(china))
china <- china %>%
  subset(., select = c(country:ADM2, N:E, year:day, materialSampleID, genus:scientific,
                       susceptibility, nativeStatus, lifeStage:sex, individualCount, diseaseTested,
                       BdDetected:fatal, specimenDisposition, basisOfRecord:sampleType, testMethod,
                       diagnosticLab, sampleRemarks, comments, collectorList:projectId)) %>%
  # Remove non-wild observations
  filter(scientific != "Andrias davidianus") %>% # farm bred/raised animals
  unite(., sampRemarks, c(sampleRemarks, comments), sep = "; ", remove = T) %>%
  # Replace any empty cells with 'NA'
  replace(., . == "", NA) %>%
  rename(Lat = N,
         Lon = E,
         specimenFate = specimenDisposition,
         sampleRemarks = sampRemarks) %>%
  mutate(principalInvestigator = "Frank Pasmans", # Last author on Yuan et al. 2018 paper
         basisOfRecord = "LivingSpecimen",
         # Create new sample IDs based on row number -- Did not use original sample IDs here to avoid confusion:
         # Bsal+ individuals in 'Table 1.xlsx' were not associated with specific sample IDs.
         rowID_prefix = "yuan",
         rowID_suffix = sprintf("%04d", 1:nrow(.)),
         scientific = trimws(scientific, which = "right"),
         ADM1 = case_when(ADM1 == "Liaoling" ~ "Liaoning",
                          TRUE ~ ADM1)) %>%
  unite(., materialSampleID, c(rowID_prefix, rowID_suffix), sep = "_", remove = T) %>%
  relocate(materialSampleID, .after = day) %>%
  relocate(c(sampleRemarks, principalInvestigator), .after = diagnosticLab)


## Need coords for these locs.; Get centroid
china %>%
  filter(is.na(Lat)) %>%
  group_by(ADM1, ADM2) %>%
  summarise(n = n())

## Summary of Bsal+ and Bsal- spp.
china %>%
  base::subset(., select = c(scientific, BsalDetected)) %>%
  group_by(scientific, BsalDetected) %>%
  summarise(n = n())


## Vietnam ---------------------------------------------------------------------
## Fix naming conventions for Vietnam
vietnam <- vietnam %>%
  # replace empty strings with NA
  replace(., . == "", NA) %>%
  # filter out locations I can't obtain lat/lon data for
  filter(!(is.na(ADM1) & is.na(ADM2) & is.na(N_start) & is.na(E_start))) %>%
  mutate(ADM1 = case_when(ADM1 == "B?c Giang" ~ "Bắc Giang",
                          ADM1 == "V?nh Phúc" ~ "Vĩnh Phúc",
                          ADM1 == "Qu?ng Ninh" ~ "Quảng Ninh",
                          ADM1 == "L?ng S?n" ~ "Lạng Sơn",
                          ADM1 == "B?c K?n " ~ "Bắc Kạn",
                          ADM1 == "Cao B?ng " ~ "Cao Bằng",
                          ADM1 == "Cao B?ng" ~ "Cao Bằng",
                          ADM1 == "Hà Giang" ~ "Hà Giang",
                          ADM1 == "Hà Giang " ~ "Hà Giang",
                          ADM1 == "Lào Cai" ~ "Lào Cai",
                          ADM1 == "Lai Chau" ~ "Lai Châu",
                          ADM1 == "S?n La" ~ "Sơn La",
                          ADM1 == "Hòa Bình " ~ "Hòa Bình",
                          TRUE ~ ADM1),
         ADM2 = case_when(ADM2 == "S?n ??ng" ~ "Sơn Động",
                          ADM2 == "Tam ??o" ~ "Tam Đảo",
                          ADM2 == "L?c Nam" ~ "Lục Nam",
                          ADM2 == "L?c Bình" ~ "Lộc Bình",
                          ADM2 == "B?o L?c" ~ "Bảo Lạc",
                          ADM2 == "B?c Mê" ~ "Bắc Mê",
                          ADM2 == "Qu?n B?" ~ "Quản Bạ",
                          ADM2 == "B?c Quang" ~ "Bắc Quang",
                          ADM2 == "V?n Bàn" ~ "Văn Bàn",
                          ADM2 == "Sìn H?" ~ "Sìn Hồ",
                          ADM2 == "Nguyên Bình" ~ "Nguyên Bình",
                          TRUE ~ ADM2),
         scientific = trimws(scientific, which = "right"))


vietnam_coords <- vietnam %>%
  subset(., select = c(ADM1, ADM2, N_start, E_start, N_end, E_end)) %>%
  group_by(ADM1, ADM2, N_start, E_start) %>%
  unique() %>%
  mutate(Lat = as.numeric(measurements::conv_unit(N_start, from = "deg_min_sec", to = "dec_deg")),
         Lon = as.numeric(measurements::conv_unit(E_start, from = "deg_min_sec", to = "dec_deg")),
         Lon2 = as.numeric(measurements::conv_unit(E_end, from = "deg_min_sec", to = "dec_deg")),
         Lat2 = as.numeric(measurements::conv_unit(N_end, from = "deg_min_sec", to = "dec_deg")))

## For data entries with two points, find the distance between the start/end
x <- vietnam_coords %>%
  subset(., select = c(Lon, Lat, Lon2, Lat2)) %>%
  na.omit(.) %>%
  subset(., select = -c(Lon2, Lat2))

y <- vietnam_coords%>%
  subset(., select = c(Lon, Lat, Lon2, Lat2)) %>%
  na.omit(.) %>%
  subset(., select = -c(Lon, Lat))

VNM_btwn <- geosphere::gcIntermediate(x, y, n = 1, addStartEnd = F, sepNA = T) %>%
  as.data.frame(.) %>%
  na.omit(.) %>%
  rename(Lon_btwn = lon,
         Lat_btwn = lat) %>%
  cbind(., x, y) %>%
  left_join(., vietnam_coords, by = c("Lon", "Lat", "Lon2", "Lat2"), keep = F) %>%
  subset(., select = c(ADM1, ADM2, N_start, E_start, N_end, E_end, Lon_btwn, Lat_btwn)) %>%
  rename(Lon = Lon_btwn,
         Lat = Lat_btwn)

vietnam_coords <- vietnam_coords %>%
  filter(is.na(Lon2)) %>%
  subset(., select = -c(Lon2, Lat2)) %>%
  relocate(Lon, .before = Lat) %>%
  rbind(., VNM_btwn)

vietnam <- vietnam %>%
  left_join(., vietnam_coords, by = c("ADM1", "ADM2", "N_start", "E_start", "N_end", "E_end"), keep = F) %>%
  unite(., sampRemarks, c(sampleRemarks, comments), sep = "; ", remove = T) %>%
  mutate(principalInvestigator = "Nguyen",
  # Create new sample IDs based on row number -- Did not use original sample IDs here to avoid confusion:
  # Bsal+ individuals in 'Table 1.xlsx' were not associated with specific sample IDs.
  rowID_prefix = "laking",
  rowID_suffix = sprintf("%03d", 1:nrow(.))) %>%
  unite(., materialSampleID, c(rowID_prefix, rowID_suffix), sep = "_", remove = T) %>%
  relocate(materialSampleID, .after = day) %>%
  rename(specimenFate = specimenDisposition,
         sampleRemarks = sampRemarks) %>%
  subset(., select = c(country:ADM2, Lat:Lon, year:day, materialSampleID,
                       genus:scientific, susceptibility:nativeStatus, lifeStage:sex,
                       individualCount, diseaseTested, BdDetected:fatal, specimenFate,
                       basisOfRecord, sampleType, testMethod, diagnosticLab,
                       sampleRemarks, principalInvestigator, collectorList:projectId))


vietnam %>% ## Need centroids for these locs
  filter(is.na(Lat) | is.na(Lon)) %>%
  group_by(ADM1, ADM2) %>%
  summarise(n = n())


rm(vietnam_coords, VNM_btwn, x, y)
## Join Spain, China, & Vietnam to main df -------------------------------------
df <- rbind(df, spain, china, vietnam)
# df %>%
#   group_by(country, BsalDetected) %>%
#   summarise(n = n())

df <- df %>%
  # make sure individualCount is numeric
  mutate(individualCount = as.numeric(individualCount),
         scientific = trimws(scientific, which = "right")) %>%
  # assume all NA values are observations for a single individual
  plyr::mutate(individualCount = case_match(individualCount, NA ~ 1, .default = individualCount),
               # replace NA values in diseaseTested with appropriate test
               diseaseTested = case_match(diseaseTested, NA ~ "Bsal", .default = diseaseTested),
               # replace NA values in lifeStage with 'unknown'
               lifeStage = case_match(lifeStage, c(NA, "") ~ "unknown", .default = lifeStage)) %>%
  # remove rows with no data
  dplyr::filter(!(is.na(BsalDetected))) %>%
  # drop rows that include sampling from Peru or the US (imported with euram df)
  dplyr::filter(country != "Peru" & country != "United States") %>%
  # drop observations of larvae
  dplyr::filter(lifeStage != "larva" & lifeStage != "larvae" & lifeStage != "larvae, adult")


rm(belgium, euram, germany, spain, uk, vietnam, china)

## Identify/resolve any taxonomic errors ---------------------------------------
sppNames <- df %>%
  subset(., select = c(scientific, genus, species)) %>%
  unique()
sppNames <- as.data.frame(sppNames[order(sppNames$scientific), ])
print(sppNames)

df <- df %>%
  mutate(scientific = case_when(scientific %in% "Cynops cyanurus" ~ "Hypselotriton cyanurus",
                                scientific %in% "Cynops fudingensis" ~ "Hypselotriton fudingensis",
                                scientific %in% "Cynops glaucus" ~ "Hypselotriton glaucus",
                                scientific %in% "Cynops orientalis" ~ "Hypselotriton orientalis",
                                scientific %in% "Cynops orphicus" ~ "Hypselotriton orphicus",
                                scientific %in% "Paramesotriton guanxiensis" ~ "Paramesotriton guangxiensis",
                                scientific %in% "Paramesotriton sp" ~ "Paramesotriton sp.",
                                scientific %in% "Tylototriton sp" ~ "Tylototriton sp.",
                                TRUE ~ scientific),
         genus = trimws(case_when(genus %in% "Cynops" ~ "Hypselotriton",
                                  TRUE ~ genus),
                        which = "right"),
         species = trimws(case_when(species %in% "guanxiensis" ~ "guangxiensis",
                             species %in% c("sp", "sp ") ~ "sp.",
                             TRUE ~ species), which = "right"))


rm(sppNames)
#### Obtain administrative level data from geodata package ---------------------
## Construct file path to store Euro country shapefiles
dir.create(file.path(dir, shppath)) # Will give warning if path already exists
setwd(file.path(dir, shppath))

## 1. Use 'raster' pckg to get shapefiles for each country. We are using EPSG:4326 (WGS 84), as these are lat/lon data
#     that are presented in decimal degrees. EPSG:3035 (ETRS-89) was initially used. However, ETRS-89 and WGS 84 are both
#     realizations of the International Terrestrial Reference System (ITRS) coincident to within 1 meter, meaning that the
#     ETRS-89 transformation has an accuracy equal to that of WGS 84 (see:https://epsg.io/3035).
polygon <- geodata::gadm(country = c('BEL', 'CHE', 'CHN', 'DEU', 'ESP', 'GBR', 'VNM'), level = 2,
             path = file.path(dir, shppath), version = "latest", resolution = 1) %>%
  sf::st_as_sf(., crs = 4326) %>%
  st_cast(., "MULTIPOLYGON")

## Write multipolygon to .shp file for later use with WorldClim
# st_write(polygon, "europe.shp", layer_options = "ENCODING=UTF-8", append = F)

points <- df %>%
  dplyr::select(Lon, Lat) %>%
  na.omit(.) %>%
  sf::st_as_sf(x = ., coords = c("Lon", "Lat"), crs = 4326, na.fail = F) %>%
  mutate(L1 = row_number(),
         Lon = sf::st_coordinates(.)[,1],
         Lat = sf::st_coordinates(.)[,2])

## Intersect lat/lon coordinates with each raster to get the correct admin levels associated with our data
## Compared this method with previous method of loading gadm.org shapefiles into QGIS fore ea. country &
#  sampling locations, and it is just as accurate.
out <- st_intersection(points, polygon) %>%
  mutate(Lon = sf::st_coordinates(.)[,1],
         Lat = sf::st_coordinates(.)[,2])

# st_write(out, "adm_info.shp", layer_options = "ENCODING=UTF-8", append = F)

## Get centroids of these 5 that are missing lat/lon coordinates and convert lat/lon back to EPSG:4326
missing_coords <- df %>%
  filter(is.na(Lat)) %>%
  group_by(ADM1, ADM2, scientific) %>%
  summarise(n = n())

adm1_centroids <- geodata::gadm(country = c('CHN', 'VNM'), level = 1, path = file.path(dir, shppath), version = "latest", resolution = 1) %>%
  st_as_sf(., crs = 4326) %>%
  st_centroid(.) %>%
  data.frame(.) %>%
  mutate(L1 = row_number()) %>%
  relocate(L1, .before = GID_1)

adm1 <- data.frame(st_coordinates(st_cast(adm1_centroids$geometry, "POINT"))) %>%
  mutate(L1 = row_number()) %>%
  left_join(adm1_centroids, ., by = "L1", keep = F) %>% # X = LON, Y = LAT
  mutate(Lat = Y,
         Lon = X,
         ADM0 = GID_0,
         country = COUNTRY,
         ADM1 = NAME_1,
         ADM2 = NA) %>%
  subset(., select = c(Lat, Lon, ADM0, country, ADM1, ADM2)) %>%
  filter(ADM1 == "Guangdong")

adm2_centroids <- geodata::gadm(country = c('CHN','VNM'), level = 2, path = file.path(dir, shppath), version = "latest", resolution = 1) %>%
  st_as_sf(., crs = 4326) %>%
  st_centroid(.) %>%
  data.frame(.) %>%
  mutate(L1 = row_number()) %>%
  relocate(L1, .before = GID_2)

adm2 <- data.frame(st_coordinates(st_cast(adm2_centroids$geometry, "POINT"))) %>%
  mutate(L1 = row_number()) %>%
  left_join(adm2_centroids, ., by = "L1", keep = F) %>% # X = LON, Y = LAT
  mutate(Lat = Y,
         Lon = X,
         ADM0 = GID_0,
         country = COUNTRY,
         ADM1 = NAME_1,
         ADM2 = NAME_2) %>%
  subset(., select = c(Lat, Lon, ADM0, country, ADM1, ADM2)) %>%
  filter(ADM1 == "Liaoning" & ADM2 == "Anshan" |
           ADM1 == "Zhejiang" & ADM2 == "Hangzhou" |
           ADM1 == "Cao Bằng" & ADM2 == "Nguyên Bình" |
           ADM1 == "Vĩnh Phúc" & ADM2 == "Tam Đảo")

missing_coords <- df %>%
  filter(scientific == "Echinotriton maxiquadratus" |
           scientific == "Hynobius leechii" & ADM1 == "Liaoning" & ADM2 == "Anshan" |
           scientific == "Hypselotriton orientalis" & ADM1 == "Zhejiang" & ADM2 == "Hangzhou" |
           scientific == "Paramesotriton deloustali" &  ADM1 == "Vĩnh Phúc" & ADM2 == "Tam Đảo" |
           scientific == "Tylototriton ziegleri" & ADM1 == "Cao Bằng" & ADM2 == "Nguyên Bình") %>%
  dplyr::select(!("Lat":"Lon")) %>%
  left_join(., rbind(adm1, adm2), by = c("ADM0", "country", "ADM1", "ADM2"), keep = F) %>%
  relocate(c(Lat, Lon), .after = ADM2)



## Get admin levels in a dataframe format
# out <- st_read("adm_info.shp")
adminlvls <- data.frame(out) %>%
  dplyr::select(Lon, Lat, GID_0, COUNTRY, NAME_1, NAME_2) %>%
  unite("LatLon", c(Lat, Lon), sep = ", ") %>%
  rename(ADM0 = GID_0,
         ADM1 = NAME_1,
         ADM2 = NAME_2,
         country = COUNTRY) %>%
  unique(.)

centroid_adms <- rbind(adm1, adm2) %>%
  unite("LatLon", c(Lat, Lon), sep = ", ")

## Check for missing admin levels!!
# adminlvls %>%
#   filter(is.na(ADM2)) # only one location missing (ADM2 in GBR).

rm(out, points, polygon, adm1, adm1_centroids, adm2, adm2_centroids)
## Add ADM info back to main dataframe -----------------------------------------
df <- df %>%
  filter(!(scientific == "Echinotriton maxiquadratus" |
           scientific == "Hynobius leechii" & ADM1 == "Liaoning" & ADM2 == "Anshan" |
           scientific == "Hypselotriton orientalis" & ADM1 == "Zhejiang" & ADM2 == "Hangzhou" |
           scientific == "Paramesotriton deloustali" &  ADM1 == "Vĩnh Phúc" & ADM2 == "Tam Đảo" |
           scientific == "Tylototriton ziegleri" & ADM1 == "Cao Bằng" & ADM2 == "Nguyên Bình"))%>%
  rbind(., missing_coords) %>%
  dplyr::select(!("country":"ADM2")) %>%
  unite("LatLon", c(Lat, Lon), sep = ", ") %>%
  left_join(., rbind(adminlvls, centroid_adms), by = "LatLon", relationship = "many-to-one", keep = F) %>%
  relocate(c(country, ADM0, ADM1, ADM2), .before = LatLon) %>%
  separate(LatLon, c("Lat", "Lon"), sep = ", ")

# Save locations as .shp file for QGIS processing
locs <- df %>%
  subset(., select = c(Lat, Lon)) %>%
  sf::st_as_sf(x = ., coords = c("Lon", "Lat"), crs = 4326, na.fail = F) %>%
  mutate(Lon = sf::st_coordinates(.)[,1],
         Lat = sf::st_coordinates(.)[,2])

st_write(locs, "locations.shp", layer_options = "ENCODING=UTF-8", append = F)


rm(missing_coords, locs)
setwd(file.path(dir, csvpath))
## Generate Site #s ------------------------------------------------------------
# Group sites by unique lat/long combos and assign site #s to them, for all countries
siteNumber <- df %>%
  dplyr::select(materialSampleID, Lat, Lon) %>%
  dplyr::group_by(Lat, Lon) %>%
 mutate(Site = cur_group_id()) %>%
 ungroup()

df$Site <- siteNumber$Site[base::match(paste(df$materialSampleID),
                                      paste(siteNumber$materialSampleID))]

df <- relocate(df, Site, .after = "day")
rm(siteNumber)
## Add susceptibility data -----------------------------------------------------
## Susceptibility codes (based on a combination of Martel et al. 2014 and Bosch et al. 2021)
## 1 = resistant: no infection, no clinical signs of disease, no mortality
## 2 = tolerant/susceptible: variable (no to low) infection, may or may *not* result in
#                            disease, no to low mortality (animal recovers most of the time)
## 3 = lethal: high infection loads that lead to clinical signs of disease and
#              subsequent mortality >80% of the time in clinical trials
s <- read.csv("susceptibility.csv", header = T, encoding = "UTF-8")

df$susceptibility <- s$susceptibility[base::match(df$scientific, s$scientific)]

## double check for NAs
plyr::count(df, "susceptibility")

## close look at any missing data
# sus <- df %>% dplyr::filter(is.na(susceptibility)) %>%
#   group_by(scientific) %>%
#   summarise(n = n()) # 25 spp. total missing susceptibility scores
# print(sum(sus$n)) # 732 observations total, missing
rm(s, sus)
## NEED TO FIX** Add 'native status' of each species from each country -----------------------
# (data derived from iucnredlist.org)
sppPerCountry <- df %>%
  subset(., select = c(country, scientific)) %>%
  group_by(scientific, country) %>%
  summarise(n = n())
sppPerCountry <- with(sppPerCountry, sppPerCountry[order(scientific, country), ])

View(sppPerCountry)

nativeStatus <- read.csv("iucn_info.csv", header = T, encoding = "UTF-8")
head(nativeStatus)

test <- df %>%
  dplyr::select(-(nativeStatus))

# test$nativeStatus <- nativeStatus$nativeStatus[base::match(paste(test$country, test$scientific),
#                                                            paste(nativeStatus$country, nativeStatus$scientific))]

sum(is.na(tmp$nativeStatus))

tmp <- left_join(test, nativeStatus)

# native <- df %>%
#   subset(., select = c(country, scientific, nativeStatus)) %>%
#   group_by(country, scientific, nativeStatus) %>%
#   summarise()

rm(sppPerCountry, nativeStatus)
#### Calculate abundance, richness, and diversity ####
df <- unite(df, c("year", "month", "day"), sep = "-", col = "date", remove = F)

## Richness calculations include fire salamanders
## calculate relative spp richness (from our dataset)
spr <- df %>%
  dplyr::select(Site, date, scientific) %>%
  na.omit(.) %>%
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

df <- df %>%
  # species richness
  left_join(spr[,c(1:2, 63)], by = c("Site", "date")) %>%
  rename(relativeRichness = richness)

# Read in IUCN richness .csv (data processed in QGIS)
iucn_rich <- read.csv("iucn_richness.csv", header = T, encoding = "UTF-8") %>%
  rename(., iucn_rich = iucn_sr1) %>%
  dplyr::select(-(L1))

df$iucn_rich <- iucn_rich$iucn_rich[base::match(paste(df$Lat, df$Lon),
                                                  paste(iucn_rich$Lat, iucn_rich$Lon))]

# Read in IUCN rarity-weighted richness (RWR) score .csv (data processed in QGIS)
# RWR: The proportion of the species' range contained within a cell.
#      For this raster, it is 1/the total number of cells overlapped by that species' range.
#      The values are summed across all the species in the analysis to give the
#      relative importance of each cell to the species found there.
iucn_rwr <- read.csv("iucn_rwr.csv", header = T, encoding = "UTF-8") %>%
  rename(., iucn_rwr = iucn_rwr1) %>%
  dplyr::select(-(L1))


df$iucn_rwr <- iucn_rwr$iucn_rwr[base::match(paste(df$Lat, df$Lon),
                                                           paste(iucn_rwr$Lat, iucn_rwr$Lon))]


rm(iucn_rich, iucn_rwr, spr)

## Calculate abundance of individual spp at a site during each sampling event using ONLY our data
spa <- df %>%
  dplyr::select(Site, date, scientific, individualCount) # subset relevant data
spa <- aggregate(individualCount ~ scientific+Site+date, spa, sum) # aggregate by Site, date, spp. & summarise
names(spa)[names(spa) == 'individualCount'] <- 'sppAbun'

## Calculate abundance of total spp at a site during each sampling event using ONLY our data
SAb <- aggregate(sppAbun ~ Site + date, spa, sum)
names(SAb)[names(SAb) == 'sppAbun'] <- 'siteAbun'

## Add gbif.org data as an alternate measure of abundance (when applicable) ----
# ## Get species occurrence data using gbif "Sampling Event Data" -- an alternate measure of abundance?
# ## Citation: GBIF.org (25 March 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.z3jcfu
gbif_amphib <- data.table::fread("gbif_amphibs.csv") %>%
  rename(.,
         Lat = decimalLatitude,
         Lon = decimalLongitude,
         scientific = verbatimScientificName) %>%
  unite(c("year", "month", "day"), sep = "-", col = "date", remove = F) %>%
  dplyr::filter(!(day == "NA")) %>%
  # setNames(tolower(names(.))) %>% # set lowercase column names to work with CoordinateCleaner
  filter(occurrenceStatus  == "PRESENT") %>%
  filter(coordinatePrecision < 0.01 | is.na(coordinatePrecision)) %>%
  filter(coordinateUncertaintyInMeters < 400 | is.na(coordinateUncertaintyInMeters)) %>%
  # Assume NA vals == 0
  mutate(coordinateUncertaintyInMeters = tidyr::replace_na(coordinateUncertaintyInMeters, 0),
         coordinatePrecision = tidyr::replace_na(coordinatePrecision, 0)) %>%
  # mutate(coordinateUncertaintyInMeters = round(coordinateUncertaintyInMeters, 0)) %>%
  glimpse() # look at results of pipeline

tmp <- gbif_amphib %>%
  group_by(coordinateUncertaintyInMeters) %>%
  summarise(n = n())

## Create key to match dates in 'df' to dates in 'gbif_amphib'
dates <- df %>%
  dplyr::filter(!(day == "NA")) %>%
  subset(., select = date) %>%
  unique()

## Only retain observations that occurred on our sampling dates
gbif_amphib <- inner_join(dates, gbif_amphib) ## 427 of 454 dates matched observations

## Create buffer around points that correspond with their coordinate uncertainty
gbif_coords <- gbif_amphib %>%
  st_as_sf(., coords = c("Lon", "Lat"), crs = 4326)

## Buffers ended up not being necessary -- most were too small to consider
gbif_3035 <- gbif_coords %>%
  filter(countryCode == "BE" | countryCode == "GB" | countryCode == "DE" | countryCode == "ES") %>%
  st_transform(., 3035) %>%
  st_buffer(., dist = gbif_coords$coordinateUncertaintyInMeters) %>%
  st_cast(., "MULTIPOLYGON")

gbif_32649 <- gbif_coords %>%
  filter(countryCode == "CN" | countryCode == "VN" | countryCode == "HK" | countryCode == "MO") %>%
  st_transform(., 32649) %>%
  st_buffer(., dist = gbif_coords$coordinateUncertaintyInMeters) %>%
  st_cast(., "MULTIPOLYGON")

## Set aside unique combos of lat/lon/date from our dataset
points <- df %>%
  dplyr::filter(!(day == "NA")) %>%
  subset(., select = c(Lat, Lon, date)) %>%
  unique(.) %>%
  st_as_sf(x = ., coords = c("Lon", "Lat"), crs = 4326) %>%
  mutate(Lon = sf::st_coordinates(.)[,1],
         Lat = sf::st_coordinates(.)[,2])

  tmp <- sf::st_coordinates(points)


st_write(gbif_3035, file.path(dir, shppath, "gbif_3035.shp"), append = F)
st_write(gbif_32649, file.path(dir, shppath, "gbif_32649.shp"), append = F)
st_write(gbif_coords, file.path(dir, shppath, "gbif_coords.shp"), append = F)
# st_write(gbif_coordsNA, file.path(dir, shppath, "gbif_NA_data.shp"), append = F)
# st_write(points, file.path(dir, shppath, "datelatlon.shp"), append = F)

## Add abundance measures back into df
df <-
  # species abundance
  left_join(spa, by = c("scientific", "Site", "date")) %>%
  # site abundance
  left_join(SAb, by = c("Site", "date"))

## Make sure columns that have categorical data are uniform in coding
df <- df %>%
  # code all NA values as 'False'
  # mutate(BdDetected = as.factor(BdDetected),
  #        BsalDetected = as.factor(BsalDetected),
  #        fatal = as.factor(fatal)) %>%
  mutate(BdDetected = tidyr::replace_na(BdDetected, 0),
         BsalDetected = tidyr::replace_na(BsalDetected, 0))


df$fatal <- toupper(df$fatal)
df$specimenFate <- toupper(df$specimenFate)

## Convert factors with two levels to binary integers
df$BdDetected <- as.factor(df$BdDetected)
levels(df$BdDetected) <- c(0,1) #0 = F, 1 = T
df$BsalDetected <- as.factor(df$BsalDetected)
levels(df$BsalDetected) <- c(0,1) #0 = F, 1 = T
df$fatal <- as.factor(df$fatal)
levels(df$fatal) <- c(0,1) #0 = F, 1 = T

rm(s, SAb, siteNumber, spa, spr)


## Obtain unique lat/long/date combinations to extract weather data
Sys.setenv(TZ = "UTC")
weather <- df %>%
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

## get date for 1 month prior
  weather[i,29] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(30)

## get date for 2 months prior
  weather[i,30] <- as.Date(weather$date[i], format = "%Y-%m-%d") - days(60)

}


## First, get dates of the start of each week & add back into main df
start_dates <- weather %>%
  rename(t4 = ...7,
         t8 = ...8, t9 = ...9, t10 = ...10, t11 = ...11, t12 = ...12, t13 = ...13, wk1_start = ...14,
         t15 = ...15, t16 = ...16, t17 = ...17, t18 = ...18, t19 = ...19, t20 = ...20, wk2_start = ...21,
         t22 = ...22, t23 = ...23, t24 = ...24, t25 = ...25, t26 = ...26, t27 = ...27, wk3_start = ...28,
         date_m1 = ...29, date_m2 = ...30) %>%
  subset(., select = c(Lat, Lon, date_m2, date_m1, wk3_start, wk2_start, wk1_start, t4, date)) %>%
  arrange(Lat, Lon, date)

df <- df %>%
  mutate(date = base::as.Date(date, format = "%Y-%m-%d"),) %>%
  left_join(., start_dates, by = c("Lat", "Lon", "date")) %>%
  relocate(c(date_m2, date_m1, wk3_start, wk2_start, wk1_start, t4), .before = date) %>%
  subset(., select = -c(wk3_start, wk2_start)) # we really don't need these two dates, however I am keeping them in 'start_dates' in case they need to be referenced later


## Then finish editing weather df before exporting as .csv
weather <- weather %>%
  rename(t0 = date, t4 = ...7,
         t8 = ...8, t9 = ...9, t10 = ...10, t11 = ...11, t12 = ...12, t13 = ...13, t14 = ...14,
         t15 = ...15, t16 = ...16, t17 = ...17, t18 = ...18, t19 = ...19, t20 = ...20, t21 = ...21,
         t22 = ...22, t23 = ...23, t24 = ...24, t25 = ...25, t26 = ...26, t27 = ...27, t28 = ...28,
         t30 = ...29, t60 = ...30) %>%
  group_by(Lat, Lon) %>%
  pivot_longer(cols = c(t0, t4, t8, t9, t10, t11, t12, t13, t14,
                        t15, t16, t17, t18, t19, t20, t21,
                        t22, t23, t24, t25, t26, t27, t28,
                        t30, t60),
               names_to = "timepoint",
               values_to = "date") %>%
  ungroup() %>%
  distinct_all() %>%
  relocate(c(timepoint, date), .after = Lon) %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date)) %>%
  dplyr::select(-("date")) %>%
  filter(year != '1905') # likely a museum specimen --  not useful for our analyses


weather_d <- weather %>%
  filter(timepoint != "t30" & timepoint != "t60")

weather_m <- weather %>%
  filter(timepoint == "t30" | timepoint == "t60")


tmp <- weather %>%
  unite(., "date", c("year", "month", "day"), sep = "-")
## Export to use in Python and PyQGIS to obtain weather data
# write.csv(weather_d, 'weather.csv', row.names = F, fileEncoding = "UTF-8")
# write.csv(weather_m, 'weather_m.csv', row.names = F, fileEncoding = "UTF-8")
# rm(weather)

## Python v3.12.0 used to download .nc4 files from NASA's EarthData data repository for each date and location.
## PyQGIS Python v3.9.5 used to process data in QGIS v3.28.3-Firenze.

## Import DAILY temperature & soil moisture data from NASA's EarthData website (citation below) ----
#  Li, B., H. Beaudoing, and M. Rodell, NASA/GSFC/HSL (2020), GLDAS Catchment Land Surface Model L4 daily 0.25 x 0.25 degree GRACE-DA1 V2.2,
#     Greenbelt, Maryland, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Last accessed: 2024-03-25. doi:10.5067/TXBMLX370XX8.
#  Li, B., M. Rodell, S. Kumar, H. Beaudoing, A. Getirana, B. F. Zaitchik, et al. (2019) Global GRACE data assimilation for groundwater and drought
#     monitoring: Advances and challenges. Water Resources Research, 55, 7564-7586. doi:10.1029/2018wr024618.
gldas_daily <- read.csv("weather_merged.csv", header = T, encoding = "UTF-8")

gldas_daily <- gldas_daily %>%
  subset(., select = -c(fid, layer, path)) %>%
  mutate(row = row_number(),
         day = as.integer(case_match(day, "true" ~ "1", .default = day)),
         month = as.integer(case_match(month, "true" ~ "1", .default = month))) %>%
  relocate(row, .before = Lat) %>%
  unite(c("year", "month", "day"), sep = "-", col = "date", remove = F) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         start_date = NA,
         week = NA) %>%
  rename(soilMoisture = "SOILMOIST_kgm.21",
         temp = "SURFTEMP_K1") %>%
  assign_week(.,"timepoint", "week")


## Separate and process temperature data ---------------------------------------
temperature <- gldas_daily %>%
  dplyr::select(-c(row, soilMoisture, year, month, day)) %>%
  drop_na(temp) %>%
  # Assign start date for each week
  assign_start_date(., "timepoint", "date", "start_date") %>%
  mutate(temp = as.numeric(temp - 273.15)) %>% # Convert temp from K to C
  unique()

daily_temp <- temperature %>%
  filter((week == "t0" |week == "t4")) %>%
  arrange(Lat, Lon, week) %>%
  # for t0 and t4, we just need to copy the temp for that day -- no need to avg.
  mutate(avg_temp = temp)

## Calculate mean temperature for each week
avg_temp <- temperature %>%
  filter(!(week == "t0" |week == "t4")) %>%
  distinct_at(., .vars = c("Lat", "Lon", "timepoint", "date"), .keep_all = T) %>%
  arrange(Lat, Lon, week) %>%
  group_by(Lat, Lon, week) %>%
  mutate(avg_temp = mean(temp)) %>%
  rbind(daily_temp) %>%
  drop_na() %>%
  mutate(id = row_number())
#temp <- aggregate(temp ~ Lat+Lon+week, FUN = mean, data = temperature)

## Add mean temperatures back into temperature df
temperature <- temperature %>%
  drop_na(start_date) %>%
  left_join(., avg_temp, by = c("Lat", "Lon", "timepoint", "temp", "date", "start_date", "week")) %>%
  dplyr::select(-c(temp, start_date, timepoint)) %>%
  pivot_wider(names_from = week, values_from = c(date, avg_temp))


## Separate and process soil moisture data -------------------------------------
soilMoisture <- gldas_daily %>%
  dplyr::select(-c(row, temp, year, month, day)) %>%
  drop_na(soilMoisture) %>%
  # Assign start date for each week
  assign_start_date(., "timepoint", "date", "start_date") %>%
  unique()

daily_SM <- soilMoisture %>%
  filter((week == "t0" |week == "t4")) %>%
  arrange(Lat, Lon, week) %>%
  # for t0 and t4, we just need to copy the temp for that day -- no need to avg.
  mutate(avg_SM = soilMoisture)

avg_soilMoist <- soilMoisture %>%
  filter(!(week == "t0" |week == "t4")) %>%
  distinct_at(., .vars = c("Lat", "Lon", "timepoint", "date"), .keep_all = T) %>%
  arrange(Lat, Lon, week) %>%
  group_by(Lat, Lon, week) %>%
  mutate(avg_SM = mean(soilMoisture)) %>%
  rbind(daily_SM) %>%
  drop_na() %>%
  mutate(id = row_number())

soilMoisture <- soilMoisture %>%
  drop_na(start_date) %>%
  left_join(., avg_soilMoist, by = c("Lat", "Lon", "timepoint", "soilMoisture", "date", "start_date", "week")) %>%
  dplyr::select(-c(soilMoisture, start_date, timepoint)) %>%
  pivot_wider(names_from = week, values_from = c(date, avg_SM))


## Combine weekly soil moisture/temp data frames -------------------------------
weatherData <- left_join(temperature, soilMoisture, by = c("id", "Lat", "Lon", "date_wk3", "date_wk2",
                                                 "date_wk1", "date_t4", "date_t0")) %>%
  dplyr::select(-c(id, date_wk3, date_wk2)) %>%
  # these column names can be shortened
  rename(wk1_start = date_wk1, t4 = date_t4, date = date_t0,
         temp_wk3 = avg_temp_wk3, temp_wk2 = avg_temp_wk2, temp_wk1 = avg_temp_wk1, temp_d4 = avg_temp_t4, temp_d = avg_temp_t0,
         sMoist_wk3 = avg_SM_wk3, sMoist_wk2 = avg_SM_wk2, sMoist_wk1 = avg_SM_wk1, sMoist_d4 = avg_SM_t4, sMoist_d = avg_SM_t0) %>%
  mutate(date = base::as.Date(date, format = "%Y-%m-%d"),
         t4 = base::as.Date(t4, format = "%Y-%m-%d"),
         wk1_start = base::as.Date(wk1_start, format = "%Y-%m-%d"))

## Add back to main df
df <- df %>%
  plyr::mutate(Lat = as.double(Lat),
               Lon = as.double(Lon)) %>%
  left_join(., weatherData, by = c("Lat", "Lon", "date", "t4", "wk1_start")) %>%
  relocate(c(richness, sppAbun, siteAbun), .after = Site) %>%
  relocate(c(wk1_start, t4, date), .after = date_m1) %>%
  relocate(c(temp_wk3, temp_wk2, temp_wk1, temp_d4, temp_d,
             sMoist_wk3, sMoist_wk2, sMoist_wk1, sMoist_d4, sMoist_d), .after = sampleRemarks)

## Import MONTHLY temperature, precip, & soil moisture data from NASA's EarthData website (citation below)
## Beaudoing, H. and M. Rodell, NASA/GSFC/HSL (2020), GLDAS Noah Land Surface Model L4 monthly 0.25 x 0.25 degree V2.1,
## Greenbelt, Maryland, USA,Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/SXAVCZFAQLNO
gldas_monthly <- read.csv("monthly_weather.csv", header = T, encoding = "UTF-8")

gldas_monthly <- gldas_monthly %>%
  mutate(monthCollected = as.numeric(dplyr::recode(monthCollected,
                                                   "true" = "1")),
         dayCollected = as.numeric(dplyr::recode(dayCollected,
                                                 "true" = "1"))) %>%
  unite(c("yearCollected", "monthCollected", "dayCollected"), sep = "-", col = "date", remove = F) %>%
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
  dplyr::select(decimalLatitude, decimalLongitude, date, timepoint, temp) %>%
  # exclude rows with sM and precip data, as well as any NAs in the temp data
  drop_na() %>%
  group_by(decimalLatitude, decimalLongitude, timepoint) %>%
  distinct_all() %>%
  # assign row # for matching purposes
  mutate(row = row_number(),
         date = as.Date(date, format = "%Y-%m-%d"),
         # Convert temp from K to C
         temp = as.numeric(temp - 273.15)) %>%
  dplyr::group_by(decimalLatitude, decimalLongitude, timepoint) %>%
  pivot_wider(names_from = timepoint, values_from = c(date, temp)) %>%
  rename(date_m2 = "date_date_t2",
         date_m1 = "date_date_t1",
         date = "date_date",
         temp_m2 = "temp_date_t2",
         temp_m1 = "temp_date_t1",
         temp_m = "temp_date",
         Lat = decimalLatitude,
         Lon = decimalLongitude)

precip_m <- gldas_monthly %>%
  dplyr::select(decimalLatitude, decimalLongitude, date, timepoint, precip) %>%
  drop_na() %>%
  group_by(decimalLatitude, decimalLongitude, timepoint) %>%
  distinct_all() %>%
  mutate(row = row_number(),
         date = as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::group_by(decimalLatitude, decimalLongitude, timepoint) %>%
  pivot_wider(names_from = timepoint, values_from = c(date, precip)) %>%
  rename(date_m2 = "date_date_t2",
         date_m1 = "date_date_t1",
         date = "date_date",
         precip_m2 = "precip_date_t2",
         precip_m1 = "precip_date_t1",
         precip_m = "precip_date",
         Lat = decimalLatitude,
         Lon = decimalLongitude) %>%
  # convert mm to cm to match precip climate data
  mutate(precip_m2 = precip_m2*0.1,
         precip_m1 = precip_m1*0.1,
         precip_m = precip_m*0.1)


gldas_monthly <- gldas_monthly %>%
  dplyr::select(decimalLatitude, decimalLongitude, date, timepoint, soilMoisture) %>%
  drop_na() %>%
  group_by(decimalLatitude, decimalLongitude, timepoint) %>%
  distinct_all() %>%
  mutate(row = row_number(),
         date = as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::group_by(decimalLatitude, decimalLongitude, timepoint) %>%
  pivot_wider(names_from = timepoint, values_from = c(date, soilMoisture)) %>%
  rename(date_m2 = "date_date_t2",
         date_m1 = "date_date_t1",
         date = "date_date",
         sMoist_m2 = "soilMoisture_date_t2",
         sMoist_m1 = "soilMoisture_date_t1",
         sMoist_m = "soilMoisture_date",
         Lat = decimalLatitude,
         Lon = decimalLongitude)


# Add temp and precip data back into gldas_monthly df in preparation to add it back to the main df.
gldas_monthly <- gldas_monthly %>%
  left_join(., precip_m, by = c("Lat", "Lon", "date", "date_m1", "date_m2", "row")) %>%
  left_join(., temp_m, by = c("Lat", "Lon", "date", "date_m1", "date_m2", "row")) %>%
  subset(., select = -c(row))

# Add temp and precip data back into gldas_monthly df in preparation to add it back to the main df.
# monthlyWeather <- sMoist_m %>%
#   left_join(., precip_m, by = c("Lat", "Lon", "date", "date_m1", "date_m2", "row")) %>%
#   left_join(., temp_m, by = c("Lat", "Lon", "date", "date_m1", "date_m2", "row")) %>%
#   subset(., select = -c(row, date_m1, date_m2, temp_m2, sMoist_m2, precip_m2)) %>%
#   mutate(Lat = round(Lat, 6),
#          Lon = round(Lon, 6))


## Add monthly weather data to main dataframe ----------------------------------
df <- df %>%
  left_join(., gldas_monthly, by = c("Lat", "Lon", "date", "date_m1", "date_m2")) %>%
  relocate(c(temp_m2, temp_m1, temp_m), .before = temp_wk3) %>%
  relocate(c(sMoist_m2, sMoist_m1, sMoist_m), .before = sMoist_wk3) %>%
  relocate(c(temp_m2, temp_m1, temp_m, temp_wk3, temp_wk2, temp_wk1, temp_d4, temp_d,
             sMoist_m2, sMoist_m1, sMoist_m, sMoist_wk3, sMoist_wk2, sMoist_wk1, sMoist_d4, sMoist_d,
             precip_m2, precip_m1, precip_m), .after = sampleRemarks)


temp_prev <- df ## in case I need to come back to this point
#df <- temp_prev
rm(temp_m, precip_m, avg_soilMoist, daily_SM, avg_temp, daily_temp, gldas_daily, soilMoisture, temperature)


## Sampling elevation with polygons instead of points --------------------------
#     a. First, we need to obtain elevation as a raster layer (elevatr package)
#        zoom 5 = 2.5 arc minutes; same resolution as our other data
setwd(file.path(dir, csvpath))
elevation <- get_elev_raster(col_adm2, prj = "EPSG:4326", src = "aws", z = 5)

#     b. Use ADM geometries to sample elevation raster
elev_crop <- crop(elev_COL, col_adm2)
elev_mask <- mask(elev_crop, col_adm2)
elev_extract <- as.data.frame(raster::extract(x = elev_mask, y = col_adm2, fun = mean, df = T, sp = T))
elev_df <- elev_extract %>%
  dplyr::select(8, 3, 15)
# Elevation column name subject to change if code is ran at a later date.
names(elev_df) <- c("ADM1", "ADM2", "elev_m")

#     c. Add back to main dataframe
colombia <- left_join(colombia, elev_df, by = c("ADM1", "ADM2")) %>%
  relocate(., elev_m, .before = tmin)

## The following was used to check for missing vals -- retaining just in case
# missing <- colombia %>%
#   dplyr::select(ADM0, ADM1, ADM2, Lat, Lon, EpiYear, EpiWeek, Disease, Incidence) %>%
#   filter(is.na("elev_m")) %>%
#   dplyr::select(ADM1, ADM2) %>%
#   unique()


# Combine BdDetected and BsalDetected into one "diseaseDetected" column
df <- df %>%
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
df$diseaseDetected <- as.factor(df$diseaseDetected)
levels(df$diseaseDetected) <- c(0,1) #0 = F, 1 = T


## Climate data from geodata package
## Construct file path to store WorldClim data
dir.create(file.path(dir, shppath, "/WorldClim")) # Will give warning if path already exists
wclim_path <- path.expand("csvFiles/shapefiles/WorldClim")
setwd(file.path(dir, wclim_path))

## Create bounding box for Europe extent to crop WorldClim rasters
euro_ext <- c(-15, 45, 35, 72)

## add id column to adminlvls df
adminlvls <- adminlvls %>%
  separate(., LatLon, into = c("Lat", "Lon"), sep = ", ", remove = T) %>%
  plyr::mutate(Lat = as.numeric(Lat),
               Lon = as.numeric(Lon))

latlon_id <- adminlvls %>%
  mutate(id = row_number()) %>%
  relocate(id, .before = Lat)


## Obtain WorldClim data as SpatRasters and extract data using lat/lon from adminlvls df
## WorldClim resolution of 2.5 arc minutes is approximately equivalent to 0.0417 degrees (finer scale than needed, but that's ok)
#     a. tmin | temporal scale: monthly (30yr avg)
tmin <- geodata::worldclim_global(var = 'tmin', path = file.path(dir, wclim_path), res = 2.5, version = "2.1")

tmin_cropped <- terra::crop(tmin, euro_ext)
names(tmin_cropped) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
tmin_extract <- terra::extract(tmin_cropped, points[,1], ID = F)
tmin_df <- as.data.frame(tmin_extract) %>%
  reshape(., varying = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
          v.names = "tmin",
          timevar = "month",
          times = as.integer(seq_len(12)),
          direction = "long") %>%
  left_join(., latlon_id, by = "id", keep = F) %>%
  relocate(c(id, ADM0, country, ADM1, ADM2, Lon, Lat), .before = month)


rm(tmin, tmin_extract)


#     b. tmax | temporal scale: monthly (30yr avg)
tmax <- geodata::worldclim_global(var = 'tmax', path = file.path(dir, wclim_path), res = 2.5, version = "2.1")

tmax_cropped <- terra::crop(tmax, euro_ext)
names(tmax_cropped) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
tmax_extract <- terra::extract(tmax_cropped, points[,1], ID = F)
tmax_df <- as.data.frame(tmax_extract) %>%
  reshape(., varying = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
          v.names = "tmax",
          timevar = "month",
          times = as.integer(seq_len(12)),
          direction = "long") %>%
  left_join(., latlon_id, by = "id", keep = F) %>%
  relocate(c(id, ADM0, country, ADM1, ADM2, Lon, Lat), .before = month)


rm(tmax, tmax_extract)


#     c. tavg | temporal scale: monthly (30yr avg)
tavg <- geodata::worldclim_global(var = 'tavg', path = file.path(dir, wclim_path), res = 2.5, version = "2.1")

tavg_cropped <- terra::crop(tavg, euro_ext)
names(tavg_cropped) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
tavg_extract <- terra::extract(tavg_cropped, points[,1], ID = F)
tavg_df <- as.data.frame(tavg_extract) %>%
  reshape(., varying = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
          v.names = "tavg",
          timevar = "month",
          times = as.integer(seq_len(12)),
          direction = "long") %>%
  left_join(., latlon_id, by = "id", keep = F) %>%
  relocate(c(id, ADM0, country, ADM1, ADM2, Lon, Lat), .before = month)


rm(tavg, tavg_extract)

#     d. prec | temporal scale: monthly (30yr avg)
prec <- geodata::worldclim_global(var = 'prec', path = file.path(dir, wclim_path), res = 2.5, version = "2.1")

prec_cropped <- terra::crop(prec, euro_ext)
names(prec_cropped) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
prec_extract <- terra::extract(prec_cropped, points[,1], ID = F)
prec_df <- as.data.frame(prec_extract) %>%
  reshape(., varying = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
          v.names = "prec",
          timevar = "month",
          times = as.integer(seq_len(12)),
          direction = "long") %>%
  left_join(., latlon_id, by = "id", keep = F) %>%
  relocate(c(id, ADM0, country, ADM1, ADM2, Lon, Lat), .before = month) %>%
  mutate(prec = (prec * 0.1)) # convert mm to cm


rm(prec, prec_extract)

#     e. bio | temporal scale: annual (30yr avg)
bio <- geodata::worldclim_global(var = 'bio', path = file.path(dir, wclim_path), res = 2.5, version = "2.1")

bio_cropped <- terra::crop(bio, euro_ext)
names(bio_cropped) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12",
                        "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")
bio_extract <- terra::extract(bio_cropped, points[,1], ID = F)
bio_df <- as.data.frame(bio_extract) %>%
  mutate(bio12 = (0.1*bio12), # All bioclim precip vals are in mm. Convert to cm.
         bio13 = (0.1*bio13),
         bio14 = (0.1*bio14),
         bio15 = (0.1*bio15),
         bio16 = (0.1*bio16),
         bio17 = (0.1*bio17),
         bio18 = (0.1*bio18),
         bio19 = (0.1*bio19),
         id = row_number()) %>%
  left_join(., latlon_id, by = "id", keep = F) %>%
  relocate(c(id, ADM0, country, ADM1, ADM2, Lon, Lat), .before = bio1)


rm(bio, bio_extract)


## 6. Merge WorldClim data with main data frame
wclim <- tmin_df %>%
  left_join(., tavg_df, by = c("id", "Lat", "Lon", "month", "country", "ADM0", "ADM1", "ADM2"), keep = F) %>%
  left_join(., tmax_df, by = c("id", "Lat", "Lon", "month", "country", "ADM0", "ADM1", "ADM2"), keep = F) %>%
  left_join(., prec_df, by = c("id", "Lat", "Lon", "month", "country", "ADM0", "ADM1", "ADM2"), keep = F) %>%
  left_join(., bio_df, by = c("id", "Lat", "Lon", "country", "ADM0", "ADM1", "ADM2")) %>%
  dplyr::select(-(id))

rm(tmin_cropped, tmin_df, tavg_cropped, tavg_df, tmax_cropped, tmax_df, bio_cropped, bio_df, latlon_id, prec_cropped, prec_df)

df <- df %>%
  left_join(., wclim, by = c("Lat", "Lon", "month", "country", "ADM0", "ADM1", "ADM2"), keep = F) %>%
  relocate(c(tmin, tavg, tmax, prec, bio1:bio19), .after = precip_m)


df$genus <- gsub("[[:space:]]", "", df$genus) # get rid of weird spaces in this column


#### cbind model df ####
# Classify which sites are Bsal positive, i.e., if that site has ever had any Bsal+ cases
Bsalpos <- df %>%
  tidyr::drop_na(., any_of(c("BsalDetected", "date", "sMoist_d", "temp_d"))) %>%
  # Drop species that have <10 observations
  subset(scientific != "Calotriton asper" & # Only one observation with NA vals for date
         scientific != "Lissotriton boscai" &
         scientific != "Hyla meridionalis" &
         scientific != "Pleurodeles waltl") %>%
  mutate(scientific = gsub(pattern = "Pelophylax perezi", replacement = "Pelophylax sp.", scientific))
  subset(scientific != "Pelophylax sp.") %>% # now has 2 obs. need to drop.
    group_by(Site) %>%
    mutate(posSite = ifelse(sum(BsalDetected) != 0, 1, 0)) # bool; The site associated with this observation has tested positive (1) for Bsal at some point, or has never (0) tested positive for Bsal


## Double checking #s
nBsalPos <- aggregate(individualCount ~ BsalDetected+scientific, data = Bsalpos, sum) %>%
  pivot_wider(names_from = BsalDetected, values_from = individualCount)
View(nBsalPos)


# Add all data back into 'df' data frame, even sites that are negative
df <- Bsalpos %>%
  relocate(c(posSite), .after = Site) %>%
  subset(., select = -c(date_m1, date_m2, wk1_start, t4, year, month, day)) %>%
  rename(tmin_wc = tmin, tmax_wc = tmax, tavg_wc = tavg, prec_wc = prec, bio1_wc = bio1, bio2_wc = bio2, bio3_wc = bio3, bio4_wc = bio4, bio5_wc = bio5, bio6_wc = bio6,
         bio7_wc = bio7, bio8_wc = bio8, bio9_wc = bio9, bio10_wc = bio10, bio11_wc = bio11, bio12_wc = bio12, bio13_wc = bio13, bio14_wc = bio14, bio15_wc = bio15, bio16_wc = bio16,
         bio17_wc = bio17, bio18_wc = bio18, bio19_wc = bio19)


# Return data frame containing all observations from countries that had confirmed Bsal positive sites (will separate out Bsal+ and Bsal- sites later)
# Bsalpos_cbind <- df %>%
#   relocate(c(posSite), .after = Site) %>%
#   filter(posSite == 1)  %>%
#   group_by(Site, date, scientific) %>%
#   mutate(nPos_FS = sum(BsalDetected != 0 & scientific == "Salamandra salamandra"),
#          nNeg_FS = sum(BsalDetected == 0 & scientific == "Salamandra salamandra"),
#          nDead_FS = sum(fatal != 0 & scientific == "Salamandra salamandra", na.rm = T),
#          nAlive_FS = sum(fatal == 0 & scientific == "Salamandra salamandra", na.rm = T),
#          nFatalUnk_FS = sum(is.na(fatal) & scientific == "Salamandra salamandra"),
#          nPos_all = sum(BsalDetected != 0),
#          nNeg_all = sum(BsalDetected == 0),
#          nDead_all = sum(fatal != 0, na.rm = T),
#          nAlive_all = sum(fatal == 0, na.rm = T),
#          nFatalUnk_all = sum(is.na(fatal)),
#          nPos_all_noFS = sum(BsalDetected != 0 & scientific != "Salamandra salamandra"),
#          nNeg_all_noFS = sum(BsalDetected == 0 & scientific != "Salamandra salamandra"),
#          nDead_all_noFS = sum(fatal != 0 & scientific != "Salamandra salamandra", na.rm = T),
#          nAlive_all_noFS = sum(fatal == 0 & scientific != "Salamandra salamandra", na.rm = T),
#          nFatalUnk_all_noFS = sum(is.na(fatal)),
#          posSite = as.factor(posSite)) %>%
#   slice(1) %>%
#   ungroup() %>%
#   relocate(c(nPos_FS, nNeg_FS, nDead_FS, nAlive_FS, nFatalUnk_FS, nPos_all, nNeg_all, nDead_all, nAlive_all, nFatalUnk_all,
#              nPos_all_noFS, nNeg_all_noFS, nDead_all_noFS, nAlive_all_noFS, nFatalUnk_all_noFS), .after = nativeStatus) %>%
#   dplyr::select(country, Lat, Lon, Site, posSite, date, genus, scientific:nFatalUnk_all_noFS, richness, sppAbun, siteAbun,
#                 temp_m2:bio19_wc, diagnosticLab, principalInvestigator, Sample_bcid, collectorList, sMoist_m2:temp_m2)
#
# Bsalpos_cbind <- with(Bsalpos_cbind, Bsalpos_cbind[order(Site, scientific), ])


Bsal_all <- df %>%
  relocate(c(posSite), .after = Site) %>%
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
         nPos_all_noFS = sum(BsalDetected != 0 & scientific != "Salamandra salamandra"),
         nNeg_all_noFS = sum(BsalDetected == 0 & scientific != "Salamandra salamandra"),
         nDead_all_noFS = sum(fatal != 0 & scientific != "Salamandra salamandra", na.rm = T),
         nAlive_all_noFS = sum(fatal == 0 & scientific != "Salamandra salamandra", na.rm = T),
         nFatalUnk_all_noFS = sum(is.na(fatal)),
         posSite = as.factor(posSite)) %>%
  slice(1) %>%
  ungroup() %>%
  relocate(c(nPos_FS, nNeg_FS, nDead_FS, nAlive_FS, nFatalUnk_FS, nPos_all, nNeg_all, nDead_all, nAlive_all, nFatalUnk_all,
             nPos_all_noFS, nNeg_all_noFS, nDead_all_noFS, nAlive_all_noFS, nFatalUnk_all_noFS), .after = nativeStatus) %>%
  dplyr::select(ADM0:ADM2, Lat, Lon, Site, posSite, date, genus, scientific:nFatalUnk_all_noFS, richness, sppAbun, siteAbun,
                temp_m2:bio19_wc, diagnosticLab, principalInvestigator, Sample_bcid, collectorList, sMoist_m2:temp_m2)


# check_dcbind <- dcbind %>%
#   tidyr::drop_na(., any_of(c(24:42))) %>%
#   filter(country == "Germany" | country == "Spain")
# filtered_dcbind <- dcbind %>%
#   filter(country == "Germany" | country == "Spain")
#
#
# missing <- anti_join(check_dcbind, filtered_dcbind)

## Double check everything matches
Bsal_all %>% dplyr::select(scientific, nPos_all, nNeg_all) %>%
  group_by(scientific) %>%
  summarise(nPos = sum(nPos_all), nNeg = sum(nNeg_all),
            n = sum(nPos_all, nNeg_all))

df %>% dplyr::select(scientific, individualCount, BsalDetected) %>%
  group_by(scientific) %>%
  summarise(nPos = sum(BsalDetected != 0), nNeg = sum(BsalDetected != 1),
            n = n())

setwd(file.path(dir, csvpath))
## File for final df dataframe:
write.csv(df, file = "Bsal_all.csv", row.names = FALSE)

# ## File for cbind model -- only Bsal positive sites:
# write.csv(Bsalpos_cbind, file = "cbind_posSites.csv", row.names = FALSE)

## File for cbind model -- only Bsal positive countries:
write.csv(Bsal_all, file = "cbind_allSites.csv", row.names = FALSE)


## Export data for collaborators to xlsx sheet:
options(java.parameters = "-Xmx8000m")
library(openxlsx)
library(rJava)
setwd(file.path(dir, csvpath))
df <- read.csv("Bsal_all.csv", header = TRUE, encoding = "UTF-8")

all_data <- df %>%
  filter(country == "Germany") %>%
  dplyr::select(country, ADM0, ADM1, ADM2, Lat, Lon, date, Site, posSite,
                richness, siteAbun, materialSampleID, genus, species,
                scientific, susceptibility,  nativeStatus, sex, individualCount,
                BsalDetected, BsalLoad, fatal, specimenFate, basisOfRecord,
                sampleType, testMethod, diagnosticLab, sampleRemarks, principalInvestigator,
                collectorList, Sample_bcid, expeditionCode, projectId) %>%
  group_by(Site) %>%
  mutate(total_pos = ave(BsalDetected == 1, FUN = sum)) %>%
  relocate(total_pos, .after = posSite)


repeated_sampling <- all_data %>%
  dplyr::select(c(Site, date)) %>%
  unique() %>%
  group_by(Site) %>%
  summarise(samplingEvents = n())


all_data <- left_join(all_data, repeated_sampling, by = "Site") %>%
  relocate(samplingEvents, .after = Site)
all_data <- all_data[order(all_data$Site),]
data.frame(colnames(all_data))


site_summary <- all_data %>%
  subset(., select = c(country:Lon, Site:posSite, BsalDetected, total_pos,
                       diagnosticLab, principalInvestigator,
                       collectorList, expeditionCode, projectId)) %>%
  unique()

#site_summary <- site_summary[order(site_summary$Site),]

repeated_sampling <- site_summary %>%
  filter(samplingEvents > 1)

positive_sites <- site_summary %>%
  subset(., select = -(BsalDetected)) %>%
  filter(posSite != 0) %>%
  unique()


# Create workbook to store all 'sheets'
Germany_wb <- createWorkbook()

addWorksheet(Germany_wb, "germany_all")
addWorksheet(Germany_wb, "site_summary")
addWorksheet(Germany_wb, "repeated_sampling")
addWorksheet(Germany_wb, "positive_sites")

writeData(Germany_wb, "germany_all", all_data, colNames = T)
writeData(Germany_wb, "site_summary", site_summary, colNames = T)
writeData(Germany_wb, "repeated_sampling", repeated_sampling, colNames = T)
writeData(Germany_wb, "positive_sites", positive_sites, colNames = T)

## Save workbook
saveWorkbook(Germany_wb, "Germany_data.xlsx", overwrite = TRUE)
