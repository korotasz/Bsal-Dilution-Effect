library(tidyverse)
library(reshape2)
library(codyn)
library(raster)
library(ncdf4)
library(maptools)
library(geodata) # cmip6 
library(gstat)
library(sp)

setwd('C:/Users/alexi/OneDrive/Documents/01_GradSchool/_Dissertation work/Chapter4/03_code')
euram <- read.csv("EurAm_Bsal.csv", header = T, encoding = "UTF-8")
germany <- read.csv("Germany_Bsal.csv", header = T, encoding = "UTF-8")
uk <- read.csv("UK_Bsal.csv", header = T, encoding = "UTF-8")
spain <- read.csv("Spain.csv", header = T, encoding = "UTF-8")

## combine dataframes
prev <- rbind(euram, germany, uk)

## remove empty columns
prev <- Filter(function(x)!all(is.na(x)), prev)

## Rename countries in the "countries" column based on lat/long coords
prev$country <- gsub(prev$country, pattern = "USA",
                     replacement = "United States")
prev$country <- gsub(prev$country, pattern = "Italy", 
                     replacement = "Switzerland")

## Delete irrelevant columns
data.frame(colnames(prev))
prev <- prev[-c(1, 3, 21, 24, 28, 31)]


## Add empty columns for climactic variables/clean up the df a bit
newcols <- c("ADM0", "ADM1", "ADM2", "monthCollected", "susceptibility", "scientific", 
             "tmin", "tmax", "prec", "bio1", "bio2", "bio3", "bio4", "bio5", "bio6", 
             "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", 
             "bio16", "bio17", "bio18", "bio19")
prev[newcols] <- NA 
names(prev)[names(prev) == 'specificEpithet'] <- 'species' # change name of this column
prev$scientific <- paste(prev$genus, prev$species) # combine genus & species 
data.frame(colnames(prev)) # returns indexed data frame 
prev <- prev[, c(5, 1, 30:32, 3:4, 19:20, 2, 14:15, 6:8, # reorder columns to make more intuitive
                 34, 33, 9:13, 16:18, 22:25, 35:56, 21, 26:29)] 
prev$monthCollected <- cut(prev$dayCollected, breaks= c(0,4,9,13,17,22,26,30,35,39,43,48,Inf), 
                          labels = month.abb)
prev$monthCollected <- match(prev$monthCollected, month.abb)

## Remove rows with no data from Spain
spain <- spain %>% 
  dplyr::filter(!(materialSampleID=="")) %>%
  dplyr::filter(!(quantityDetected=="NA"))

## Add Spain
prev <- rbind(prev, spain)

prev <- prev %>%
  # make sure individualCount is numeric
  mutate(individualCount = as.numeric(individualCount)) %>% 
  # assume all NA values are observations for a single individual
  replace_na(list(individualCount = 1)) %>%
  # remove all pooled samples
  dplyr::filter(!(individualCount != "1")) %>%
  # drop rows that only tested for Bd
  dplyr::filter(!(diseaseTested == "Bd")) %>%
  # rename elevation column 
  rename(Elevation = minimumElevationInMeters)


## Export data frame to work with in QGIS
#write.csv(prev, 'C:/Users/alexi/OneDrive/Documents/01_Grad.School/_Dissertation work/BsalData/02_originalData/bsalprev.csv',
#          row.names = FALSE)

## Read in attribute table with ADM data and case match ADM levels in prev
a <- read.csv("admattribute.csv", header = T, encoding = "UTF-8")
a2 <- a %>%
     dplyr::select(NAME_0, GID_0, NAME_1, NAME_2, materialSampleID)
colnames(a2) <- c("country", "ADM0", "ADM1", "ADM2", "materialSampleID")

prev$ADM0 = a2$ADM0[base::match(paste(prev$country), 
                                paste(a2$country))]
prev$ADM1 = a2$ADM1[base::match(paste(prev$ADM0, prev$materialSampleID), 
                                      paste(a2$ADM0, a2$materialSampleID))]
prev$ADM2 = a2$ADM2[base::match(paste(prev$ADM1, prev$materialSampleID), 
                                paste(a2$ADM1, a2$materialSampleID))]
prev$ADM1 <- toupper(prev$ADM1)
prev$ADM2 <- toupper(prev$ADM2)

## Drop rows that include sampling from Peru or the US
prev <- prev[!(prev$ADM0 == "PER" | prev$ADM0 == "USA"),]

## Group sites by unique lat/long combos and assign site #s to them, for all countries excluding Spain
temp <- prev%>%
  dplyr::filter(country != "Spain") %>%
  dplyr::select(materialSampleID, decimalLatitude, decimalLongitude) %>%
  dplyr::group_by(decimalLatitude, decimalLongitude) %>%
  mutate(Site = cur_group_id()) %>% ## group_indices() deprecated
  ungroup()
temp <- temp %>%
  dplyr::select(materialSampleID, Site) 

## Assign Spain Site #s based on locations in Martel 2020 supplement 
#  (Exact lat/long was not given for this data)
martel <- read.csv("Martel2020supplement.csv", header = T, encoding = "UTF-8")
tempSpain <- martel %>%
  dplyr::select(Code, Location) %>%
  mutate(Site = group_indices(., Location))
names(tempSpain)[names(tempSpain) == 'Code'] <- 'materialSampleID'
tempSpain$Site <- tempSpain$Site+290
tempSpain <- tempSpain %>%
  dplyr::select(materialSampleID, Site) 


tempSite <- rbind(temp, tempSpain) # combine the two for easier matching

prev$Site <- NA

prev$Site = tempSite$Site[base::match(paste(prev$materialSampleID), 
                                      paste(tempSite$materialSampleID))]

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
## Add abundance and richness columns in
prev$sppAbun <- NA
prev$siteAbun <- NA
prev$richness <- NA
prev$alphadiv <- NA

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
names(spa)[names(spa) == 'individualCount'] <- 'SppAbun'

## Calculate abundance of total spp at a site during each sampling event
SAb <- aggregate(SppAbun ~ Site + date, spa, sum)
names(SAb)[names(SAb) == 'SppAbun'] <- 'SiteAbun'

## Add abundance and richness back into prev df
prev <- prev %>%
  left_join(spr[,c(1:2,22)], by = c("Site", "date")) %>%
  mutate(richness = Richness) %>%
  left_join(spa, by = c("scientific", "Site", "date")) %>%
  mutate(sppAbun = SppAbun) %>%
  left_join(SAb[,c(1:2,3)], by = c("Site", "date")) %>%
  mutate(siteAbun = SiteAbun)

## Calculate community diversity
## Consult codyn package documentation --  I'm not sure if this is right
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
prev$diseaseDetected <- tolower(prev$diseaseDetected)
prev$fatal <- tolower(prev$fatal)
prev$specimenDisposition <- tolower(prev$specimenDisposition)

## Convert factors with two levels to binary integers
prev$diseaseDetected <- as.factor(prev$diseaseDetected)
levels(prev$diseaseDetected) <- c(0,1) #0 = F, 1 = T
prev$fatal <- as.factor(prev$fatal)
levels(prev$fatal) <- c(0,1) #0 = F, 1 = T


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

CHEadm2 <- raster::getData('GADM', country = CHE$ADM0, level = 2)
DEUadm2 <- raster::getData('GADM', country = DEU$ADM0, level = 2)
GBRadm2 <- raster::getData('GADM', country = GBR$ADM0, level = 2)
ESPadm2 <- raster::getData('GADM', country = ESP$ADM0, level = 2)


#### Obtain WorldClim data as raster layers ####
tmin <- raster::getData('worldclim', var = 'tmin', res = 2.5)
gain(tmin) = 0.1 # convert to C
tmax <- raster::getData('worldclim', var = 'tmax', res = 2.5)
gain(tmax) = 0.1 # convert to C
prec <- raster::getData('worldclim', var = 'prec', res = 2.5)
gain(prec) = 0.1 # convert to cm
bio <- raster::getData('worldclim', var = 'bio', res = 2.5)
gain(bio) = 0.1 # convert data to C and cm


###### Use ADM geometries to sample WorldClim rasters 
#### tmin ####
### CHE
r1 <- crop(tmin, extent(CHEadm2)) 
r_tmin1 <- mask(r1, CHEadm2)
rsp_tmin1 <- as.data.frame(raster::extract(x = r_tmin1, y = CHEadm2, fun = mean, sp = T))
rsp_tmin1 <- rsp_tmin1 %>% 
  dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, tmin1, tmin2, tmin3, tmin4, 
                tmin5, tmin6, tmin7, tmin8, tmin9, tmin10, tmin11, tmin12)
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
r2 <- crop(tmin, extent(DEUadm2)) 
r_tmin2 <- mask(r2, DEUadm2)
rsp_tmin2 <- as.data.frame(raster::extract(x = r_tmin2, y = DEUadm2, fun = mean, sp = T))
rsp_tmin2 <- rsp_tmin2 %>% 
  dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, tmin1, tmin2, tmin3, tmin4, 
                tmin5, tmin6, tmin7, tmin8, tmin9, tmin10,tmin11, tmin12)
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
r3 <- crop(tmin, extent(GBRadm2)) 
r_tmin3 <- mask(r3, GBRadm2)
rsp_tmin3 <- as.data.frame(raster::extract(x = r_tmin3, y = GBRadm2, fun = mean, sp = T))
rsp_tmin3 <- rsp_tmin3 %>% dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, tmin1, tmin2, tmin3, 
                                         tmin4, tmin5, tmin6, tmin7, tmin8, tmin9, tmin10, 
                                         tmin11, tmin12)
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
r4 <- crop(tmin, extent(ESPadm2)) 
r_tmin4 <- mask(r4, ESPadm2)
rsp_tmin4 <- as.data.frame(raster::extract(x = r_tmin4, y = ESPadm2, fun = mean, sp = T))
rsp_tmin4 <- rsp_tmin4 %>% dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, tmin1, tmin2, tmin3, 
                                         tmin4, tmin5, tmin6, tmin7, tmin8, tmin9, tmin10, 
                                         tmin11, tmin12)
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


rsp_tmin <- rbind(rsp_tmin1, rsp_tmin2, rsp_tmin3, rsp_tmin4)
rsp_tmin$monthCollected <- match(rsp_tmin$monthCollected, month.abb)

#### tmax ####
### CHE
r1 <- crop(tmax, extent(CHEadm2)) 
r_tmax1 <- mask(r1, CHEadm2)
rsp_tmax1 <- as.data.frame(raster::extract(x = r_tmax1, y = CHEadm2, fun = mean, sp = T))
rsp_tmax1 <- rsp_tmax1 %>% 
  dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, tmax1, tmax2, tmax3, tmax4, 
                tmax5, tmax6, tmax7, tmax8, tmax9, tmax10, tmax11, tmax12)
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
r2 <- crop(tmax, extent(DEUadm2)) 
r_tmax2 <- mask(r2, DEUadm2)
rsp_tmax2 <- as.data.frame(raster::extract(x = r_tmax2, y = DEUadm2, fun = mean, sp = T))
rsp_tmax2 <- rsp_tmax2 %>% 
  dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, tmax1, tmax2, tmax3, tmax4, 
                tmax5, tmax6, tmax7, tmax8, tmax9, tmax10,tmax11, tmax12)
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
r3 <- crop(tmax, extent(GBRadm2)) 
r_tmax3 <- mask(r3, GBRadm2)
rsp_tmax3 <- as.data.frame(raster::extract(x = r_tmax3, y = GBRadm2, fun = mean, sp = T))
rsp_tmax3 <- rsp_tmax3 %>% dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, tmax1, tmax2, tmax3, 
                                         tmax4, tmax5, tmax6, tmax7, tmax8, tmax9, tmax10, 
                                         tmax11, tmax12)
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
r4 <- crop(tmax, extent(ESPadm2)) 
r_tmax4 <- mask(r4, ESPadm2)
rsp_tmax4 <- as.data.frame(raster::extract(x = r_tmax4, y = ESPadm2, fun = mean, sp = T))
rsp_tmax4 <- rsp_tmax4 %>% dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, tmax1, tmax2, tmax3, 
                                         tmax4, tmax5, tmax6, tmax7, tmax8, tmax9, tmax10, 
                                         tmax11, tmax12)
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


rsp_tmax <- rbind(rsp_tmax1, rsp_tmax2, rsp_tmax3, rsp_tmax4)
rsp_tmax$monthCollected <- match(rsp_tmax$monthCollected, month.abb)


#### precip ####
### CHE
r1 <- crop(prec, extent(CHEadm2)) 
r_prec1 <- mask(r1, CHEadm2)
rsp_prec1 <- as.data.frame(raster::extract(x = r_prec1, y = CHEadm2, fun = mean, sp = T))
rsp_prec1 <- rsp_prec1 %>% 
  dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, prec1, prec2, prec3, prec4, 
                prec5, prec6, prec7, prec8, prec9, prec10, prec11, prec12)
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
r2 <- crop(prec, extent(DEUadm2)) 
r_prec2 <- mask(r2, DEUadm2)
rsp_prec2 <- as.data.frame(raster::extract(x = r_prec2, y = DEUadm2, fun = mean, sp = T))
rsp_prec2 <- rsp_prec2 %>% 
  dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, prec1, prec2, prec3, prec4, 
                prec5, prec6, prec7, prec8, prec9, prec10,prec11, prec12)
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
r3 <- crop(prec, extent(GBRadm2)) 
r_prec3 <- mask(r3, GBRadm2)
rsp_prec3 <- as.data.frame(raster::extract(x = r_prec3, y = GBRadm2, fun = mean, sp = T))
rsp_prec3 <- rsp_prec3 %>% dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, prec1, prec2, prec3, 
                                         prec4, prec5, prec6, prec7, prec8, prec9, prec10, 
                                         prec11, prec12)
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
r4 <- crop(prec, extent(ESPadm2)) 
r_prec4 <- mask(r4, ESPadm2)
rsp_prec4 <- as.data.frame(raster::extract(x = r_prec4, y = ESPadm2, fun = mean, sp = T))
rsp_prec4 <- rsp_prec4 %>% dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, prec1, prec2, prec3, 
                                         prec4, prec5, prec6, prec7, prec8, prec9, prec10, 
                                         prec11, prec12)
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

rsp_prec <- rbind(rsp_prec1, rsp_prec2, rsp_prec3, rsp_prec4)
rsp_prec$monthCollected <- match(rsp_prec$monthCollected, month.abb)


#### bio; these are annual trends, not monthly ####
### CHE
r1 <- crop(bio, extent(CHEadm2)) 
r_bio1 <- mask(r1, CHEadm2)
rsp_bio1 <- as.data.frame(raster::extract(x = r_bio1, y = CHEadm2, fun = mean, sp = T))
head(rsp_bio1)
rsp_bio1 <- rsp_bio1 %>% 
  dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, bio1, bio2, bio3, bio4, 
                bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, 
                bio14, bio15, bio16, bio17, bio18, bio19)
colnames(rsp_bio1) <- c("ADM0", "Country", "ADM1", "ADM2", "bio1", "bio2", "bio3",
                        "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10",
                        "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17",
                        "bio18", "bio19")
rsp_bio1$ADM1 <- toupper(rsp_bio1$ADM1)
rsp_bio1$ADM2 <- toupper(rsp_bio1$ADM2)
head(rsp_bio1)

### DEU
r2 <- crop(bio, extent(DEUadm2)) 
r_bio2 <- mask(r2, DEUadm2)
rsp_bio2 <- as.data.frame(raster::extract(x = r_bio2, y = DEUadm2, fun = mean, sp = T))
head(rsp_bio2)
rsp_bio2 <- rsp_bio2 %>% 
  dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, bio1, bio2, bio3, bio4, 
                bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, 
                bio14, bio15, bio16, bio17, bio18, bio19)
colnames(rsp_bio2) <- c("ADM0", "Country", "ADM1", "ADM2", "bio1", "bio2", "bio3",
                        "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", 
                        "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", 
                        "bio18", "bio19")
rsp_bio2$ADM1 <- toupper(rsp_bio2$ADM1)
rsp_bio2$ADM2 <- toupper(rsp_bio2$ADM2)
head(rsp_bio2)

### GBR
r3 <- crop(bio, extent(GBRadm2)) 
r_bio3 <- mask(r3, GBRadm2)
rsp_bio3 <- as.data.frame(raster::extract(x = r_bio3, y = GBRadm2, fun = mean, sp = T))
head(rsp_bio3)
rsp_bio3 <- rsp_bio3 %>% 
  dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, bio1, bio2, bio3, bio4, 
                bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, 
                bio14, bio15, bio16, bio17, bio18, bio19)
colnames(rsp_bio3) <- c("ADM0", "Country", "ADM1", "ADM2", "bio1", "bio2", "bio3",
                        "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", 
                        "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", 
                        "bio18", "bio19")
rsp_bio3$ADM1 <- toupper(rsp_bio3$ADM1)
rsp_bio3$ADM2 <- toupper(rsp_bio3$ADM2)
head(rsp_bio3)

### ESP
r4 <- crop(bio, extent(ESPadm2)) 
r_bio4 <- mask(r4, ESPadm2)
rsp_bio4 <- as.data.frame(raster::extract(x = r_bio4, y = ESPadm2, fun = mean, sp = T))
head(rsp_bio4)
rsp_bio4 <- rsp_bio4 %>% 
  dplyr::select(GID_0, NAME_0, NAME_1, NAME_2, bio1, bio2, bio3, bio4, 
                bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, 
                bio14, bio15, bio16, bio17, bio18, bio19)
colnames(rsp_bio4) <- c("ADM0", "Country", "ADM1", "ADM2", "bio1", "bio2", "bio3",
                        "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", 
                        "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", 
                        "bio18", "bio19")
rsp_bio4$ADM1 <- toupper(rsp_bio4$ADM1)
rsp_bio4$ADM2 <- toupper(rsp_bio4$ADM2)
head(rsp_bio4)



rsp_bio <- rbind(rsp_bio1, rsp_bio2, rsp_bio3, rsp_bio4)




#### Obtain elevation data as raster layers (SRTM) ####
CHE_el <- raster::getData('alt', country = 'CHE', mask = F)
DEU_el <- raster::getData('alt', country = 'DEU', mask = F)
GBR_el <- raster::getData('alt', country = 'GBR', mask = F)
ESP_el <- raster::getData('alt', country = 'ESP', mask = F)


## Create dfs to extract values from rasters
CHE_pts <- prev %>%
  dplyr::select(ADM0, decimalLongitude, decimalLatitude) %>%
  dplyr::filter(ADM0 == "CHE")
CHE_pts <- unique(CHE_pts)
CHE_pts <- CHE_pts %>%
  dplyr::select(decimalLongitude, decimalLatitude)

DEU_pts <- prev %>%
  dplyr::select(ADM0, decimalLongitude, decimalLatitude) %>%
  dplyr::filter(ADM0 == "DEU")
DEU_pts <- unique(DEU_pts)
DEU_pts <- DEU_pts %>%
  dplyr::select(decimalLongitude, decimalLatitude)

GBR_pts <- prev %>%
  dplyr::select(ADM0, decimalLongitude, decimalLatitude) %>%
  dplyr::filter(ADM0 == "GBR")
GBR_pts <- unique(GBR_pts)
GBR_pts <- GBR_pts %>%
  dplyr::select(decimalLongitude, decimalLatitude)

ESP_pts <- prev %>%
  dplyr::select(ADM0, decimalLongitude, decimalLatitude) %>%
  dplyr::filter(ADM0 == "ESP")
ESP_pts <- unique(ESP_pts)
ESP_pts <- ESP_pts %>%
  dplyr::select(decimalLongitude, decimalLatitude)


## Sample rasters using lat/long
CHE_elval <- extract(CHE_el, CHE_pts)
CHE_elval <- cbind(CHE_pts, CHE_elval)
names(CHE_elval)[names(CHE_elval) == 'CHE_elval'] <- 'Elevation' 

DEU_elval <- extract(DEU_el, DEU_pts)
DEU_elval <- cbind(DEU_pts, DEU_elval)
names(DEU_elval)[names(DEU_elval) == 'DEU_elval'] <- 'Elevation' 

GBR_elval <- extract(GBR_el, GBR_pts)
GBR_elval <- cbind(GBR_pts, GBR_elval)
names(GBR_elval)[names(GBR_elval) == 'GBR_elval'] <- 'Elevation' 

ESP_elval <- extract(ESP_el, ESP_pts)
ESP_elval <- cbind(ESP_pts, ESP_elval)
names(ESP_elval)[names(ESP_elval) == 'ESP_elval'] <- 'Elevation' 

elevation <- rbind(CHE_elval, DEU_elval, GBR_elval, ESP_elval)


#### Obtain projected climate change data (CMIP6) ####
#### Merge extracted data to original dataframe ####
prev$tmin = rsp_tmin$tmin[base::match(paste(prev$ADM2, prev$monthCollected), 
                                           paste(rsp_tmin$ADM2, rsp_tmin$monthCollected))]
prev$tmax = rsp_tmax$tmax[base::match(paste(prev$ADM2, prev$monthCollected), 
                                           paste(rsp_tmax$ADM2, rsp_tmax$monthCollected))]
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
prev$Elevation = elevation$Elevation[base::match(paste(prev$decimalLatitude, prev$decimalLongitude), 
                                      paste(elevation$decimalLatitude, elevation$decimalLongitude))]



data.frame(colnames(prev))








#### Export cleaned data to .csv to work on in the "analyses" R file ####
prev <-  prev %>%
  mutate(materialSampleID = factor(materialSampleID),
         country = factor(country),
         ADM0 = factor(ADM0),
         ADM1 = factor(ADM1),
         ADM2 = factor(ADM2),
         Site = factor(Site),
         yearCollected = factor(yearCollected),
         dayCollected = factor(dayCollected),
         basisOfRecord = factor(basisOfRecord),
         genus = factor(genus),
         scientific = factor(scientific),
         susceptibility = factor(susceptibility),
         sampleType = factor(sampleType),
         lifeStage = factor(lifeStage),
         sex = factor(sex),
         diseaseDetected = factor(diseaseDetected),
         fatal = factor(fatal),
         projectId = as.character(projectId))




## OG file:
write.csv(prev, 'C:/Users/alexi/OneDrive/Documents/01_GradSchool/_Dissertation work/Chapter4/03_code/bsalprev_final.csv',
          row.names = FALSE)







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

















