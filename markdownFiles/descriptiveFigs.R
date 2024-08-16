## GETTING STARTED -------------------------------------------------------------
## Please review the program version requirements in the .README document associated with this GitHub repository.
## 1. Make sure you have the correct version of R (4.3.3 "Angel Food Cake") loaded for this session.

## 2. Load 'renv'
require(renv)

## 3. Restore project dependencies from the renv lockfile ('renv.lock'). You should get a message that
##    no issues have been found and that the project is in a consistent state.
# renv::restore()

## If step 3 does give an error, try running:
# renv::init(repos = "https://packagemanager.posit.co/cran/2023-10-13") # (should install the correct versions of maptools, rgdal, and sp)

## Packages --------------------------------------------------------------------
## These packages need to be loaded first (commented out pckgs only need to be run once)
# remotes::install_version("Rttf2pt1", version = "1.3.8") # install this version, latest ver. not compatible
# remotes::install_github("gorkang/html2latex") # convert sjPlot::tab_model() hmtl table to tex and pdf in .Rmd docs
# extrafont::font_import("C:/Windows/Fonts") # load fonts before ggplot2; only need to do this once

## As of 2024-04-04, there are issues with patchwork and ggplot2 that require specific pull requests to resolve:
# remotes::install_github("thomasp85/patchwork")
# remotes::install_github("tidyverse/ggplot2", ref = remotes::github_pull("5592"))


require(pacman)
extrafont::loadfonts(device = "all", quiet = T) # plot fonts

### General Visualization Packages----------------------------------------------
pckgs <- c("tidyverse", # data wrangling/manipulation
             "ggbreak", # create axis breaks in ggplots
              "ggtext", # for text type/arrangements w/ ggplot2
            "Rttf2pt1", # to use with the extrafont package
            "ggthemes", # contains 'scales', 'themes', and 'geoms' packages
           "grDevices", # saves high quality svg, pdf, and ps files
            "graphics", # dependency of grDevices
             "cowplot", # arranging plots/figs
           "gridExtra", # arranging plots/figs                                  ### PROBABLY DELETE
           "patchwork", # arranging plots/figs
           "ggpattern", # plot patterns inside of maps
          "hrbrthemes", # plot colors                                           ### PROBABLY DELETE
        "RColorBrewer", # plot colors                                           ### PROBABLY DELETE
             "viridis", # plot colors                                           ### PROBABLY DELETE
### Mapping Packages -----------------------------------------------------------
            "eurostat", # obtain spatial data from europe                       ### PROBABLY DELETE
             "geodata", # obtain geographic data (world map)
       "rnaturalearth", # obtain spatial polygons that can be used with sf
                  "sf", # mapping
           "ggspatial", # north arrow and scale bar
             "mapproj", # apply map projection
          "scatterpie"  # add pie charts to maps                                ### PROBABLY DELETE
           # "latex2exp", # allows use of LaTeX in R
           #      "glue", # allows concatenation of LaTeX and R syntax
           #    "magick", # image_read() -- needed for cowplot::draw_image
)

## Load packages
pacman::p_load(pckgs, character.only = T)
#### IF RENV CANNOT INSTALL/LOAD PACKAGES, USE CODE BELOW TO NAVIGATE TO OTHER .libPaths() OUTSIDE OF PROJECT.
## Home computer:
# renv::hydrate(packages = c(pckgs, "pacman"), sources = c("C:/Users/alexi/AppData/Local/R/win-library/4.3", "C:/Program Files/R/R-4.3.1/library"))
## Work computer
# renv::hydrate(packages = c(pckgs, "pacman"), sources = c("C:/Users/Alexis/AppData/Local/R/win-library/4.3", "C:/Program Files/R/R-4.3.1/library"))


## Functions -------------------------------------------------------------------
map_bounds <- function(x1, x2, y1, y2, crs){
  df <- data.frame(Lon = c(x1, x2),
                   Lat = c(y1, y2)) %>%
    st_as_sf(., coords = c("Lon", "Lat"), crs = 4326) %>%
    st_transform(., crs)

  out_df <- data.frame(x1 = sf::st_coordinates(df)[1,1],
                       x2 = sf::st_coordinates(df)[2,1],
                       y1 = sf::st_coordinates(df)[1,2],
                       y2 = sf::st_coordinates(df)[2,2])
  return(out_df)

}

# flip <- function(data) {
#   new <- data[rev(rownames(data)), ]
#   rownames(new) <- NULL
#   new
# }

## File paths ------------------------------------------------------------------
dir <- rstudioapi::getActiveProject()
mdpath <- file.path(dir, path.expand("markdownFiles"))
figpath <- file.path(mdpath, path.expand("figures"))
shppath <- file.path(dir, path.expand("csvFiles/shapefiles"))

## Read in .csv files and prep data --------------------------------------------
setwd(mdpath)
## All data -- not meant for analyses, just visualization.
d <- read.csv("BsalData_all.csv", header = T, encoding = "UTF-8") %>%
  # transform vars
  mutate(logsppAbun = log(sppAbun + 1),
         logsiteAbun = log(siteAbun + 1),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
  relocate(c(logsppAbun, logsiteAbun), .after = sppAbun)

## only data I am confident about
# d_conf <- read.csv("BsalData_OK.csv", header = T, encoding = "UTF-8") %>%
#   # transform vars
#   mutate(logsppAbun = log(sppAbun + 1),
#          logsiteAbun = log(siteAbun + 1),
#          scientific = as.factor(scientific),
#          susceptibility = as.factor(susceptibility)) %>%
#   relocate(c(logsppAbun, logsiteAbun), .after = sppAbun)



## 'dcbind' dataset is what will be used to run our models
dcbind_all <- read.csv("Bsal_cbind_all.csv", header = T, encoding = "UTF-8") %>%
  # transform vars
  mutate(logsppAbun = log(sppAbun + 1),
         logsiteAbun = log(siteAbun + 1),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
  relocate(c(logsppAbun, logsiteAbun), .after = sppAbun)


## 'dcbind' dataset is what will be used to run our models
# dcbind_conf <- read.csv("Bsal_cbind_OK.csv", header = T, encoding = "UTF-8") %>%
#   # transform vars
#   mutate(logsppAbun = log(sppAbun + 1),
#          logsiteAbun = log(siteAbun + 1),
#          scientific = as.factor(scientific),
#          susceptibility = as.factor(susceptibility)) %>%
#   relocate(c(logsppAbun, logsiteAbun), .after = sppAbun)


## Data prep for cbind models: drop rows with NA vals in weather data & scale relevant vars
#   bio1_wc = "annual mean temperature" | bio12_wc = annual precipitation
dcbindScaled <- dcbind_all %>%
  filter(continent == "Europe") %>%
  tidyr::drop_na(., any_of(c("temp_d", "sMoist_d", "tmin_wc:bio1_wc", "bio12_wc"))) %>%
  mutate_at(c("temp_d", "sMoist_d",
              "bio1_wc", "bio12_wc", "tavg_wc", "prec_wc"),
            ~(scale(., center = T, scale = T %>% as.numeric))) %>%
  mutate(year = year(date),
         locality = case_when(is.na(locality) ~ ADM2,
                              TRUE ~ locality))


# dcbindScaled_conf <- dcbind_conf %>%
#   filter(continent == "Europe") %>%
#   tidyr::drop_na(., any_of(c("temp_d", "sMoist_d", "tmin_wc:bio1_wc", "bio12_wc"))) %>%
#   mutate_at(c("temp_d", "sMoist_d",
#               "bio1_wc", "bio12_wc", "tavg_wc", "prec_wc"),
#             ~(scale(., center = T, scale = T %>% as.numeric))) %>%
#   mutate(year = year(date),
#          locality = case_when(is.na(locality) ~ ADM2,
#                               TRUE ~ locality))


## Define general ggplot theme for plot uniformity -----------------------------
ak_theme <- theme_ipsum(base_family = "Segoe UI Light") +
  theme(axis.line = element_line(color = 'black'),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32, face = "italic"),
        axis.title.x = element_text(size = 34, hjust = 0.5, face = "plain",
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 34, hjust = 0.5, face = "plain",
                                    margin = margin(t = 0, r = 15, b = 0, l = 5)),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 42, hjust = 0.5, face = "plain"),
        plot.subtitle = element_markdown(size = 18, face = "plain"),
        plot.caption = element_markdown(hjust = 0, size = 14, face = "plain"),
        plot.caption.position = "plot",
        plot.tag = element_text(size = 36, face = "bold"),
        legend.position = "top",
        legend.spacing = unit(1, 'cm'),
        legend.key.size = unit(2,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 32, hjust = 0),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.spacing.y = unit(1.5,"cm"),
        strip.text = element_text(size = 14, face = "bold", hjust = 0.5))



## I. Generate maps ------------------------------------------------------------
# Europe polygons were re-projected to EPSG:27704 (WGS84/Equi7 Europe) for mapping
# purposes. These projections were made as a part of a larger project to optimize
# handling remote-sensing data, with each continent having their own centroid.
#  doi: https://doi.org/10.1016/j.cageo.2014.07.005
#  Equi7Grid GitHub: https://github.com/TUW-GEO/Equi7Grid
epsg27704 <- crs('+proj=aeqd +lat_0=53 +lon_0=24 +x_0=5837287.81977 +y_0=2121415.69617 +datum=WGS84 +units=m +no_defs')

obs <- d %>%
  dplyr::select(country, ADM0, ADM1, ADM2, locality, Lat, Lon, BsalDetected, individualCount) %>%
  filter(country == "Germany" | country == "Spain") %>%
  plyr::mutate(BsalDetected = as.factor(dplyr::recode(BsalDetected,
                                                      "1" = "Bsal positive",
                                                      "0" = "Bsal negative"))) %>%
  st_as_sf(., coords = c("Lon", "Lat"), crs = 4326) %>%
  mutate(jittered = sf::st_jitter(geometry, 0.25)) %>%
  arrange(BsalDetected)

# Get the coordinates of each country
## TRY & USE EUROSTAT
country_lookup <- read.csv("countries.csv", stringsAsFactors = F) %>%
  rename(country_code = country,
         country = name)

# Summarise data & combine with country_lookup
sampSize <- obs %>%
  group_by(country) %>%
  summarise(n = n())

# Combine summarised data
mapLabels <- left_join(sampSize, country_lookup, by = "country") %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>%
  subset(., select = c(country, country_code, n, latitude, longitude, geometry))

# Obtain world map
worldmap <- ne_countries(scale = "medium", type = "map_units", returnclass = "sf") %>%
  st_transform(., crs = 4326)


# Summarise the number of observations from each Bsal+ country and plot individual points on a map.
obs <- obs %>%
  filter(country == "Germany" | country == "Spain") %>%
  st_transform(., crs = epsg27704)

# Subset country polygons for base maps
europe <- worldmap %>%
  filter(continent == "Europe" & !name %in% c("Russia")) %>%
  st_transform(., crs = epsg27704)

# Subset polygons for Bsal+ countries
countries <- worldmap %>%
  filter(sovereignt %in% c("Spain", "Germany")) %>%
  st_transform(., crs = epsg27704)

### a. Data overview: Europe ---------------------------------------------------
map_bounds(-8, 15, 34, 56, crs = epsg27704)

europe_map <- ggplot() +
  geom_sf(data = europe, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = countries, aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  geom_sf(data = obs, aes(geometry = jittered, fill = BsalDetected, shape = BsalDetected),
          alpha = 0.3, size = 4, stroke = 1, color = "gray30", show.legend = "point") +
  geom_sf_label(data = filter(mapLabels, country == "Germany"), aes(label = country), nudge_x = 300000, size = 7,
                fontface = "plain", label.size = NA, alpha = 0) +
  geom_sf_label(data = filter(mapLabels, country == "Spain"), aes(label = country), nudge_x = -180000, nudge_y = -220000,
                size = 7,
                fontface = "plain", label.size = NA, alpha = 0) +
  scale_fill_manual(values = c("gray40", "#b30000"), guide = "none") +
  scale_shape_manual(values = c(21, 24), guide = "none") +
  coord_sf(xlim = c(2903943, 5277030), # c(-9, 15)
           ylim = c(625595.8, 2491036)) + # c(34, 56)
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 2.5, text_face = "plain",
                   pad_y = unit(0.5, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(2.5, "cm"), width = unit(2.5, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 1.8, text_size = 18)) +
  ak_theme + theme(legend.title = element_blank(),
                   legend.position = "top",
                   legend.spacing = unit(1, 'cm'),
                   legend.key.size = unit(1,"cm"),
                   legend.text = element_text(size = 38, hjust = 0,
                                              margin = margin(l = 5, r = 30, unit = "pt")),
                   axis.text.x = element_text(size = 38, angle = 45, vjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 38, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = c("gray40", "#b30000"),
                                                 shape = c(21, 24),
                                                 size = c(7, 7),
                                                 alpha = c(1, 1))))



europe_map

# ggsave("Europe.pdf", europe_map, device = cairo_pdf, path = file.path(dir, figpath, "/maps"),
#        width = 2900, height = 2500, scale = 2, units = "px", dpi = 300, limitsize = F)

### b. Germany -----------------------------------------------------------------
#### > Individual data points --------------------------------------------------
map_bounds(7.5, 14.5, 46.5, 55.5, crs = epsg27704)

deu_map <- ggplot() +
  geom_sf(data = europe, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = countries, aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  geom_sf(data = obs, aes(geometry = jittered, fill = BsalDetected, shape = BsalDetected),
          alpha = 0.3, size = 4, stroke = 1, color = "gray30", show.legend = "point") +
  scale_fill_manual(values = c("gray40", "#b30000"), guide = "none") +
  scale_shape_manual(values = c(21, 24), guide = "none") +
  coord_sf(xlim = c(4452875, 5238551), # c(7.5, 14.5)
           ylim = c(1540088, 2435744)) + # c(46.5, 55.5)
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 2.5, text_face = "plain",
                   pad_y = unit(0.5, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(2.5, "cm"), width = unit(2.5, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 1.8, text_size = 18)) +
  ak_theme + theme(legend.title = element_blank(),
                   legend.position = "top",
                   legend.spacing = unit(1, "cm"), # Space legend labels
                   legend.key.size = unit(1,"cm"),
                   legend.text = element_text(size = 32, hjust = 0,
                                              margin = margin(l = 5, r = 30, unit = "pt")),
                   axis.text.x = element_text(size = 32, angle = 45, vjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 32, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = c("gray40", "#b30000"),
                                                 shape = c(21, 24),
                                                 size = c(5, 5),
                                                 alpha = c(1, 1))))

deu_map

# ggsave("Germany.pdf", deu_map, device = cairo_pdf, path = file.path(dir, figpath, "/maps"),
#        width = 2100, height = 2400, scale = 2, units = "px", dpi = 300, limitsize = F)

#### > Choropleth map ----------------------------------------------------------
## subset data and calculate prevalence at the ADM2 level
g_prev <- d %>%
  dplyr::select(country, ADM0, ADM1, ADM2, locality, BsalDetected, individualCount) %>%
  filter(country == "Germany") %>%
  plyr::mutate(BsalDetected = as.factor(dplyr::recode(BsalDetected,
                                                      "1" = "Pos",
                                                      "0" = "Neg"))) %>%
  group_by(country, locality, BsalDetected) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = BsalDetected, id_cols = locality, values_from = n) %>%
  mutate(Pos = case_when(is.na(Pos) ~ 0,
                         TRUE ~ Pos),
         Neg = case_when(is.na(Neg) ~ 0,
                         TRUE ~ Neg)) %>%
  group_by(locality) %>%
  mutate(pop = sum(Pos, Neg),
         prev = round((Pos/pop)*100, 2),
         posSite = ifelse(Pos > 1, "Bsal positive", "Bsal negative"))


## Use 'geodata' pckg to get shapefiles for Germany (rnaturalearth only goes to ADM1)
germany <- geodata::gadm(country = 'DEU', level = 3,
                         path = file.path(shppath),
                         version = "latest", resolution = 1) %>%
  sf::st_as_sf(., crs = 4326) %>%
  st_cast(., "MULTIPOLYGON") %>%
  rename(ADM0 = GID_0,
         country = COUNTRY,
         ADM1 = NAME_1,
         ADM2 = NAME_2) %>%
  # Left-join g_prev
  left_join(., g_prev, by = "locality") %>%
  st_transform(., crs = epsg27704) %>%
  # clean NA vals from data set
  filter(!is.na(posSite))

tmp1 <- germany %>%
  dplyr::select(locality) %>%
  distinct()

tmp2 <- g_prev %>%
  dplyr::select(locality) %>%
  distinct()

tmp <- anti_join(tmp1, tmp2, by = "locality")

# Create breaks and discretize values
br <- c(0, 0.1, 5, 10, 15, 20, 100)

germany$binned <- cut(germany$prev,
                      breaks = br,
                      dig.lab = 2,
                      include.lowest = T)

# Create custom labels for plot
labs <- c(0, 5, 10, 15, 20, 100)
labs_plot <- c("0%", paste0(labs[0:4], ".1%-", labs[2:5], "%"), "≥20%")
# labs_plot <- c(paste0(labs[1:4], "%"), "≥20%")

# Create palette with custom breaks
pal <- c("gray40",
         "#dfb8b8",
         "#d38585",
         "#c65252",
         "#ba1e1e",
         "#b30000")

deu_choropleth <- ggplot() +
  # Europe base-map
  geom_sf(data = europe, col = "gray30", fill = "#ECECEC", show.legend = F) +
  # distinguish between  sampled countries & non sampled countries*
  # *Note: These aes specifications will also serve to highlight NA vals (areas not sampled)
  geom_sf_pattern(data = countries, aes(fill = sovereignt), show.legend = F,
                  pattern = "circle", col = "gray30", fill = "#B2BEB5",
                  pattern_fill = "gray30", pattern_color = "NA", pattern_spacing = 0.0125,
                  pattern_density = 0.25) +
  # add choropleth values for areas with data
  geom_sf(data = germany, aes(geometry = geometry, fill = binned),
          linetype = 1, lwd = 0.3, color = NA, show.legend = T) +
  scale_fill_manual(values = pal,
                    drop = F,
                    na.translate = F,
                    # na.value = "transparent",
                    label = labs_plot,
                    guide = guide_legend(ncol = 1,
                                         reverse = T,
                                         label.hjust = 1)) +
  coord_sf(xlim = c(4452875, 5238551), # c(7.5, 14.5)
           ylim = c(1540088, 2435744)) + # c(46.5, 55.5)
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 2.5, text_face = "plain",
                   pad_y = unit(0.5, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(2.5, "cm"), width = unit(2.5, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 1.8, text_size = 18)) +
  ak_theme + theme(legend.title = element_blank(),
                   legend.position = "right",
                   # legend.key.spacing.x = unit(0.01, "cm"), # Space legend labels
                   legend.key.size = unit(1,"cm"),
                   legend.text.position = "left",
                   legend.text = element_text(size = 18, hjust = 0,
                                              margin = margin(0, 0, 0, 0, unit = "pt")),
                   axis.text.x = element_text(size = 32, angle = 45, vjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 32, face = "plain"),
                   axis.title.y = element_blank())

deu_choropleth

# ggsave("Germany_choropleth.pdf", deu_choropleth, device = cairo_pdf, path = file.path(dir, figpath, "/maps"),
#        width = 2100, height = 2400, scale = 2, units = "px", dpi = 300, limitsize = F)





g_obs <- d %>%
  dplyr::select(country, ADM0, ADM1, ADM2, BsalDetected, individualCount) %>%
  filter(country == "Germany") %>%
  plyr::mutate(BsalDetected = as.factor(dplyr::recode(BsalDetected,
                                                      "1" = "Bsal positive",
                                                      "0" = "Bsal negative"))) %>%
  filter(BsalDetected == "Bsal negative") %>%
  group_by(country, ADM0, ADM1) %>%
  summarise(n = n())



### c. Spain -------------------------------------------------------------------
s <- mapLabels %>% filter(country == "Spain") %>%
  plyr::mutate(label = paste(country, " (n = ", n, ")", sep = ""))

map_bounds(-9, 3.75, 34.25, 46.5, crs = epsg27704)

esp_map <- ggplot() +
  geom_sf(data = europe, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = countries, aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  # geom_sf_label(data = s, aes(label = paste(label)), nudge_x = 400000, nudge_y = 250000,
  #               size = 12, fontface = "bold", label.size = NA, alpha = 0.5) +
  geom_sf(data = obs, aes(geometry = jittered, fill = BsalDetected, shape = BsalDetected),
          alpha = 0.3, size = 4, stroke = 1, color = "gray30", show.legend = "point") +
  scale_fill_manual(values = c("gray40", "#b30000"), guide = "none") +
  scale_shape_manual(values = c(21, 24), guide = "none") +
  coord_sf(xlim = c(2860148, 4298549), # c(-9, 4)
           ylim = c(690856.1, 1606424)) + # c(34.25, 46.5)
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 2.5, text_face = "plain",
                   pad_y = unit(0.5, "cm"), pad_x = unit(3.25, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(2.5, "cm"), width = unit(2.5, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 1.8, text_size = 18)) +
  ak_theme + theme(legend.title = element_blank(),
                   legend.position = "top",
                   legend.spacing = unit(1, "cm"), # Space legend labels
                   legend.key.size = unit(1,"cm"),
                   legend.text = element_text(size = 28, hjust = 0,
                                              margin = margin(l = 5, r = 30, unit = "pt")),
                   axis.text.x = element_text(size = 28, angle = 45, vjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 28, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = c("gray40", "#b30000"),
                                                 shape = c(21, 24),
                                                 size = c(5, 5),
                                                 alpha = c(1, 1))))

esp_map


# ggsave("Spain.pdf", esp_map, device = cairo_pdf, path = file.path(dir, figpath, "/maps"),
#        width = 2000, height = 1500, scale = 2, units = "px", dpi = 300, limitsize = F)
