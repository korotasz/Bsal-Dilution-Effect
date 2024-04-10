## Project utilized the 'renv' package for better code reproducibility

#  *** ONLY NEED TO DO THIS ONCE; I HAVE ALREADY DONE THIS FOR THIS PROJ. ***
## Create a project-local environment to ensure code reproducibility
# setwd(file.path(dir))
## Initialize the project
# renv::init() # this also activates the project


## Save the state of the project (including package versions); only need to re-run this
##  if you update packages/introduce new packages in this project.
#renv::settings$snapshot.type("explicit") # records and reflects latest proj. changes in "renv.lock" file


## IF THIS IS YOUR FIRST TIME RUNNING THIS PROJECT: ----------------------------
## Run the following lines:
# require(renv)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to activate project
# renv::activate() # activate project

## These packages need to be loaded first (commented out pckgs only need to be run once)
# remotes::install_version("Rttf2pt1", version = "1.3.8") # install this version, latest ver. not compatible
# remotes::install_github("gorkang/html2latex") # convert sjPlot::tab_model() hmtl table to tex and pdf in .Rmd docs
# extrafont::font_import("C:/Windows/Fonts") # load fonts before ggplot2; only need to do this once
require(pacman)
require(renv)
require(extrafontdb)
require(extrafont)
extrafont::loadfonts(device = "all", quiet = T) # plot fonts

## As of 2024-04-04, there are issues with patchwork and ggplot2 that require specific pull requests to resolve:
# remotes::install_github("thomasp85/patchwork")
# remotes::install_github("tidyverse/ggplot2", ref = remotes::github_pull("5592"))

## As of 2024-04-04, the Matrix package (a dependency of glmmTMB) is throwing errors
##  (Matrix v. 1.6-5) and must be reverted to Matrix v. 1.6-1.1 to work
# remotes::install_version("Matrix", version = "1.6-1.1")

## Packages --------------------------------------------------------------------
### > Visualization Packages----------------------------------------------------
pckgs <- c("ggsignif", # adds labels to significant groups
             "ggpubr", # stat_compare_means()
            "ggbreak", # create axis breaks in ggplots
             "gtools", # signif. value styling
         "kableExtra", # table styling
               "renv", # environment lock
             "ggtext", # for text type/arrangements w/ ggplot2
           "Rttf2pt1", # to use with the extrafont package
           "ggthemes", # contains 'scales', 'themes', and 'geoms' packages
          "grDevices", # saves high quality svg, pdf, and ps files
           "graphics", # dependency of grDevices
            "cowplot", # arranging plots/figs
          "gridExtra", # arranging plots/figs
          "patchwork", # arranging plots/figs
         "hrbrthemes", # plot colors
       "RColorBrewer", # plot colors
            "viridis", # plot colors
             "scales", # plot customization
           "eurostat", # obtain spatial data from europe
            "geodata", # obtain geographic data (world map)
      "rnaturalearth", # obtain spatial polygons that can be used with sf
                 "sf", # mapping
          "ggspatial", # north arrow and scale bar
            "mapproj", # apply map projection
         "scatterpie", # add pie charts to maps
       "multcompView", # cld.emmGrid()
          "latex2exp", # allows use of LaTeX in R
               "glue", # allows concatenation of LaTeX and R syntax
             "sjPlot", # plot_model(), tab_model()
               "ragg", # converts plots to tiff files
          "htmltools", # visualizes model outputs as html tables
           "webshot2", # converts html files to png
             "magick", # image_read() -- needed for cowplot::draw_image
              "Hmisc", # binconf() provides CI of binomial distribution
### > Analysis Packages --------------------------------------------------------
          "tidyverse", # data wrangling/manipulation
            "glmmTMB", # glmmTMB()
           "multcomp", # glht()
            "emmeans", # lsmeans()
                "car", # Anova()
             "DHARMa", # simulateResiduals(), testZeroInflation(), testDispersion()
              "MuMIn", # model.sel()
          "ggeffects", # ggpredict()
               "epiR", # calculate prevalence & CIs
              "binom", # credible intervals
             "sjmisc"  # data and variable transformation
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

flip <- function(data) {
  new <- data[rev(rownames(data)), ]
  rownames(new) <- NULL
  new
}
## File paths ------------------------------------------------------------------
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
csvpath <- (path.expand("/csvFiles"))
figpath <- (path.expand("/figures"))

## Read in .csv files-----------------------------------------------------------
setwd(file.path(dir, csvpath))
d <- read.csv("BsalData_all.csv", header = T, encoding = "UTF-8")
dcbind <- read.csv("BsalData_cbind.csv", header = T, encoding = "UTF-8")


## Define general ggplot theme for plot uniformity -----------------------------
ak_theme <- theme_ipsum(base_family = "Segoe UI Light") +
  theme(axis.text.x = element_text(size = 26),
        axis.title.x = element_text(size = 34, hjust = 0.5,
                                    margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    face = "plain"),
        axis.text.y = element_text(size = 26, face = "italic"),
        axis.title.y = element_text(size = 34, hjust = 0.5,
                                    margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    face = "plain"),
        axis.ticks.length = unit(.25, "cm"),
        axis.ticks = element_blank(),
        plot.tag = element_text(size = 36, face = "bold"),
        plot.title = element_text(size = 42, hjust = 0.5, face = "plain"),
        plot.subtitle = element_markdown(size = 12, face = "plain"),
        plot.margin = margin(1, 1, 1.5, 1.2, "cm"),
        plot.caption = element_markdown(hjust = 1, size = 14, face = "plain"),
        plot.caption.position = "plot",
        legend.position = "top",
        legend.key.size = unit(2,"cm"),
        legend.text = element_text(size = 28, hjust = -1),
        legend.title = element_text(size = 28, face = "bold"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.spacing.y = unit(1.5,"cm"),
        strip.text = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.line = element_line(color = 'black'))



# log transform vars
d <- d %>%
  mutate(logsppAbun = log(sppAbun + 1),
         logsiteAbun = log(siteAbun + 1),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
  relocate(c(logsppAbun, logsiteAbun), .after = sppAbun)

dcbind <- dcbind %>%
  mutate(logsppAbun = log(sppAbun + 1),
         logsiteAbun = log(siteAbun + 1),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
  relocate(c(logsppAbun, logsiteAbun), .after = sppAbun)


## Vector of cleaner labels for model output table
nicelabs <- c(`(Intercept)` = "Intercept",
              richness = "Richness",
              logsiteAbun = "log(Site abundance)",
              "richness:logsiteAbun" = "Richness:log(Site abundance)",
              temp_t0 = "Temperature",
              soilM_t0 = "Soil moisture",
              "temp_d:sMoist_d" = "Temp:Soil moisture")



## I. Generate descriptive figures ---------------------------------------------
# Asia polygons were re-projected to EPSG:27703 (WGS84/Equi7 Asia) and Europe polygons were
# re-projected to EPSG:27704 (WGS84/Equi7 Europe) for mapping purposes. These projections were
# made as a part of a larger project to optimize handling remote-sensing data, with each
# continent having their own centroid. doi: https://doi.org/10.1016/j.cageo.2014.07.005
# Equi7Grid GitHub: https://github.com/TUW-GEO/Equi7Grid
epsg27703 <- crs('+proj=aeqd +lat_0=47 +lon_0=94 +x_0=4340913.84808 +y_0=4812712.92347 +datum=WGS84 +units=m +no_defs')
epsg27704 <- crs('+proj=aeqd +lat_0=53 +lon_0=24 +x_0=5837287.81977 +y_0=2121415.69617 +datum=WGS84 +units=m +no_defs')

obs <- d %>%
  dplyr::select(country, ADM0, Lat, Lon, BsalDetected, individualCount) %>%
  plyr::mutate(BsalDetected = as.factor(dplyr::recode(BsalDetected,
                                                      "1" = "Bsal positive",
                                                      "0" = "Bsal negative"))) %>%
  st_as_sf(., coords = c("Lon", "Lat"), crs = 4326) %>%
  mutate(jittered = sf::st_jitter(geometry, 0.25)) %>%
  arrange(BsalDetected)

# summarise data
sampSize <- obs %>%
  group_by(country) %>%
  summarise(n = n())

# Get the coordinates of each country
country_lookup <- read.csv("countries.csv", stringsAsFactors = F)
names(country_lookup)[1] <- "country_code"


# Combine summarised data
mapLabels <- merge(x = sampSize, y = country_lookup,
                   by.x = "country", by.y = "name", all.x = T) %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326)

# Obtain world map
worldmap <- ne_countries(scale = "medium", type = "map_units", returnclass = "sf") %>%
  st_transform(., crs = 4326)


### > Maps ---------------------------------------------------------------------
# Summarise the number of observations from each Bsal+ country and plot individual points on a map.
euro_obs <- obs %>%
  filter(country == "Germany" | country == "Spain") %>%
  st_transform(., crs = epsg27704)
asia_obs <- obs %>%
  filter(country == "China" | country == "Vietnam") %>%
  st_transform(., crs = epsg27703)

# Create country labels for maps
europeLabs <- mapLabels %>%
  filter(country == "Germany" | country == "Spain") %>%
  st_transform(geometry, crs = epsg27704)
asiaLabs <- mapLabels %>%
  filter(country == "China" | country == "Vietnam") %>%
  st_transform(geometry, crs = epsg27703)

# Subset country polygons for base maps
europe <- worldmap %>%
  filter(continent == "Europe" & !name %in% c("Russia")) %>%
  st_transform(., crs = epsg27704)
asia <- worldmap %>%
  filter(continent == "Asia") %>%
  st_transform(., crs = epsg27703)

# Subset polygons for Bsal+ countries
euroCountries <- worldmap %>%
  filter(sovereignt %in% c("Spain", "Germany")) %>%
  st_transform(., crs = epsg27704)
asiaCountries <- worldmap %>%
  filter(geounit %in% c("China", "Vietnam")) %>%
  st_transform(., crs = epsg27703)

#### a. Data overview: Europe --------------------------------------------------
map_bounds(-8, 15, 34, 56, crs = epsg27704)

europe_map <- ggplot() +
  geom_sf(data = europe, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = euroCountries, aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  geom_sf(data = euro_obs, aes(geometry = jittered, fill = BsalDetected, shape = BsalDetected),
          alpha = 0.3, size = 4, stroke = 1, color = "gray30", show.legend = "point") +
  geom_sf_text(data = euroCountries, aes(label = name), position = "identity", size = 10) +
  scale_fill_manual(values = c("gray40", "#b30000"), guide = "none") +
  scale_shape_manual(values = c(21, 24), guide = "none") +
  coord_sf(xlim = c(2903943, 5277030), # c(-9, 15)
           ylim = c(625595.8, 2491036)) + # c(34, 56)
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 2, text_face = "plain",
                   pad_y = unit(0.5, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1.5, "cm"), width = unit(1.5, "cm"),
                         pad_x = unit(0, "cm"), pad_y = unit(0, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 1.8, text_size = 18)) +
  ak_theme + theme(legend.title = element_blank(),
                   legend.position = "top",
                   legend.spacing = unit(1, "cm"), # Space legend labels
                   legend.key.size = unit(1,"cm"),
                   legend.text = element_text(size = 28, hjust = 0),
                   axis.text.x = element_text(size = 28, angle = 45, vjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 28, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = c("gray40", "#b30000"),
                                                 shape = c(21, 24),
                                                 size = c(5, 5),
                                                 alpha = c(1, 1))))



europe_map

# ggsave("EuropeData.pdf", europe_map, device = cairo_pdf, path = file.path(dir, figpath),
#        width = 4000, height = 2000, scale = 2, units = "px", dpi = 300, limitsize = F)

#### b. Germany data distribution map ------------------------------------------
g <- mapLabels %>% filter(country == "Germany") %>%
  plyr::mutate(label = paste(country, " (n = ", n, ")", sep = ""))

map_bounds(7.5, 14.5, 46, 55.5, crs = epsg27704)

deu_map <- ggplot() +
  geom_sf(data = europe, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = euroCountries, aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  # geom_sf_label(data = g, aes(label = paste(label)),  nudge_x = -40000,  nudge_y = 430000,
  #               size = 7, fontface = "bold", label.size = NA, alpha = 0.5) +
  geom_sf(data = euro_obs, aes(geometry = jittered, fill = BsalDetected, shape = BsalDetected),
          alpha = 0.3, size = 4, stroke = 1, color = "gray30", show.legend = "point") +
  scale_fill_manual(values = c("gray40", "#b30000"), guide = "none") +
  scale_shape_manual(values = c(21, 24), guide = "none") +
  coord_sf(xlim = c(4452875, 5238551), # c(7.5, 14.5)
           ylim = c(1531966, 2435744)) + # c(46, 55.5)
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 2, text_face = "plain",
                   pad_y = unit(0.5, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1.5, "cm"), width = unit(1.5, "cm"),
                         pad_x = unit(0, "cm"), pad_y = unit(0, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 1.8, text_size = 18)) +
  ak_theme + theme(legend.title = element_blank(),
                   legend.position = "top",
                   legend.spacing = unit(1, "cm"), # Space legend labels
                   legend.key.size = unit(1,"cm"),
                   legend.text = element_text(size = 28, hjust = 0),
                   axis.text.x = element_text(size = 24, angle = 45, vjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 24, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = c("gray40", "#b30000"),
                                                 shape = c(21, 24),
                                                 size = c(5, 5),
                                                 alpha = c(1, 1))))

deu_map

# ggsave("GermanyData.pdf", deu_map, device = cairo_pdf, path = file.path(dir, figpath),
#        width = 2000, height = 2000, scale = 2, units = "px", dpi = 300, limitsize = F)

#### c. Spain data distribution map --------------------------------------------
s <- mapLabels %>% filter(country == "Spain") %>%
  plyr::mutate(label = paste(country, " (n = ", n, ")", sep = ""))

map_bounds(-9, 4.5, 34, 47, crs = epsg27704)

esp_map <- ggplot() +
  geom_sf(data = europe, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = euroCountries, aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  # geom_sf_label(data = s, aes(label = paste(label)), nudge_x = -700000, nudge_y = 250000,
  #               size = 7, fontface = "bold", label.size = NA, alpha = 0.5) +
  geom_sf(data = euro_obs, aes(geometry = jittered, fill = BsalDetected, shape = BsalDetected),
          alpha = 0.3, size = 4, stroke = 1, color = "gray30", show.legend = "point") +
  scale_fill_manual(values = c("gray40", "#b30000"), guide = "none") +
  scale_shape_manual(values = c(21, 24), guide = "none") +
  coord_sf(xlim = c(2860148, 4368433), # c(-9, 4.5)
           ylim = c(664626.4, 1650303)) + # c(34, 46)
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 2, text_face = "plain",
                   pad_y = unit(0.5, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1.5, "cm"), width = unit(1.5, "cm"),
                         pad_x = unit(0, "cm"), pad_y = unit(0, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 1.8, text_size = 18)) +
  ak_theme + theme(legend.title = element_blank(),
                   legend.position = "top",
                   legend.spacing = unit(1, "cm"), # Space legend labels
                   legend.key.size = unit(1,"cm"),
                   legend.text = element_text(size = 28, hjust = 0),
                   axis.text.x = element_text(size = 24, angle = 45, vjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 24, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = c("gray40", "#b30000"),
                                                 shape = c(21, 24),
                                                 size = c(5, 5),
                                                 alpha = c(1, 1))))

esp_map


# ggsave("SpainData.pdf", esp_map, device = cairo_pdf, path = file.path(dir, figpath),
#        width = 2000, height = 2000, scale = 2, units = "px", dpi = 300, limitsize = F)

#### d. Data overview: Asia ----------------------------------------------------
map_bounds(95, 125, 10, 35, crs = epsg27703)

asia_map <- ggplot() +
  geom_sf(data = asia, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = asiaCountries, aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  geom_sf(data = asia_obs, aes(geometry = jittered, fill = BsalDetected, shape = BsalDetected),
          alpha = 0.3, size = 4, stroke = 1, color = "gray30", show.legend = "point") +
  geom_sf_text(data = asiaCountries, aes(label = name), position = "identity", size = 12) +
  scale_fill_manual(values = c("gray40", "#b30000"), guide = "none") +
  scale_shape_manual(values = c(21, 24), guide = "none") +
  coord_sf(xlim = c(4458518, 7129851), # c(95, 125)
           ylim = c(711916.5, 4008620)) + # c(10, 35)
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 2, text_face = "plain",
                   pad_y = unit(0.5, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1.5, "cm"), width = unit(1.5, "cm"),
                         pad_x = unit(0, "cm"), pad_y = unit(0, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 1.8, text_size = 18)) +
  ak_theme + theme(legend.title = element_blank(),
                   legend.position = "top",
                   legend.spacing = unit(1, "cm"), # Space legend labels
                   legend.key.size = unit(1,"cm"),
                   legend.text = element_text(size = 28, hjust = 0),
                   axis.text.x = element_text(size = 28, angle = 45, vjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 28, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = c("gray40", "#b30000"),
                                                 shape = c(21, 24),
                                                 size = c(5, 5),
                                                 alpha = c(1, 1))))



asia_map

# ggsave("AsiaData.pdf", asia_map, device = cairo_pdf, path = file.path(dir, figpath),
#        width = 4000, height = 2000, scale = 2, units = "px", dpi = 300, limitsize = F)

#### e. China data distribution map --------------------------------------------
c <- mapLabels %>% filter(country == "China") %>%
  plyr::mutate(label = paste(country, " (n = ", n, ")", sep = ""))

map_bounds(98, 122, 19, 31, crs = epsg27703)

chn_map <- ggplot() +
  geom_sf(data = asia, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = subset(asiaCountries, geounit == "China"),
          aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  # geom_sf_label(data = c, aes(label = paste(label)), nudge_x = -900000, nudge_y = 850000,
  #               size = 9, fontface = "bold", label.size = NA, alpha = 0.5) +
  geom_sf(data = subset(asia_obs, country == "China"), aes(geometry = jittered, fill = BsalDetected, shape = BsalDetected),
          alpha = 0.3, size = 4, stroke = 1, color = "gray30", show.legend = "point") +
  scale_fill_manual(values = c("gray40", "#b30000"), guide = "none") +
  scale_shape_manual(values = c(21, 24), guide = "none") +
  coord_sf(xlim = c(4782975, 7005601), # c(98, 122)
           ylim = c(1716599, 3479777)) + # c(19, 31)
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 2, text_face = "plain",
                   pad_y = unit(0.5, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1.5, "cm"), width = unit(1.5, "cm"),
                         pad_x = unit(0, "cm"), pad_y = unit(0, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 1.8, text_size = 18)) +
  ak_theme + theme(legend.title = element_blank(),
                   legend.position = "top",
                   legend.spacing = unit(1, "cm"), # Space legend labels
                   legend.key.size = unit(1,"cm"),
                   legend.text = element_text(size = 28, hjust = 0),
                   axis.text.x = element_text(size = 24, angle = 45, vjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 24, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = c("gray40", "#b30000"),
                                                 shape = c(21, 24),
                                                 size = c(5, 5),
                                                 alpha = c(1, 1))))

chn_map

# ggsave("ChinaData.pdf", chn_map, device = cairo_pdf, path = file.path(dir, figpath),
#        width = 2000, height = 2000, scale = 2, units = "px", dpi = 300, limitsize = F)

#### f. Vietnam data distribution map ------------------------------------------
v <- mapLabels %>% filter(country == "Vietnam") %>%
  plyr::mutate(label = paste(country, " (n = ", n, ")", sep = ""))

map_bounds(100, 112, 8, 23, crs = epsg27703)

vnm_map <- ggplot() +
  geom_sf(data = asia, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = subset(asiaCountries, geounit == "Vietnam"), aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  # geom_sf_label(data = v, aes(label = paste(label)), nudge_x = -290000, nudge_y = 250000,
  #               size = 7, fontface = "bold", label.size = NA, alpha = 0.5) +
  geom_sf(data = subset(asia_obs, country == "Vietnam"), aes(geometry = jittered, fill = BsalDetected, shape = BsalDetected),
          alpha = 0.3, size = 4, stroke = 1, color = "gray30", show.legend = "point") +
  scale_fill_manual(values = c("gray40", "#b30000"), guide = "none") +
  scale_shape_manual(values = c(21, 24), guide = "none") +
  coord_sf(xlim = c(5055747, 6230263), # c(100, 112)
           ylim = c(511536.1, 2408791)) + # c(8, 23)
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 2, text_face = "plain",
                   pad_y = unit(0.5, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1.5, "cm"), width = unit(1.5, "cm"),
                         pad_x = unit(0, "cm"), pad_y = unit(0, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 1.8, text_size = 18)) +
  ak_theme + theme(legend.title = element_blank(),
                   legend.position = "top",
                   legend.spacing = unit(1, "cm"), # Space legend labels
                   legend.key.size = unit(1,"cm"),
                   legend.text = element_text(size = 28, hjust = 0),
                   axis.text.x = element_text(size = 24, angle = 45, vjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 24, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = c("gray40", "#b30000"),
                                                 shape = c(21, 24),
                                                 size = c(5, 5),
                                                 alpha = c(1, 1))))

vnm_map

# ggsave("VietnamData.pdf", vnm_map, device = cairo_pdf, path = file.path(dir, figpath),
#        width = 2000, height = 2000, scale = 2, units = "px", dpi = 300, limitsize = F)

# rm(asia, asia_obs, asiaCountries, asiaLabs, c, country_lookup, euro_obs,
#    euroCountries, europe, europeLabs, g, mapLabels, obs, s, sampSize, v, worldmap)

### Figure 1. Data distribution maps(combined) ---------------------------------
#### FIX THIS SECTION ----------------------------------------------------------
# p <- ggplot() + labs(x = "Longitude", y = "Latitude") + ak_theme + theme(plot.margin = margin(0, 0, 0, 0, "cm"),
#                                                                          panel.spacing.y = unit(0,"cm"))
# x_axis <- cowplot::get_plot_component(p, "xlab-b")
# y_axis <- cowplot::get_plot_component(p, "ylab-l")
#
# layout <- "
# #AAA
# BAAA
# #AAA
# ##C#
# "
#
# ## Country maps
# fig1a <- deu_map +
#   geom_sf_label(data = g, aes(label = paste(label)),  nudge_x = 90000,  nudge_y = 430000,
#                 size = 5, fontface = "bold", label.size = NA, alpha = 0.5) +
#   theme(plot.tag.position = c(0.96, 0.80),
#         panel.spacing.y = unit(0,"cm"))
# fig1b <- esp_map +
#   geom_sf_label(data = s, aes(label = paste(label)), nudge_x = -500000, nudge_y = 250000,
#                 size = 5, fontface = "bold", label.size = NA, alpha = 0.5) +
#   theme(plot.tag.position = c(0.95, 0.92),
#         panel.spacing.y = unit(0,"cm"))
# fig1c <- chn_map +
#   geom_sf_label(data = c, aes(label = paste(label)), nudge_x = -700000, nudge_y = 850000,
#                 size = 7, fontface = "bold", label.size = NA, alpha = 0.5) +
#   coord_sf(xlim = c(4782975, 7005601), # c(98, 122)
#            ylim = c(1716599, 3479777)) + # c(19, 31)
#   theme(plot.tag.position = c(0.95, 0.73),
#         axis.text.x = element_blank(),
#         panel.spacing.y = unit(0,"cm"))
# fig1d <- vnm_map +
#   geom_sf_label(data = v, aes(label = paste(label)), nudge_x = -200000, nudge_y = 270000,
#                 size = 7, fontface = "bold", label.size = NA, alpha = 0.5) +
#   coord_sf(xlim = c(4782975, 7005601), # c(98, 122)
#            ylim = c(499634.9, 2501979)) + # c(8, 23)
#   theme(plot.tag.position = c(0.95, 0.95),
#         panel.spacing.y = unit(0,"cm"))
#
#
# fig1ab <- fig1a + fig1b +
#   plot_annotation(tag_levels = "A") +
#   plot_layout(guides = "collect",
#               ncol = 1, nrow = 2,
#               design = "1
#                         2") &
#   theme(plot.margin = margin(0.25, 0, 0, 0, "cm"),
#         legend.position = "top",
#         legend.box.margin = margin(0, 1, 1, 1, "cm"),
#         legend.text = element_text(margin = margin(r = 1, unit = "cm")),
#         legend.title = element_blank())
#
# fig1ab
#
#
#
# fig1abcd <- fig1a + fig1b + fig1c + fig1d +
#   # plot_annotation(tag_levels = "A") +
#   plot_layout(guides = "collect", axes = "collect_x",
#               ncol = 2,
#               design = "13
#                         24") &
#   theme(plot.margin = margin(0.25, 0, 0, 0, "cm"),
#         legend.position = "top",
#         legend.box.margin = margin(0, 1, 1, 1, "cm"),
#         legend.text = element_text(margin = margin(r = 1, unit = "cm")),
#         legend.title = element_blank())
#
# fig1abcd
#
# ## Continent maps
# fig1e <- europe_map + theme(plot.tag.position = c(0.92, 0.88))
# fig1f <- asia_map + theme(plot.tag.position = c(0.95, 0.95))
#
# # overview_maps <- (fig1e | fig1f) + plot_layout(guides = "collect") &
# #   theme(plot.margin = margin(0.25, 0.25, 0, -0.5, "cm"),
# #         legend.position = "top",
# #         legend.box.margin = margin(0, 1, 1, 1, "cm"),
# #         legend.text = element_text(margin = margin(r = 1, unit = "cm")),
# #         legend.title = element_blank())
# #
# # overview_maps
#
# ggsave("Euro_map.pdf", fig1_map_annotated, device = cairo_pdf, path = file.path(dir, figpath),
#         width = 4000, height = 2000, scale = 2, units = "px", dpi = 300, limitsize = F)

# rm(asia, asia_map, asia_obs, asiaCountries, asiaLabs, c, chn_map, country_lookup,
#    deu_map, esp_map, euro_obs, euroCountries, europe, europe_map, europeLabs, layout,
#    fig1a, fig1b, fig1c, fig1d, g, mapLabels, p, s, v, vnm_map, worldmap, x_axis, y_axis,
#    epsg27703, epsg27704)
rm(asia_map, chn_map, deu_map, esp_map, europe_map, vnm_map)
## II. Testing assumptions of the dilution effect hypothesis -------------------
### a. Hosts differ in their reservoir competence. -----------------------------
sampSize <- d %>%
  dplyr::select(country, ADM0, Lat, Lon, Site, diseaseTested,
                BsalDetected, BdDetected, individualCount) %>%
  plyr::mutate(BsalDetected = as.factor(dplyr::recode(BsalDetected,
                                                      "1" = "Bsal positive",
                                                      "0" = "Bsal negative"))) %>%
  group_by(country, Site, BsalDetected) %>%
  summarise(n = n()) %>%
  ungroup()


deu <- sampSize %>%
  filter(country == "Germany") %>%
  plyr::mutate(BsalDetected = as.factor(dplyr::recode(BsalDetected,
                                                      "Bsal positive" = "Pos", "Bsal negative" = "Neg"))) %>%
  pivot_wider(names_from = BsalDetected, id_cols = Site, values_from = n) %>%
  mutate(Pos = case_when(is.na(Pos) ~ 0,
                         TRUE ~ Pos),
         Neg = case_when(is.na(Neg) ~ 0,
                         TRUE ~ Neg)) %>%
  group_by(Site) %>%
  mutate(pop = sum(Pos, Neg),
         sitePrev = round((Pos/pop)*100, 2))
print(deu$sitePrev) # observed prevalence at each site

esp <- sampSize %>%
  filter(country == "Spain") %>%
  plyr::mutate(BsalDetected = as.factor(dplyr::recode(BsalDetected,
                                                      "Bsal positive" = "Pos", "Bsal negative" = "Neg"))) %>%
  pivot_wider(names_from = BsalDetected, id_cols = Site, values_from = n) %>%
  mutate(Pos = case_when(is.na(Pos) ~ 0,
                         TRUE ~ Pos),
         Neg = case_when(is.na(Neg) ~ 0,
                         TRUE ~ Neg)) %>%
  group_by(Site) %>%
  mutate(pop = sum(Pos, Neg),
         sitePrev = round((Pos/pop)*100, 2))
print(esp$sitePrev) # observed prevalence at each site

chn <- sampSize %>%
  filter(country == "China") %>%
  plyr::mutate(BsalDetected = as.factor(dplyr::recode(BsalDetected,
                                                      "Bsal positive" = "Pos", "Bsal negative" = "Neg"))) %>%
  pivot_wider(names_from = BsalDetected, id_cols = Site, values_from = n) %>%
  mutate(Pos = case_when(is.na(Pos) ~ 0,
                         TRUE ~ Pos),
         Neg = case_when(is.na(Neg) ~ 0,
                         TRUE ~ Neg)) %>%
  group_by(Site) %>%
  mutate(pop = sum(Pos, Neg),
         sitePrev = round((Pos/pop)*100, 2))
print(chn$sitePrev) # observed prevalence at each site

vnm <- sampSize %>%
  filter(country == "Vietnam") %>%
  plyr::mutate(BsalDetected = as.factor(dplyr::recode(BsalDetected,
                                                      "Bsal positive" = "Pos", "Bsal negative" = "Neg"))) %>%
  pivot_wider(names_from = BsalDetected, id_cols = Site, values_from = n) %>%
  mutate(Pos = case_when(is.na(Pos) ~ 0,
                         TRUE ~ Pos),
         Neg = case_when(is.na(Neg) ~ 0,
                         TRUE ~ Neg)) %>%
  group_by(Site) %>%
  mutate(pop = sum(Pos, Neg),
         sitePrev = round((Pos/pop)*100, 2))
print(vnm$sitePrev) # observed prevalence at each site

## Observed species prevalence
prev <- d %>%
  group_by(scientific) %>%
  mutate(ncas_Bsal = sum(BsalDetected == 1), # number of observed Bsal+ cases
         npop = sum(individualCount)) %>% # pop size (total # individuals/spp.)
  drop_na(date) %>%
  ungroup() %>%
  mutate(Bsal_prev = (ncas_Bsal/npop)*100) %>% # prevalence as a percentage
  dplyr::select(scientific, susceptibility, ncas_Bsal, Bsal_prev, npop) %>%
  unique() %>%
  dplyr::mutate(row_id = row_number()) %>%
  relocate(row_id, .before = scientific)


# Use binconf function to sample binomial distribution and get CIs for prevalence
bsal_ci <- binconf(prev$ncas_Bsal, prev$npop, method = "exact", return.df = T)
bsal_bayesci <- binom::binom.bayes(x = prev$ncas_Bsal, n = prev$npop, type = "highest", tol = 1e-9, maxit = 1000)

# Convert binconf point estimates + CIs to percentages for plotting
bsal_ci <-  bsal_ci %>%
  plyr::mutate(PointEst = round((PointEst*100), 1),
               Lower = round((Lower*100), 1),
               Upper = round((Upper*100), 1)) %>%
  dplyr::mutate(row_id = row_number()) %>%
  relocate(row_id, .before = PointEst)

# Convert bayesian estimates + CIs to percentages for plotting
bsal_bayesci <-  bsal_bayesci %>%
  plyr::mutate(mean = round((mean*100), 1),
               lower = round((lower*100), 1),
               upper = round((upper*100), 1)) %>%
  dplyr::mutate(row_id = row_number()) %>%
  relocate(row_id, .before = method)


# Join with original 'prev' dataset, so we can plot actual vs expected
prev <- prev %>%
  left_join(., bsal_ci, by = "row_id") %>%
  left_join(., bsal_bayesci, by = "row_id") %>%
  plyr::arrange(., scientific)

prev_bayes_plot <- ggplot(prev, aes(scientific, sapply(mean, FUN = function(x) ifelse(x == 0.0, round(x, 0), x)),
                                          colour = susceptibility,
                                          label = paste(mean,"%"))) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5, linewidth = 1) +
  geom_text(aes(colour = susceptibility, x = (as.numeric(scientific) + 0.05), y = upper + 2),
            size = 6, fontface = "bold", alpha = 0.75) +
  coord_flip(clip = "off") +
  ylab("Disease prevalence (%)") +
  xlab("Species") +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  scale_y_continuous(labels = seq(0, 50, 10),
                     breaks = seq(0, 50, 10),
                     limits = c(0, 51)) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(1, 0.25))) +
  ak_theme + theme(axis.text.y = element_text(face = "italic"),
                   legend.title = element_blank())

prev_bayes_plot

# ggsave("prev_bayes_plot.pdf", prev_bayes_plot, device = cairo_pdf, path = file.path(dir, figpath),
#          width = 2600, height = 2000, scale = 1.5, units = "px", dpi = 300, limitsize = F)

# rm(prev_bayes_plot, bsal_bayesci, bsal_ci, chn, deu, esp, prev, sampSize, vnm)

### b. The most susceptible species are the most abundant ----------------------
#### > All data ----------------------------------------------------------------
##### i. Including fire salamanders --------------------------------------------
m2b <- glmmTMB(logsppAbun ~ scientific + (1|Site),
               data = d,
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))


summary(m2b)
Anova(m2b)
## post-hoc test for multiple comparison of means
# m2b_post.hoc <- glht(m2b, linfct = mcp(scientific = "Tukey")) %>%
#   broom::tidy() %>%
#   dplyr::select(-(term)) %>%
#   dplyr::mutate(signif = gtools::stars.pval(adj.p.value)) %>%
#   kableExtra::kbl(digits = 2) %>%
#   kableExtra::kable_styling()
#
# m2bpost.hoc

# Subset observed abundance
obs_abun <- d %>%
  dplyr::select(scientific, sppAbun, Site) %>%
  rename(obs_abun = sppAbun)

textcol <- d %>%
  dplyr::select(scientific, susceptibility, Site) %>% # subset relevant data
  mutate(Site = as.factor(Site)) %>%
  group_by(scientific) %>%
  mutate(No.Sites = length(unique(Site))) %>%
  ungroup() %>%
  dplyr::select(!Site) %>%
  unique() %>%
  left_join(., obs_abun, by = "scientific")

m2b_predict <- ggpredict(m2b, terms = "scientific") %>%
  dplyr::rename("scientific" = "x",
                "logsppAbun" = "predicted",
                "expectedAbun" = "group") %>%
  left_join(., textcol, by = "scientific") %>%
  plyr::mutate(expectedAbun = exp(logsppAbun - 1),
               conf.low = exp(conf.low - 1),
               conf.high = exp(conf.high - 1),
               susceptibility = as.factor(susceptibility)) %>%
  group_by(scientific, obs_abun, expectedAbun) %>%
  unique()


# xhat <- TeX(r"($\hat{X}_{\textit{a}} =)") ## LaTeX formula: $\hat{X}_{\textit{a}} = ## THIS ALSO WORKS, BUT WILL TRY USING LATEX EXP BELOW
TeXlabl <- glue::glue("$\\textbf{\\hat{x}_{\\textit{a}}}}}=", .open = "{{") # THIS WORKS
xhat <- latex2exp::TeX(TeXlabl, output = "expression")

# Create color coded labels for graph annotation
# Resistant
df1 <- m2b_predict %>%
  filter(susceptibility == "1")

# Tolerant
df2 <- m2b_predict %>%
  filter(susceptibility == "2")

# Susceptible
df3 <- m2b_predict %>%
  filter(susceptibility == "3")

m2b_plot <- ggplot(m2b_predict, aes(x = scientific, label = round(expectedAbun, 0))) +
  geom_jitter(aes(y = obs_abun, colour = susceptibility), shape = 23, size = 2,
              alpha = 0.5, show.legend = F) +
  geom_point(aes(y = expectedAbun, colour = susceptibility), size = 4.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = susceptibility), width = 0.5, linewidth = 1) +
  # geom_text(aes(colour = susceptibility, x = (as.numeric(scientific) + 0.05), y = conf.high + 3),
  #           size = 6, fontface = "bold", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df1$scientific) + 0.1), y = (df1$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#548078", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df2$scientific) + 0.1), y = (df2$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#E3A630", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df3$scientific) + 0.1), y = (df3$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#b30000", alpha = 0.75) +
  coord_flip(clip = "off") +
  ylab("Abundance") +
  xlab("Species") +
  labs(caption = "Figure displays all species from our dataset.") +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  scale_y_continuous(labels = seq(0, 110, 10),
                     breaks = seq(0, 110, 10),
                     limits = c(0, 113)) + # 1 obs. point cut off -- @112 (L. helveticus)
  scale_y_break(c(64, 84)) +
  scale_y_break(c(95, 110)) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(1, 0.29))) +
  ak_theme + theme(axis.text.y = element_text(face = "italic"),
                   axis.title.y = element_text(vjust = 0.5),
                   axis.title.x = element_text(hjust = 0.65),
                   axis.text.x.top = element_blank(),
                   legend.title = element_blank())


m2b_plot

# ggsave("fig2b_all.pdf", m2b_plot, device = cairo_pdf, path = file.path(dir, figpath),
#           width = 2000, height = 1400, scale = 2, units = "px", dpi = 300, limitsize = F)

rm(df1, df2, df3, obs_abun, m2b_plot, m2b_predict, textcol)
##### ii. Excluding fire salamanders -------------------------------------------
m2b_noFS <- glmmTMB(logsppAbun ~ scientific + (1|Site),
                    data = filter(d, scientific != "Salamandra salamandra"),
                    control = glmmTMBControl(optimizer = optim,
                                             optArgs = list(method = "BFGS")))


summary(m2b_noFS)
Anova(m2b_noFS)
## post-hoc test for multiple comparison of means
# m2b_noFS_post.hoc <- glht(m2b_noFS, linfct = mcp(scientific = "Tukey")) %>%
#   broom::tidy() %>%
#   dplyr::select(-(term)) %>%
#   dplyr::mutate(signif = gtools::stars.pval(adj.p.value)) %>%
#   kableExtra::kbl(digits = 2) %>%
#   kableExtra::kable_styling()
#
# m2b_noFSpost.hoc

# Subset observed abundance
obs_abun <- d %>%
  filter(!(scientific == "Salamandra salamandra")) %>%
  dplyr::select(scientific, sppAbun, Site) %>%
  rename(obs_abun = sppAbun)

textcol <- d %>%
  filter(!(scientific == "Salamandra salamandra")) %>%
  dplyr::select(scientific, susceptibility, Site) %>% # subset relevant data
  mutate(Site = as.factor(Site)) %>%
  group_by(scientific) %>%
  mutate(No.Sites = length(unique(Site))) %>%
  ungroup() %>%
  dplyr::select(!Site) %>%
  unique() %>%
  left_join(., obs_abun, by = "scientific")

m2b_noFS_predict <- ggpredict(m2b_noFS, terms = "scientific") %>%
  dplyr::rename("scientific" = "x",
                "logsppAbun" = "predicted",
                "expectedAbun" = "group") %>%
  left_join(., textcol, by = "scientific") %>%
  plyr::mutate(expectedAbun = exp(logsppAbun - 1),
               conf.low = exp(conf.low - 1),
               conf.high = exp(conf.high - 1),
               susceptibility = as.factor(susceptibility)) %>%
  group_by(scientific, obs_abun, expectedAbun) %>%
  unique()


# Create color coded labels for graph annotation
# Resistant
df1 <- m2b_noFS_predict %>%
  filter(susceptibility == "1")

# Tolerant
df2 <- m2b_noFS_predict %>%
  filter(susceptibility == "2")

# Susceptible
df3 <- m2b_noFS_predict %>%
  filter(susceptibility == "3")

m2b_noFS_plot <- ggplot(m2b_noFS_predict, aes(x = scientific, label = round(expectedAbun, 0))) +
  geom_jitter(aes(y = obs_abun, colour = susceptibility), shape = 23, size = 2,
              alpha = 0.5, show.legend = F) +
  geom_point(aes(y = expectedAbun, colour = susceptibility), size = 4.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = susceptibility), width = 0.5, linewidth = 1) +
  # geom_text(aes(colour = susceptibility, x = (as.numeric(scientific) + 0.05), y = conf.high + 3),
  #           size = 6, fontface = "bold", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df1$scientific) + 0.1), y = (df1$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#548078", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df2$scientific) + 0.1), y = (df2$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#E3A630", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df3$scientific) + 0.1), y = (df3$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#b30000", alpha = 0.75) +
  coord_flip(clip = "off") +
  ylab("Abundance") +
  xlab("Species") +
  labs(caption = "Figure displays all species from our dataset, excluding fire salamanders.") +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  scale_y_continuous(labels = seq(0, 110, 10),
                     breaks = seq(0, 110, 10),
                     limits = c(0, 113)) + # 1 obs. point cut off -- @112 (L. helveticus)
  scale_y_break(c(64, 77)) +
  scale_y_break(c(95, 110)) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(1, 0.29))) +
  ak_theme + theme(axis.text.y = element_text(face = "italic"),
                   axis.title.y = element_text(vjust = 0.5),
                   axis.title.x = element_text(hjust = 0.65),
                   axis.text.x.top = element_blank(),
                   legend.title = element_blank())


m2b_noFS_plot

# ggsave("fig2b_noFS.pdf", m2b_noFS_plot, device = cairo_pdf, path = file.path(dir, figpath),
#           width = 2000, height = 1400, scale = 2, units = "px", dpi = 300, limitsize = F)

rm(df1, df2, df3, obs_abun, m2b_noFS_plot, m2b_noFS_predict, textcol)
#### > Europe data only --------------------------------------------------------
##### i. Including fire salamanders -----------------------------------------
m2b_EU <- glmmTMB(logsppAbun ~ scientific + (1|Site),
               data = filter(d, continent == "Europe"),
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))


summary(m2b_EU)
Anova(m2b_EU)
## post-hoc test for multiple comparison of means
# m2b_EU_post.hoc <- glht(m2b_EU, linfct = mcp(scientific = "Tukey")) %>%
#   broom::tidy() %>%
#   dplyr::select(-(term)) %>%
#   dplyr::mutate(signif = gtools::stars.pval(adj.p.value)) %>%
#   kableExtra::kbl(digits = 2) %>%
#   kableExtra::kable_styling()
#
# m2b_EU_post.hoc

# Subset observed abundance
EU_abun <- d %>%
  filter(continent == "Europe") %>%
  dplyr::select(scientific, sppAbun, Site) %>%
  rename(EU_abun = sppAbun)

textcol <- d %>%
  filter(continent == "Europe") %>%
  dplyr::select(scientific, susceptibility, Site) %>% # subset relevant data
  mutate(Site = as.factor(Site)) %>%
  group_by(scientific) %>%
  mutate(No.Sites = length(unique(Site))) %>%
  ungroup() %>%
  dplyr::select(!Site) %>%
  unique() %>%
  left_join(., EU_abun, by = "scientific")

m2b_EU_predict <- ggpredict(m2b_EU, terms = "scientific") %>%
  dplyr::rename("scientific" = "x",
         "logsppAbun" = "predicted",
         "expectedAbun" = "group") %>%
  left_join(., textcol, by = "scientific") %>%
  plyr::mutate(expectedAbun = exp(logsppAbun - 1),
               conf.low = exp(conf.low - 1),
               conf.high = exp(conf.high - 1),
               susceptibility = as.factor(susceptibility)) %>%
  group_by(scientific, EU_abun, expectedAbun) %>%
  unique()


# Create color coded labels for graph annotation
# Resistant
df1 <- m2b_EU_predict %>%
  filter(susceptibility == "1")

# Tolerant
df2 <- m2b_EU_predict %>%
  filter(susceptibility == "2")

# Susceptible
df3 <- m2b_EU_predict %>%
  filter(susceptibility == "3")

m2b_EU_plot <- ggplot(m2b_EU_predict, aes(x = scientific, label = round(expectedAbun, 0))) +
  geom_jitter(aes(y = EU_abun, colour = susceptibility), shape = 23, size = 2,
             alpha = 0.5, show.legend = F) +
  geom_point(aes(y = expectedAbun, colour = susceptibility), size = 4.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = susceptibility), width = 0.5, linewidth = 1) +
  # geom_text(aes(colour = susceptibility, x = (as.numeric(scientific) + 0.05), y = conf.high + 3),
  #           size = 6, fontface = "bold", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df1$scientific) + 0.1), y = (df1$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#548078", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df2$scientific) + 0.1), y = (df2$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#E3A630", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df3$scientific) + 0.1), y = (df3$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#b30000", alpha = 0.75) +
  coord_flip(clip = "off") +
  ylab("Abundance") +
  xlab("Species") +
  labs(caption = "Figure displays all observations from Europe.") +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  scale_y_continuous(labels = seq(0, 110, 10),
                     breaks = seq(0, 110, 10),
                     limits = c(0, 113)) + # 1 obs. point cut off -- @112 (L. helveticus)
  scale_y_break(c(66, 110)) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(1, 0.29))) +
  ak_theme + theme(axis.text.y = element_text(face = "italic"),
                   axis.title.y = element_text(vjust = 0.5),
                   axis.title.x = element_text(hjust = 0.65),
                   axis.text.x.top = element_blank(),
                   legend.title = element_blank())


m2b_EU_plot

# ggsave("fig2b_europe.pdf", m2b_EU_plot, device = cairo_pdf, path = file.path(dir, figpath),
#           width = 2000, height = 1400, scale = 2, units = "px", dpi = 300, limitsize = F)

rm(df1, df2, df3, EU_abun, m2b_EU_plot, m2b_EU_predict, textcol)
##### ii. Excluding fire salamanders -------------------------------------------
m2b_noFS_EU <- glmmTMB(logsppAbun ~ scientific + (1|Site),
                       data = filter(d, (continent == "Europe" & scientific != "Salamandra salamandra")),
                       control = glmmTMBControl(optimizer = optim,
                                                optArgs = list(method = "BFGS")))


summary(m2b_noFS_EU)
Anova(m2b_noFS_EU)
## post-hoc test for multiple comparison of means
# m2b_noFS_EU_post.hoc <- glht(m2b_noFS_EU, linfct = mcp(scientific = "Tukey")) %>%
#   broom::tidy() %>%
#   dplyr::select(-(term)) %>%
#   dplyr::mutate(signif = gtools::stars.pval(adj.p.value)) %>%
#   kableExtra::kbl(digits = 2) %>%
#   kableExtra::kable_styling()
#
# m2b_noFS_EU_post.hoc

# Subset observed abundance
noFS_EU_abun <- d %>%
  filter(continent == "Europe" & scientific != "Salamandra salamandra") %>%
  dplyr::select(scientific, sppAbun, Site) %>%
  rename(noFS_EU_abun = sppAbun)

textcol <- d %>%
  filter(continent == "Europe" & scientific != "Salamandra salamandra") %>%
  dplyr::select(scientific, susceptibility, Site) %>% # subset relevant data
  mutate(Site = as.factor(Site)) %>%
  group_by(scientific) %>%
  mutate(No.Sites = length(unique(Site))) %>%
  ungroup() %>%
  dplyr::select(!Site) %>%
  unique() %>%
  left_join(., noFS_EU_abun, by = "scientific")

m2b_noFS_EU_predict <- ggpredict(m2b_noFS_EU, terms = "scientific") %>%
  dplyr::rename("scientific" = "x",
                "logsppAbun" = "predicted",
                "expectedAbun" = "group") %>%
  left_join(., textcol, by = "scientific") %>%
  plyr::mutate(expectedAbun = exp(logsppAbun - 1),
               conf.low = exp(conf.low - 1),
               conf.high = exp(conf.high - 1),
               susceptibility = as.factor(susceptibility)) %>%
  group_by(scientific, noFS_EU_abun, expectedAbun) %>%
  unique()

# Create color coded labels for graph annotation
# Resistant
df1 <- m2b_noFS_EU_predict %>%
  filter(susceptibility == "1")

# Tolerant
df2 <- m2b_noFS_EU_predict %>%
  filter(susceptibility == "2")

# Susceptible
df3 <- m2b_noFS_EU_predict %>%
  filter(susceptibility == "3")

m2b_noFS_EU_plot <- ggplot(m2b_noFS_EU_predict, aes(x = scientific, label = round(expectedAbun, 0))) +
  geom_jitter(aes(y = noFS_EU_abun, colour = susceptibility), shape = 23, size = 2,
              alpha = 0.5, show.legend = F) +
  geom_point(aes(y = expectedAbun, colour = susceptibility), size = 4.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = susceptibility), width = 0.5, linewidth = 1) +
  # geom_text(aes(colour = susceptibility, x = (as.numeric(scientific) + 0.05), y = conf.high + 3),
  #           size = 6, fontface = "bold", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df1$scientific) + 0.1), y = (df1$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#548078", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df2$scientific) + 0.1), y = (df2$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#E3A630", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df3$scientific) + 0.1), y = (df3$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#b30000", alpha = 0.75) +
  coord_flip(clip = "off") +
  ylab("Abundance") +
  xlab("Species") +
  labs(caption = "Figure displays observations from Europe, excluding fire salamanders.") +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  scale_y_continuous(labels = seq(0, 110, 10),
                     breaks = seq(0, 110, 10),
                     limits = c(0, 113)) + # 1 obs. point cut off -- @112 (L. helveticus)
  scale_y_break(c(63, 110)) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(1, 0.29))) +
  ak_theme + theme(axis.text.y = element_text(face = "italic"),
                   axis.title.y = element_text(vjust = 0.5),
                   axis.title.x = element_text(hjust = 0.65),
                   axis.text.x.top = element_blank(),
                   legend.title = element_blank())


m2b_noFS_EU_plot

# ggsave("fig2b_noFS_EU.pdf", m2b_noFS_EU_plot, device = cairo_pdf, path = file.path(dir, figpath),
#           width = 2000, height = 1400, scale = 2, units = "px", dpi = 300, limitsize = F)

rm(textcol, noFS_EU_abun, m2b_noFS_EU_predict, m2b_noFS_EU_plot, df1, df2, df3)
#### > Asia data only ----------------------------------------------------------
m2b_AS <- glmmTMB(logsppAbun ~ scientific + (1|Site),
                  data = filter(d, (continent == "Asia")),
                  control = glmmTMBControl(optimizer = optim,
                                           optArgs = list(method = "BFGS")))


summary(m2b_AS)
Anova(m2b_AS)
## post-hoc test for multiple comparison of means
# m2b_AS_post.hoc <- glht(m2b_AS, linfct = mcp(scientific = "Tukey")) %>%
#   broom::tidy() %>%
#   dplyr::select(-(term)) %>%
#   dplyr::mutate(signif = gtools::stars.pval(adj.p.value)) %>%
#   kableExtra::kbl(digits = 2) %>%
#   kableExtra::kable_styling()
#
# m2b_AS_post.hoc

# Subset observed abundance
AS_abun <- d %>%
  filter(continent == "Asia") %>%
  dplyr::select(scientific, sppAbun, Site) %>%
  rename(AS_abun = sppAbun)

textcol <- d %>%
  dplyr::select(scientific, susceptibility, Site) %>% # subset relevant data
  mutate(Site = as.factor(Site)) %>%
  group_by(scientific) %>%
  mutate(No.Sites = length(unique(Site))) %>%
  ungroup() %>%
  dplyr::select(!Site) %>%
  unique() %>%
  left_join(., AS_abun, by = "scientific")

m2b_AS_predict <- ggpredict(m2b_AS, terms = "scientific") %>%
  dplyr::rename("scientific" = "x",
                "logsppAbun" = "predicted",
                "expectedAbun" = "group") %>%
  left_join(., textcol, by = "scientific") %>%
  plyr::mutate(expectedAbun = exp(logsppAbun - 1),
               conf.low = exp(conf.low - 1),
               conf.high = exp(conf.high - 1),
               susceptibility = as.factor(susceptibility)) %>%
  group_by(scientific, AS_abun, expectedAbun) %>%
  unique()


# Create color coded labels for graph annotation
# Resistant
df1 <- m2b_AS_predict %>%
  filter(susceptibility == "1")

# Tolerant
df2 <- m2b_AS_predict %>%
  filter(susceptibility == "2")

# Susceptible
df3 <- m2b_AS_predict %>%
  filter(susceptibility == "3")

m2b_AS_plot <- ggplot(m2b_AS_predict, aes(x = scientific, label = round(expectedAbun, 0))) +
  geom_jitter(aes(y = AS_abun, colour = susceptibility), shape = 23, size = 2,
              alpha = 0.5, show.legend = F) +
  geom_point(aes(y = expectedAbun, colour = susceptibility), size = 4.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = susceptibility), width = 0.5, linewidth = 1) +
  # geom_text(aes(colour = susceptibility, x = (as.numeric(scientific) + 0.05), y = conf.high + 3),
  #           size = 6, fontface = "bold", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df1$scientific) + 0.1), y = (df1$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#548078", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df2$scientific) + 0.1), y = (df2$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#E3A630", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df3$scientific) + 0.1), y = (df3$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#b30000", alpha = 0.75) +
  coord_flip(clip = "off") +
  ylab("Abundance") +
  xlab("Species") +
  labs(caption = "Figure displays all observations from Asia.") +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  scale_y_continuous(labels = seq(0, 80, 10),
                     breaks = seq(0, 80, 10),
                     limits = c(0, 80)) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(1, 0.29))) +
  ak_theme + theme(axis.text.y = element_text(face = "italic"),
                   axis.text.x.top = element_blank(),
                   legend.title = element_blank())


m2b_AS_plot

# ggsave("fig2b_asia.pdf", m2b_AS_plot, device = cairo_pdf, path = file.path(dir, figpath),
#           width = 2000, height = 1400, scale = 2, units = "px", dpi = 300, limitsize = F)

rm(m2b_AS_plot, m2b_AS_predict, df1, df2, df3, textcol, AS_abun)
### c. Host abundance and susceptibility in our dataset ------------------------
#### > All data ----------------------------------------------------------------
##### i. Including fire salamanders --------------------------------------------
m2c <- glmmTMB(logsppAbun ~ susceptibility + (1|Site),
               data = d,
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))
summary(m2c)
Anova(m2c)

## post-hoc test for multiple comparison of means
post.hoc <- glht(m2c, linfct = mcp(susceptibility = "Tukey"),
                 alternative = "greater")
fortify(post.hoc)

mcLabs <- glht(m2c, linfct = mcp(susceptibility = "Tukey"),
               alternative = "greater") %>%
  broom::tidy() %>%
  dplyr::select(-(term)) %>%
  flip(.) %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  subset(., select = c(group1, group2, adj.p.value)) %>%
  # add letters for comparison
  mutate(cld = cld(post.hoc, decreasing = T)$mcletters$Letters) %>%
  mutate(signif = gtools::stars.pval(adj.p.value)) %>%
  mutate(signif = case_when(signif == " " ~ "n.s.",
                            TRUE ~ signif))

## Summarise data for labels
n <- d %>%
  group_by(susceptibility) %>%
  summarise(n = n())

m2c_predict <- ggpredict(m2c, terms = c("susceptibility")) %>%
  dplyr::rename("susceptibility" = "x",
                "group" = "group") %>%
  mutate(susceptibility = as.factor(susceptibility),
         predicted = exp(as.numeric(predicted - 1)),
         conf.high = exp(as.numeric(conf.high - 1)),
         conf.low = exp(as.numeric(conf.low - 1))) %>%
  drop_na(.) %>%
  left_join(., n, by = "susceptibility")


m2c_plot <- ggplot(m2c_predict, aes(susceptibility, predicted, color = susceptibility)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 1) +
  stat_pvalue_manual(filter(mcLabs, !(signif == "n.s.")), y.position = 4.5, label = "signif",
                     step.increase = 0.25, label.size = 10,bracket.size = 1) +
  # geom_richtext(aes(y = (conf.high + 0.5), label = paste0("n<span style = 'font-size:15pt'><sub>*obs*</sub> </span>= ", n)),
  #               alpha = 0.75, size = 8, label.size = NA, fill = NA, fontface = "bold", show.legend = F) +
  # annotate("text", x = 3, y = 4.25, label = "***", size = 10, fontface = "bold",
  #          colour = "#b30000") +
  # annotate("text", x = 0.65, y = 9, label = "C", size = 12, fontface = "bold",
  #          colour = "black") +
  scale_x_discrete(labels = c("Resistant", "Tolerant", "Susceptible")) +
  ylab("Species abundance") +
  xlab("Susceptibility level") +
  labs(caption = "All species in the dataset and all observations from both continents were retained in these analyses.") +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, 2)) +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", # 9 spp
                                 "#E3A630", # 15 spp
                                 "#b30000"),# 5 spp
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  ak_theme + theme(axis.text.y = element_text(size = 26, face = "plain"),)

m2c_plot


# ggsave("fig2c.pdf", m2c_plot, device = cairo_pdf, path = file.path(dir, figpath),
#           width = 2000, height = 1300, scale = 2, units = "px", dpi = 300, limitsize = F)

rm(m2c_predict, m2c_plot, mcLabs, n, post.hoc)
##### ii. Excluding fire salamanders -------------------------------------------
m2c_noFS <- glmmTMB(logsppAbun ~ susceptibility + (1|Site),
                    data = filter(d, scientific != "Salamandra salamandra"),
                    control = glmmTMBControl(optimizer = optim,
                                             optArgs = list(method = "BFGS")))
summary(m2c_noFS)
Anova(m2c_noFS)

## post-hoc test for multiple comparison of means
post.hoc <- glht(m2c_noFS, linfct = mcp(susceptibility = "Tukey"),
                 alternative = "greater")
fortify(post.hoc)

mcLabs <- glht(m2c_noFS, linfct = mcp(susceptibility = "Tukey"),
               alternative = "greater") %>%
  broom::tidy() %>%
  dplyr::select(-(term)) %>%
  flip(.) %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  subset(., select = c(group1, group2, adj.p.value)) %>%
  # add letters for comparison
  mutate(cld = cld(post.hoc, decreasing = T)$mcletters$Letters) %>%
  mutate(signif = gtools::stars.pval(adj.p.value)) %>%
  mutate(signif = case_when(signif == " " ~ "n.s.",
                            TRUE ~ signif))

## Summarise data for labels
n <- d %>%
  filter(!(scientific == "Salamandra salamandra")) %>%
           group_by(susceptibility) %>%
           summarise(n = n())

m2c_noFS_predict <- ggpredict(m2c_noFS, terms = c("susceptibility")) %>%
  dplyr::rename("susceptibility" = "x",
                         "group" = "group") %>%
  mutate(susceptibility = as.factor(susceptibility),
                  predicted = exp(as.numeric(predicted - 1)),
                  conf.high = exp(as.numeric(conf.high - 1)),
                  conf.low = exp(as.numeric(conf.low - 1))) %>%
  drop_na(.) %>%
  left_join(., n, by = "susceptibility")


m2c_noFS_plot <- ggplot(m2c_noFS_predict, aes(susceptibility, predicted, color = susceptibility)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 1) +
  stat_pvalue_manual(filter(mcLabs, !(signif == "n.s.")), y.position = 7.5, label = "signif",
                     step.increase = 0.15, label.size = 10,bracket.size = 1) +
  # geom_richtext(aes(y = (conf.high + 0.5), label = paste0("n<span style = 'font-size:15pt'><sub>*obs*</sub> </span>= ", n)),
  #               alpha = 0.75, size = 8, label.size = NA, fill = NA, fontface = "bold", show.legend = F) +
  # annotate("text", x = 3, y = 4.25, label = "***", size = 10, fontface = "bold",
  #          colour = "#b30000") +
  # annotate("text", x = 0.65, y = 9, label = "C", size = 12, fontface = "bold",
  #          colour = "black") +
  scale_x_discrete(labels = c("Resistant", "Tolerant", "Susceptible")) +
  ylab("Species abundance") +
  xlab("Susceptibility level") +
  labs(caption = "All species in the dataset (except for fire salamanders) and all observations from both continents were retained in these analyses.") +
  scale_y_continuous(limits = c(0, 9),
                     breaks = seq(0, 8, 2)) +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", # 9 spp
                                 "#E3A630", # 15 spp
                                 "#b30000"),# 5 spp
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  ak_theme + theme(axis.text.y = element_text(size = 26, face = "plain"),)

m2c_noFS_plot


# ggsave("fig2c_noFS.pdf", m2c_noFS_plot, device = cairo_pdf, path = file.path(dir, figpath),
#           width = 2000, height = 1300, scale = 2, units = "px", dpi = 300, limitsize = F)

rm(m2c_noFS_predict, m2c_noFS_plot, mcLabs, n, post.hoc)
#### > Europe data only --------------------------------------------------------
##### i. Including fire salamanders --------------------------------------------
m2c_EU <- glmmTMB(logsppAbun ~ susceptibility + (1|Site),
                  data = filter(d, continent == "Europe"),
                  control = glmmTMBControl(optimizer = optim,
                                           optArgs = list(method = "BFGS")))
summary(m2c_EU)
Anova(m2c_EU)

## post-hoc test for multiple comparison of means
post.hoc <- glht(m2c_EU, linfct = mcp(susceptibility = "Tukey"),
                 alternative = "greater")
fortify(post.hoc)

mcLabs <- glht(m2c_EU, linfct = mcp(susceptibility = "Tukey"),
               alternative = "greater") %>%
  broom::tidy() %>%
  dplyr::select(-(term)) %>%
  flip(.) %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  subset(., select = c(group1, group2, adj.p.value)) %>%
  # add letters for comparison
  mutate(cld = cld(post.hoc, decreasing = T)$mcletters$Letters) %>%
  mutate(signif = gtools::stars.pval(adj.p.value)) %>%
  mutate(signif = case_when(signif == " " ~ "n.s.",
                            TRUE ~ signif))

## Summarise data for labels
n <- d %>%
  filter(continent == "Europe") %>%
  group_by(susceptibility) %>%
  summarise(n = n())

m2c_EU_predict <- ggpredict(m2c_EU, terms = c("susceptibility")) %>%
  dplyr::rename("susceptibility" = "x",
                "group" = "group") %>%
  mutate(susceptibility = as.factor(susceptibility),
         predicted = exp(as.numeric(predicted - 1)),
         conf.high = exp(as.numeric(conf.high - 1)),
         conf.low = exp(as.numeric(conf.low - 1))) %>%
  drop_na(.) %>%
  left_join(., n, by = "susceptibility")


m2c_EU_plot <- ggplot(m2c_EU_predict, aes(susceptibility, predicted, color = susceptibility)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 1) +
  stat_pvalue_manual(filter(mcLabs, !(signif == "n.s.")), y.position = 4.5, label = "signif",
                     step.increase = 0.25, label.size = 10,bracket.size = 1) +
  # geom_richtext(aes(y = (conf.high + 0.5), label = paste0("n<span style = 'font-size:15pt'><sub>*obs*</sub> </span>= ", n)),
  #               alpha = 0.75, size = 8, label.size = NA, fill = NA, fontface = "bold", show.legend = F) +
  # annotate("text", x = 3, y = 4.25, label = "***", size = 10, fontface = "bold",
  #          colour = "#b30000") +
  # annotate("text", x = 0.65, y = 9, label = "C", size = 12, fontface = "bold",
  #          colour = "black") +
  scale_x_discrete(labels = c("Resistant", "Tolerant", "Susceptible")) +
  ylab("Species abundance") +
  xlab("Susceptibility level") +
  labs(caption = "All observations from Europe were included in these analyses.") +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, 2)) +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", # 9 spp
                                 "#E3A630", # 15 spp
                                 "#b30000"),# 5 spp
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  ak_theme + theme(axis.text.y = element_text(size = 26, face = "plain"),)

m2c_EU_plot


# ggsave("fig2c_EU.pdf", m2c_EU_plot, device = cairo_pdf, path = file.path(dir, figpath),
#           width = 2000, height = 1300, scale = 2, units = "px", dpi = 300, limitsize = F)

rm(m2c_EU_predict, m2c_EU_plot, mcLabs, n, post.hoc)
##### ii. Excluding fire salamanders -------------------------------------------
m2c_noFS_EU <- glmmTMB(logsppAbun ~ susceptibility + (1|Site),
                       data = filter(d, (continent == "Europe" & scientific != "Salamandra salamandra")),
                       control = glmmTMBControl(optimizer = optim,
                                                optArgs = list(method = "BFGS")))
summary(m2c_noFS_EU)
Anova(m2c_noFS_EU)

## post-hoc test for multiple comparison of means
post.hoc <- glht(m2c_noFS_EU, linfct = mcp(susceptibility = "Tukey"),
                 alternative = "greater")
fortify(post.hoc)

mcLabs <- glht(m2c_noFS_EU, linfct = mcp(susceptibility = "Tukey"),
               alternative = "greater") %>%
  broom::tidy() %>%
  dplyr::select(-(term)) %>%
  flip(.) %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  subset(., select = c(group1, group2, adj.p.value)) %>%
  # add letters for comparison
  mutate(cld = cld(post.hoc, decreasing = T)$mcletters$Letters) %>%
  mutate(signif = gtools::stars.pval(adj.p.value)) %>%
  mutate(signif = case_when(signif == " " ~ "n.s.",
                            TRUE ~ signif))

## Summarise data for labels
n <- d %>%
  filter(continent == "Europe" & scientific != "Salamandra salamandra") %>%
  group_by(susceptibility) %>%
  summarise(n = n())

m2c_noFS_EU_predict <- ggpredict(m2c_noFS_EU, terms = c("susceptibility")) %>%
  dplyr::rename("susceptibility" = "x",
                "group" = "group") %>%
  mutate(susceptibility = as.factor(susceptibility),
         predicted = exp(as.numeric(predicted - 1)),
         conf.high = exp(as.numeric(conf.high - 1)),
         conf.low = exp(as.numeric(conf.low - 1))) %>%
  drop_na(.) %>%
  left_join(., n, by = "susceptibility")


m2c_noFS_EU_plot <- ggplot(m2c_noFS_EU_predict, aes(susceptibility, predicted, color = susceptibility)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 1) +
  stat_pvalue_manual(filter(mcLabs, !(signif == "n.s.")), y.position = 6.5, label = "signif",
                     step.increase = 0.25, label.size = 10,bracket.size = 1) +
  # geom_richtext(aes(y = (conf.high + 0.5), label = paste0("n<span style = 'font-size:15pt'><sub>*obs*</sub> </span>= ", n)),
  #               alpha = 0.75, size = 8, label.size = NA, fill = NA, fontface = "bold", show.legend = F) +
  # annotate("text", x = 3, y = 4.25, label = "***", size = 10, fontface = "bold",
  #          colour = "#b30000") +
  # annotate("text", x = 0.65, y = 9, label = "C", size = 12, fontface = "bold",
  #          colour = "black") +
  scale_x_discrete(labels = c("Resistant", "Tolerant", "Susceptible")) +
  ylab("Species abundance") +
  xlab("Susceptibility level") +
  labs(caption = "All species (except for fire salamanders) from Europe were included in these analyses.") +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, 2)) +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", # 9 spp
                                 "#E3A630", # 15 spp
                                 "#b30000"),# 5 spp
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  ak_theme + theme(axis.text.y = element_text(size = 26, face = "plain"),)

m2c_noFS_EU_plot


# ggsave("fig2c_noFS_EU.pdf", m2c_noFS_EU_plot, device = cairo_pdf, path = file.path(dir, figpath),
#           width = 2000, height = 1300, scale = 2, units = "px", dpi = 300, limitsize = F)

rm(m2c_noFS_EU_predict, m2c_noFS_EU_plot, mcLabs, n, post.hoc)
#### > Asia data only ----------------------------------------------------------
m2c_AS <- glmmTMB(logsppAbun ~ susceptibility + (1|Site),
                  data = filter(d, continent == "Asia"),
                  control = glmmTMBControl(optimizer = optim,
                                           optArgs = list(method = "BFGS")))
summary(m2c_AS)
Anova(m2c_AS)

## post-hoc test for multiple comparison of means
post.hoc <- glht(m2c_AS, linfct = mcp(susceptibility = "Tukey"),
                 alternative = "greater")
fortify(post.hoc)

mcLabs <- glht(m2c_AS, linfct = mcp(susceptibility = "Tukey"),
               alternative = "greater") %>%
  broom::tidy() %>%
  dplyr::select(-(term)) %>%
  flip(.) %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  subset(., select = c(group1, group2, adj.p.value)) %>%
  # add letters for comparison
  mutate(cld = cld(post.hoc, decreasing = T)$mcletters$Letters) %>%
  mutate(signif = gtools::stars.pval(adj.p.value)) %>%
  mutate(signif = case_when(signif == " " ~ "n.s.",
                            TRUE ~ signif))

## Summarise data for labels
n <- d %>%
  filter(continent == "Asia") %>%
  group_by(susceptibility) %>%
  summarise(n = n())

m2c_AS_predict <- ggpredict(m2c_AS, terms = c("susceptibility")) %>%
  dplyr::rename("susceptibility" = "x",
                "group" = "group") %>%
  mutate(susceptibility = as.factor(susceptibility),
         predicted = exp(as.numeric(predicted - 1)),
         conf.high = exp(as.numeric(conf.high - 1)),
         conf.low = exp(as.numeric(conf.low - 1))) %>%
  drop_na(.) %>%
  left_join(., n, by = "susceptibility")


m2c_AS_plot <- ggplot(m2c_AS_predict, aes(susceptibility, predicted, color = susceptibility)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 1) +
  stat_pvalue_manual(filter(mcLabs, !(signif == "n.s.")), y.position = 6.5, label = "signif",
                     step.increase = 0.25, label.size = 10,bracket.size = 1) +
  # geom_richtext(aes(y = (conf.high + 0.5), label = paste0("n<span style = 'font-size:15pt'><sub>*obs*</sub> </span>= ", n)),
  #               alpha = 0.75, size = 8, label.size = NA, fill = NA, fontface = "bold", show.legend = F) +
  # annotate("text", x = 3, y = 4.25, label = "***", size = 10, fontface = "bold",
  #          colour = "#b30000") +
  # annotate("text", x = 0.65, y = 9, label = "C", size = 12, fontface = "bold",
  #          colour = "black") +
  scale_x_discrete(labels = c("Resistant", "Tolerant", "Susceptible")) +
  ylab("Species abundance") +
  xlab("Susceptibility level") +
  labs(caption = "All observations from Asia were included in these analyses.") +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 10),
                     labels = seq(0, 100, 10)) +
  scale_y_break(c(30, 95)) +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", # 9 spp
                                 "#E3A630", # 15 spp
                                 "#b30000"),# 5 spp
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  ak_theme + theme(axis.text.y = element_text(size = 26, face = "plain"),
                   axis.text.y.right = element_blank())

m2c_AS_plot


# ggsave("fig2c_AS.pdf", m2c_AS_plot, device = cairo_pdf, path = file.path(dir, figpath),
#           width = 2000, height = 1300, scale = 2, units = "px", dpi = 300, limitsize = F)

rm(m2c_AS_predict, m2c_AS_plot, mcLabs, n, post.hoc)
### Figure 2. ------------------------------------------------------------------
## Create guide to force a common legend
commonLegend <- guides(fill = guide_legend(override.aes = list(color = c("#548078", "#E3A630", "#b30000"),
                                                    shape = c(16, 16, 16),
                                                    size = c(2, 2, 2),
                                                    alpha = c(1, 1, 1))))

## Create combined plot for manuscript
fig2a <- prev_binconf_plot + labs(caption = NULL) +
                     theme(plot.tag.position = c(0.96, 0.92),
                           plot.margin = margin(.5, .75, .5, .75, "cm"),
                           axis.ticks.length = unit(.25, "cm"),
                           axis.ticks = element_blank(),
                           axis.text.y = element_text(size = 28),
                           axis.text.x = element_text(size = 24),
                           axis.title.y = element_blank(),
                           axis.title.x = element_text(size = 30)) +
                      commonLegend

fig2b <- m2b_plot + labs(caption = NULL) +
                       theme(axis.text.y = element_blank(),
                             axis.title.y = element_blank(),
                             axis.title.x = element_text(size = 30),
                             axis.text.x = element_text(size = 24),
                             axis.ticks.length = unit(.25, "cm"),
                             axis.ticks = element_blank(),
                             plot.margin = margin(.5, .75, .5, .5, "cm"),
                             plot.tag.position = c(0.92, 0.92)) +
                       commonLegend

fig2b_noFS <- m2b_noFS_plot + labs(caption = NULL) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size = 24),
        axis.ticks.length = unit(.25, "cm"),
        axis.ticks = element_blank(),
        plot.margin = margin(.5, .75, .5, .5, "cm"),
        plot.tag.position = c(0.92, 0.92)) +
  commonLegend

fig2c <- m2c_plot + labs(caption = NULL) +
                      theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                            plot.margin = margin(.75, 2, .75, 0, "cm"),
                            axis.ticks.length.y = unit(.25, "cm"),
                            axis.ticks = element_blank(),
                            axis.title.y = element_text(margin = margin(r = -500)),
                            axis.title.x = element_text(size = 34),
                            axis.text.y = element_text(face = "plain"),
                            plot.tag.position = c(0.33, 0.92)) +
                      commonLegend

fig2c_noFS <- m2c_noFS_plot + labs(caption = NULL) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        plot.margin = margin(.75, 2, .75, 0, "cm"),
        axis.ticks.length.y = unit(.25, "cm"),
        axis.ticks = element_blank(),
        axis.title.y = element_text(margin = margin(r = -500)),
        axis.title.x = element_text(size = 34),
        axis.text.y = element_text(face = "plain"),
        plot.tag.position = c(0.33, 0.92)) +
  commonLegend



fig2ab <-(fig2a | fig2b) + plot_layout(guides = "collect", heights = c(20, 16)) +
  plot_annotation(tag_levels = "A") & theme(legend.position = "top",
                                            legend.box.margin = margin(0, 1, 1, 1, "cm"),
                                            legend.text = element_text(margin = margin(r = 1, unit = "cm")),
                                            legend.title = element_blank())
fig2ab

ggsave("fig2ab.pdf", fig2ab, device = cairo_pdf, path = file.path(dir, figpath),
        width = 2800, height = 1400, scale = 2, units = "px", dpi = 300, limitsize = F)

fig2combined <- ((fig2a | fig2b)/fig2c) + plot_layout(guides = "collect", heights = c(20, 16)) +
                plot_annotation(tag_levels = "A") & theme(legend.position = "top",
                                                          legend.box.margin = margin(0, 1, 1, 1, "cm"),
                                                          legend.text = element_text(margin = margin(r = 1, unit = "cm")),
                                                          legend.title = element_blank())
fig2combined

ggsave("fig2_combined.pdf", fig2combined, device = cairo_pdf, path = file.path(dir, figpath),
       width = 2800, height = 2600, scale = 2, units = "px", dpi = 300, limitsize = F)

## Code to see how many sites each species are found at in each country
# d %>%
#   dplyr::select(country, species, Site) %>%
#   filter(country == "Spain" & species == "marmoratus") %>%
#   unique() #%>%
#   nrow()


## III. "All Spp" Cbind models testing the dilution dffect hypothesis ----------
## Data prep for cbind models
# Drop rows with NA vals in weather data & scale relevant vars
dcbindScaled <- dcbind %>%
  tidyr::drop_na(., any_of(c(36:53))) %>%
  mutate_at(c("temp_d", "sMoist_d",
              "bio1_wc", "bio12_wc", "tavg_wc", "prec_wc"),
            ~(scale(., center = T, scale = T %>% as.numeric))) %>%
  mutate(year = year(date))


#### > Relative richness (derived from dataset) --------------------------------
##### i. Europe data (all) -----------------------------------------------------
all_EU_RR <- glmmTMB(cbind(nPos_all, nNeg_all) ~ richness*logsiteAbun +
                    temp_d*sMoist_d + (1|scientific),
                  data = filter(dcbindScaled, continent == "Europe"),
                  family = "binomial", na.action = "na.fail",
                  control = glmmTMBControl(optimizer = optim,
                                           optArgs = list(method = "BFGS")))

summary(all_EU_RR)
Anova(all_EU_RR)

resid <- simulateResiduals(all_EU_RR)
testResiduals(resid)
testQuantiles(resid)
testZeroInflation(resid)

## Need to subset unique lat/lon vals to test for spatial autocorrelation
coords <- dcbindScaled %>%
  distinct(Site, Lat, Lon, .keep_all = T) %>%
  group_by(Site) %>%
  mutate(Lat = mean(Lat), Lon = mean(Lon)) %>%
  distinct() %>%
  ungroup()

recalc.resid <- recalculateResiduals(resid, group = coords$Site)
testSpatialAutocorrelation(recalc.resid, x = coords$Lon, y = coords$Lat)
# DHARMa Moran's I test for distance-based autocorrelation
#
# data:  recalc.resid
# observed = 0.025432, expected = -0.002924, sd = 0.022145, p-value = 0.2004
# alternative hypothesis: Distance-based autocorrelation




##  Prevalence by Abundance & Richness Plots for 'All spp.' model
all_EU_RR_pred <- ggpredict(all_EU_RR,  terms = c("richness", "logsiteAbun")) %>%
  dplyr::rename("richness" = "x",
                "logsiteAbun" = "group") %>%
  plyr::mutate(richness = as.numeric(as.character(richness)),
               logsiteAbun = as.numeric(as.character(logsiteAbun)),
               # Convert scaled prediction to original data scale:
               siteAbun = as.factor(round(exp(as.numeric(logsiteAbun)), 0)))


all_EU_RR_plot <- ggplot(all_EU_RR_pred, aes(x = richness, y = predicted, linetype = siteAbun, colour = siteAbun)) +
  geom_line(aes(linetype = siteAbun, colour = siteAbun), linewidth = 1) +
  geom_rug(data = dcbindScaled, aes(x = richness, y = 0), sides = "b",
           alpha = 0.5, position = position_jitter(width = 0.4, height = 0.1),
           inherit.aes = F, na.rm = T) +
  labs(x = "Species richness",
       y = "Bsal prevalence (%)",
       # title = "All spp. model",
  ) +
  #  geom_ribbon(aes(x = richness, ymin = conf.low, ymax = conf.high,
  #              fill = siteAbun), alpha = 0.2, colour = NA, show.legend = F) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_grey(start = 0.8, end = 0.2) +
  scale_x_continuous(labels = seq(0, 10, 1),
                     breaks = seq(0, 10, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, .06, 0.02),
                     limits = c(0, 0.06),
                     minor_breaks = seq(0, 0.06, 0.02)) +
  ak_theme + theme(plot.tag.position = c(0.96, 0.9),
                   axis.text.y = element_text(face = "plain")) +
  guides(colour = guide_legend("Site-level abundance", title.position = "top", title.hjust = 0.5),
         linetype = guide_legend("Site-level abundance", title.position = "top", title.hjust = 0.5))

all_EU_RR_plot
# ggsave("Europe_allRR_plot.pdf", all_EU_RR_plot, device = cairo_pdf, path = file.path(dir, figpath),
#        width = 1250, height = 1500, scale = 2, units = "px", dpi = 300, limitsize = F)

##### ii. Asia data (all) ------------------------------------------------------
all_AS_RR <- glmmTMB(cbind(nPos_all, nNeg_all) ~ richness*logsiteAbun +
                       temp_d*sMoist_d + (1|scientific),
                     data = filter(dcbindScaled, continent == "Asia"),
                     family = "binomial", na.action = "na.fail",
                     control = glmmTMBControl(optimizer = optim,
                                              optArgs = list(method = "BFGS")))

summary(all_AS_RR)
Anova(all_AS_RR)

resid <- simulateResiduals(all_AS_RR)
testResiduals(resid)
testQuantiles(resid)
testZeroInflation(resid)

## Need to subset unique lat/lon vals to test for spatial autocorrelation
coords <- dcbindScaled %>%
  filter(continent == "Asia") %>%
  distinct(Site, Lat, Lon, .keep_all = T) %>%
  group_by(Site) %>%
  mutate(Lat = mean(Lat), Lon = mean(Lon)) %>%
  distinct() %>%
  ungroup()

recalc.resid <- recalculateResiduals(resid, group = coords$Site)
testSpatialAutocorrelation(recalc.resid, x = coords$Lon, y = coords$Lat)
# DHARMa Moran's I test for distance-based autocorrelation
#
# data:  recalc.resid
# observed = 0.025432, expected = -0.002924, sd = 0.022145, p-value = 0.2004
# alternative hypothesis: Distance-based autocorrelation




##  Prevalence by Abundance & Richness Plots for 'All spp.' model
all_AS_RR_pred <- ggpredict(all_AS_RR,  terms = c("richness", "logsiteAbun")) %>%
  dplyr::rename("richness" = "x",
                "logsiteAbun" = "group") %>%
  plyr::mutate(richness = as.numeric(as.character(richness)),
               logsiteAbun = as.numeric(as.character(logsiteAbun)),
               # Convert scaled prediction to original data scale:
               siteAbun = as.factor(round(exp(as.numeric(logsiteAbun)), 0)))


all_AS_RR_plot <- ggplot(all_AS_RR_pred, aes(x = richness, y = predicted, linetype = siteAbun, colour = siteAbun)) +
  geom_line(aes(linetype = siteAbun, colour = siteAbun), linewidth = 1) +
  geom_rug(data = dcbindScaled, aes(x = richness, y = 0), sides = "b",
           alpha = 0.5, position = position_jitter(width = 0.4, height = 0.1),
           inherit.aes = F, na.rm = T) +
  labs(x = "Species richness",
       y = "Bsal prevalence (%)",
       # title = "All spp. model",
  ) +
  #  geom_ribbon(aes(x = richness, ymin = conf.low, ymax = conf.high,
  #              fill = siteAbun), alpha = 0.2, colour = NA, show.legend = F) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_grey(start = 0.8, end = 0.2) +
  scale_x_continuous(labels = seq(0, 10, 1),
                     breaks = seq(0, 10, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, .06, 0.02),
                     limits = c(0, 0.06),
                     minor_breaks = seq(0, 0.06, 0.02)) +
  ak_theme + theme(plot.tag.position = c(0.96, 0.9),
                   axis.text.y = element_text(face = "plain")) +
  guides(colour = guide_legend("Site-level abundance", title.position = "top", title.hjust = 0.5),
         linetype = guide_legend("Site-level abundance", title.position = "top", title.hjust = 0.5))

all_AS_RR_plot
# ggsave("Asia_allRR_plot.pdf", all_AS_RR_plot, device = cairo_pdf, path = file.path(dir, figpath),
#        width = 1250, height = 1500, scale = 2, units = "px", dpi = 300, limitsize = F)
#### > Relative richness + no FS prevalence ------------------------------------
## Fire salamanders are still accounted for in site abundance and richness calculations
all_RR_noFS <- glmmTMB(cbind(nPos_all_noFS, nNeg_all_noFS) ~ richness*logsiteAbun +
                    temp_d*sMoist_d + (1|scientific),
                  data = dcbindScaled, family = "binomial", na.action = "na.fail",
                  control = glmmTMBControl(optimizer = optim,
                                           optArgs = list(method = "BFGS")))

summary(all_RR_noFS)
Anova(all_RR_noFS)

resid_noFS <- simulateResiduals(all_RR_noFS)
testResiduals(resid_noFS)
testQuantiles(resid_noFS)
testZeroInflation(resid_noFS)

# Test for spatial autocorrelation
recalc.resid_noFS <- recalculateResiduals(resid_noFS, group = coords$Site)
testSpatialAutocorrelation(recalc.resid_noFS, x = coords$Lon, y = coords$Lat)
# not spatially autocorrelated


#### > IUCN Richness estimate --------------------------------------------------
all_iucnR <- glmmTMB(cbind(nPos_all, nNeg_all) ~ iucn_rich*logsiteAbun +
                   temp_d*sMoist_d + (1|scientific),
                 data = filter(dcbindScaled, continent != "Asia"), family = "binomial", na.action = "na.fail",
                 control = glmmTMBControl(optimizer = optim,
                                          optArgs = list(method = "BFGS")))

summary(all_iucnR)
Anova(all_iucnR)

resid_iucnR <- simulateResiduals(all_iucnR)
testResiduals(resid_iucnR)
testQuantiles(resid_iucnR)
testZeroInflation(resid_iucnR)

# Test for spatial autocorrelation
recalc.resid_iucnR <- recalculateResiduals(resid_iucnR, group = coords$Site)
testSpatialAutocorrelation(recalc.resid_iucnR, x = coords$Lon, y = coords$Lat)

#### > IUCN Richness estimate + no FS prevalence -------------------------------
all_iucnR_noFS <- glmmTMB(cbind(nPos_all_noFS, nNeg_all_noFS) ~ iucn_rich*logsiteAbun +
                       temp_d*sMoist_d + (1|scientific),
                     data = dcbindScaled, family = "binomial", na.action = "na.fail",
                     control = glmmTMBControl(optimizer = optim,
                                              optArgs = list(method = "BFGS")))

summary(all_iucnR_noFS)
Anova(all_iucnR_noFS)

resid_iucnR_noFS <- simulateResiduals(all_iucnR_noFS)
testResiduals(resid_iucnR_noFS)
testQuantiles(resid_iucnR_noFS)
testZeroInflation(resid_iucnR_noFS)

# Test for spatial autocorrelation
recalc.resid_iucnR_noFS <- recalculateResiduals(resid_iucnR_noFS, group = coords$Site)
testSpatialAutocorrelation(recalc.resid_iucnR_noFS, x = coords$Lon, y = coords$Lat)











#### Clean model outputs
## ABUNDANCE x RICHNESS
tab_model(m_all, show.obs = T, collapse.ci = T,
          show.icc = F, show.ngroups = F, show.re.var = F,
          rm.terms = c("temp_d", "sMoist_d", "temp_d:sMoist_d"),
          dv.labels = "All species model",
          string.pred = "Terms",
          string.p = "P-Value",
          show.p = T,
          pred.labels = nicelabs,
          file = file.path(dir, figpath, "m_all.html"))


# take html file and make .png file
webshot2::webshot(file.path(dir, figpath, "m_all.html"),
        file.path(dir, figpath, "m_all.png"),
        vwidth = 365, vheight = 500)






# ggsave("m_all_plot.tif", m_all_plot, device = "tiff", scale = 2,
#        width = 1500, height = 1000, units = "px",
#        path = file.path(dir, figpath), dpi = 300)


## IV. "Fire salamander only" Cbind models testing the dilution effect hypothesis ------------------------------
## Subset data even further to only include Fire Salamanders
FSdata <- dcbindScaled %>%
  filter(scientific == "Salamandra salamandra")

m_FS <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~  richness * logsiteAbun + temp_d*sMoist_d + (1|scientific),
                data = FSdata, family = "binomial",
                control = glmmTMBControl(optimizer = optim,
                                         optArgs = list(method = "BFGS")))


summary(m_FS)
Anova(m_FS)


m_FS_iucnR <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~  iucn_rich * logsiteAbun + temp_d*sMoist_d + (1|scientific),
                data = FSdata, family = "binomial",
                control = glmmTMBControl(optimizer = optim,
                                         optArgs = list(method = "BFGS")))


summary(m_FS_iucnR)
Anova(m_FS_iucnR)

# m_FS2 <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~  richness + logsppAbun + temp_d*sMoist_d + (1|scientific) + (1|principalInvestigator),
#                  data = FSdata, family = "binomial", na.action = "na.fail",
#                  control = glmmTMBControl(optimizer = optim,
#                                           optArgs = list(method = "BFGS")))
#
# summary(m_FS2)
# Anova(m_FS2)
#
#
 anova(m_FS2, m_FS) # same as the "all spp. model" -- simpler model is just as effective at accounting for variance




#### Clean model outputs
## ABUNDANCE x RICHNESS
tab_model(m_FS, show.obs = T, collapse.ci = T,
          show.icc = F, show.ngroups = F, show.re.var = F,
          rm.terms = c("temp_d", "sMoist_d", "temp_d:sMoist_d"),
          dv.labels = "Fire salamander model",
          string.pred = "Terms",
          string.p = "P-Value",
          show.p = T,
          pred.labels = nicelabs,
          file = file.path(dir, figpath, "m_FS.html"))


# take html file and make .png file
webshot2::webshot(file.path(dir, figpath, "m_FS.html"),
                  file.path(dir, figpath, "m_FS.png"),
                  vwidth = 365, vheight = 500)



# # take html file and make .png file
# webshot(file.path(dir, figpath, "m_FS_weather.html"),file.path(dir, figpath, "m_FS_weather.png"))


##      Prevalence by Abundance & Richness Plots for 'fire salamander' model
m_FS_predict <- ggpredict(m_FS,  terms = c("richness", "logsiteAbun")) %>%
  dplyr::rename("richness" = "x",
         "logsiteAbun" = "group") %>%
  plyr::mutate(richness = as.numeric(as.character(richness)),
               logsiteAbun = as.numeric(as.character(logsiteAbun)),
               # Convert scaled prediction to original data scale:
               siteAbun = as.factor(round(exp(as.numeric(logsiteAbun)), 0)))

m_FS_plot <- ggplot(m_FS_predict, aes(x = richness , y = predicted, colour = siteAbun, linetype = siteAbun)) +
  geom_line(aes(linetype = siteAbun, colour = siteAbun), linewidth = 1) +
  geom_rug(data = FSdata, aes(x = richness, y = 0), sides = "b", alpha = 0.5,
           position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
  labs(linetype = "Site-level abundance",
       # title =  "Fire salamander model",
       y = "Bsal prevalence (%)",
       x = "Species richness") +
  # geom_ribbon(aes(x = richness, ymin = conf.low, ymax = conf.high, fill = sppAbun), alpha = 0.2, colour = NA, show.legend = F) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_grey(start = 0.8, end = 0.2) +
  scale_x_continuous(labels = seq(0, 10, 1),
                     breaks = seq(0, 10, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 0.12, 0.04),
                     limits = c(0, 0.12),
                     minor_breaks = seq(0, 0.12, 0.01)) +
  ak_theme + theme(plot.tag.position = c(0.96, 0.9),
                   axis.text.y = element_text(face = "plain")) +
  guides(colour = guide_legend("Site-level abundance", title.position = "top", title.hjust = 0.5),
         linetype = guide_legend("Site-level abundance", title.position = "top", title.hjust = 0.5))


m_FS_plot
# ggsave("m_FS_plot.pdf", m_FS_plot, device = cairo_pdf, path = file.path(dir, figpath),
#        width = 1250, height = 1500, scale = 2, units = "px", dpi = 300, limitsize = F)

# ggsave("m_FS_plot.pdf", m_FS_plot, device = cairo_pdf, scale = 2, width = 1920, height = 1080, units = "px",
#        path = file.path(dir, figpath), dpi = 300)


## Create combined plot for manuscript
fig3a <- m_all_plot + labs(caption = NULL, y = NULL, x = NULL) +
  theme(plot.tag.position = c(0.95, 0.95),
        plot.margin = margin(.5, .75, .5, .5, "cm"),
        legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill = alpha ("white", 0.75), color = NA),
        legend.box = "horizontal",
        legend.key.size = unit(1,"cm"),
        legend.text = element_text(margin = margin(r = 1, unit = "cm"), size = 24),
        legend.title = element_text(face = "plain")) +
  guides(linetype = guide_legend(nrow = 1, "Site-level abundance", title.position = "top", title.hjust = 0.5))

fig3a

fig3b <- m_FS_plot + labs(caption = NULL, y = NULL) +
  theme(plot.tag.position = c(0.95, 0.95),
        plot.margin = margin(.5, .75, .5, .5, "cm"),
        legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill = alpha ("white", 0.75), color = NA),
        legend.box = "horizontal",
        legend.key.size = unit(1,"cm"),
        legend.title = element_text(face = "plain"),
        legend.text = element_text(margin = margin(r = 1, unit = "cm"), size = 24)) +
  guides(linetype = guide_legend(nrow = 1, "Site-level abundance", title.position = "top", title.hjust = 0.5))
fig3b


# ## Vertical plots*
# #-- need to alter x and y labs in fig3a/3b if switching between h and v plots
# http://127.0.0.1:29403/graphics/70691af6-0ed8-4077-8e3e-00e9c65965ad.png

fs_img <- image_read("firesalamander.png") # add image to 3b

fig3ab_v <- (fig3a / fig3b) + plot_annotation(tag_levels = "A")

fig3ab_v_combined <- ggdraw(fig3ab_v) +
  draw_label("Bsal prevalence (%)", x = 0, y = 0.5, angle = 90,
             size = 34, fontfamily = "Arial") +
  draw_image(image = fs_img, x = 0.35, y = -0.15, scale = 0.25) +
  ak_theme + theme(axis.line = element_blank())
fig3ab_v_combined


ggsave("modelPlots_vertical.pdf", fig3ab_v_combined, device = cairo_pdf, scale = 2,
       width = 2000, height = 2400, units = "px",
       path = file.path(dir, figpath), dpi = 300)


## Horizontal plots


fig3ab_h <- (fig3a | fig3b) + plot_annotation(tag_levels = "A")

fig3ab_h_combined <- ggdraw(fig3ab_h) +
  draw_label("Species richness", x = 0.5, y = 0, angle = 0,
             size = 34, fontfamily = "Arial") +
  draw_image(image = FS_imgpath, x = 0.4, y = 0.22, scale = 0.25) +
  ak_theme + theme(axis.line = element_blank())
fig3ab_h_combined


ggsave("modelPlots_horizontal.pdf", fig3ab_h_combined, device = cairo_pdf, scale = 2,
       width = 2400, height = 1250, units = "px",
       path = file.path(dir, figpath), dpi = 300)


# ##      3c. Prevalence by Temperature & Soil Moisture Plots for T0, T-1, T-2 (Fire Salamanders Only)
# # T0
# m3_t0_weather <- ggpredict(m3_t0, terms = c("temp_d [all]", "sMoist_d"))%>%
#   rename("temp_d" = "x",
#          "sMoist_d" = "group") %>%
#   mutate(temp_d = as.numeric(as.character(temp_d)),
#          sMoist_d = as.numeric(as.character(sMoist_d)),
#          # Convert scaled prediction to original data scale:
#          temp_dUnscaled = (temp_d * as.numeric(attr(dcbindScaled$temp_d, "scaled:scale")) +
#                                 as.numeric(attr(dcbindScaled$temp_d, "scaled:center"))),
#          sMoistUnscaled = as.factor((round(sMoist_d * as.numeric(attr(dcbindScaled$sMoist_d, "scaled:scale")) +
#                                                    as.numeric(attr(dcbindScaled$sMoist_d, "scaled:center")), 2))))
# # Create dummy column for soil moisture labels
# m3_t0_weather <- create_dummy_col(m3_t0_weather)
#
#
# m3_t0_p2 <- ggplot(m3_t0_weather, aes(x = temp_dUnscaled , y = predicted, colour = dummy)) +
#   geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), linewidth = 1) +
#   geom_rug(data = subset(dcbindScaled, scientific =="Salamandra salamandra"), aes(x = temp_d, y = 0), sides = "b", alpha = 0.5,
#            position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
#   #  geom_ribbon(aes(x = temp_dUnscaled, ymin = conf.low, ymax = conf.high, fill = dummy), alpha = 0.2, colour = NA, show.legend = F) +
#   labs(title =  bquote(paste(italic(t))[(0~days)]), linetype = "Soil moisture") +
#   ylab("Fire salamander Bsal prevalence (%)") +
#   xlab(expression("Temperature (°C)")) +
#   scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
#   scale_color_viridis(option = "G", discrete = T, begin = 0.8, end = 0.3) +
#   scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.3)) +
#   ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + guides(colour = guide_legend("Soil moisture"))
#
# m3_t0_p2
# ggsave("m3_t0_weather.tif", m3_t0_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px",
#        path = file.path(dir, figpath), dpi = 300)
#
# # T-1
# m3_t1_weather <- ggpredict(m3_t1, terms = c("temp_m_t1 [all]", "sMoist_m_t1"))%>%
#   rename("temp_m_t1" = "x",
#          "sMoist_m_t1" = "group") %>%
#   mutate(temp_m_t1 = as.numeric(as.character(temp_m_t1)),
#          sMoist_m_t1 = as.numeric(as.character(sMoist_m_t1)),
#          # Convert scaled prediction to original data scale:
#          temp_m_t1Unscaled = (temp_m_t1 * as.numeric(attr(dcbindScaled$temp_m_t1, "scaled:scale")) +
#                                    as.numeric(attr(dcbindScaled$temp_m_t1, "scaled:center"))),
#          sMoistUnscaled = as.factor((round(sMoist_m_t1 * as.numeric(attr(dcbindScaled$sMoist_m_t1, "scaled:scale")) +
#                                                    as.numeric(attr(dcbindScaled$sMoist_m_t1, "scaled:center")), 2))))
# # Create dummy column for soil moisture labels
# m3_t1_weather <- create_dummy_col(m3_t1_weather)
#
#
# m3_t1_p2 <- ggplot(m3_t1_weather, aes(x = temp_m_t1Unscaled , y = predicted, colour = dummy)) +
#   geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), linewidth = 1) +
#   geom_rug(data = subset(dcbindScaled, scientific =="Salamandra salamandra"), aes(x = temp_m_t1, y = 0), sides = "b", alpha = 0.5,
#            position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
#   #  geom_ribbon(aes(x = temp_m_t1Unscaled, ymin = conf.low, ymax = conf.high, fill = dummy), alpha = 0.2, colour = NA, show.legend = F) +
#   labs(title =  bquote(paste(italic(t))[(-30~days)]), linetype = "Soil moisture") +
#   ylab("Fire salamander Bsal prevalence (%)") +
#   xlab(expression("Temperature (°C)")) +
#   scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
#   scale_color_viridis(option = "G", discrete = T, begin = 0.8, end = 0.3) +
#   scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.3)) +
#   ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + guides(colour = guide_legend("Soil moisture"))
#
# m3_t1_p2
# ggsave("m3_t1_weather.tif", m3_t1_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px",
#        path = file.path(dir, figpath), dpi = 300)
#
#
# # T-2
# m3_t2_weather <- ggpredict(m3_t2, terms = c("temp_m_t2 [all]", "sMoist_m_t2"))%>%
#   rename("temp_m_t2" = "x",
#          "sMoist_m_t2" = "group") %>%
#   mutate(temp_m_t2 = as.numeric(as.character(temp_m_t2)),
#          sMoist_m_t2 = as.numeric(as.character(sMoist_m_t2)),
#          # Convert scaled prediction to original data scale:
#          temp_m_t2Unscaled = (temp_m_t2 * as.numeric(attr(dcbindScaled$temp_m_t2, "scaled:scale")) +
#                                    as.numeric(attr(dcbindScaled$temp_m_t2, "scaled:center"))),
#          sMoistUnscaled = as.factor((round(sMoist_m_t2 * as.numeric(attr(dcbindScaled$sMoist_m_t2, "scaled:scale")) +
#                                                    as.numeric(attr(dcbindScaled$sMoist_m_t2, "scaled:center")), 2))))
# # Create dummy column for soil moisture labels
# m3_t2_weather <- create_dummy_col(m3_t2_weather)
#
#
# m3_t2_p2 <- ggplot(m3_t2_weather, aes(x = temp_m_t2Unscaled , y = predicted, colour = dummy)) +
#   geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), linewidth = 1) +
#   geom_rug(data = subset(dcbindScaled, scientific =="Salamandra salamandra"), aes(x = temp_m_t2, y = 0), sides = "b", alpha = 0.5,
#            position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
#   #  geom_ribbon(aes(x = temp_d_t2Unscaled, ymin = conf.low, ymax = conf.high, fill = dummy), alpha = 0.2, colour = NA, show.legend = F) +
#   labs(title =  bquote(paste(italic(t))[(-60~days)]), linetype = "Soil moisture") +
#   ylab("Fire salamander Bsal prevalence (%)") +
#   xlab(expression("Temperature (°C)")) +
#   scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
#   scale_color_viridis(option = "G", discrete = T, begin = 0.8, end = 0.3) +
#   scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.3)) +
#   ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + guides(colour = guide_legend("Soil moisture"))
#
# m3_t2_p2
# ggsave("m3_t2_weather.tif", m3_t2_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px",
#        path = file.path(dir, figpath), dpi = 300)
#
# # 3 Panel Graph
# m3_p2_combined <- m2_p2_combined <- ((m3_t2_p2 + labs(caption = NULL, x = NULL) + theme(legend.position = "none")) |
#                                      (m3_t1_p2 + labs(caption = NULL, y = NULL)) |
#                                      (m3_t0_p2 + labs(caption = NULL, y = NULL, x = NULL) + theme(legend.position = "none"))) +
#                                       plot_annotation(tag_levels = "A",
#                                       caption = "Soil moisture ranged from 5.42-5.52 (kg/m"^2~"), 6.16-6.22 (kg/m"^2~"), and 6.89-6.92 (kg/m"^2~") for the Low, Medium, and High categories respectively. Timepoints above each graph indicate the time from the initial observation.",
#                                       theme = theme(plot.caption = element_text(size = 12, hjust = 0)))
#
# m3_p2_combined
# ggsave("m3_weather_combined.tif", m3_p2_combined, device = "tiff", scale = 2, width = 2600, height = 1500, units = "px",
#        path = file.path(dir, figpath), dpi = 300)
#
#
# # Remove saved objects from the global environment to speed up processing
# rm(m3_p2_combined, m3_t0_p1, m3_t0_p2, m3_t0_rich, m3_t0_weather, m3_t1_p2,
#    m3_t1_weather,m3_t2_p2, m3_t2_weather)
#
#
#
# #### 4. Fatality model ####################################################
# # Remove any instances of NA within the fatal column & weather vars columns, as well as I. alpestris
# d_fatal <- d %>%
#   tidyr::drop_na(any_of(c(31, 40:45))) %>%
#   subset(scientific != "Ichthyosaura alpestris") %>%
# # Scale relevant vars
#   mutate_at(c("tavg_wc", "tmin_wc", "tmax_wc", "prec_wc", "bio1_wc", "bio12_wc", "bio1_wc"),
#             ~(scale(., center = T, scale = T %>% as.numeric)))
# # bio1 == annual mean temp
# # bio12 == annual precip
#
# ##      4a.  Fatality of fire salamanders given an interaction between average monthly temperature (tavg) and if Bsal has ever been detected at a site.
# m4 <- glmmTMB(fatal ~ bio1_wc*prev_above_0 + (1|Site),
#                  family = "binomial",
#                  data = filter(d, scientific == "Salamandra salamandra"),
#                  control = glmmTMBControl(optimizer = optim,
#                                           optArgs = list(method = "BFGS")))
#
# summary(m4)
# Anova(m4)
#
#
# ##      4b. Plots
# # bio1 - all data
# m4_predict <- ggpredict(m4, terms = c("bio1_wc [all]", "prev_above_0")) %>%
#   rename("bio1_wc" = "x",
#          "BsalDetected" = "group") %>%
#   mutate(bio1_wc = as.numeric(as.character(bio1_wc)),
#          BsalDetected = as.factor(ifelse(BsalDetected == 0, "Bsal ( - )", "Bsal ( + )")),
#          # Convert scaled prediction to original data scale
#          bio1_wcUnscaled = (bio1_wc * as.numeric(attr(d_fatal$bio1_wc, "scaled:scale")) + as.numeric(attr(d_fatal$bio1_wc, "scaled:center"))))
#
#
# m4_plot <- ggplot(m4_predict, aes(x = sMoist_dUnscaled , y = predicted, colour = BsalDetected)) +
#   geom_line(aes(linetype = BsalDetected), linewidth = 1) +
# #  geom_rug(data = dcbindScaled, aes(x = richness, y = 0), sides = "b", alpha = 0.5,
# #           position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
#   labs(linetype = "Status") +
# #  geom_ribbon(aes(x = tavgUnscaled, ymin = conf.low, ymax = conf.high, fill = dummy), alpha = 0.2, colour = NA, show.legend = F) +
#   ylab("Fire salamander mortality (%)") +
#   xlab(expression("Mean annual temperature (°C)")) +
#   scale_linetype_manual(values = c("solid", "longdash")) +
#   scale_color_manual(values = c("black", "#C23113")) +
#   scale_x_continuous(labels = seq(17, 23, 2), breaks = seq(17, 23, 2), limits = c(17, 23)) +
#   scale_y_continuous(labels = scales::percent, limits = c(0, .5)) +
#   ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + guides(colour = guide_legend("Status"))
#
# m4_plot
#
# ggsave("m4_fatality.tif", m4_plot, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px",
#        path = file.path(dir, figpath), dpi = 300)
#


################# Extra code
# is slightly spatially autocorrelated -- tried to correct but it made the model significantly worse
# dcbindScaled$pos <- numFactor(dcbindScaled$Lon, dcbindScaled$Lat)
# dcbindScaled$group <- factor(rep(1, nrow(dcbindScaled))) # dummy grouping var
#
# # model is fit by adding the pos term in
# all_RR2 <- glmmTMB(cbind(nPos_all, nNeg_all) ~ richness*logsiteAbun +
#                     temp_d*sMoist_d + (1|scientific) + mat(pos + 0 | group),
#                   data = dcbindScaled, family = "binomial", na.action = "na.fail",
#                   control = glmmTMBControl(optimizer = optim,
#                                            optArgs = list(method = "BFGS")))
#
#
# summary(all_RR2)
# Anova(all_RR2)
#
# resid2 <- simulateResiduals(all_RR2)
# testResiduals(resid2)
# testQuantiles(resid2)
# testZeroInflation(resid2)
#
# recalc.resid2 <- recalculateResiduals(resid2, group = dcbindScaled$Site)
# testSpatialAutocorrelation(recalc.resid2, x = dcbindScaled$Lon, y = dcbindScaled$Lat)
