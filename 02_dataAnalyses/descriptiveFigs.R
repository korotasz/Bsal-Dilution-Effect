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

## As of 2024-04-04, there are issues with patchwork and ggplot2 that require specific pull requests to resolve:
# remotes::install_github("thomasp85/patchwork")
# remotes::install_github("tidyverse/ggplot2", ref = remotes::github_pull("5592"))

require(pacman)
extrafont::loadfonts("all", quiet = T)

### General Visualization Packages----------------------------------------------
pckgs <- c("tidyverse", # data wrangling/manipulation
             "ggbreak", # create axis breaks in ggplots
              "ggtext", # for text type/arrangements w/ ggplot2
            "Rttf2pt1", # to use with the extrafont package
            "ggthemes", # contains 'scales', 'themes', and 'geoms' packages
           "grDevices", # saves high quality svg, pdf, and ps files
            "graphics", # dependency of grDevices
             "cowplot", # arranging plots/figs
           "gridExtra", # arranging plots/figs
           "patchwork", # arranging plots/figs
           "ggpattern", # plot patterns inside of maps
          "hrbrthemes", # plot colors
           "flextable", # create tables compatible with Word
           "gtsummary", # better package for creating tables from glmmTMB objects
         "broom.mixed", # required to create flextable/gtsummary objects from mixed model outputs
       "broom.helpers", # required for gtsummary
### Mapping Packages -----------------------------------------------------------
             "geodata", # obtain geographic data (world map)
       "rnaturalearth", # obtain spatial polygons that can be used with sf
                  "sf", # mapping
           "ggspatial", # north arrow and scale bar
              "mapproj" # apply map projection
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
analysis <- file.path(dir, path.expand("02_dataAnalyses"))
outputs <- file.path(dir, path.expand("03_outputs"))
figpath <- file.path(outputs, path.expand("figures"))
shppath <- file.path(dir, path.expand("01_dataCleaning/shapefiles"))

## Read in .csv files and prep data --------------------------------------------
setwd(analysis)
## only data I am confident about -- not meant for analyses, just visualization.
## only data I am confident about
d <- read.csv("BsalData_OK.csv", header = T, encoding = "UTF-8") %>%
  filter(continent == "Europe") %>%
  #   Retain sites that have ever had a Bsal+, regardless of when the first positive at that site was
  filter(!(is.na(dateFirstPositive))) %>%
  #   transform vars
  mutate(logsppAbun = log(sppAbun + 1),
         logsiteAbun = log(siteAbun + 1),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
  relocate(c(logsppAbun, logsiteAbun), .after = sppAbun)


## 'dcbind' dataset is what will be used to run our models
dcbind <- read.csv("Bsal_cbind_OK.csv", header = T, encoding = "UTF-8") %>%
  filter(continent == "Europe") %>%
  #   Retain sites that have ever had a Bsal+, regardless of when the first positive at that site was
  filter(!(is.na(dateFirstPositive))) %>%
  # transform vars
  mutate(logsppAbun = log(sppAbun + 1),
         logsiteAbun = log(siteAbun + 1),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
  relocate(c(logsppAbun, logsiteAbun), .after = sppAbun) %>%
  ## Data prep for cbind models: drop rows with NA vals in weather data & scale relevant vars
  #   bio1_wc = "annual mean temperature" | bio12_wc = annual precipitation
  tidyr::drop_na(., any_of(c("temp_d", "sMoist_d", "tmin_wc:bio1_wc", "bio12_wc"))) %>%
  mutate_at(c("temp_d", "sMoist_d",
              "bio1_wc", "bio12_wc", "tavg_wc", "prec_wc"),
            ~(scale(., center = T, scale = T %>% as.numeric))) %>%
  mutate(year = year(date))


## Define general ggplot theme for plot uniformity -----------------------------
## Load fonts from grDevices
grDevices::pdfFonts()

ak_theme <- hrbrthemes::theme_ipsum(base_family = "Segoe UI Light") +
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
        # plot.margin = margin(1, 1, 1.5, 1.2, "cm"),
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


## I. Generate maps ------------------------------------------------------------
# Europe polygons were re-projected to EPSG:27704 (WGS84/Equi7 Europe) for mapping
# purposes. These projections were made as a part of a larger project to optimize
# handling remote-sensing data, with each continent having their own centroid.
#  doi: https://doi.org/10.1016/j.cageo.2014.07.005
#  Equi7Grid GitHub: https://github.com/TUW-GEO/Equi7Grid
epsg27704 <- crs('+proj=aeqd +lat_0=53 +lon_0=24 +x_0=5837287.81977 +y_0=2121415.69617 +datum=WGS84 +units=m +no_defs')


# Summarise number of Bsal positive sites
sites <- d %>%
  subset(., select = c(country, ADM0, Lat, Lon, posSite, Site)) %>%
  mutate(posSite = case_when(posSite == "1" ~ "Bsal positive site",
                             posSite == "0" ~ "Bsal negative site")) %>%
  filter(posSite != "Bsal negative site") %>%
  # distinct() %>%
  distinct(Site, .keep_all = T) %>%
  arrange(posSite)

sampSize <- d %>%
  filter(continent == "Europe") %>%
  subset(., select = c(country, Site, Lat, Lon,  scientific,
                       BsalDetected, individualCount)) %>%
  plyr::mutate(BsalDetected = as.factor(dplyr::recode(BsalDetected,
                                                      "1" = "Bsal positive",
                                                      "0" = "Bsal negative"))) %>%
  group_by(country, Site, BsalDetected) %>%
  summarise(n = n()) %>%
  ungroup()

# summarise # Bsal positive sites data
nSites <- sites %>%
  group_by(country) %>%
  summarise(n = n())

# Get the coordinates of each country
country_lookup <- read.csv("countries.csv", stringsAsFactors = F) %>%
  rename(country_code = country,
         country = name)


# Combine summarised data
mapLabels <- merge(x = nSites, y = country_lookup,
                   by.x = "country", all.x = T)
mapLabels <- st_as_sf(mapLabels, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(., crs = epsg27704)


# Obtain world map
worldmap <- ne_countries(scale = "medium", type = "map_units", returnclass = "sf")

## Transform coordinates to EPSG27704
# transform 'sites' into sf object for plotting on map
sites_transformed <- sites %>%
  group_by(country, Lat, Lon) %>%
  distinct () %>%
  st_as_sf(., coords = c("Lon", "Lat"), crs = 4326) %>%
  st_transform(., crs = epsg27704) %>%
  mutate(jittered = sf::st_jitter(geometry, 0.25))

# Subset Europe from world map
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
  geom_sf(data = sites_transformed, aes(geometry = geometry,
                                        fill = posSite,
                                        shape = posSite),
          position = position_dodge(0.5, preserve = "total"), alpha = 0.3, size = 3, stroke = 1,
          color = "gray30", show.legend = "point") +
  # geom_sf_text(data = countries, aes(label = name), position = "identity", size = 5) +
  scale_fill_manual(values = "#b30000", guide = "none") +
  scale_shape_manual(values = 23, guide = "none") +
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
                   axis.text.x = element_text(size = 28, angle = 45, hjust = 1),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 28, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(fill = "#b30000",
                                                 color = "gray30",
                                                 shape = 23,
                                                 size = 5,
                                                 stroke = 0.75,
                                                 alpha = 1)))


europe_map

# ggsave("Europe.pdf", europe_map, device = cairo_pdf, path = file.path(figpath, "/maps"),
#        width = 2900, height = 2500, scale = 2, units = "px", dpi = 300, limitsize = F)

### b. Germany -----------------------------------------------------------------
map_bounds(7.5, 14.5, 46.5, 55.5, crs = epsg27704)

g <- mapLabels %>% filter(country == "Germany") %>%
  plyr::mutate(label = paste(n, "Bsal positive sites", sep = " "))

deu_map <- ggplot() +
  geom_sf(data = europe, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = countries, aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  # geom_sf_label(data = g, aes(label = paste(label)), nudge_x = -80000, nudge_y = 400000,
  #               size = 5, fontface = "bold", label.size = NA, alpha = 0.5) +
  geom_sf(data = sites_transformed, aes(geometry = geometry, fill = posSite, shape = posSite),
          alpha = 0.3, size = 3, stroke = 1, color = "gray30", show.legend = "point") +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = "#b30000", guide = "none") +
  scale_shape_manual(values = 23, guide = "none") +
  coord_sf(xlim = c(4452875, 5238551), # c(7.5, 14.5)
           ylim = c(1540088, 2435744)) + # c(46.5, 55.5)
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
                   axis.text.x = element_text(size = 22, angle = 45, hjust = 1),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 22, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(fill = "#b30000",
                                                 color = "gray30",
                                                 shape = 23,
                                                 size = 5,
                                                 stroke = 0.75,
                                                 alpha = 1)))

deu_map

# ggsave("Germany.pdf", deu_map, device = cairo_pdf, path = file.path(figpath, "/maps"),
#        width = 2100, height = 2400, scale = 2, units = "px", dpi = 300, limitsize = F)

### c. Spain -------------------------------------------------------------------
s <- mapLabels %>% filter(country == "Spain") %>%
  plyr::mutate(label = paste(n, "Bsal positive site", sep = " "))

esp_map <- ggplot() +
  geom_sf(data = europe, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = countries, aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  # geom_sf_label(data = s, aes(label = paste(label)), nudge_x = -350000, nudge_y = 400000,
  #               size = 5, fontface = "bold", label.size = NA, alpha = 0.5) +
  geom_sf(data = sites_transformed, aes(geometry = geometry, fill = posSite, shape = posSite),
          alpha = 1, size = 3, stroke = 1, color = "gray30", show.legend = "point") +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = "#b30000", guide = "none") +
  scale_shape_manual(values = 23, guide = "none") +
  coord_sf(xlim = c(2860148, 4298549), # c(-9, 4)
           ylim = c(690856.1, 1606424)) + # c(34.25, 46.5)
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
                   axis.text.x = element_text(size = 22, angle = 45, hjust = 1),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 22, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(fill = "#b30000",
                                                 color = "gray30",
                                                 shape = 23,
                                                 size = 5,
                                                 stroke = 0.75,
                                                 alpha = 1)))

esp_map
# ggsave("Spain.pdf", esp_map, device = cairo_pdf, path = file.path(figpath, "/maps"),
#        width = 2000, height = 1500, scale = 2, units = "px", dpi = 300, limitsize = F)

### > Figure 1. Overview of Bsal positive sites in Europe -------------------------
## Create dummy map
p <- ggplot() + labs(x = "Longitude", y = "Latitude") +
  ak_theme + theme(plot.margin = margin(0, 0, 0, 0, "cm"), panel.spacing.y = unit(0,"cm"))

x_axis <- cowplot::get_plot_component(p, "xlab-b")
y_axis <- cowplot::get_plot_component(p, "ylab-l")

layout <- "
#AAA
BAAA
#AAA
##C#
"
## place tags on maps
fig1a <- europe_map +
  geom_sf_text(data = countries, aes(label = name), position = "identity", size = 9) +
  theme(plot.tag.position = c(0.97, 0.87))

fig1b <- esp_map +
  geom_sf_label(data = s, aes(label = paste(label)), nudge_x = -240000, nudge_y = 360000,
                size = 7, fontface = "bold", label.size = NA, alpha = 0.5) +
  theme(plot.tag.position = c(0.95, 0.74))

fig1c <- deu_map +
  geom_sf_label(data = g, aes(label = paste(label)), nudge_x = -200000, nudge_y = 400000,
                                   size = 7, fontface = "bold", label.size = NA, alpha = 0.5) +
  theme(plot.tag.position = c(0.95, 0.95))


## First combine all maps
fig1_map <- (fig1a + plot_spacer() + (fig1b/fig1c)) +
  plot_annotation(tag_levels = "A") +
  plot_layout(widths = c(4, -0.0025, 1.75),
              heights = 1,
              guides = "collect") &
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),
        legend.position = "top",
        legend.box.margin = margin(0.25, 1, 0.25, 1, "cm"),
        legend.text = element_text(margin = margin(r = 0.5, unit = "cm")),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Segoe UI Semibold", face = "bold", size = 42))


## Annotate combined map + add axes labels
fig1_map_annotated <- wrap_plots(wrap_elements(fig1_map),ggdraw(y_axis),ggdraw(x_axis)) +
  plot_layout(widths = c(.15, 2),
              heights = c(2.25, 0.25),
              design = layout) +
  theme(axis.title.x = element_text(vjust = -3))

fig1_map_annotated

# ggsave("fig1_map.pdf", fig1_map_annotated, device = cairo_pdf, path = file.path(figpath, "/maps"),
#        width = 2800, height = 1800, scale = 2, units = "px", dpi = 300, limitsize = F)
#
# rm(countries, country_lookup, deu_map, esp_map, europe, europe_map, fig1_map,
#    fig1_map_annotated, fig1a, fig1b, fig1c, g, mapLabels, nSites, p, s, sampSize,
#    sites, sites_transformed, worldmap, x_axis, y_axis, epsg27704, layout)

## II. Table 1. Tabular overview of host data, Bsal+ cases, and mortality in our data ----
tbl1_data <- d %>%
  filter(continent == "Europe" & posSite == 1) %>%
  dplyr::select(country, susceptibility, scientific, establishmentMeans, redListCategory,
                populationTrend, BsalDetected, fatal, individualCount) %>%
  mutate(BsalDetected = case_when(BsalDetected == "1" ~ "Bsal_positive",
                                  BsalDetected == "0" ~ "Bsal_negative"),
         fatal = case_when(fatal == "1" ~ "Dead",
                           fatal == "0" ~ "Alive")) %>%
  group_by(country, susceptibility, scientific, establishmentMeans, redListCategory,
           populationTrend, BsalDetected, fatal) %>%
  summarize(n = sum(individualCount)) %>%
  pivot_wider(names_from = c(BsalDetected, fatal),
              id_cols = c(country, susceptibility, scientific, establishmentMeans, redListCategory,
                          populationTrend),
              values_from = n, values_fill = 0) %>%
  mutate(nPos = sum(Bsal_positive_Alive, Bsal_positive_Dead),
         nDead = sum(Bsal_positive_Dead, Bsal_negative_Dead),
         nBoth = sum(Bsal_positive_Dead),
         nTotal = sum(Bsal_positive_Alive, Bsal_positive_Dead, Bsal_negative_Alive, Bsal_negative_Dead)) %>%
  subset(., select = c(country:populationTrend, nPos:nTotal)) %>%
  mutate(susceptibility = case_when(susceptibility == "1" ~ "Resistant",
                                    susceptibility == "2" ~ "Tolerant",
                                    susceptibility == "3" ~ "Susceptible"),
         redListCategory = case_when(redListCategory == "Least Concern" ~ "LC",
                                     redListCategory == "Vulnerable" ~ "VU"))


table1 <- tbl1_data %>%
  flextable(theme_fun = theme(booktabs))

table1

#### > Prevalence by species & site (tables) -----------------------------------
## > Observed prevalence by species
deu <- sampSize %>%
  filter(country == "Germany") %>%
  subset(., select = c(country, scientific, susceptibility, nPos, sppTotal)) %>%
  group_by(scientific) %>%
  mutate(ncas_Bsal = sum(nPos), # number of observed Bsal+ cases
         npop = sum(sppTotal)) %>% # pop size (total # individuals/spp.)
  filter(!(npop < 15)) %>%
  ungroup() %>%
  mutate(Bsal_prev = (ncas_Bsal/npop)*100) %>% # prevalence as a percentage
  subset(., select = c(scientific, susceptibility, ncas_Bsal, Bsal_prev, npop)) %>%
  unique() %>%
  dplyr::mutate(row_id = row_number()) %>%
  relocate(row_id, .before = scientific)

esp <- sampSize %>%
  filter(country == "Spain") %>%
  subset(., select = c(country, scientific, susceptibility, nPos, sppTotal)) %>%
  group_by(scientific) %>%
  mutate(ncas_Bsal = sum(nPos), # number of observed Bsal+ cases
         npop = sum(sppTotal)) %>% # pop size (total # individuals/spp.)
  filter(!(npop < 15)) %>%
  ungroup() %>%
  mutate(Bsal_prev = (ncas_Bsal/npop)*100) %>% # prevalence as a percentage
  subset(., select = c(scientific, susceptibility, ncas_Bsal, Bsal_prev, npop)) %>%
  unique() %>%
  dplyr::mutate(row_id = row_number()) %>%
  relocate(row_id, .before = scientific)


deu_bayesci <- binom::binom.bayes(x = deu$ncas_Bsal, n = deu$npop, type = "highest", tol = 1e-9, maxit = 1000)
esp_bayesci <- binom::binom.bayes(x = esp$ncas_Bsal, n = esp$npop, type = "highest", tol = 1e-9, maxit = 1000)


# Convert bayesian estimates + CIs to percentages for plotting
deu_bayesci <-  deu_bayesci %>%
  plyr::mutate(mean = round((mean*100), 1),
               lower = round((lower*100), 1),
               upper = round((upper*100), 1)) %>%
  dplyr::mutate(row_id = row_number()) %>%
  relocate(row_id, .before = method)

# Convert bayesian estimates + CIs to percentages for plotting
esp_bayesci <-  esp_bayesci %>%
  plyr::mutate(mean = round((mean*100), 1),
               lower = round((lower*100), 1),
               upper = round((upper*100), 1)) %>%
  dplyr::mutate(row_id = row_number()) %>%
  relocate(row_id, .before = method)



# Join with original dataset, so we can plot actual vs expected
deu <- deu %>%
  left_join(., deu_bayesci, by = "row_id") %>%
  plyr::arrange(., scientific) %>%
  mutate(country = "Germany") %>%
  relocate(country, .after = "row_id") %>%
  dplyr::select(-("row_id"))

# Join with original dataset, so we can plot actual vs expected
esp <- esp %>%
  left_join(., esp_bayesci, by = "row_id") %>%
  plyr::arrange(., scientific) %>%
  mutate(country = "Spain") %>%
  relocate(country, .after = "row_id") %>%
  dplyr::select(-("row_id"))

supp_prevtbl <- rbind(deu, esp)
View(supp_prevtbl)

## III. Extra data for extra figures -------------------------------------------
## May not use all of this.
## Sites that have >15 observations over all sampling periods
prevBySite <- d %>%
  filter(continent == "Europe" & posSite == 1) %>%
  subset(., select = c(country, Site, BsalDetected, individualCount)) %>%
  plyr::mutate(BsalDetected = as.factor(dplyr::recode(BsalDetected,
                                                      "1" = "nPos",
                                                      "0" = "nNeg"))) %>%
  group_by(country, Site, BsalDetected) %>%
  mutate(n = sum(individualCount)) %>%
  dplyr::select(!(individualCount)) %>%
  unique() %>%
  pivot_wider(names_from = BsalDetected, id_cols = c(country, Site), values_from = n) %>%
  mutate(nPos = case_when(is.na(nPos) ~ 0,
                          TRUE ~ nPos),
         nNeg = case_when(is.na(nNeg) ~ 0,
                          TRUE ~ nNeg)) %>%
  group_by(country, Site) %>%
  mutate(siteTotal = sum(nPos, nNeg))
# group_by(Site) %>%
# filter(siteTotal > 15) # 26 out of 49 sites have >15 observations on any given date


tmp <- d_subset %>%
  filter(continent == "Europe" & posSite == "1") %>%
  dplyr::select(Site, country, susceptibility, BsalDetected, fatal, scientific, individualCount) %>%
  filter(BsalDetected == "1") %>%
  group_by(country, susceptibility, scientific) %>%
  summarise(n = sum(individualCount))
  ungroup()   %>%
  group_by(country, scientific) %>%
  summarise(n = n()) %>%
  print(n = 31)

tmp2<-d %>%
    filter(continent == "Europe"  & posSite == "1") #%>%
    dplyr::select(Site, country, susceptibility, BsalDetected, fatal, scientific, individualCount) %>%
    # filter(BsalDetected == "1") %>%
    group_by(country, susceptibility, scientific) %>%
    summarise(n = sum(individualCount))
  ungroup()   %>%
    group_by(country, scientific) %>%
    summarise(n = n()) %>%
    print(n = 31)

