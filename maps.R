#remotes::install_version("Rttf2pt1", version = "1.3.8") # install this version, latest ver. not compatible
#remotes::install_github("gorkang/html2latex") # convert sjPlot::tab_model() hmtl table to tex and pdf in .Rmd docs
#extrafont::font_import("C:/Windows/Fonts") # load fonts before ggplot2; only need to do this once
require(pacman)
require(rstudioapi) # Set working directory to current file location
require(extrafontdb)
require(extrafont) 
extrafont::loadfonts(device = "all", quiet = T) # plot fonts 


#### Visualization Packages ####
pckgs <- c("ggsignif", # adds labels to significant groups
               "renv", # environment lock  
             "ggtext", # for text type/arrangements w/ ggplot2
           "Rttf2pt1", # to use with the extrafont package
           "ggthemes", # contains 'scales', 'themes', and 'geoms' packages
          "grDevices", # saves high quality svg, pdf, and ps files
           "graphics", # dependency of grDevices
           "showtext", # fonts
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
             "ggpubr", # prepares plots to be ready for publication
          # "latex2exp", # allows use of LaTeX in R
          #      "glue", # allows concatenation of LaTeX and R syntax
             "sjPlot", # plot_model(), tab_model()
          "htmltools", # visualizes model outputs as html tables
           "webshot2", # converts html files to png
             "magick" # image_read() -- needed for cowplot::draw_image
)

## Load packages
#### IF RENV CANNOT INSTALL/LOAD PACKAGES, USE CODE BELOW TO NAVIGATE TO OTHER .libPaths() OUTSIDE OF PROJECT.
## Home computer:
# renv::hydrate(packages = c(pckgs, "pacman"), sources = c("C:/Users/alexi/AppData/Local/R/win-library/4.3", "C:/Program Files/R/R-4.3.1/library"))
## Work computer
# renv::hydrate(packages = c(pckgs, "pacman"), sources = c("C:/Users/Alexis/AppData/Local/R/win-library/4.3", "C:/Program Files/R/R-4.3.1/library"))


pacman::p_load(pckgs, update = F, install = T, character.only = T)


## Set working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
csvpath <- (path.expand("/csvFiles"))
figpath <- (path.expand("figures"))
setwd(file.path(dir, csvpath))

## If this is your first time running this project, run the following two lines:
# setwd(file.path(dir)) # set working directory to activate project
# renv::activate() # activate project

#-------------------------------------------------------------------------------
#  *** ONLY NEED TO DO THIS ONCE; I HAVE ALREADY DONE THIS FOR THIS PROJ. ***

## Create a project-local environment to ensure code reproducibility
# setwd(file.path(dir))
# renv::init() # this also activates the project


## Save the state of the project (including package versions); only need to re-run this
##  if you update packages/introduce new packages in this project.
#renv::settings$snapshot.type("explicit") # records and reflects latest proj. changes in "renv.lock" file

#-------------------------------------------------------------------------------

## Set plot theme 
ak_theme <- hrbrthemes::theme_ipsum() +
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
        plot.caption = element_markdown(hjust = 0, size = 14, face = "plain"),
        plot.caption.position = "plot",
        legend.position = "top", 
        legend.key.size = unit(2,"cm"), 
        legend.text.align = 1,
        legend.text = element_text(size = 28, hjust = -1),
        legend.title = element_text(size = 28, face = "bold"), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.spacing.y = unit(1.5,"cm"),
        strip.text = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.line = element_line(color = 'black'))


#### Load csv files ####
d <- read.csv("bsalData_clean.csv", header = T, encoding = "UTF-8")
dcbind <- read.csv("bsalData_cbind.csv", header = T, encoding = "UTF-8")

# log transform vars
d <- d %>%
  mutate(logsppAbun = log(sppAbun),
         logsiteAbun = log(siteAbun),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
  relocate(c(logsppAbun, logsiteAbun), .after = sppAbun) 


dcbind <- dcbind %>%
  mutate(logsppAbun = log(sppAbun),
         logsiteAbun = log(siteAbun),
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



#### 1) Descriptive Figures ####################################################
## Figure 1. Richness maps
# Get the coordinates of each country
country_lookup <- read.csv("countries.csv", stringsAsFactors = F)
names(country_lookup)[1] <- "country_code"


# Combine summarised data
mapLabels <- merge(x = sampSize, y = country_lookup, 
              by.x = "country", by.y = "name", all.x = T)
mapLabels <- st_as_sf(mapLabels, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(., crs = 3035)


# Obtain world map
worldmap <- ne_countries(scale = "medium", type = "map_units", returnclass = "sf")

# Polygons are re-projected to the EPSG:3035 (Lambert Azimuthal Equal Area Projection) 
# This projection covers all of Europe (on- and off-shore), is based on the GCS ETRS 89,
# and has been used in the EU's INSPIRE directive. In other words, it is well established.
# Coordinates are displayed in meters and must be translated back into decimal degrees.
europe <- ne_countries(scale = "medium", type = "map_units", returnclass = "sf") %>%
  filter(continent == "Europe" & !name %in% c("Russia")) %>%
  st_transform(., crs = 4326) 

# st_write(europe, "europe.shp", append = F)

# Specify countries with Bsal field sampling efforts
countriesSampled <- worldmap %>% 
  filter(sovereignt %in% c("Spain", "Switzerland", "Germany", "United Kingdom") 
         & !name %in% c("Guernsey", "Isle of Man", "Jersey", "N. Ireland", 
                        "Scotland", "Wales", "Anguilla", "Bermuda", "Br. Indian Ocean Ter.",
                        "Cayman Is.",  "Falkland Is.", "Montserrat", "Pitcairn Is.",
                        "S. Geo. and S. Sandw. Is.", "Saint Helena", "Turks and Caicos Is.",
                        "British Virgin Is.")) %>%
  st_transform(., crs = 3035) 

plot(europe)
# Map
europe_map <- ggplot() +
  geom_sf(data = europe, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = countriesSampled, aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  geom_sf(data = obs_transformed, aes(geometry = geometry, fill = BsalDetected, shape = BsalDetected),
          alpha = 0.3, size = 4, stroke = 1, color = "gray30", show.legend = "point") +
  geom_sf_text(data = countriesSampled, aes(label = name), position = "identity", size = 5) +
  scale_fill_manual(values = c("gray40", "#b30000"), guide = "none") +
  scale_shape_manual(values = c(21, 24), guide = "none") +
  coord_sf(xlim = c(2652777.0489846086, 4632884.549024921), # c(-16, 15)
           ylim = c(1615336.1806950625, 3665962.1500697937)) + # c(37, 56)
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
                   legend.text.align = 0,
                   legend.text = element_text(size = 28, hjust = 0),
                   axis.text.x = element_text(size = 28),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 28, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = c("gray40", "#b30000"),
                                                 shape = c(21, 24), 
                                                 size = c(5, 5),
                                                 alpha = c(1, 1))))

  

europe_map  

## Germany map
g <- mapLabels %>% filter(country == "Germany") %>%
  plyr::mutate(label = paste(country, " (n = ", n, ")", sep = ""))

deu_map <- ggplot() +
  geom_sf(data = europe, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = countriesSampled, aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  geom_sf_label(data = g, aes(label = paste(label)), nudge_x = -160000, nudge_y = 400000,
                size = 7, fontface = "bold", label.size = NA, alpha = 0.5) +
  geom_sf(data = obs_transformed, aes(geometry = geometry, fill = BsalDetected, shape = BsalDetected),
          alpha = 0.3, size = 4, stroke = 1, color = "gray30", show.legend = "point") +
  scale_fill_manual(values = c("gray40", "#b30000"), guide = "none") +
  scale_shape_manual(values = c(21, 24), guide = "none") +
  coord_sf(xlim = c(4031004.6092165913, 4662450.609393332), # c(5, 15)
           ylim = c(2742165.4171582675, 3505801.4326768997)) + # c(47.5, 54.4)
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
                   legend.text.align = 0,
                   legend.text = element_text(size = 28, hjust = 0),
                   axis.text.x = element_text(size = 24),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 24, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = c("gray40", "#b30000"),
                                                 shape = c(21, 24), 
                                                 size = c(5, 5),
                                                 alpha = c(1, 1))))

deu_map

## Spain map
s <- mapLabels %>% filter(country == "Spain") %>%
  plyr::mutate(label = paste(country, " (n = ", n, ")", sep = ""))

esp_map <- ggplot() +
  geom_sf(data = europe, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = countriesSampled, aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  geom_sf_label(data = s, aes(label = paste(label)), nudge_x = -180000, nudge_y = 520000,
                 size = 7, fontface = "bold", label.size = NA, alpha = 0.5) +
  geom_sf(data = obs_transformed, aes(geometry = geometry, fill = BsalDetected, shape = BsalDetected),
          alpha = 0.3, size = 4, stroke = 1, color = "gray30", show.legend = "point") +
  scale_fill_manual(values = c("gray40", "#b30000"), guide = "none") +
  scale_shape_manual(values = c(21, 24), guide = "none") +
  coord_sf(xlim = c(2762860.9104916425, 3804910.3673174703), # c(-6, 4.5)
           ylim = c(1396746.8807063631, 2550763.7938421355)) + # c(33.5, 45)
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
                   legend.text.align = 0,
                   legend.text = element_text(size = 28, hjust = 0),
                   axis.text.x = element_text(size = 24),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 24, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = c("gray40", "#b30000"),
                                                 shape = c(21, 24), 
                                                 size = c(5, 5),
                                                 alpha = c(1, 1))))

esp_map


## Switzerland map
swz <- mapLabels %>% filter(country == "Switzerland") %>%
  plyr::mutate(label = paste(country, " (n = ", n, ")", sep = ""))

# geometry <- data.frame(st_coordinates(st_cast(swz$geometry, "POINT"))) # scale_y_continuous didn't like the format of geom_sf_label
# 
# swz <- cbind(swz, geometry)

che_map <- ggplot() +
  geom_sf(data = europe, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = countriesSampled, aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  geom_sf_label(data = swz, aes(label = paste(label)), nudge_x = -80000, nudge_y = 120000,
                size = 7, fontface = "bold", label.size = NA, alpha = 0.5, inherit.aes = F) +
  geom_sf(data = obs_transformed, aes(geometry = geometry, fill = BsalDetected, shape = BsalDetected),
          alpha = 0.3, size = 4, stroke = 1, color = "gray30", show.legend = "point") +
  scale_fill_manual(values = c("gray40", "#b30000"), guide = "none") +
  scale_shape_manual(values = c(21, 24), guide = "none") +
  coord_sf(xlim = c(4010139.011070984, 4359878.636359634), # c(6, 11)
           ylim = c(2498679.761531601, 2765224.576573791)) + # c(45.75, 48)
  # scale_y_continuous(breaks = c(46, 47, 48),
  #                    limits = c(45.75, 48)) +
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
                   legend.text.align = 0,
                   legend.text = element_text(size = 28, hjust = 0),
                   axis.text.x = element_text(size = 24),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 24, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = c("gray40", "#b30000"),
                                                 shape = c(21, 24), 
                                                 size = c(5, 5),
                                                 alpha = c(1, 1))))

che_map

## UK map
uk <- mapLabels %>% filter(country == "United Kingdom") %>%
  plyr::mutate(country = as.factor(dplyr::recode(country,
                                                      "United Kingdom" = "England"))) %>%
  plyr::mutate(label = paste(country, " (n = ", n, ")", sep = "")) 

gbr_map <- ggplot() +
  geom_sf(data = europe, col = "gray40", fill = "#ECECEC", show.legend = F) +
  geom_sf(data = countriesSampled, aes(fill = sovereignt), col = "gray40", fill = "#B2BEB5", show.legend = F) +
  geom_sf_label(data = uk, aes(label = paste(label)), nudge_x = -150000, nudge_y = 0,
                size = 7, fontface = "bold", label.size = NA, alpha = 0.5) +
  geom_sf(data = obs_transformed, aes(geometry = geometry, fill = BsalDetected, shape = BsalDetected),
          alpha = 0.3, size = 4, stroke = 1, color = "gray30", show.legend = "point") +
  scale_fill_manual(values = c("gray40", "#b30000"), guide = "none") +
  scale_shape_manual(values = c(21, 24), guide = "none") +
  coord_sf(xlim = c(3184199.4221568545, 3819948.287940598), # c(5, 11)
           ylim = c(3083054.5969610764, 3670734.0742844036)) +
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
                   legend.text.align = 0,
                   legend.text = element_text(size = 28, hjust = 0),
                   axis.text.x = element_text(size = 24),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 24, face = "plain"),
                   axis.title.y = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = c("gray40", "#b30000"),
                                                 shape = c(21, 24), 
                                                 size = c(5, 5),
                                                 alpha = c(1, 1))))

gbr_map



fig1a <- europe_map + theme(plot.tag.position = c(0.92, 0.88)) 
fig1b <- gbr_map + theme(plot.tag.position = c(0.95, 0.78)) 
fig1c <- esp_map + theme(plot.tag.position = c(0.95, 0.95)) 
fig1d <- deu_map + theme(plot.tag.position = c(0.96, 0.82)) 
fig1e <- che_map + theme(plot.tag.position = c(0.96, 0.92))

p <- ggplot() + labs(x = "Longitude", y = "Latitude") + ak_theme + theme(plot.margin = margin(0, 0, 0, 0, "cm"),
                                                                         panel.spacing.y = unit(0,"cm"))
x_axis <- cowplot::get_plot_component(p, "xlab-b") 
y_axis <- cowplot::get_plot_component(p, "ylab-l") 

layout <- "
#AAA
BAAA
#AAA
##C#
"


fig1_map <- ((fig1b/fig1c)|(fig1a)|(fig1d/fig1e)) +
  plot_annotation(tag_levels = list(c("B", "C", "A", "D", "E"))) +
  plot_layout(guides = "collect",
              widths = c(1, 2, 1),
              heights = c(1, 2, 1)) &
  theme(plot.margin = margin(0.25, 0.25, 0, -0.5, "cm"),
        legend.position = "top",
        legend.box.margin = margin(0, 1, 1, 1, "cm"),
        legend.text = element_text(margin = margin(r = 1, unit = "cm")),
        legend.title = element_blank())

fig1_map

fig1_map_annotated <- wrap_plots(wrap_elements(fig1_map), ggdraw(y_axis), ggdraw(x_axis)) +
  plot_layout(widths = c(.15, 2.25),
              heights = c(2.25, 0.25),
              design = layout) + theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"))
 


fig1_map_annotated

# ggsave("Euro_map.pdf", fig1_map_annotated, device = cairo_pdf, path = file.path(dir, figpath),
#         width = 4000, height = 2000, scale = 2, units = "px", dpi = 300, limitsize = F)

rm(che_map, countriesSampled, country_lookup, deu_map, esp_map, europe, europe_map,
   fig1_map, fig1a, fig1b, fig1c, fig1d, fig1e, g, gbr_map, mapLabels, obs, obs_transformed,
   p, s, sampSize, swz, uk, worldmap, x_axis, y_axis, layout)

