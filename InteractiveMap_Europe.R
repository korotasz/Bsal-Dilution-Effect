#### Map Specific Packages ####
map_pckgs <- c("tidyverse",
               "htmltools",
               "htmlwidgets", # save leaflet output as html file
               "stars", # spatiotemporal data handling
               "RColorBrewer",
               "ggspatial", # north arrow and scale bar,
               "raster", # raster data handling
               "sf", # vector data handling
               "sp", 
               "leaflet", # making interactive maps
               "leaftime", # add time scale to map
               "geojsonio",
               "geojsonlint",
               "ggpubr"# validate GeoJSON and display it on a map
)

## Set working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
csvpath <- (path.expand("/csvFiles"))
setwd(file.path(dir, csvpath))

## Load packages
pacman::p_load(map_pckgs, character.only = T)

## Load csv files
d <- read.csv("bsalData_clean.csv", header = T, encoding = "UTF-8")

##      1d. Maps (Interactive)
d <- d %>%
  mutate(color = case_when(
    susceptibility == "1" ~ "Resistant",
    susceptibility == "2" ~ "Tolerant",
    susceptibility == "3" ~ "Susceptible"
  ),
  fatalStatus = case_when(
    fatal == "1" ~ "Dead",
    fatal == "0" ~ "Alive",
    is.na(fatal) == T ~ "Unk")
  )

## Interactive Map
pal <- colorFactor(c("#b30000", "#f8ae5d", "#8bd3c7"), domain = c("Susceptible", "Resistant", "Tolerant")) # marker colors
cols <- c("#b30000", "#f8ae5d", "#8bd3c7") # legend
labs <- c("Susceptible", "Resistant", "Tolerant") # legend
#d_geo <- geojsonio::geojson_json(d, lat = "decimalLatitude", lon = "decimalLongitude")

map <- leaflet(data = d) %>%
  addProviderTiles(provider = "Stamen.TonerLite", group = "Basic Map") %>%
  addProviderTiles(provider = "Esri.WorldImagery", group = "World Imagery") %>%
  addLayersControl(baseGroups = c("Basic Map", "World Imagery")) %>%
  addCircleMarkers(lng = d$decimalLongitude, lat = d$decimalLatitude,
                   radius = 10,
                   color = ~pal(d$color),
                   fillOpacity = ifelse(d$fatalStatus == "Alive", 1, 0.5),
                   stroke = FALSE,
                   clusterOptions = markerClusterOptions(),
                   label = d$scientific,
                   labelOptions = labelOptions(direction = "auto", offset = c(0,0),
                                               style = list("color" = "black",
                                                            "font-family" = "sans-serif",
                                                            "font-style" = "italic",
                                                            "font-weight" = "bold",
                                                            "box-shadow" = "1px 1px rgba(0,0,0,0.25)",
                                                            "font-size" = "12px",
                                                            "padding" = "4px"
                                               )),
                   popup = paste("<b>Site:</b>", d$Site, "<br>",
                                 "<b>Bsal Detected:</b>", ifelse(d$BsalDetected == 1, "Yes", "No"), "<br>",
                                 "<b>Bd Detected:</b>", ifelse(d$BdDetected == 1, "Yes", "No"), "<br>",
                                 "<b>Status:</b>", d$fatalStatus)) %>%
  setView(lat = 47.81757743622691, lng = 6.5171597480332135, zoom = 4) %>%
  addLegend(position = "bottomleft",
            colors = ~cols,
            labels = ~labs,
            title = paste("Bsal Susceptibility"),
            opacity = 0.75) %>%
  addScaleBar(position = "bottomleft",
              options = scaleBarOptions(maxWidth = 100,
                                        metric = TRUE,
                                        updateWhenIdle = TRUE))

map


saveWidget(map, file = "BsalMap.html")
