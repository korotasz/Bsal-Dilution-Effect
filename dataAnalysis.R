#remotes::install_version("Rttf2pt1", version = "1.3.8") # install this version, latest ver. not compatible
#extrafont::font_import() # load fonts before ggplot2; only need to do this once
require(pacman)
require(rstudioapi) # Set working directory to current file location
require(extrafont) 
extrafont::loadfonts(device = "win", quiet = T) # plot fonts
#### Plot Specific Packages ####
plot_pckgs <- c("tidyverse",
                "sjPlot",
                "ggeffects",
                "Rttf2pt1", # to use with the extrafont package
                "ggtext", # for text type/arrangements w/ ggplot2
                "hrbrthemes", # plot colors
                "patchwork", # arranging figures
                "ggsignif", # adds labels to significant groups
                "RColorBrewer",
                "colorspace", # color scale
                "viridis", # arranging figures
                "gridExtra" 
)

#### Map Specific Packages ####
map_pckgs <- c("tidyverse",
               "htmltools",
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

#### Analysis Specific Packages ####
analysis_pckgs <- c("tidyverse",
                    "data.table", # data wrangling
                    "glmmTMB", # glmmTMB()
                    "car", # Anova()
                    "DHARMa", # simulateResiduals(), testZeroInflation(), testDispersion()
                    "lme4", # lm()
                    "MASS", # negative binomial models
                    "sjPlot" # plot_model()
)




#### ####
## Set working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
csvpath <- (path.expand("/csvFiles"))
setwd(file.path(dir, csvpath))

## Load relevant packages
pacman::p_load(analysis_pckgs, character.only = T)
pacman::p_load(plot_pckgs, character.only = T)

## Load csv files
d <- read.csv("bsalData_clean.csv", header = T, encoding = "UTF-8")
dcbind <- read.csv("bsalData_cbind.csv", header = T, encoding = "UTF-8")

## Set plot theme
ak_theme <- theme_ipsum() +
  theme(axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 24, hjust = 0.5, margin = margin(t = 15, r = 0, b = 0, l = 0), face = "plain"),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 24, hjust = 0.5, margin = margin(t = 0, r = 15, b = 0, l = 5), face = "plain"),
        plot.tag = element_text(size = 32,  hjust = 0.5, face = "plain"),
        plot.title = element_text(size = 32, hjust = 0.5, face = "plain"),
        plot.subtitle = element_markdown(size = 12, face = "plain"),
        plot.margin = margin(6, 12, 2, 2, "pt"), 
        plot.caption = element_markdown(hjust = 0, size = 14, face = "plain"),
        plot.caption.position = "plot",
        legend.position = "bottom", legend.text = element_text(hjust = -1, size = 14),
        legend.spacing = unit(1, "cm"), # Space legend labels
        legend.key.size = unit(1,"cm"), 
        legend.text.align = 1,
        legend.title = element_text(size = 14, face = "bold"), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'))


## Include Pelophylax perezi dta  (5 rows) into Pelophylax sp.
d$scientific <- gsub(d$scientific, pattern = "Pelophylax perezi",
                     replacement = "Pelophylax sp.")

# log transform + scale vars
d <- d %>%
  mutate(logsppAbun = log(sppAbun),
         logsiteAbun = log(siteAbun),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility))#,


dcbind <- dcbind %>%
  mutate(logsppAbun = log(sppAbun),
         logsiteAbun = log(siteAbun),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility))#,


# Remove non-native species
dcbind_alpestris <- subset(dcbind, scientific != "Ichthyosaura alpestris")
d_alpestris <- subset(d, scientific != "Ichthyosaura alpestris")

# Remove Jaime's data 
d_noJB <- subset(d, collectorList != "Jaime Bosch")
dcbind_noJB <- subset(dcbind, collectorList != "Jaime Bosch")


#### 1) Descriptive Figures ####################################################
##      1a. Species ~ Abundance

m1a <- glmmTMB(logsppAbun ~ scientific + (1|Site),
               data = d,
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))
summary(m1a)
Anova(m1a)
m1a_predict <- ggpredict(m1a, terms = "scientific")

m1a_plot <- ggplot(m1a_predict, aes(x , exp(predicted))) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(conf.low), ymax = exp(conf.high))) +
  coord_flip() +
  ylab("Species abundance") +
  xlab("Species") + scale_x_discrete(expand = expansion(mult = c(0, 0.1), add = c(1, 0))) +
  ak_theme +
  labs(caption = c("Relative species abundance at a given site by species. Error bars represent 95% confidence intervals."))

m1a_plot



##      1b. Prevalence ~ Species
m1b <- glmmTMB(diseaseDetected ~ scientific + (1|Site), 
               data = d, family = "binom", na.action = "na.exclude",
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))

summary(m1b)
Anova(m1b)

m1b_predict <- ggpredict(m1b, terms = "scientific")

m1b_plot <- ggplot(m1b_predict, aes(x, predicted)) +
  geom_point() +
  coord_flip() +
  ylab("Disease prevalence") +
  xlab("Species") + scale_x_discrete(expand = expansion(mult = c(0, 0.1), add = c(1, 0))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  ak_theme +
  labs(caption = c("Percent disease prevalence (including both Bsal and Bd) by species."))


m1b_plot

p1combined <- ((m1a_plot + labs(caption = NULL) + theme(plot.tag.position = c(0.95, 0.92)))|
               (m1b_plot + labs(caption = NULL) +theme(axis.text.y = element_blank(), 
                                                       axis.title.y = element_blank(),
                                                       plot.tag.position = c(0.93, 0.92)))) +
                plot_annotation(tag_levels = "A") 
                 
                

p1combined


## m1b_test
library(epiR)
tmp <- d %>%
  dplyr::select(Site, date, scientific, diseaseDetected, individualCount) %>%
  group_by(Site, date, scientific) %>%
  mutate(ncas = sum(diseaseDetected == 1),
         npop = sum(individualCount)) %>%
  drop_na(date) %>%
  ungroup() %>%
  mutate(prev = ncas/npop)
  
ggplot(data = tmp, aes(x = prev)) +
  theme_bw() +
  geom_histogram(binwidth = 0.01, colour = "gray", fill = "dark blue", size = 0.1) +
  scale_x_continuous(limits = c(0,1), name = "Prevalence") +
  scale_y_continuous(limits = c(0, 150), name = "N")

tmp2 <- as.matrix(cbind(tmp$ncas, tmp$npop))
View(tmp2)

tmp2 <- epi.conf(tmp2, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                 conf.level = 0.95) * 100

tmp <- cbind(tmp, tmp2)
tmp <- tmp[sort.list(tmp$est),]

m1b_test <- glmmTMB(ncas ~ scientific + (1|Site), 
               data = tmp, family = "binomial", na.action = "na.exclude",
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))

summary(m1b_test)
Anova(m1b_test)

m1b_plot <- ggplot(tmp, aes(scientific, est)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_point() +
  coord_flip() +
  ylab("Disease prevalence") +
#  xlab("Species") + scale_x_discrete(expand = expansion(mult = c(0, 0.1), add = c(1, 0))) +
#  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  ak_theme +
  labs(caption = c("Percent disease prevalence (including both Bsal and Bd) by species."))

m1b_plot


##      1c. Susceptibility ~ Abundance
## Model based
m1c <- glmmTMB(logsppAbun ~ susceptibility + (1|Site),
               data = d,
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))
summary(m1c)
Anova(m1c)
m1c_predict <- ggpredict(m1c, terms = c("susceptibility"))

m1c_plot <- ggplot(m1c_predict, aes(x , exp(predicted))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = exp(conf.low), ymax = exp(conf.high))) +
  scale_x_discrete(labels = c("Resistant", "Tolerant", "Susceptible")) +
  ylab("Species abundance") +
  xlab("Susceptibility level") + 
  ak_theme +
  labs(caption = c("Resistant = No to low infection and no clinical signs of disease; Tolerant = low infection loads with<br> 
  low or variable mortality; and Susceptible = High infection loads resulting in consistently high mortality.<br>On average, less abundant species at a given site tend to be more resistant while more abundant species tend to be<br>susceptible. Thus, given that rare species are lost from communities first, biodiversity loss might increase<br>disease risk in ecosystems with Bsal. Error bars represent 95% confidence intervals. "))

m1c_plot


## Descriptive (no stats)
# mean spp. abundance by site
mean_sppAbun <- aggregate(individualCount ~ scientific+Site, d, mean)
names(mean_sppAbun)[names(mean_sppAbun) == 'individualCount'] <- 'meanSppAbun'


descriptive <-d %>%
  group_by(Site, date, scientific, susceptibility, individualCount, richness, sppAbun, siteAbun) %>%
  dplyr::summarize(count = n()) %>%
  mutate(susceptibility = as.factor(susceptibility)) %>% 
  left_join(mean_sppAbun, by = c("scientific", "Site")) %>%
  mutate(meanSppAbun = round(meanSppAbun, )) %>%
  arrange(Site) %>%
  subset(individualCount != 208 & individualCount != 72)

# mean spp. abundance by site with susceptibility level noted
abun <-ggplot(descriptive, aes(x = Site, y = scientific, size = meanSppAbun, colour = susceptibility)) +
  geom_point(aes(size = meanSppAbun), alpha=0.5) +
  scale_size_continuous(name = "Mean Species Abundance", range = c(3, 10)) +
  scale_colour_manual(name = "Susceptibility",values = c("#8bd3c7", "#f8ae5d", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible")) +
  scale_x_discrete(limits = factor(c(0:400)), breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 
                                                         225, 250, 275, 300, 325, 350, 375, 400),
                   labels = c("0", "", "", "", "100", "", "", "", "200", "", "", "", "300", "", "", "", "400")) +
  ylab("Species") +
  xlab("Site #") + guides(color = guide_legend(override.aes = list(size = 10))) + # increase size of 'susceptibility' legend icons
  ak_theme + 
  labs(caption = c("Abundance of individual species at each site. Outliers (*e.g.*, instances of a single mass die-off) were<br>removed to better highlight data. Size of each point indicates the site abundance (*i.e.*, how many<br>species total there are at a site for  all sampling occurrences). Color indicates the susceptibility level<br>of each species: Resistant (blue) = No to low infection and no clinical signs of disease;<br> Tolerant (orange) = low infection loads with low or variable mortality; and Susceptible (red) = High<br> infection loads resulting in consistently high mortality."))

abun



##      1d. Maps (Interactive)
pacman::p_load(map_pckgs, character.only = T)

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



##      1d. Maps (Static Maps showing sampling locations)
# Code for Europe map with points for each site
library(mapproj)

v.europe <- c("Norway", "Sweden", "Finland", "Denmark", "United Kingdom","Ireland", "Greece",
              "Belgium", "Netherlands", "France", "Spain", "Portugal", "Luxembourg", "Croatia",
              "Germany", "Switzerland", "Austria", "Slovenia", "Italy", "Bulgaria", "Romania",
              "Czech Rep.", "Slovakia", "Hungary", "Poland", "Bosnia Hercegovina", "Serbia",
              "Turkey", "Ukraine", "Moldova", "Belarus", "Estonia", "Latvia", "Lithuania",
              "Montenegro", "Albania", "Macedonia", "UK")

wmap <- map_data("world") %>%
  filter(region %in% v.europe)

# Aggregate observations to the site level
d2 <- d %>%
  group_by(Site, decimalLongitude, decimalLatitude) %>%
  summarize(Abundance = sum(individualCount),
            Richness = length(unique(scientific)),
            Bsal = ifelse(sum(BsalDetected) > 0,
                          "Detected",
                          "Not detected"))

# Adding the geom_rect for the inset box takes a bit; comment out for faster mapping
ggplot(wmap, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = NA, colour = "grey60")+
  geom_point(data = d2, aes(x = decimalLongitude, y = decimalLatitude,
                            fill = Bsal, alpha = Bsal),
             shape = 21, size = 3)+
  scale_alpha_manual(values = c(1,.3))+
  geom_rect(aes(xmin = 2.5, xmax = 11,
                ymin = 49, ymax = 52.5),
            color = "red", linewidth = 1, fill = NA)+
  coord_map(ylim = c(35,60), xlim = c(-10,15))+
  theme_bw()


ggplot(wmap, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = NA, colour = "grey60")+
  geom_point(data = d2, aes(x = decimalLongitude, y = decimalLatitude,
                            fill = Bsal, alpha = Bsal),
             shape = 21, size = 3)+
  scale_alpha_manual(values = c(1,.3))+
  coord_map(ylim = c(49.1,52.1), xlim = c(3,11))+
  theme_bw()



#### 2. Cbind models for all salamander spp. ####################################################
##      2a. T0 (At time of observation); T-1 (30 days prior to initial obs.); T-2 (60 days prior to initial obs.)
# Drop rows with NA vals
dcbind_alpestris <- tidyr::drop_na(dcbind_alpestris, any_of(c(14:15, 29:34)))

# Scale relevant vars
scaledData <- dcbind_alpestris %>%
  mutate_at(c("temp_date", "temp_date_t1", "temp_date_t2",
              "soilMoisture_date", "soilMoisture_date_t1", "soilMoisture_date_t2"), 
            ~(scale(., center = T, scale = T %>% as.numeric)))


# T0
m2_t0 <- glmmTMB(cbind(YesBsal, NoBsal) ~  richness*logsiteAbun + temp_date*soilMoisture_date + (1|scientific),
                 data = scaledData, family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m2_t0)
Anova(m2_t0)


# T-1
m2_t1 <- glmmTMB(cbind(YesBsal, NoBsal) ~  richness*logsiteAbun + temp_date_t1*soilMoisture_date_t1 + (1|scientific),
                 data = scaledData, family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m2_t1)
Anova(m2_t1)



# T-2
m2_t2 <- glmmTMB(cbind(YesBsal, NoBsal) ~  richness*logsiteAbun + temp_date_t2*soilMoisture_date_t2 + (1|scientific),
                 data = scaledData, family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m2_t2)
Anova(m2_t2)



##      2b. Prevalence by Abundance & Richness Plots for T0, T-1, T-2 
# T0
m2_t0_rich <- ggpredict(m2_t0,  terms = c("richness", "logsiteAbun [0.5, 2, 4]"))


m2_t0_p1 <- ggplot(m2_t0_rich, aes(x = x , y = predicted)) +
  geom_line(aes(linetype = group)) +
  labs(title = bquote(Time[(0)]), linetype = "ln(Site-Level Abundance)") +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal Prevalence\nAcross All Salamander Species") +
  xlab("Species Richness") + 
  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.05)) + 
  ak_theme

m2_t0_p1 

# T-1
m2_t1_rich <- ggpredict(m2_t1,  terms = c("richness", "logsiteAbun [0.5, 2, 4]"))


m2_t1_p1 <- ggplot(m2_t1_rich, aes(x = x , y = predicted)) +
  geom_line(aes(linetype = group)) +
  labs(title = bquote(Time[(-1)]), linetype = "ln(Site-Level Abundance)") +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal Prevalence\nAcross All Salamander Species") +
  xlab("Species Richness") + 
  guides(fill = guide_legend(title = "ln(Site-Level Abundance)")) +
  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.05)) + 
  ak_theme

m2_t1_p1 


# T-2
m2_t2_rich <- ggpredict(m2_t2,  terms = c("richness", "logsiteAbun [0.5, 2, 4]"))


m2_t2_p1 <- ggplot(m2_t2_rich, aes(x = x , y = predicted)) +
  geom_line(aes(linetype = group)) +
  labs(title = bquote(Time[(-2)]), linetype = "ln(Site-Level Abundance)") +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal Prevalence\nAcross All Salamander Species") +
  xlab("Species Richness") + 
  guides(fill = guide_legend(title = "ln(Site-Level Abundance)")) +
  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.05)) + 
  ak_theme

m2_t2_p1 

# 3 Panel Graph
m2_p1_combined <- ggarrange(m2_t2_p1 + rremove("xlab"), 
                            m2_t1_p1 + rremove("ylab"), 
                            m2_t0_p1 + rremove("ylab") + rremove("xlab"), 
                            labels = NULL,
                            ncol = 3, nrow = 1,
                            common.legend = TRUE, legend = "bottom",
                            align = "hv", 
                            font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

m2_p1_combined



##      2c. Prevalence by Temperature & Soil Moisture Plots for T0, T-1, T-2 
# T0
m2_t0_weather <- ggpredict(m2_t0, terms = c("temp_date [all]", "soilMoisture_date"))
m2_t0_weather <- m2_t0_weather %>%
  rename("temp_date" = "x",
         "soilMoisture_date" = "group") %>%
  mutate(temp_date = as.numeric(as.character(temp_date)),
         soilMoisture_date = as.numeric(as.character(soilMoisture_date)))

# Convert scaled prediction to original data scale:
m2_t0_w_unscaled <- m2_t0_weather
m2_t0_w_unscaled$temp_date <- m2_t0_weather$temp_date * as.numeric(attr(scaledData$temp_date, "scaled:scale")) +
  as.numeric(attr(scaledData$temp_date, "scaled:center"))
m2_t0_w_unscaled$soilMoisture_date <- m2_t0_weather$soilMoisture * as.numeric(attr(scaledData$soilMoisture_date,
                                                                                   "scaled:scale")) +
  as.numeric(attr(scaledData$soilMoisture_date, "scaled:center"))
m2_t0_w_unscaled$soilMoisture_date <- round(m2_t0_w_unscaled$soilMoisture_date, 2)


m2_t0_p2 <- ggplot(m2_t0_w_unscaled, aes(x = temp_date , y = predicted)) +
  geom_line(aes(linetype = as.factor(soilMoisture_date))) +
  labs(title = bquote(Time[(0)]), linetype = bquote("Soil Moisture (kg/m"^2~")")) +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal Prevalence\nAcross All Salamander Species") +
  xlab(expression("Temperature (°C)")) + 
  #  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  ak_theme

m2_t0_p2 


# T-1
m2_t1_weather <- ggpredict(m2_t1, terms = c("temp_date_t1 [all]", "soilMoisture_date_t1"))
m2_t1_weather <- m2_t1_weather %>%
  rename("temp_date_t1" = "x",
         "soilMoisture_date_t1" = "group") %>%
  mutate(temp_date_t1 = as.numeric(as.character(temp_date_t1)),
         soilMoisture_date_t1 = as.numeric(as.character(soilMoisture_date_t1)))

# Convert scaled prediction to original data scale:
m2_t1_w_unscaled <- m2_t1_weather
m2_t1_w_unscaled$temp_date_t1 <- m2_t1_weather$temp_date_t1 * as.numeric(attr(scaledData$temp_date_t1, "scaled:scale")) +
  as.numeric(attr(scaledData$temp_date_t1, "scaled:center"))
m2_t1_w_unscaled$soilMoisture_date_t1 <- m2_t1_weather$soilMoisture * as.numeric(attr(scaledData$soilMoisture_date_t1,
                                                                                      "scaled:scale")) +
  as.numeric(attr(scaledData$soilMoisture_date_t1, "scaled:center"))
m2_t1_w_unscaled$soilMoisture_date_t1 <- round(m2_t1_w_unscaled$soilMoisture_date_t1, 2)


m2_t1_p2 <- ggplot(m2_t1_w_unscaled, aes(x = temp_date_t1 , y = predicted)) +
  geom_line(aes(linetype = as.factor(soilMoisture_date_t1))) +
  labs(title = bquote(Time[(-1)]), linetype = bquote("Soil Moisture (kg/m"^2~")")) +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal Prevalence\nAcross All Salamander Species") +
  xlab(expression("Temperature (°C)")) + 
  #  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  ak_theme

m2_t1_p2 


# T-2
m2_t2_weather <- ggpredict(m2_t2, terms = c("temp_date_t2 [all]", "soilMoisture_date_t2"))
m2_t2_weather <- m2_t2_weather %>%
  rename("temp_date_t2" = "x",
         "soilMoisture_date_t2" = "group") %>%
  mutate(temp_date_t2 = as.numeric(as.character(temp_date_t2)),
         soilMoisture_date_t2 = as.numeric(as.character(soilMoisture_date_t2)))

# Convert scaled prediction to original data scale:
m2_t2_w_unscaled <- m2_t2_weather
m2_t2_w_unscaled$temp_date_t2 <- m2_t2_weather$temp_date_t2 * as.numeric(attr(scaledData$temp_date_t2, "scaled:scale")) +
  as.numeric(attr(scaledData$temp_date_t2, "scaled:center"))
m2_t2_w_unscaled$soilMoisture_date_t2 <- m2_t2_weather$soilMoisture * as.numeric(attr(scaledData$soilMoisture_date_t2,
                                                                                      "scaled:scale")) +
  as.numeric(attr(scaledData$soilMoisture_date_t2, "scaled:center"))
m2_t2_w_unscaled$soilMoisture_date_t2 <- round(m2_t2_w_unscaled$soilMoisture_date_t2, 2)


m2_t2_p2 <- ggplot(m2_t2_w_unscaled, aes(x = temp_date_t2 , y = predicted)) +
  geom_line(aes(linetype = as.factor(soilMoisture_date_t2))) +
  labs(title = bquote(Time[(-2)]), linetype = bquote("Soil Moisture (kg/m"^2~")")) +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal Prevalence\nAcross All Salamander Species") +
  xlab(expression("Temperature (°C)")) + 
  #  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  ak_theme

m2_t2_p2 


# 3 Panel Graph
m2_p2_combined <- ggarrange(m2_t2_p2 + rremove("xlab"), 
                            m2_t1_p2  + rremove("ylab"), 
                            m2_t0_p2 +rremove("ylab") + rremove("xlab"), 
                            labels = NULL,
                            ncol = 3, nrow = 1,
                            common.legend = TRUE, legend = "bottom",
                            align = "hv", 
                            font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

m2_p2_combined



#### 3. Cbind models for fire salamanders only ####################################################
##      3a. T0 (At time of observation); T-1 (30 days prior to initial obs.); T-2 (60 days prior to initial obs.)
# T0
m3_t0 <- glmmTMB(cbind(YesBsal, NoBsal) ~  richness*logsiteAbun + temp_date*soilMoisture_date + (1|scientific),
                 data = subset(scaledData, scientific =="Salamandra salamandra"), family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m3_t0)
Anova(m3_t0)


# T-1
m3_t1 <- glmmTMB(cbind(YesBsal, NoBsal) ~  richness*logsiteAbun + temp_date_t1*soilMoisture_date_t1 + (1|scientific),
                 data = subset(scaledData, scientific =="Salamandra salamandra"), family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m3_t1)
Anova(m3_t1)



# T-2
m3_t2 <- glmmTMB(cbind(YesBsal, NoBsal) ~  richness*logsiteAbun + temp_date_t2*soilMoisture_date_t2 + (1|scientific),
                 data = subset(scaledData, scientific =="Salamandra salamandra"), family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m3_t2)
Anova(m3_t2)



##      3b. Prevalence by Abundance & Richness Plots for T0, T-1, T-2 (Fire Salamanders Only)
# T0
m3_t0_rich <- ggpredict(m3_t0,  terms = c("richness", "logsiteAbun [0.5, 1.5, 3]"))


m3_t0_p1 <- ggplot(m3_t0_rich, aes(x = x , y = predicted)) +
  geom_line(aes(linetype = group)) +
  labs(title = bquote(Time[(0)]), linetype = "ln(Site-Level Abundance)") +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal Prevalence\nin Fire Salamanders") +
  xlab("Species Richness") + 
  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.6)) + 
  ak_theme

m3_t0_p1 

# T-1
m3_t1_rich <- ggpredict(m3_t1,  terms = c("richness", "logsiteAbun [0.5, 1.5, 3]"))


m3_t1_p1 <- ggplot(m3_t1_rich, aes(x = x , y = predicted)) +
  geom_line(aes(linetype = group)) +
  labs(title = bquote(Time[(-1)]), linetype = "ln(Site-Level Abundance)") +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal Prevalence\nin Fire Salamanders") +
  xlab("Species Richness") + 
  guides(fill = guide_legend(title = "ln(Site-Level Abundance)")) +
  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.6)) + 
  ak_theme

m3_t1_p1 


# T-2
m3_t2_rich <- ggpredict(m3_t2,  terms = c("richness", "logsiteAbun [0.5, 1.5, 3]"))


m3_t2_p1 <- ggplot(m3_t2_rich, aes(x = x , y = predicted)) +
  geom_line(aes(linetype = group)) +
  labs(title = bquote(Time[(-2)]), linetype = "ln(Site-Level Abundance)") +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal Prevalence\nin Fire Salamanders") +
  xlab("Species Richness") + 
  guides(fill = guide_legend(title = "ln(Site-Level Abundance)")) +
  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.6)) + 
  ak_theme

m3_t2_p1 

# 3 Panel Graph
m3_p1_combined <- ggarrange(m3_t2_p1 + rremove("xlab"), 
                            m3_t1_p1 + rremove("ylab"), 
                            m3_t0_p1 + rremove("ylab") + rremove("xlab"), 
                            labels = NULL,
                            ncol = 3, nrow = 1,
                            common.legend = TRUE, legend = "bottom",
                            align = "hv", 
                            font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

m3_p1_combined



##      3c. Prevalence by Temperature & Soil Moisture Plots for T0, T-1, T-2 (Fire Salamanders Only)
# T0
m3_t0_weather <- ggpredict(m3_t0, terms = c("temp_date [all]", "soilMoisture_date"))
m3_t0_weather <- m3_t0_weather %>%
  rename("temp_date" = "x",
         "soilMoisture_date" = "group") %>%
  mutate(temp_date = as.numeric(as.character(temp_date)),
         soilMoisture_date = as.numeric(as.character(soilMoisture_date)))

# Convert scaled prediction to original data scale:
m3_t0_w_unscaled <- m3_t0_weather
m3_t0_w_unscaled$temp_date <- m3_t0_weather$temp_date * as.numeric(attr(scaledData$temp_date, "scaled:scale")) +
  as.numeric(attr(scaledData$temp_date, "scaled:center"))
m3_t0_w_unscaled$soilMoisture_date <- m3_t0_weather$soilMoisture * as.numeric(attr(scaledData$soilMoisture_date,
                                                                                   "scaled:scale")) +
  as.numeric(attr(scaledData$soilMoisture_date, "scaled:center"))
m3_t0_w_unscaled$soilMoisture_date <- round(m3_t0_w_unscaled$soilMoisture_date, 2)


m3_t0_p2 <- ggplot(m3_t0_w_unscaled, aes(x = temp_date , y = predicted)) +
  geom_line(aes(linetype = as.factor(soilMoisture_date))) +
  labs(title = bquote(Time[(0)]), linetype = bquote("Soil Moisture (kg/m"^2~")")) +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal Prevalence\nin Fire Salamanders") +
  xlab(expression("Temperature (°C)")) + 
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.6)) + 
  ak_theme

m3_t0_p2 


# T-1
m3_t1_weather <- ggpredict(m3_t1, terms = c("temp_date_t1 [all]", "soilMoisture_date_t1"))
m3_t1_weather <- m3_t1_weather %>%
  rename("temp_date_t1" = "x",
         "soilMoisture_date_t1" = "group") %>%
  mutate(temp_date_t1 = as.numeric(as.character(temp_date_t1)),
         soilMoisture_date_t1 = as.numeric(as.character(soilMoisture_date_t1)))

# Convert scaled prediction to original data scale:
m3_t1_w_unscaled <- m3_t1_weather
m3_t1_w_unscaled$temp_date_t1 <- m3_t1_weather$temp_date_t1 * as.numeric(attr(scaledData$temp_date_t1, "scaled:scale")) +
  as.numeric(attr(scaledData$temp_date_t1, "scaled:center"))
m3_t1_w_unscaled$soilMoisture_date_t1 <- m3_t1_weather$soilMoisture * as.numeric(attr(scaledData$soilMoisture_date_t1,
                                                                                      "scaled:scale")) +
  as.numeric(attr(scaledData$soilMoisture_date_t1, "scaled:center"))
m3_t1_w_unscaled$soilMoisture_date_t1 <- round(m3_t1_w_unscaled$soilMoisture_date_t1, 2)


m3_t1_p2 <- ggplot(m3_t1_w_unscaled, aes(x = temp_date_t1 , y = predicted)) +
  geom_line(aes(linetype = as.factor(soilMoisture_date_t1))) +
  labs(title = bquote(Time[(-1)]), linetype = bquote("Soil Moisture (kg/m"^2~")")) +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal Prevalence\nin Fire Salamanders") +
  xlab(expression("Temperature (°C)")) + 
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.6)) + 
  ak_theme

m3_t1_p2 



# T-2
m3_t2_weather <- ggpredict(m3_t2, terms = c("temp_date_t2 [all]", "soilMoisture_date_t2"))
m3_t2_weather <- m3_t2_weather %>%
  rename("temp_date_t2" = "x",
         "soilMoisture_date_t2" = "group") %>%
  mutate(temp_date_t2 = as.numeric(as.character(temp_date_t2)),
         soilMoisture_date_t2 = as.numeric(as.character(soilMoisture_date_t2)))

# Convert scaled prediction to original data scale:
m3_t2_w_unscaled <- m3_t2_weather
m3_t2_w_unscaled$temp_date_t2 <- m3_t2_weather$temp_date_t2 * as.numeric(attr(scaledData$temp_date_t2, "scaled:scale")) +
  as.numeric(attr(scaledData$temp_date_t2, "scaled:center"))
m3_t2_w_unscaled$soilMoisture_date_t2 <- m3_t2_weather$soilMoisture * as.numeric(attr(scaledData$soilMoisture_date_t2,
                                                                                      "scaled:scale")) +
  as.numeric(attr(scaledData$soilMoisture_date_t2, "scaled:center"))
m3_t2_w_unscaled$soilMoisture_date_t2 <- round(m3_t2_w_unscaled$soilMoisture_date_t2, 2)


m3_t2_p2 <- ggplot(m3_t2_w_unscaled, aes(x = temp_date_t2 , y = predicted)) +
  geom_line(aes(linetype = as.factor(soilMoisture_date_t2))) +
  labs(title = bquote(Time[(-2)]), linetype = bquote("Soil Moisture (kg/m"^2~")")) +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal Prevalence\nin Fire Salamanders") +
  xlab(expression("Temperature (°C)")) + 
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.6)) + 
  ak_theme

m3_t2_p2 


# 3 Panel Graph
m3_p2_combined <- ggarrange(m3_t2_p2 + rremove("xlab"), 
                            m3_t1_p2 + rremove("ylab"), 
                            m3_t0_p2 + rremove("ylab") + rremove("xlab"), 
                            labels = NULL,
                            ncol = 3, nrow = 1,
                            common.legend = TRUE, legend = "bottom",
                            align = "hv", 
                            font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

m3_p2_combined




#### 4. Fatality models ####################################################
##      4a. T0 (At time of observation); T-1 (30 days prior to initial obs.); T-2 (60 days prior to initial obs.)
# Drop rows with NA vals
d_alpestris <- tidyr::drop_na(d_alpestris, any_of(c(37:42)))

# Scale relevant vars
scaledData_fatal <- d_alpestris %>%
  mutate_at(c("temp_date", "temp_date_t1", "temp_date_t2",
              "soilMoisture_date", "soilMoisture_date_t1", "soilMoisture_date_t2"), 
            ~(scale(., center = T, scale = T %>% as.numeric)))


# T0
m4_t0 <- glmmTMB(fatal ~ temp_date*BsalDetected + (1|Site),
                 family = "binomial",
                 data = subset(scaledData_fatal, scientific =="Salamandra salamandra"),
                 control = glmmTMBControl(optimizer = optim,
                                          optArgs = list(method = "BFGS")))

summary(m4_t0)
Anova(m4_t0)



# T-1
m4_t1 <- glmmTMB(fatal ~ temp_date_t1*BsalDetected  + (1|Site),
                 family = "binomial",
                 data = subset(scaledData_fatal, scientific =="Salamandra salamandra"),
                 control = glmmTMBControl(optimizer = optim,
                                          optArgs = list(method = "BFGS")))

summary(m4_t1)
Anova(m4_t1)

# T-2
m4_t2 <- glmmTMB(fatal ~ temp_date_t2*BsalDetected + (1|Site),
                 family = "binomial",
                 data = subset(scaledData_fatal, scientific =="Salamandra salamandra"),
                 control = glmmTMBControl(optimizer = optim,
                                          optArgs = list(method = "BFGS")))

summary(m4_t2)
Anova(m4_t2)



##      4b. Interaction plot for fire salamanders only
# T0
m4_t0_predict <- ggpredict(m4_t0, terms = c("temp_date [all]", "BsalDetected"))
m4_t0_predict <- m4_t0_predict %>%
  rename("temp_date" = "x",
         "BsalDetected" = "group") %>%
  mutate(temp_date = as.numeric(as.character(temp_date)),
         BsalDetected = ifelse(BsalDetected == 0, "No", "Yes"))

# Convert scaled prediction to original data scale:
m4_t0_unscaled <- m4_t0_predict
m4_t0_unscaled$temp_date <- m4_t0_predict$temp_date * as.numeric(attr(scaledData_fatal$temp_date, "scaled:scale")) +
  as.numeric(attr(scaledData$temp_date, "scaled:center"))

m4_t0_plot <- ggplot(m4_t0_unscaled, aes(x = temp_date , y = predicted)) +
  geom_line(aes(linetype = as.factor(BsalDetected))) +
  labs(title = bquote(Time[(0)]), linetype = "Bsal Positive") +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Fire Salamander\nMortality (%)") +
  xlab(expression("Temperature (°C)")) + 
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) +  
  scale_y_continuous(breaks = seq(0, .02, .01),
                     labels = scales::percent,
                     limits = c(0, .02)) +
  ak_theme


m4_t0_plot

# T-1
m4_t1_predict <- ggpredict(m4_t1, terms = c("temp_date_t1 [all]", "BsalDetected"))
m4_t1_predict <- m4_t1_predict %>%
  rename("temp_date_t1" = "x",
         "BsalDetected" = "group") %>%
  mutate(temp_date = as.numeric(as.character(temp_date_t1)),
         BsalDetected = ifelse(BsalDetected == 0, "No", "Yes"))

# Convert scaled prediction to original data scale:
m4_t1_unscaled <- m4_t1_predict
m4_t1_unscaled$temp_date_t1 <- m4_t1_predict$temp_date_t1 * as.numeric(attr(scaledData_fatal$temp_date_t1, "scaled:scale")) +
  as.numeric(attr(scaledData$temp_date_t1, "scaled:center"))

m4_t1_plot <- ggplot(m4_t1_unscaled, aes(x = temp_date_t1 , y = predicted)) +
  geom_line(aes(linetype = as.factor(BsalDetected))) +
  labs(title = bquote(Time[(-1)]), linetype = "Bsal Positive") +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Fire Salamander\nMortality (%)") +
  xlab(expression("Temperature (°C)")) + 
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) +  
  scale_y_continuous(breaks = seq(0, .002, .001),
                     labels = scales::percent,
                     limits = c(0, .002)) +
  ak_theme


m4_t1_plot

# T-2
m4_t2_predict <- ggpredict(m4_t2, terms = c("temp_date_t2 [all]", "BsalDetected"))
m4_t2_predict <- m4_t2_predict %>%
  rename("temp_date_t2" = "x",
         "BsalDetected" = "group") %>%
  mutate(temp_date = as.numeric(as.character(temp_date_t2)),
         BsalDetected = ifelse(BsalDetected == 0, "No", "Yes"))

# Convert scaled prediction to original data scale:
m4_t2_unscaled <- m4_t2_predict
m4_t2_unscaled$temp_date_t2 <- m4_t2_predict$temp_date_t2 * as.numeric(attr(scaledData_fatal$temp_date_t2, "scaled:scale")) +
  as.numeric(attr(scaledData$temp_date_t2, "scaled:center"))

m4_t2_plot <- ggplot(m4_t2_unscaled, aes(x = temp_date_t2 , y = predicted)) +
  geom_line(aes(linetype = as.factor(BsalDetected))) +
  labs(title = bquote(Time[(-2)]), linetype = "Bsal Positive") +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Fire Salamander\nMortality (%)") +
  xlab(expression("Temperature (°C)")) + 
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) +  
  scale_y_continuous(breaks = seq(0, .002, .001),
                     labels = scales::percent,
                     limits = c(0, .002)) + 
  ak_theme


m4_t2_plot


# 3 Panel Graph
m4_combined <- ggarrange(m4_t2_plot  + rremove("xlab"), 
                         m4_t1_plot  + rremove("ylab"), 
                         m4_t0_plot +rremove("ylab") + rremove("xlab"), 
                         labels = NULL,
                         ncol = 3, nrow = 1,
                         common.legend = TRUE, legend = "bottom",
                         align = "hv", 
                         font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

m4_combined








