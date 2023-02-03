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
                "gridExtra",
                "grid",
                "cowplot",
                "ggpubr"
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
                    "epiR",
                    "sjPlot",
                    "effects"# plot_model()
)




#### ####
## Set working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
csvpath <- (path.expand("/csvFiles"))
figpath <- (path.expand("/figures"))
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
        plot.tag = element_text(size = 32, face = "plain"),
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
         susceptibility = as.factor(susceptibility)) %>%
  relocate(c(logsppAbun, logsiteAbun), .after = sppAbun) %>%
  subset(scientific != "Calotriton asper") # Only one observation with NA vals for date


dcbind <- dcbind %>%
  mutate(logsppAbun = log(sppAbun),
         logsiteAbun = log(siteAbun),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
  relocate(c(logsppAbun, logsiteAbun), .after = sppAbun) %>%
  subset(scientific != "Calotriton asper") # Only one observation with NA vals for date



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
  geom_errorbar(aes(ymin = exp(conf.low), ymax = exp(conf.high)), width = 0.4) +
  coord_flip() +
  ylab("Species abundance") +
  xlab("Species") + scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(1, 0.5))) +
  ak_theme #+
 # labs(caption = c("Relative species abundance at a given site by species. Error bars represent 95% confidence intervals."))

m1a_plot
ggsave("plot1a_sppAbun.tif", m1a_plot, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

##      1b. Prevalence ~ Species
d_prev <- d %>%
  group_by(scientific) %>%
  mutate(ncas_all = sum(diseaseDetected == 1), # number of pos. cases; both Bd and Bsal
         ncas_Bsal = sum(BsalDetected == 1), # number of pos. Bsal cases
         npop = sum(individualCount)) %>% # pop size (# of ALL individuals of that spp. for graphing purposes)
  drop_na(date) %>%
  ungroup() %>%
  mutate(prev_all = ncas_all/npop, # prevalence as a proportion
         prev_Bsal = ncas_Bsal/npop) 

# Frequency histogram of disease prevalence estimates for our data
ggplot(data = d_prev, aes(x = prev_all)) +
  theme_bw() +
  geom_histogram(binwidth = 0.01, colour = "gray", fill = "dark blue", linewidth = 0.1) +
  xlab("Prevalence") +
  ylab("N")

# Use a matrix containing number of cases (ncas) and population size (npop) to calculate the prevalence of disease in each population and its 95% confint
tmp <- as.matrix(cbind(d_prev$ncas_all, d_prev$npop))
tmp <- epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                 conf.level = 0.95) * 100
tmp <- tmp %>% 
  dplyr::rename(est_all = est,
                lower_all = lower,
                upper_all = upper)

# Add back to d_prev dataframe and sort by estimated prevalence
d_prev <- cbind(d_prev, tmp)


# Now repeat for Bsal
tmp <- as.matrix(cbind(d_prev$ncas_Bsal, d_prev$npop))
tmp <- epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100
tmp <- tmp %>% 
  dplyr::rename(est_Bsal = est,
                lower_Bsal = lower,
                upper_Bsal = upper)

# Add back to d_prev dataframe
d_prev <- cbind(d_prev, tmp)
d_prev <- d_prev[sort.list(d_prev$est_Bsal),]
d_prev <- d_prev %>%
  relocate(c(est_all, lower_all, upper_all), .after = prev_all)

# Descriptive plot showing estimated disease prevalence values per species with a 95% ci
m1b_plot <- ggplot(d_prev, aes(scientific, est_all)) +
  geom_errorbar(aes(ymin = lower_all, ymax = upper_all), width = 0.4) +
  geom_point() +
  coord_flip() +
  ylab("Disease prevalence (%)") +
  xlab("Species") + scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(1, 0.5))) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, suffix = "%")) +
  ak_theme #+
#  labs(caption = c("Descriptive plot showing disease prevalence (including both Bsal and Bd) for each species as a percent. Error bars represent 95% confidence intervals."))

m1b_plot
ggsave("plot1b_prevalence.tif", m1b_plot, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

p1combined <- ((m1a_plot + labs(caption = NULL) + theme(plot.tag.position = c(0.96, 0.95)))|
               (m1b_plot + labs(caption = NULL) + theme(axis.text.y = element_blank(), 
                                                       axis.title.y = element_blank(),
                                                       plot.tag.position = c(0.93, 0.95)))) +
                plot_annotation(tag_levels = "A") 
                 
                

p1combined

ggsave("plots1ab_combined.tif", p1combined, device = "tiff", scale = 2, width = 2400, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

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
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = exp(conf.low), ymax = exp(conf.high)), width = 0.2, linewidth = 1) +
  scale_x_discrete(labels = c("Resistant", "Tolerant", "Susceptible")) +
  ylab("Species abundance") + scale_y_discrete(limits = factor(0:8), breaks = c("0", "2", "4", "6", "8")) +
  xlab("Susceptibility level") + 
  ak_theme #+
#  labs(caption = c("Resistant = No to low infection and no clinical signs of disease; Tolerant = low infection loads with low or variable mortality;
#                   and Susceptible = High infection loads resulting in consistently high mortality. On average, less abundant species at a given site 
#                   tend to be more resistant while more abundant species<br>tend to be susceptible. Thus, given that rare species are lost from 
#                   communities first, biodiversity loss might increase disease risk in ecosystems with Bsal. Error bars represent 95% confidence 
#                   intervals."))

m1c_plot
ggsave("plot1c_susceptibility.tif", m1c_plot, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)


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
jitter <- position_jitter(width = 0.2, height = 0.25)

abun <-ggplot(descriptive, aes(x = Site, y = scientific, size = meanSppAbun, colour = susceptibility)) +
  geom_point(aes(size = meanSppAbun), alpha = 0.4, position = jitter) +
  scale_size_continuous(name = "Mean Species Abundance", limits = c(1, 24), breaks = c(1, 12, 24), range = c(2, 9)) +
  scale_colour_manual(name = "Susceptibility",values = c("#8bd3c7", "#f8ae5d", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible")) +
  scale_x_discrete(limits = factor(c(0:400)), breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 
                                                         225, 250, 275, 300, 325, 350, 375, 400),
                   labels = c("0", "", "", "", "100", "", "", "", "200", "", "", "", "300", "", "", "", "400")) +
  ylab("Species") +
  xlab("Site #") + guides(color = guide_legend(override.aes = list(size = 10))) + # increase size of 'susceptibility' legend icons
  ak_theme + theme(plot.margin = margin(2, 16, 2, 2, "pt")) 
#  labs(caption = c("Abundance of individual species at each site. Outliers (*e.g.*, instances of a single mass die-off) were<br>removed to better highlight data. Size of each point indicates the site abundance (*i.e.*, how many<br>species total there are at a site for  all sampling occurrences). Color indicates the susceptibility level<br>of each species: Resistant (blue) = No to low infection and no clinical signs of disease;<br> Tolerant (orange) = low infection loads with low or variable mortality; and Susceptible (red) = High<br> infection loads resulting in consistently high mortality."))

abun

ggsave("sppAbunxSite.tif", abun, device = "tiff", scale = 2, width = 2600, height = 1200, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

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
dcbind_alpestris <- tidyr::drop_na(dcbind_alpestris, any_of(c(25:30)))

# Scale relevant vars
dcbindScaled <- dcbind_alpestris %>%
  mutate_at(c("temp_date", "temp_date_t1", "temp_date_t2",
              "soilMoisture_date", "soilMoisture_date_t1", "soilMoisture_date_t2"), 
            ~(scale(., center = T, scale = T %>% as.numeric)))


# T0
m2_t0 <- glmmTMB(cbind(nPos_all, nNeg_all) ~  richness*logsiteAbun + temp_date*soilMoisture_date + (1|scientific),
                 data = dcbindScaled, family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m2_t0)
Anova(m2_t0)


# T-1
m2_t1 <- glmmTMB(cbind(nPos_all, nNeg_all) ~  richness*logsiteAbun + temp_date_t1*soilMoisture_date_t1 + (1|scientific),
                 data = dcbindScaled, family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m2_t1)
Anova(m2_t1)



# T-2
m2_t2 <- glmmTMB(cbind(nPos_all, nNeg_all) ~  richness*logsiteAbun + temp_date_t2*soilMoisture_date_t2 + (1|scientific),
                 data = dcbindScaled, family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m2_t2)
Anova(m2_t2)



##      2b. Prevalence by Abundance & Richness Plots for T0, T-1, T-2 
# T0
m2_t0_rich <- ggpredict(m2_t0,  terms = c("richness", "logsiteAbun"))
m2_t0_rich$dummy <- as.factor(round(exp(as.numeric(m2_t0_rich$group)), 0))

m2_t0_p1 <- ggplot(m2_t0_rich, aes(x = x , y = predicted)) +
  geom_line(aes(linetype = dummy), linewidth = 1) +
  labs(title = bquote(paste(italic(t))[(0~days)]), linetype = "Site-level abundance") +
#  geom_ribbon(aes(ymin = conf.low, ymax = conf.high)) +
  ylab("Predicted Bsal prevalence across all salamander species") +
  xlab("Species richness") + 
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0, 0.017)) + 
  ak_theme + theme(plot.tag.position = c(0.9, 0.9))

m2_t0_p1 
ggsave("m2_t0_AbunRich.tif", m2_t0_p1, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)



# T-1
m2_t1_rich <- ggpredict(m2_t1,  terms = c("richness", "logsiteAbun"))
m2_t1_rich$dummy <- as.factor(round(exp(as.numeric(m2_t1_rich$group)), 0))

m2_t1_p1 <- ggplot(m2_t1_rich, aes(x = x , y = predicted)) +
  geom_line(aes(linetype = dummy), size = 1) +
  labs(title = bquote(paste(italic(t))[(-30~days)]), linetype = "Site-level abundance") +
  #  geom_ribbon(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal prevalence across all salamander species") +
  xlab("Species richness") + 
  guides(fill = guide_legend(title = "Site-Level abundance")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0, 0.017)) + 
  ak_theme + theme(plot.tag.position = c(0.9, 0.9))

m2_t1_p1 
ggsave("m2_t1_AbunRich.tif", m2_t1_p1, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

# T-2
m2_t2_rich <- ggpredict(m2_t2,  terms = c("richness", "logsiteAbun"))
m2_t2_rich$dummy <- as.factor(round(exp(as.numeric(m2_t2_rich$group)), 0))

m2_t2_p1 <- ggplot(m2_t2_rich, aes(x = x , y = predicted)) +
  geom_line(aes(linetype = dummy), size = 1) +
  labs(title = bquote(paste(italic(t))[(-60~days)]), linetype = "Site-level abundance") +
  #  geom_ribbon(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal prevalence across all salamander species") +
  xlab("Species richness") + 
  guides(fill = guide_legend(title = "Site-Level Abundance")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0, 0.017)) + 
  ak_theme + theme(plot.tag.position = c(0.9, 0.9))

m2_t2_p1 
ggsave("m2_t2_AbunRich.tif", m2_t2_p1, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

# 3 Panel Graph
m2_p1_combined <- ggarrange(m2_t2_p1 + labs(tag = "A") + rremove("xlab"), 
                            m2_t1_p1 + labs(tag = "B") +  rremove("ylab"), 
                            m2_t0_p1 + labs(tag = "C") + rremove("ylab") + rremove("xlab"), 
                            labels = NULL,
                            ncol = 3, nrow = 1,
                            common.legend = TRUE, legend = "bottom",
                            align = "hv", 
                            font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

annotate_figure(m2_p1_combined, bottom = textGrob("Timepoints above each graph indicate the time from the initial observation.", x = 0, 
                                                  hjust = 0, gp = gpar(fontsize = 12)))


ggsave("model2_AbunRich_combined.tif", m2_p1_combined, device = "tiff", scale = 2, width = 2600, height = 1200, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

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
m2_t0_w_unscaled$temp_date <- m2_t0_weather$temp_date * as.numeric(attr(dcbindScaled$temp_date, "scaled:scale")) +
  as.numeric(attr(dcbindScaled$temp_date, "scaled:center"))
m2_t0_w_unscaled$soilMoisture_date <- m2_t0_weather$soilMoisture * as.numeric(attr(dcbindScaled$soilMoisture_date,
                                                                                   "scaled:scale")) +
  as.numeric(attr(dcbindScaled$soilMoisture_date, "scaled:center"))
m2_t0_w_unscaled$soilMoisture_date <- round(m2_t0_w_unscaled$soilMoisture_date, 2)

# Create dummy column for soil moisture labels
m2_t0_w_unscaled$dummy <- NA
for(i in 1:nrow(m2_t0_w_unscaled)){
    if(m2_t0_w_unscaled[i,6] == 5.38){
      m2_t0_w_unscaled[i,7] <- "Low"
    }
  else if(m2_t0_w_unscaled[i,6] == 6.11){
    m2_t0_w_unscaled[i,7] <- "Med"
  }
  else if(m2_t0_w_unscaled[i,6] == 6.84){
    m2_t0_w_unscaled[i,7] <- "High"
  }
}


m2_t0_p2 <- ggplot(m2_t0_w_unscaled, aes(x = temp_date , y = predicted)) +
  geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), size = 1) +
  labs(title =  bquote(paste(italic(t))[(0~days)]), linetype = bquote("Soil moisture")) +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal prevalence\nacross all salamander species") +
  xlab(expression("Temperature (°C)")) + 
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.2)) + 
  ak_theme + theme(plot.tag.position = c(0.9, 0.9))

m2_t0_p2 

ggsave("m2_t0_weather.tif", m2_t0_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)


# T-1
m2_t1_weather <- ggpredict(m2_t1, terms = c("temp_date_t1 [all]", "soilMoisture_date_t1"))
m2_t1_weather <- m2_t1_weather %>%
  rename("temp_date_t1" = "x",
         "soilMoisture_date_t1" = "group") %>%
  mutate(temp_date_t1 = as.numeric(as.character(temp_date_t1)),
         soilMoisture_date_t1 = as.numeric(as.character(soilMoisture_date_t1)))

# Convert scaled prediction to original data scale:
m2_t1_w_unscaled <- m2_t1_weather
m2_t1_w_unscaled$temp_date_t1 <- m2_t1_weather$temp_date_t1 * as.numeric(attr(dcbindScaled$temp_date_t1, "scaled:scale")) +
  as.numeric(attr(dcbindScaled$temp_date_t1, "scaled:center"))
m2_t1_w_unscaled$soilMoisture_date_t1 <- m2_t1_weather$soilMoisture * as.numeric(attr(dcbindScaled$soilMoisture_date_t1,
                                                                                      "scaled:scale")) +
  as.numeric(attr(dcbindScaled$soilMoisture_date_t1, "scaled:center"))
m2_t1_w_unscaled$soilMoisture_date_t1 <- round(m2_t1_w_unscaled$soilMoisture_date_t1, 2)

# Create dummy column for soil moisture labels
m2_t1_w_unscaled$dummy <- NA
for(i in 1:nrow(m2_t1_w_unscaled)){
  if(m2_t1_w_unscaled[i,6] == 5.39){
    m2_t1_w_unscaled[i,7] <- "Low"
  }
  else if(m2_t1_w_unscaled[i,6] == 6.13){
    m2_t1_w_unscaled[i,7] <- "Med"
  }
  else if(m2_t1_w_unscaled[i,6] == 6.87){
    m2_t1_w_unscaled[i,7] <- "High"
  }
}

m2_t1_p2 <- ggplot(m2_t1_w_unscaled, aes(x = temp_date_t1 , y = predicted)) +
  geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), size = 1) +
  labs(title = bquote(paste(italic(t))[(-30~days)]), linetype = bquote("Soil moisture")) +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal prevalence\nacross all salamander species") +
  xlab(expression("Temperature (°C)")) + 
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.2)) + 
  ak_theme + theme(plot.tag.position = c(0.9, 0.9))

m2_t1_p2 
ggsave("m2_t1_weather.tif", m2_t1_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)


# T-2
m2_t2_weather <- ggpredict(m2_t2, terms = c("temp_date_t2 [all]", "soilMoisture_date_t2"))
m2_t2_weather <- m2_t2_weather %>%
  rename("temp_date_t2" = "x",
         "soilMoisture_date_t2" = "group") %>%
  mutate(temp_date_t2 = as.numeric(as.character(temp_date_t2)),
         soilMoisture_date_t2 = as.numeric(as.character(soilMoisture_date_t2)))

# Convert scaled prediction to original data scale:
m2_t2_w_unscaled <- m2_t2_weather
m2_t2_w_unscaled$temp_date_t2 <- m2_t2_weather$temp_date_t2 * as.numeric(attr(dcbindScaled$temp_date_t2, "scaled:scale")) +
  as.numeric(attr(dcbindScaled$temp_date_t2, "scaled:center"))
m2_t2_w_unscaled$soilMoisture_date_t2 <- m2_t2_weather$soilMoisture * as.numeric(attr(dcbindScaled$soilMoisture_date_t2,
                                                                                      "scaled:scale")) +
  as.numeric(attr(dcbindScaled$soilMoisture_date_t2, "scaled:center"))
m2_t2_w_unscaled$soilMoisture_date_t2 <- round(m2_t2_w_unscaled$soilMoisture_date_t2, 2)

# Create dummy column for soil moisture labels
m2_t2_w_unscaled$dummy <- NA
for(i in 1:nrow(m2_t2_w_unscaled)){
  if(m2_t2_w_unscaled[i,6] == 5.36){
    m2_t2_w_unscaled[i,7] <- "Low"
  }
  else if(m2_t2_w_unscaled[i,6] == 6.14){
    m2_t2_w_unscaled[i,7] <- "Med"
  }
  else if(m2_t2_w_unscaled[i,6] == 6.92){
    m2_t2_w_unscaled[i,7] <- "High"
  }
}


m2_t2_p2 <- ggplot(m2_t2_w_unscaled, aes(x = temp_date_t2 , y = predicted)) +
  geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), size = 1) +
  labs(title = bquote(paste(italic(t))[(-60~days)]), linetype = bquote("Soil moisture")) +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal prevalence across all salamander species") +
  xlab(expression("Temperature (°C)")) + 
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.2)) + 
  ak_theme + theme(plot.tag.position = c(0.9, 0.9))

m2_t2_p2 
ggsave("m2_t2_weather.tif", m2_t2_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

# 3 Panel Graph
m2_p2_combined <- ggarrange((m2_t2_p2 + labs(tag = "A") + rremove("xlab")),
                            (m2_t1_p2 + labs(tag = "B") + rremove("ylab")), 
                            (m2_t0_p2 + labs(tag = "C") + rremove("ylab") + rremove("xlab")), 
                            labels = NULL,
                            ncol = 3, nrow = 1,
                            common.legend = TRUE, legend = "bottom",
                            align = "hv", 
                            font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))


annotate_figure(m2_p2_combined, bottom = textGrob(bquote("Soil moisture ranged from 5.36-5.39 (kg/m"^2~"), 6.11-6.14 (kg/m"^2~"), and 6.84-6.92 (kg/m"^2~"), for the Low, Medium, and High categories respectively. Timepoints above each graph indicate the time from the initial observation."), x = 0, 
                                                  hjust = 0, gp = gpar(fontsize = 12)))

ggsave("m2_weather_combined.tif", m2_p2_combined, device = "tiff", scale = 2, width = 2600, height = 1200, units = "px", 
       path = file.path(dir, figpath), dpi = 300)


#### 3. Cbind models for fire salamanders only ####################################################
##      3a. T0 (At time of observation); T-1 (30 days prior to initial obs.); T-2 (60 days prior to initial obs.)
# T0
m3_t0 <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~  richness*logsiteAbun + temp_date*soilMoisture_date + (1|scientific),
                 data = subset(dcbindScaled, scientific =="Salamandra salamandra"), family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m3_t0)
Anova(m3_t0)


# T-1
m3_t1 <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~  richness*logsiteAbun + temp_date_t1*soilMoisture_date_t1 + (1|scientific),
                 data = subset(dcbindScaled, scientific =="Salamandra salamandra"), family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m3_t1)
Anova(m3_t1)



# T-2
m3_t2 <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~  richness*logsiteAbun + temp_date_t2*soilMoisture_date_t2 + (1|scientific),
                 data = subset(dcbindScaled, scientific =="Salamandra salamandra"), family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m3_t2)
Anova(m3_t2)



##      3b. Prevalence by Abundance & Richness Plots for T0, T-1, T-2 (Fire Salamanders Only)
# T0
m3_t0_rich <- ggpredict(m3_t0,  terms = c("richness", "logsiteAbun"))
m3_t0_rich$dummy <- as.factor(round(exp(as.numeric(m3_t0_rich$group)), 0))

m3_t0_p1 <- ggplot(m3_t0_rich, aes(x = x , y = predicted)) +
  geom_line(aes(linetype = dummy), size = 1) +
  labs(title =  bquote(paste(italic(t))[(0~days)]), linetype = "Site-level abundance") +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal prevalence in fire salamanders") +
  xlab("Species richness") + 
  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.65)) + 
  ak_theme + theme(plot.tag.position = c(0.9, 0.9))

m3_t0_p1 
ggsave("m3_t0_AbunRich.tif", m3_t0_p1, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

# T-1
m3_t1_rich <- ggpredict(m3_t1,  terms = c("richness", "logsiteAbun"))
m3_t1_rich$dummy <- as.factor(round(exp(as.numeric(m3_t1_rich$group)), 0))


m3_t1_p1 <- ggplot(m3_t1_rich, aes(x = x , y = predicted)) +
  geom_line(aes(linetype = dummy), size = 1) +
  labs(title = bquote(paste(italic(t))[(-30~days)]), linetype = "Site-level abundance") +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal prevalence in fire salamanders") +
  xlab("Species richness") + 
  guides(fill = guide_legend(title = "ln(Site-Level Abundance)")) +
  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.65)) + 
  ak_theme + theme(plot.tag.position = c(0.9, 0.9))

m3_t1_p1 
ggsave("m3_t1_AbunRich.tif", m3_t1_p1, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

# T-2
m3_t2_rich <- ggpredict(m3_t2,  terms = c("richness", "logsiteAbun"))
m3_t2_rich$dummy <- as.factor(round(exp(as.numeric(m3_t2_rich$group)), 0))


m3_t2_p1 <- ggplot(m3_t2_rich, aes(x = x , y = predicted)) +
  geom_line(aes(linetype = group), size = 1) +
  labs(title = bquote(paste(italic(t))[(-60~days)]), linetype = "Site-level abundance") +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal prevalence in fire salamanders") +
  xlab("Species richness") + 
  guides(fill = guide_legend(title = "ln(Site-Level Abundance)")) +
  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.65)) + 
  ak_theme + theme(plot.tag.position = c(0.9, 0.9))

m3_t2_p1 
ggsave("m3_t1_AbunRich.tif", m3_t2_p1, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

# 3 Panel Graph
m3_p1_combined <- ggarrange(m3_t2_p1 + labs(tag = "A") + rremove("xlab"), 
                            m3_t1_p1 + labs(tag = "B") + rremove("ylab"), 
                            m3_t0_p1 + labs(tag = "C") + rremove("ylab") + rremove("xlab"), 
                            labels = NULL,
                            ncol = 3, nrow = 1,
                            common.legend = TRUE, legend = "bottom",
                            align = "hv", 
                            font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

m3_p1_combined
ggsave("m3_AbunRich_combined.tif", m3_p1_combined, device = "tiff", scale = 2, width = 2600, height = 1200, units = "px", 
       path = file.path(dir, figpath), dpi = 300)


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
m3_t0_w_unscaled$temp_date <- m3_t0_weather$temp_date * as.numeric(attr(dcbindScaled$temp_date, "scaled:scale")) +
  as.numeric(attr(dcbindScaled$temp_date, "scaled:center"))
m3_t0_w_unscaled$soilMoisture_date <- m3_t0_weather$soilMoisture * as.numeric(attr(dcbindScaled$soilMoisture_date,
                                                                                   "scaled:scale")) +
  as.numeric(attr(dcbindScaled$soilMoisture_date, "scaled:center"))
m3_t0_w_unscaled$soilMoisture_date <- round(m3_t0_w_unscaled$soilMoisture_date, 2)


# Create dummy column for soil moisture labels
m3_t0_w_unscaled$dummy <- NA
for(i in 1:nrow(m3_t0_w_unscaled)){
  if(m3_t0_w_unscaled[i,6] == 5.53){
    m3_t0_w_unscaled[i,7] <- "Low"
  }
  else if(m3_t0_w_unscaled[i,6] == 6.22){
    m3_t0_w_unscaled[i,7] <- "Med"
  }
  else if(m3_t0_w_unscaled[i,6] == 6.91){
    m3_t0_w_unscaled[i,7] <- "High"
  }
}

m3_t0_p2 <- ggplot(m3_t0_w_unscaled, aes(x = temp_date , y = predicted)) +
  geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), size = 1) +
  labs(title = bquote(paste(italic(t))[(0~days)]), linetype = bquote("Soil moisture")) +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal Prevalence in Fire Salamanders") +
  xlab(expression("Temperature (°C)")) + 
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.7)) + 
  ak_theme + theme(plot.tag.position = c(0.9, 0.9))

m3_t0_p2 
ggsave("m3_t0_weather.tif", m3_t0_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

# T-1
m3_t1_weather <- ggpredict(m3_t1, terms = c("temp_date_t1 [all]", "soilMoisture_date_t1"))
m3_t1_weather <- m3_t1_weather %>%
  rename("temp_date_t1" = "x",
         "soilMoisture_date_t1" = "group") %>%
  mutate(temp_date_t1 = as.numeric(as.character(temp_date_t1)),
         soilMoisture_date_t1 = as.numeric(as.character(soilMoisture_date_t1)))

# Convert scaled prediction to original data scale:
m3_t1_w_unscaled <- m3_t1_weather
m3_t1_w_unscaled$temp_date_t1 <- m3_t1_weather$temp_date_t1 * as.numeric(attr(dcbindScaled$temp_date_t1, "scaled:scale")) +
  as.numeric(attr(dcbindScaled$temp_date_t1, "scaled:center"))
m3_t1_w_unscaled$soilMoisture_date_t1 <- m3_t1_weather$soilMoisture * as.numeric(attr(dcbindScaled$soilMoisture_date_t1,
                                                                                      "scaled:scale")) +
  as.numeric(attr(dcbindScaled$soilMoisture_date_t1, "scaled:center"))
m3_t1_w_unscaled$soilMoisture_date_t1 <- round(m3_t1_w_unscaled$soilMoisture_date_t1, 2)

# Create dummy column for soil moisture labels
m3_t1_w_unscaled$dummy <- NA
for(i in 1:nrow(m3_t1_w_unscaled)){
  if(m3_t1_w_unscaled[i,6] == 5.52){
    m3_t1_w_unscaled[i,7] <- "Low"
  }
  else if(m3_t1_w_unscaled[i,6] == 6.21){
    m3_t1_w_unscaled[i,7] <- "Med"
  }
  else if(m3_t1_w_unscaled[i,6] == 6.89){
    m3_t1_w_unscaled[i,7] <- "High"
  }
}


m3_t1_p2 <- ggplot(m3_t1_w_unscaled, aes(x = temp_date_t1, y = predicted)) +
  geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), size = 1) +
  labs(title = bquote(paste(italic(t))[(-30~days)]), linetype = bquote("Soil moisture")) +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal prevalence in fire salamanders") +
  xlab(expression("Temperature (°C)")) + 
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.7)) + 
  ak_theme + theme(plot.tag.position = c(0.9, 0.9))

m3_t1_p2 

ggsave("m3_t1_weather.tif", m3_t1_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

# T-2
m3_t2_weather <- ggpredict(m3_t2, terms = c("temp_date_t2 [all]", "soilMoisture_date_t2"))
m3_t2_weather <- m3_t2_weather %>%
  rename("temp_date_t2" = "x",
         "soilMoisture_date_t2" = "group") %>%
  mutate(temp_date_t2 = as.numeric(as.character(temp_date_t2)),
         soilMoisture_date_t2 = as.numeric(as.character(soilMoisture_date_t2)))

# Convert scaled prediction to original data scale:
m3_t2_w_unscaled <- m3_t2_weather
m3_t2_w_unscaled$temp_date_t2 <- m3_t2_weather$temp_date_t2 * as.numeric(attr(dcbindScaled$temp_date_t2, "scaled:scale")) +
  as.numeric(attr(dcbindScaled$temp_date_t2, "scaled:center"))
m3_t2_w_unscaled$soilMoisture_date_t2 <- m3_t2_weather$soilMoisture * as.numeric(attr(dcbindScaled$soilMoisture_date_t2,
                                                                                      "scaled:scale")) +
  as.numeric(attr(dcbindScaled$soilMoisture_date_t2, "scaled:center"))
m3_t2_w_unscaled$soilMoisture_date_t2 <- round(m3_t2_w_unscaled$soilMoisture_date_t2, 2)


m3_t2_w_unscaled$dummy <- NA
for(i in 1:nrow(m3_t2_w_unscaled)){
  if(m3_t2_w_unscaled[i,6] == 5.46){
    m3_t2_w_unscaled[i,7] <- "Low"
  }
  else if(m3_t2_w_unscaled[i,6] == 6.18){
    m3_t2_w_unscaled[i,7] <- "Med"
  }
  else if(m3_t2_w_unscaled[i,6] == 6.90){
    m3_t2_w_unscaled[i,7] <- "High"
  }
}

m3_t2_p2 <- ggplot(m3_t2_w_unscaled, aes(x = temp_date_t2, y = predicted)) +
  geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), size = 1) +
  labs(title = bquote(paste(italic(t))[(-60~days)]), linetype = bquote("Soil moisture")) +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted Bsal prevalence in fire salamanders") +
  xlab(expression("Temperature (°C)")) + 
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.7)) + 
  ak_theme + theme(plot.tag.position = c(0.9, 0.9))

m3_t2_p2 
ggsave("m3_t2_weather.tif", m3_t2_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

# 3 Panel Graph
m3_p2_combined <- ggarrange((m3_t2_p2 + labs(tag = "A") + rremove("xlab")),
                            (m3_t1_p2 + labs(tag = "B") + rremove("ylab")), 
                            (m3_t0_p2 + labs(tag = "C") + rremove("ylab") + rremove("xlab")), 
                            labels = NULL,
                            ncol = 3, nrow = 1,
                            common.legend = TRUE, legend = "bottom",
                            align = "hv", 
                            font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))


annotate_figure(m3_p2_combined, bottom = textGrob(bquote("Soil moisture ranged from 5.46-5.53 (kg/m"^2~"), 6.18-6.22 (kg/m"^2~"), and 6.89-6.91 (kg/m"^2~"), for the Low, Medium, and High categories respectively. Timepoints above each graph indicate the time from the initial observation."), x = 0, 
                                                  hjust = 0, gp = gpar(fontsize = 12)))


ggsave("m3_weather_combined.tif", m3_p2_combined, device = "tiff", scale = 2, width = 2600, height = 1200, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

#### 4. Fatality models ####################################################
# Remove any instances of NA within the fatal column & weather vars columns, as well as I. alpestris
d_fatal <- d_prev %>%
  tidyr::drop_na(any_of(c(31, 40:45))) %>%
  subset(scientific != "Ichthyosaura alpestris")

# Scale relevant vars
d_fatal <- d_fatal %>%
  mutate_at(c("tavg", "tmin", "tmax", "prec", "bio1", "bio12"), 
            ~(scale(., center = T, scale = T %>% as.numeric)))
# bio1 == annual mean temp
# bio12 == annual precip

##      4a.  Fatality of fire salamanders given an interaction between average monthly temperature (tavg) and if Bsal has ever been detected at a site. 
# including Jaime's data
m4_tavg <- glmmTMB(fatal ~ tavg*prev_above_0 + (1|Site),
                 family = "binomial",
                 data = filter(d_fatal, scientific == "Salamandra salamandra"),
                 control = glmmTMBControl(optimizer = optim,
                                          optArgs = list(method = "BFGS")))

summary(m4_tavg)
Anova(m4_tavg)

# excluding Jaime's data
m4_tavg_noJB <- glmmTMB(fatal ~ tavg*prev_above_0 + (1|Site),
                   family = "binomial",
                   data = filter(d_fatal, scientific == "Salamandra salamandra" & collectorList != "Jaime Bosch"),
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))

summary(m4_tavg_noJB)
Anova(m4_tavg_noJB)


##      4b. Plots
# tavg - all data
m4_tavg_predict <- ggpredict(m4_tavg, terms = c("tavg [all]", "prev_above_0"))
m4_tavg_predict <- m4_tavg_predict %>%
  rename("tavg" = "x",
         "BsalDetected" = "group") %>%
  mutate(tavg = as.numeric(as.character(tavg)),
         BsalDetected = ifelse(BsalDetected == 0, "No", "Yes"))

# Convert scaled prediction to original data scale:
m4_tavg_unscaled <- m4_tavg_predict
m4_tavg_unscaled$tavg <- m4_tavg_predict$tavg * as.numeric(attr(d_fatal$tavg, "scaled:scale")) +
  as.numeric(attr(d_fatal$tavg, "scaled:center"))

m4_tavg_plot <- ggplot(m4_tavg_unscaled, aes(x = tavg , y = predicted)) +
  geom_line(aes(linetype = as.factor(BsalDetected))) +
  labs(title = bquote(paste("Average monthly temperature")), linetype = "Bsal Positive") +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted fire salamander mortality (%)") +
  xlab(expression("Temperature (°C)")) + 
  scale_x_continuous(labels = seq(0, 20, 5), breaks = seq(0, 20, 5), limits = c(0, 20)) +  
  scale_y_continuous(breaks = seq(0, .015, .005),
                     labels = scales::percent,
                     limits = c(0, .015)) +
  ak_theme + theme(plot.tag.position = c(0.9, 0.9))


m4_tavg_plot
ggsave("m4_fatality.tif", m4_tavg_plot, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)



# tavg_noJB - excluding Jaime's data
m4_tavg_noJB_predict <- ggpredict(m4_tavg_noJB, terms = c("tavg [all]", "prev_above_0"))
m4_tavg_noJB_predict <- m4_tavg_noJB_predict %>%
  rename("tavg" = "x",
         "BsalDetected" = "group") %>%
  mutate(tavg = as.numeric(as.character(tavg)),
         BsalDetected = ifelse(BsalDetected == 0, "No", "Yes"))

# Convert scaled prediction to original data scale:
m4_tavg_noJB_unscaled <- m4_tavg_noJB_predict
m4_tavg_noJB_unscaled$tavg <- m4_tavg_noJB_predict$tavg * as.numeric(attr(d_fatal$tavg, "scaled:scale")) +
  as.numeric(attr(d_fatal$tavg, "scaled:center"))

m4_tavg_noJB_plot <- ggplot(m4_tavg_noJB_unscaled, aes(x = tavg, y = predicted)) +
  geom_line(aes(linetype = as.factor(BsalDetected))) +
  labs(title = bquote(paste("Average monthly temperature")), linetype = "Bsal Positive") +
  #  geom_errorbar(aes(ymin = exp(conf.low), ymax = (conf.high))) +
  ylab("Predicted fire salamander mortality (%)") +
  xlab(expression("Temperature (°C)")) + 
  scale_x_continuous(labels = seq(-10, 10, 5), breaks = seq(-10, 10, 5), limits = c(-10, 10)) +  
  scale_y_continuous(breaks = seq(0, .015, .005),
                     labels = scales::percent,
                     limits = c(0, .015)) +
  ak_theme + theme(plot.tag.position = c(0.9, 0.9)) +
  labs(caption = c("The figure depicts the plot output for the same fatality model that the other figure describes, excluding Jaime Bosch's data."))
  

m4_tavg_noJB_plot

ggsave("m4_fatality_noJB.tif", m4_tavg_noJB_plot, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)




