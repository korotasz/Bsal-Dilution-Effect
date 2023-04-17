#remotes::install_version("Rttf2pt1", version = "1.3.8") # install this version, latest ver. not compatible
#remotes::install_github("gorkang/html2latex") # convert sjPlot::tab_model() hmtl table to tex and pdf in .Rmd docs
#extrafont::font_import() # load fonts before ggplot2; only need to do this once
require(pacman)
require(rstudioapi) # Set working directory to current file location
require(extrafont) 
extrafont::loadfonts(device = "win", quiet = T) # plot fonts 

#### Visualization Packages ####
pckgs <- c("ggsignif", # adds labels to significant groups
             "ggtext", # for text type/arrangements w/ ggplot2
           "Rttf2pt1", # to use with the extrafont package
           "ggthemes", # contains 'scales', 'themes', and 'geoms' packages
            "cowplot", # arranging plots/figs
          "gridExtra", # arranging plots/figs
          "patchwork", # arranging plots/figs
         "hrbrthemes", # plot colors
       "RColorBrewer", # plot colors
            "viridis", # plot colors
           "eurostat", # obtain spatial data from europe
            "geodata", # obtain geographic data (world map)
              "ggmap", # creates maps
          "ggspatial", # north arrow and scale bar
            "mapproj", # apply map projection
         "scatterpie", # add pie charts to maps
             "ggpubr", # prepares plots to be ready for publication
             "sjPlot", # plot_model(), tab_model()
          "htmltools", # visualizes model outputs as html tables
            "webshot", # converts .html object to .jpg
          "phantomjs", # dependency of webshot
#### Analysis Specific Packages ####
          "tidyverse", # data wrangling/manipulation
            "glmmTMB", # glmmTMB()
            "emmeans", # lsmeans()
                "car", # Anova()
             "DHARMa", # simulateResiduals(), testZeroInflation(), testDispersion() 
              "MuMIn", # model.sel()
          "ggeffects", # ggpredict()
               "epiR", # calculate prevalence & CIs
             "sjmisc" # data and variable transformation
)

## Load packages
pacman::p_load(pckgs, update = TRUE, character.only = T)


## Set working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
csvpath <- (path.expand("/csvFiles"))
figpath <- (path.expand("/figures"))
setwd(file.path(dir, csvpath))

## Set plot theme 
ak_theme <- theme_ipsum() +
  theme(axis.text.x = element_text(size = 24),
        axis.title.x = element_text(size = 28, hjust = 0.5, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0), 
                                    face = "plain"),
        axis.text.y = element_text(size = 24),
        axis.title.y = element_text(size = 28, hjust = 0.5, 
                                    margin = margin(t = 0, r = 15, b = 0, l = 5), 
                                    face = "plain"),
        axis.ticks.length = unit(.25, "cm"),
        axis.ticks = element_blank(),
        plot.tag = element_text(size = 32, face = "plain"),
        plot.title = element_text(size = 32, hjust = 0.5, face = "plain"),
        plot.subtitle = element_markdown(size = 12, face = "plain"),
        plot.margin = margin(6, 12, 2, 2, "pt"), 
        plot.caption = element_markdown(hjust = 0, size = 14, face = "plain"),
        plot.caption.position = "plot",
        legend.position = "bottom", 
#        legend.spacing = unit(1, "cm"), # Space legend labels
        legend.key.size = unit(2,"cm"), 
        legend.text.align = 1,
        legend.text = element_text(size = 18, hjust = -1),
        legend.title = element_text(size = 18, face = "bold"), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.spacing.y = unit(1.5,"cm"),
        strip.text = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.line = element_line(color = 'black'))


#### Load csv files ####
d <- read.csv("bsalData_clean.csv", header = T, encoding = "UTF-8")
dcbind <- read.csv("bsalData_cbind.csv", header = T, encoding = "UTF-8")



# log transform + scale vars
d <- d %>%
  mutate(logsppAbun = log(sppAbun),
         logsiteAbun = log(siteAbun),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
         relocate(c(logsppAbun, logsiteAbun), .after = sppAbun) %>%
         filter(scientific != "Ichthyosaura alpestris") # invasive 


dcbind <- dcbind %>%
  mutate(logsppAbun = log(sppAbun),
         logsiteAbun = log(siteAbun),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
         relocate(c(logsppAbun, logsiteAbun), .after = sppAbun) %>%
         filter(scientific != "Ichthyosaura alpestris") # invasive

## Vector of cleaner labels for model output table
nicelabs <- c(`(Intercept)` = "Intercept",
              richness = "Richness",
              logsiteAbun = "log(Site abundance)",
              "richness:logsiteAbun" = "Richness:log(Site abundance)",
              temp_d = "Temp (t0)", temp_m_t1 = "Temp (t-30)", temp_m_t2 = "Temp (t-60)",
              sMoist_d = "Soil moisture (t0)", sMoist_m_t1 = "Soil moisture (t-30)",
              sMoist_m_t2 = "Soil moisture (t-60)",
              "temp_d:sMoist_d" = "Temp (t0):Soil moisture (t0)",
              "temp_m_t1:sMoist_m_t1" = "Temp (t-30):Soil moisture (t-30)",
              "temp_m_t2:sMoist_m_t2" = "Temp (t-60):Soil moisture (t-60)")


## Function to populate dummy columns with uniform labels in ggpredict dataframe
create_dummy_col <- function(df){
  values <- c("Low", "Med", "High")
  keys <-  unique(df[,8])
  index <- setNames(as.list(values), keys)
  
  df$dummy <- dplyr::recode(df[,8], !!!index)
  
  return(df)
}


#### 1) Descriptive Figures ####################################################
##      1a. Species ~ Abundance
m1a <- glmmTMB(logsppAbun ~ scientific + (1|Site),
               data = d,
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))
summary(m1a)
Anova(m1a)


textcol <- d %>%
dplyr::select(scientific, susceptibility, Site) %>% # subset relevant data
mutate(Site = as.factor(Site)) %>%
group_by(scientific) %>%
mutate(No.Sites = length(unique(Site))) %>%
ungroup() %>%
dplyr::select(!Site) %>%
unique()


m1a_predict <- ggpredict(m1a, terms = "scientific") %>%
  rename("scientific" = "x",
         "expectedAbun" = "group") %>%
  mutate(predicted = exp(predicted),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high),
         expectedAbun = round(predicted,0)) %>%
  left_join(., textcol, by = "scientific") %>%
  relocate(susceptibility, .after = scientific) %>%
  mutate(susceptibility = as.factor(susceptibility))


## Alternative Yellow/orange -> #E37830
m1a_plot <- ggplot(m1a_predict, aes(scientific, predicted, colour = susceptibility)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.5) +
  geom_richtext(aes(y = (conf.high + 0.25), 
                    label = paste("n<sub>sites</sub>=", No.Sites)), #Site #s 
                vjust = 0.4, hjust = 0, alpha = 0.5, size = 5.5,
                label.size = NA, fontface = "bold", show.legend = F) +
  coord_flip(clip = "off") +
  ylab("Abundance") +
  xlab("Species") + 
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible")) +
  scale_y_continuous(labels = seq(0, 20, 5), 
                     breaks = seq(0, 20, 5), 
                     limits = c(0, 22)) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(1, 0.5))) +
  ak_theme + theme(axis.text.y = element_text(face = "italic")) 
#  labs(caption = c("Predicted species abundance at a given site and sampling 
#       occurrence. Error bars represent 95% confidence intervals and 'n'  
#       represents the predicted species abundance given by the model."))

m1a_plot
ggsave("plot1a_sppAbun.tif", m1a_plot, device = "tiff", scale = 2, 
       width = 2200, height = 1200, units = "px", 
       path = file.path(dir, figpath), dpi = 300)


##      1b. Prevalence ~ Species
prev <- d %>%
  group_by(scientific) %>%
  mutate(ncas_Bsal = sum(BsalDetected == 1), # number of pos. Bsal cases
         npop = sum(individualCount)) %>% # pop size (total # individuals/spp.)
  drop_na(date) %>%
  ungroup() %>%
  mutate(Bsal_prev = ncas_Bsal/npop) # prevalence as a proportion

# Use a matrix containing number of cases (ncas) and population size (npop) to 
# calculate the prevalence of disease in each population and its 95% confint
tmp <- as.matrix(cbind(prev$ncas_Bsal, prev$npop))
tmp <- epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, 
                design = 1, conf.level = 0.95) * 100
tmp <- tmp %>% 
  dplyr::rename(lowerCI = lower,
                upperCI = upper)

# Add back to dataframe
prev <- cbind(prev, tmp)
prev <- prev[sort.list(prev$est),]
prev <- prev %>%
  relocate(c(est, lowerCI, upperCI), .after = diseaseTested)

tmp <- prev %>%
  dplyr::select(Site, date, scientific, individualCount) # subset relevant data
tmp <- aggregate(individualCount ~ scientific, tmp, sum) # aggregate by Site, date, spp. & summarise
names(tmp)[names(tmp) == 'individualCount'] <- 'totalspp'

prev <- prev %>% 
  left_join(., tmp, by = "scientific") %>%
  relocate(totalspp, .after = individualCount) %>%
  dplyr::select(scientific, susceptibility, est, lowerCI, upperCI, totalspp) %>%
  unique()
  

# Descriptive plot showing disease prevalence values per species with a 95% ci
m1b_plot <- ggplot(prev, aes(scientific, est, col = susceptibility)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width = 0.5) +
  geom_richtext(aes(y = (upperCI + 0.5), label = paste("n<sub>obs</sub> =", totalspp)), 
             vjust = 0.4, hjust = 0, alpha = 0.5, size = 5.5, 
             label.size = NA, fontface = "bold", show.legend = F) +
  coord_flip(clip = "off") +
  ylab("Disease prevalence (%)") + 
  xlab("Species") + 
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible")) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(1, 0.5))) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, suffix = "%"), 
                     breaks = seq(0, 65, 20), limit = c(0, 70)) +
  ak_theme + theme(axis.text.y = element_text(face = "italic")) 
#  labs(caption = c("Descriptive plot showing Bsal prevalence (*i.e.*, the 
#       proportion of individuals that tested positive for Bsal) for each species 
#       as a percent. Error bars represent 95% confidence intervals and 'n' 
#       represents the total number observations of that species in the dataset."))

m1b_plot
ggsave("plot1b_prevalence.tif", m1b_plot, device = "tiff", scale = 2, 
       width = 2200, height = 1200, units = "px", 
       path = file.path(dir, figpath), dpi = 300)


p1abcombined <- ((m1b_plot + labs(caption = NULL) + 
                    theme(plot.tag.position = c(0.96, 0.95)) +
                    expand_limits(y = c(0, 70)))|
                 (m1a_plot + labs(caption = NULL) + 
                    theme(axis.text.y = element_blank(), 
                          axis.title.y = element_blank(),
                          plot.tag.position = c(0.93, 0.95)) +
                    expand_limits(y = c(0, 25)))) & 
                    theme(legend.position = "top")
p1abcombined <- p1abcombined + plot_layout(guides = "collect")
                

p1abcombined
ggsave("plots1ab_combined.tif", p1abcombined, device = "tiff", scale = 2, 
       width = 2600, height = 1200, units = "px", 
       path = file.path(dir, figpath), dpi = 300)


##      1c. Susceptibility ~ Abundance
## Model based
m1c <- glmmTMB(logsppAbun ~ susceptibility + (1|Site),
               data = d,
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))

summary(m1c)
Anova(m1c)

m1c_predict <- ggpredict(m1c, terms = c("susceptibility")) %>%
  rename("susceptibility" = "x",
         "group" = "group") %>%
  mutate(susceptibility = as.factor(susceptibility),
         predicted = exp(as.numeric(predicted)),
         conf.high = exp(as.numeric(conf.high)),
         conf.low = exp(as.numeric(conf.low)))

m1c_rug <- d %>%
  group_by(susceptibility) %>%
  summarise(n = n())

m1c_predict <- merge(m1c_predict, m1c_rug)


m1c_plot <- ggplot(m1c_predict, aes(susceptibility, predicted, color = susceptibility)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 1) +
  geom_richtext(aes(y = (conf.high + 0.75), label = paste0("n<sub>obs</sub>= ", n)), 
             alpha = 0.6, size = 6, label.size = NA, fontface = "bold", show.legend = F) +
  annotate("text", x = 3, y = 6.4, label = "***", size = 10, fontface = "bold", 
           colour = "#b30000") +
  scale_x_discrete(labels = c("Resistant", "Tolerant", "Susceptible")) +
  ylab("Species abundance") + 
  xlab("Susceptibility level") +
  scale_y_discrete(limits = factor(0:8), breaks = c("0", "2", "4", "6", "8")) +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible")) +
  ak_theme + theme(legend.position = "none")
 # + labs(caption = c("Resistant = No to low infection and no clinical signs of 
 # disease; Tolerant = low infection loads with low or variable mortality; and 
 # Susceptible = High<br>infection loads resulting in consistently high mortality. 
 # Error bars represent 95% confidence intervals and 'n' represents the number of 
 # animals from<br>the dataset in each susceptibility category. In total, there 
 # were 11 species classified as 'Resistant', 4 species classified as 'Tolerant',
 # and 4 species<br>classified as 'Susceptible."))

# Thus, given that rare species are lost from communities first, biodiversity 
# loss might increase disease risk in ecosystems with Bsal. On average, less 
# abundant species at a given site tend to be more resistant while more abundant 
# species tend to be susceptible.

m1c_plot
ggsave("plot1c_susceptibility.tif", m1c_plot, device = "tiff", scale = 2, 
       width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

p1combined <- (((m1a_plot + labs(caption = NULL) + 
                   theme(plot.tag.position = c(0.96, 0.85),
                         plot.margin = margin(.5, .75, .5, .75, "cm"),
                         axis.ticks.length = unit(.25, "cm"),
                         axis.ticks = element_blank(),
                         axis.title.y = element_text(margin = margin(r = -75)),
                         axis.title.x = element_text(size = 26),
                         axis.text.x = element_text(size = 20))) | 
                  (m1b_plot + labs(caption = NULL) + 
                     theme(axis.text.y = element_blank(), 
                           axis.title.y = element_blank(),
                           axis.title.x = element_text(size = 26),
                           axis.text.x = element_text(size = 20),
                           axis.ticks.length = unit(.25, "cm"),
                           axis.ticks = element_blank(),
                           plot.margin = margin(.5, .75, .5, .5, "cm"),
                           plot.tag.position = c(0.93, 0.85)))) +
                 plot_layout(guides = "collect") & theme(legend.position = "top")) / 
                 (m1c_plot + labs(caption = NULL) + 
                    theme(panel.border = element_rect(colour = "black", 
                                                      fill = NA, linewidth = 1),
                          plot.margin = margin(1, 2, .75, 0, "cm"), 
                          axis.ticks.length.y = unit(.25, "cm"),
                          axis.ticks = element_blank(),
                          axis.title.y = element_text(margin = margin(r = -450), 
                                                      size = 28),
                          axis.text.y = element_text(size = 22),
                          axis.title.x = element_text(size = 26),
                          axis.text.x = element_text(size = 22),
                          plot.tag.position = c(0.96, 0.95))) 

p1combined <- p1combined + plot_annotation(tag_levels = "A") + 
                           plot_layout(widths = c(1, 2), heights = c(1,1)) 


p1combined
ggsave("plots1abc_combined.tif", p1combined, device = "tiff", scale = 2, 
       width = 2600, height = 2300, units = "px", 
       path = file.path(dir, figpath), dpi = 300)
# 
# 
# ## Descriptive (no stats)
# # mean spp. abundance by site
# mean_sppAbun <- aggregate(individualCount ~ scientific+Site, d, mean)
# names(mean_sppAbun)[names(mean_sppAbun) == 'individualCount'] <- 'meanSppAbun'
# 
# 
# descriptive <-d %>%
#   group_by(Site, date, scientific, susceptibility, individualCount, richness, sppAbun, siteAbun) %>%
#   dplyr::summarize(count = n()) %>%
#   mutate(susceptibility = as.factor(susceptibility)) %>% 
#   left_join(mean_sppAbun, by = c("scientific", "Site")) %>%
#   mutate(meanSppAbun = round(meanSppAbun, )) %>%
#   arrange(Site) %>%
#   subset(individualCount != 208 & individualCount != 72)
# 
# # mean spp. abundance by site with susceptibility level noted
# jitter <- position_jitter(width = 0.2, height = 0.25)
# 
# abun <-ggplot(descriptive, aes(x = Site, y = scientific, size = meanSppAbun, colour = susceptibility)) +
#   geom_point(aes(size = meanSppAbun), alpha = 0.4, position = jitter) +
#   scale_size_continuous(name = "Mean Species Abundance", limits = c(1, 24), breaks = c(1, 12, 24), range = c(2, 9)) +
#   scale_colour_manual(name = "Susceptibility",values = c("#8bd3c7", "#f8ae5d", "#b30000"),
#                       labels = c("Resistant", "Tolerant", "Susceptible")) +
#   scale_x_discrete(limits = factor(c(0:400)), breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 
#                                                          225, 250, 275, 300, 325, 350, 375, 400),
#                    labels = c("0", "", "", "", "100", "", "", "", "200", "", "", "", "300", "", "", "", "400")) +
#   ylab("Species") +
#   xlab("Site #") + guides(color = guide_legend(override.aes = list(size = 10))) + # increase size of 'susceptibility' legend icons
#   ak_theme + theme(plot.margin = margin(2, 16, 2, 2, "pt"),
#                    axis.text.y = element_text(face = "italic")) 
# #  labs(caption = c("Abundance of individual species at each site. Outliers (*e.g.*, instances of a single mass die-off) were<br>removed to better highlight data. Size of each point indicates the site abundance (*i.e.*, how many<br>species total there are at a site for  all sampling occurrences). Color indicates the susceptibility level<br>of each species: Resistant (blue) = No to low infection and no clinical signs of disease;<br> Tolerant (orange) = low infection loads with low or variable mortality; and Susceptible (red) = High<br> infection loads resulting in consistently high mortality."))
# 
# abun
# ggsave("sppAbunxSite.tif", abun, device = "tiff", scale = 2, width = 2600, height = 1300, units = "px", 
#        path = file.path(dir, figpath), dpi = 300)
# 

##      1d. Maps
pivot_by_country <- function(data) {
  
  s1 = reshape2::melt(data, id = c("country", "BsalDetected"), 
                      measure.vars = "individualCount")
  s2 = reshape2::dcast(s1, country ~ BsalDetected, sum)
  
  s2$Total = rowSums(s2[,2:NCOL(s2)])
  return(s2)
}


# obtain world map
worldmap <- map_data("world")

obs <- d %>%
  dplyr::select(country, ADM0, decimalLatitude, decimalLongitude, diseaseTested,
                BsalDetected, BdDetected, individualCount)

pivotted_data <- obs %>%
  pivot_by_country() %>%
  dplyr::rename("Bsal positive" = "1","Bsal negative" = "0")

# Getting the coordinates of each country
country_lookup <- read.csv("countries.csv", stringsAsFactors = F)
names(country_lookup)[1] <- "country_code"

# Combining data
final_data <- merge(x = pivotted_data, y = country_lookup, 
                    by.x = "country", by.y = "name", all.x = T)

# Data cleaning for plotting
final_data <- unique(final_data)

map <- ggplot(worldmap) +
  geom_map(data = worldmap, map = worldmap,
           aes(x =long, y = lat, map_id = region),
           col = "white", fill = "#B2BEB5") +
  scale_x_continuous(limits = c(-13, 15)) +
  scale_y_continuous(limits = c(35, 60)) +
  geom_scatterpie(data = final_data, aes(x = longitude, y = latitude, 
                                         group = country, r = 1),
                  cols = colnames(final_data[,c(2:3)])) +
  coord_fixed() +
  scale_fill_manual(values = c("gray40", "#C23113")) +
  geom_text(aes(x=longitude, y=latitude, group = country, label = country), 
            data = final_data, stat = "identity",
            hjust = 1.5, vjust = -1.5, size = 5.5,
            check_overlap = TRUE, na.rm = FALSE, show.legend = NA,
            inherit.aes = TRUE) +
  labs(x = "Longitude", y = "Latitude") +
  ak_theme + theme(legend.title = element_blank(),
                   legend.position = "top",
                   # legend.spacing = unit(1, "cm"), # Space legend labels
                   legend.key.size = unit(1,"cm"), 
                   legend.text.align = 0,
                   legend.text = element_text(size = 16, hjust = 0))

map

ggsave("map.tif", map, device = "tiff", scale = 2, 
       width = 2000, height = 2000, units = "px",
       path = file.path(dir, figpath), dpi = 300)

d %>%
  group_by(country, BsalDetected) %>%
  summarise(n = n())

# Remove objects from global environment to speed up processing
rm(country_lookup, final_data, m1a, m1a_plot, m1a_predict, m1b_plot, m1c, m1c_plot, 
   m1c_predict, m1c_rug, map, obs, p1abcombined, p1combined, pivotted_data, prev, 
   tmp, textcol, worldmap)

#### 2. Cbind models for all salamander spp. ####################################################
##      2a. T0 (At time of observation); T-1 (1 month prior to initial obs.); 
##          T-2 (2 months prior to initial obs.)
# Drop rows with NA vals in weather data
dcbindScaled <- tidyr::drop_na(dcbind, any_of(c(21:36))) %>%
  filter(scientific != "Ichthyosaura alpestris")

# Scale relevant vars
dcbindScaled <- dcbindScaled %>%
  mutate_at(c("temp_d", "temp_m", "temp_m_t1", "temp_m_t2",
              "sMoist_d", "sMoist_m", "sMoist_m_t1", "sMoist_m_t2",
              "precip_m", "precip_m_t1", "precip_m_t2",
              "bio1_wc", "bio12_wc", "tavg_wc", "prec_wc"), 
            ~(scale(., center = T, scale = T %>% as.numeric)))


# T0
m2_t0 <- glmmTMB(cbind(nPos_all, nNeg_all) ~  richness*logsiteAbun + 
                   temp_d*sMoist_d + (1|scientific),
                 data = dcbindScaled, family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m2_t0)
Anova(m2_t0)



# TEST
test <- dcbindScaled %>%
  filter(country != "United Kingdom" & scientific == "Salamandra salamandra")

tmp <- glmmTMB(cbind(nPos_all, nNeg_all) ~  richness*logsiteAbun + 
                   temp_d*sMoist_d + (1|scientific),
                 data = test, family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))
summary(tmp)
Anova(tmp)

# T-1
m2_t1 <- glmmTMB(cbind(nPos_all, nNeg_all) ~  richness*logsiteAbun + 
                   temp_m_t1*sMoist_m_t1 + (1|scientific),
                 data = dcbindScaled, family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m2_t1)
Anova(m2_t1)

# T-2
m2_t2 <- glmmTMB(cbind(nPos_all, nNeg_all) ~  richness*logsiteAbun + 
                   temp_m_t2*sMoist_m_t2 + (1|scientific),
                 data = dcbindScaled, family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m2_t2)
Anova(m2_t2)


#### Clean model outputs
## ABUNDANCE x RICHNESS
tab_model(m2_t0, show.obs = T, collapse.ci = T,
          rm.terms = c("temp_d", "sMoist_d", "temp_d:sMoist_d"),
          dv.labels = "All spp.",
          string.pred = "Terms",
          string.p = "P-Value",
          pred.labels = nicelabs,
          file = file.path(dir, figpath, "m2_t0_rich.html"))


# take html file and make .png file
webshot(file.path(dir, figpath, "m2_t0_rich.html"),
        file.path(dir, figpath, "m2_t0_rich.png"),
        vwidth = 365, vheight = 500)


## TEMP x SOIL MOISTURE
tab_model(m2_t0, show.obs = T, collapse.ci = T,
          rm.terms = c("logsiteAbun", "richness", "richness:logsiteAbun"),
          dv.labels = "Sample date",
          string.pred = "Terms",
          string.p = "P-Value",
          pred.labels = nicelabs,
          file = file.path(dir, figpath, "m2_t0_out.html"))

tab_model(m2_t1, show.obs = T, collapse.ci = T,
          rm.terms = c("logsiteAbun", "richness", "richness:logsiteAbun"),
          dv.labels = "One month prior",
          string.pred = "Terms",
          string.p = "P-Value",
          pred.labels = nicelabs,
          file = file.path(dir, figpath, "m2_t1_out.html"))

tab_model(m2_t2, show.obs = T, collapse.ci = T,
          rm.terms = c("logsiteAbun", "richness", "richness:logsiteAbun"),
          dv.labels = "Two months prior",
          string.pred = "Terms",
          string.p = "P-Value",
          pred.labels = nicelabs,
          file = file.path(dir, figpath, "m2_t2_out.html"))


# take html file and make .png file
webshot(file.path(dir, figpath, "m2_t0_out.html"),
        file.path(dir, figpath, "m2_t0_out.png"),
        vwidth = 365, vheight = 500)
webshot(file.path(dir, figpath, "m2_t1_out.html"),
        file.path(dir, figpath, "m2_t1_out.png"),
        vwidth = 365, vheight = 500)
webshot(file.path(dir, figpath, "m2_t2_out.html"),
        file.path(dir, figpath, "m2_t2_out.png"),
        vwidth = 365, vheight = 500)




##      2b. Prevalence by Abundance & Richness Plots for T0
# T0
m2_t0_rich <- ggpredict(m2_t0,  terms = c("richness", "logsiteAbun")) %>%
  rename("richness" = "x",
         "logsiteAbun" = "group") %>%
  mutate(richness = as.numeric(as.character(richness)),
         logsiteAbun = as.numeric(as.character(logsiteAbun)),
         # Convert scaled prediction to original data scale:
         siteAbun = as.factor(round(exp(as.numeric(logsiteAbun)), 0)))


m2_t0_p1 <- ggplot(m2_t0_rich, aes(x = richness, y = predicted, 
                                   colour = siteAbun)) +
  geom_line(aes(linetype = siteAbun), linewidth = 1) +
  geom_rug(data = dcbindScaled, aes(x = richness, y = 0), sides = "b", 
           alpha = 0.5, position = position_jitter(width = 0.4, height = 0.1), 
           inherit.aes = F, na.rm = T) +
  labs(x = "Species richness",
       y = "Bsal prevalence across\nall salamander species (%)",
       title = "All spp. model", 
       linetype = "Site-level abundance") +
#  geom_ribbon(aes(x = richness, ymin = conf.low, ymax = conf.high, 
#              fill = siteAbun), alpha = 0.2, colour = NA, show.legend = F) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_grey(start = 0.8, end = 0.2) +
  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), 
                     breaks = seq(0, .02, 0.005), limits = c(0, 0.022)) + 
  ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + 
  guides(colour = guide_legend("Site-level abundance"))

m2_t0_p1 

ggsave("m2_t0_AbunRich.tif", m2_t0_p1, device = "tiff", scale = 2, 
       width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)




##      2c. Prevalence by Temperature & Soil Moisture Plots for T0, T-1, T-2 
# T0
m2_t0_weather <- ggpredict(m2_t0, terms = c("temp_d [all]", "sMoist_d"))%>%
  rename("temp_d" = "x",
         "sMoist_d" = "group") %>%
  mutate(temp_d = as.numeric(as.character(temp_d)),
         sMoist_d = as.numeric(as.character(sMoist_d)),
  # Convert scaled prediction to original data scale:
         temp_dUnscaled = (temp_d * as.numeric(attr(dcbindScaled$temp_d, "scaled:scale")) + 
                              as.numeric(attr(dcbindScaled$temp_d, "scaled:center"))),
         sMoistUnscaled = as.factor((round(sMoist_d * as.numeric(attr(dcbindScaled$sMoist_d, "scaled:scale")) +
                                 as.numeric(attr(dcbindScaled$sMoist_d, "scaled:center")), 2)))) 
# Create dummy column for soil moisture labels 
m2_t0_weather <- create_dummy_col(m2_t0_weather)


m2_t0_p2 <- ggplot(m2_t0_weather, aes(x = temp_dUnscaled , y = predicted, 
                                      colour = dummy)) +
  geom_line(aes(linetype = factor(dummy, 
                                  levels  = c("Low", "Med", "High"))), 
                                  linewidth = 1) +
  geom_rug(data = dcbindScaled, aes(x = temp_d, y = 0), sides = "b", alpha = 0.5,
           position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, 
           na.rm = T) +
#  geom_ribbon(aes(x = temp_dUnscaled, ymin = conf.low, ymax = conf.high, 
#              fill = dummy), alpha = 0.2, colour = NA, show.legend = F) +
  labs(title =  bquote(paste(italic(t))[(0~days)]), linetype = "Soil moisture") +
  ylab("Bsal prevalence across\nall salamander species (%)") +
  xlab(expression("Temperature (°C)")) + 
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_viridis(option = "G", discrete = T, begin = 0.8, end = 0.3) +
  scale_x_continuous(labels = seq(-10, 30, 10), 
                     breaks = seq(-10, 30, 10), 
                     limits = c(-10, 30)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 0.1)) + 
  ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + 
  guides(colour = guide_legend("Soil moisture"))

m2_t0_p2 
ggsave("m2_t0_weather.tif", m2_t0_p2, device = "tiff", scale = 2, 
       width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)


# T-1
m2_t1_weather <- ggpredict(m2_t1, terms = c("temp_m_t1 [all]", "sMoist_m_t1")) %>%
  rename("temp_m_t1" = "x",
         "sMoist_m_t1" = "group") %>%
  mutate(temp_m_t1 = as.numeric(as.character(temp_m_t1)),
         sMoist_m_t1 = as.numeric(as.character(sMoist_m_t1)),
         temp_m_t1Unscaled = temp_m_t1 * as.numeric(attr(dcbindScaled$temp_m_t1, "scaled:scale")) + 
                                 as.numeric(attr(dcbindScaled$temp_m_t1, "scaled:center")),
         sMoist_t1Unscaled = as.factor(round(sMoist_m_t1 * as.numeric(attr(dcbindScaled$sMoist_m_t1, "scaled:scale")) +
                  as.numeric(attr(dcbindScaled$sMoist_m_t1, "scaled:center")), 2)))
# Create dummy column for soil moisture labels
m2_t1_weather <- create_dummy_col(m2_t1_weather)


m2_t1_p2 <- ggplot(m2_t1_weather, aes(x = temp_m_t1Unscaled , y = predicted, colour = dummy)) +
  geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), linewidth = 1) +
  geom_rug(data = dcbindScaled, aes(x = temp_m_t1, y = 0), sides = "b", alpha = 0.5,
           position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
#  geom_ribbon(aes(x = temp_m_t1Unscaled, ymin = conf.low, ymax = conf.high, fill = dummy), alpha = 0.2, colour = NA, show.legend = F) +
  labs(title = bquote(paste(italic(t))[(-30~days)]), linetype = bquote("Soil moisture")) +
  ylab("Bsal prevalence acrossn\all salamander species (%)") +
  xlab(expression("Temperature (°C)")) + 
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_viridis(option = "G", discrete = T, begin = 0.8, end = 0.3) +
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.1)) + 
  ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + guides(colour = guide_legend("Soil moisture"))

m2_t1_p2 
ggsave("m2_t1_weather.tif", m2_t1_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)


# T-2
m2_t2_weather <- ggpredict(m2_t2, terms = c("temp_m_t2 [all]", "sMoist_m_t2")) %>%
  rename("temp_m_t2" = "x",
         "sMoist_m_t2" = "group") %>%
  mutate(temp_m_t2 = as.numeric(as.character(temp_m_t2)),
         sMoist_m_t2 = as.numeric(as.character(sMoist_m_t2)),
         temp_m_t2Unscaled = (temp_m_t2 * as.numeric(attr(dcbindScaled$temp_m_t2, "scaled:scale")) +
                                 as.numeric(attr(dcbindScaled$temp_m_t2, "scaled:center"))),
         sMoist_t2Unscaled = as.factor((round(sMoist_m_t2 * as.numeric(attr(dcbindScaled$sMoist_m_t2, "scaled:scale")) +
                                            as.numeric(attr(dcbindScaled$sMoist_m_t2, "scaled:center")), 2))))
# Create dummy column for soil moisture labels
m2_t2_weather <- create_dummy_col(m2_t2_weather)


m2_t2_p2 <- ggplot(m2_t2_weather, aes(x = temp_m_t2Unscaled , y = predicted, colour = dummy)) +
  geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), linewidth = 1) +
  geom_rug(data = dcbindScaled, aes(x = temp_m_t2, y = 0), sides = "b", alpha = 0.5,
           position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
#  geom_ribbon(aes(x = temp_m_t2Unscaled, ymin = conf.low, ymax = conf.high, fill = dummy), alpha = 0.2, colour = NA, show.legend = F) +
  labs(title = bquote(paste(italic(t))[(-60~days)]), linetype = bquote("Soil moisture")) +
  ylab("Bsal prevalence across\nall salamander species (%)") +
  xlab(expression("Temperature (°C)")) + 
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_viridis(option = "G", discrete = T, begin = 0.8, end = 0.3) +
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.1)) + 
  ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + guides(colour = guide_legend("Soil moisture"))

m2_t2_p2 
ggsave("m2_t2_weather.tif", m2_t2_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

# 3 Panel Graph
m2_p2_combined <- ((m2_t2_p2 + labs(caption = NULL, x = NULL) + theme(legend.position = "none")) | 
                     (m2_t1_p2 + labs(caption = NULL, y = NULL)) | 
                     (m2_t0_p2 + labs(caption = NULL, y = NULL, x = NULL) + theme(legend.position = "none"))) + 
  plot_annotation(tag_levels = "A", 
                  caption = "Soil moisture ranged from 5.34-5.36 (kg/m"^2~"), 6.10-6.12 (kg/m"^2~"), and 6.84-6.90 (kg/m"^2~") for the Low, Medium, and High categories respectively. Timepoints above each graph indicate the time from the initial observation.",
                  theme = theme(plot.caption = element_text(size = 12, hjust = 0)))

m2_p2_combined
ggsave("m2_weather_combined.tif", m2_p2_combined, device = "tiff", scale = 2, width = 2600, height = 1500, units = "px", 
       path = file.path(dir, figpath), dpi = 300)


# Remove saved objects from the global environment to speed up processing
rm(m2_p2_combined, m2_t0_p1, m2_t0_p2, m2_t0_rich, m2_t0_weather, 
   m2_t1_p2, m2_t1_weather, m2_t2_p2, m2_t2_weather)



#### 3. Cbind models for fire salamanders only ####################################################
##      3a. T0 (At time of observation); T-1 (30 days prior to initial obs.); T-2 (60 days prior to initial obs.)
# T0
m3_t0 <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~  richness*logsppAbun + temp_d*sMoist_d + (1|scientific),
                 data = subset(dcbindScaled, scientific =="Salamandra salamandra"), family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m3_t0)
Anova(m3_t0)


# T-1
m3_t1 <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~  richness*logsiteAbun + temp_m_t1*sMoist_m_t1 + (1|scientific),
                 data = subset(dcbindScaled, scientific =="Salamandra salamandra"), family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m3_t1)
Anova(m3_t1)



# T-2
m3_t2 <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~  richness*logsiteAbun + temp_m_t2*sMoist_m_t2 + (1|scientific),
                 data = subset(dcbindScaled, scientific =="Salamandra salamandra"), family = "binomial",
                 control = glmmTMBControl(optimizer = optim, 
                                          optArgs = list(method = "BFGS")))

summary(m3_t2)
Anova(m3_t2)

#### Clean model outputs
## ABUNDANCE x RICHNESS
tab_model(m3_t0, show.obs = T, collapse.ci = T,
          rm.terms = c("temp_d", "sMoist_d", "temp_d:sMoist_d"),
          dv.labels = "Fire salamanders",
          string.pred = "Terms",
          string.p = "P-Value",
          pred.labels = nicelabs,
          file = file.path(dir, figpath, "m3_t0_rich.html"))


# take html file and make .png file
webshot(file.path(dir, figpath, "m3_t0_rich.html"),
        file.path(dir, figpath, "m3_t0_rich.png"),
        vwidth = 365, vheight = 500)


## TEMP x SOIL MOISTURE
tab_model(m3_t0, show.obs = T, collapse.ci = T,
          rm.terms = c("logsiteAbun", "richness", "richness:logsiteAbun"),
          dv.labels = "t(0)",
          string.pred = "Terms",
          string.p = "P-Value",
          pred.labels = nicelabs,
          file = file.path(dir, figpath, "m3_t0_out.html"))

tab_model(m3_t1, show.obs = T, collapse.ci = T,
          rm.terms = c("logsiteAbun", "richness", "richness:logsiteAbun"),
          dv.labels = "t(-30)",
          string.pred = "Terms",
          string.p = "P-Value",
          pred.labels = nicelabs,
          file = file.path(dir, figpath, "m3_t1_out.html"))

tab_model(m3_t2, show.obs = T, collapse.ci = T,
          rm.terms = c("logsiteAbun", "richness", "richness:logsiteAbun"),
          dv.labels = "t(-60)",
          string.pred = "Terms",
          string.p = "P-Value",
          pred.labels = nicelabs,
          file = file.path(dir, figpath, "m3_t2_out.html"))


# take html file and make .png file
webshot(file.path(dir, figpath, "m3_t0_out.html"),file.path(dir, figpath, "m3_t0_out.png"))
webshot(file.path(dir, figpath, "m3_t1_out.html"),file.path(dir, figpath, "m3_t1_out.png"))
webshot(file.path(dir, figpath, "m3_t2_out.html"),file.path(dir, figpath, "m3_t2_out.png"))


##      3b. Prevalence by Abundance & Richness Plots for T0, T-1, T-2 (Fire Salamanders Only)
# T0
m3_t0_rich <- ggpredict(m3_t0,  terms = c("richness", "logsiteAbun")) %>%
  rename("richness" = "x",
         "logsiteAbun" = "group") %>%
  mutate(richness = as.numeric(as.character(richness)),
         logsiteAbun = as.numeric(as.character(logsiteAbun)),
         # Convert scaled prediction to original data scale:
         siteAbun = as.factor(round(exp(as.numeric(logsiteAbun)), 0)))

m3_t0_p1 <- ggplot(m3_t0_rich, aes(x = richness , y = predicted, colour = siteAbun)) +
  geom_line(aes(linetype = siteAbun), linewidth = 1) +
  geom_rug(data = subset(dcbindScaled, scientific =="Salamandra salamandra"), aes(x = richness, y = 0), sides = "b", alpha = 0.5,
           position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
  labs(title =  "Fire salamander model", linetype = "Site-level abundance") +
  #  geom_ribbon(aes(x = richness, ymin = conf.low, ymax = conf.high, fill = siteAbun), alpha = 0.2, colour = NA, show.legend = F) +
  ylab("Fire salamander\nBsal prevalence (%)") +
  xlab("Species richness") +
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_grey(start = 0.8, end = 0.2) +
  scale_x_continuous(labels = seq(0, 10, 1), breaks = seq(0, 10, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.30)) +
  ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + guides(colour = guide_legend("Site-level abundance"))


m3_t0_p1
ggsave("m3_t0_AbunRich.tif", m3_t0_p1, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px",
       path = file.path(dir, figpath), dpi = 300)



##      3c. Prevalence by Temperature & Soil Moisture Plots for T0, T-1, T-2 (Fire Salamanders Only)
# T0
m3_t0_weather <- ggpredict(m3_t0, terms = c("temp_d [all]", "sMoist_d"))%>%
  rename("temp_d" = "x",
         "sMoist_d" = "group") %>%
  mutate(temp_d = as.numeric(as.character(temp_d)),
         sMoist_d = as.numeric(as.character(sMoist_d)),
         # Convert scaled prediction to original data scale:
         temp_dUnscaled = (temp_d * as.numeric(attr(dcbindScaled$temp_d, "scaled:scale")) + 
                                as.numeric(attr(dcbindScaled$temp_d, "scaled:center"))),
         sMoistUnscaled = as.factor((round(sMoist_d * as.numeric(attr(dcbindScaled$sMoist_d, "scaled:scale")) +
                                                   as.numeric(attr(dcbindScaled$sMoist_d, "scaled:center")), 2)))) 
# Create dummy column for soil moisture labels
m3_t0_weather <- create_dummy_col(m3_t0_weather)


m3_t0_p2 <- ggplot(m3_t0_weather, aes(x = temp_dUnscaled , y = predicted, colour = dummy)) +
  geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), linewidth = 1) +
  geom_rug(data = subset(dcbindScaled, scientific =="Salamandra salamandra"), aes(x = temp_d, y = 0), sides = "b", alpha = 0.5,
           position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
  #  geom_ribbon(aes(x = temp_dUnscaled, ymin = conf.low, ymax = conf.high, fill = dummy), alpha = 0.2, colour = NA, show.legend = F) +
  labs(title =  bquote(paste(italic(t))[(0~days)]), linetype = "Soil moisture") +
  ylab("Fire salamander Bsal prevalence (%)") +
  xlab(expression("Temperature (°C)")) + 
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_viridis(option = "G", discrete = T, begin = 0.8, end = 0.3) +
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.3)) + 
  ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + guides(colour = guide_legend("Soil moisture"))

m3_t0_p2 
ggsave("m3_t0_weather.tif", m3_t0_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

# T-1
m3_t1_weather <- ggpredict(m3_t1, terms = c("temp_m_t1 [all]", "sMoist_m_t1"))%>%
  rename("temp_m_t1" = "x",
         "sMoist_m_t1" = "group") %>%
  mutate(temp_m_t1 = as.numeric(as.character(temp_m_t1)),
         sMoist_m_t1 = as.numeric(as.character(sMoist_m_t1)),
         # Convert scaled prediction to original data scale:
         temp_m_t1Unscaled = (temp_m_t1 * as.numeric(attr(dcbindScaled$temp_m_t1, "scaled:scale")) + 
                                   as.numeric(attr(dcbindScaled$temp_m_t1, "scaled:center"))),
         sMoistUnscaled = as.factor((round(sMoist_m_t1 * as.numeric(attr(dcbindScaled$sMoist_m_t1, "scaled:scale")) +
                                                   as.numeric(attr(dcbindScaled$sMoist_m_t1, "scaled:center")), 2)))) 
# Create dummy column for soil moisture labels
m3_t1_weather <- create_dummy_col(m3_t1_weather)


m3_t1_p2 <- ggplot(m3_t1_weather, aes(x = temp_m_t1Unscaled , y = predicted, colour = dummy)) +
  geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), linewidth = 1) +
  geom_rug(data = subset(dcbindScaled, scientific =="Salamandra salamandra"), aes(x = temp_m_t1, y = 0), sides = "b", alpha = 0.5,
           position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
  #  geom_ribbon(aes(x = temp_m_t1Unscaled, ymin = conf.low, ymax = conf.high, fill = dummy), alpha = 0.2, colour = NA, show.legend = F) +
  labs(title =  bquote(paste(italic(t))[(-30~days)]), linetype = "Soil moisture") +
  ylab("Fire salamander Bsal prevalence (%)") +
  xlab(expression("Temperature (°C)")) + 
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_viridis(option = "G", discrete = T, begin = 0.8, end = 0.3) +
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.3)) + 
  ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + guides(colour = guide_legend("Soil moisture"))

m3_t1_p2 
ggsave("m3_t1_weather.tif", m3_t1_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)


# T-2
m3_t2_weather <- ggpredict(m3_t2, terms = c("temp_m_t2 [all]", "sMoist_m_t2"))%>%
  rename("temp_m_t2" = "x",
         "sMoist_m_t2" = "group") %>%
  mutate(temp_m_t2 = as.numeric(as.character(temp_m_t2)),
         sMoist_m_t2 = as.numeric(as.character(sMoist_m_t2)),
         # Convert scaled prediction to original data scale:
         temp_m_t2Unscaled = (temp_m_t2 * as.numeric(attr(dcbindScaled$temp_m_t2, "scaled:scale")) + 
                                   as.numeric(attr(dcbindScaled$temp_m_t2, "scaled:center"))),
         sMoistUnscaled = as.factor((round(sMoist_m_t2 * as.numeric(attr(dcbindScaled$sMoist_m_t2, "scaled:scale")) +
                                                   as.numeric(attr(dcbindScaled$sMoist_m_t2, "scaled:center")), 2)))) 
# Create dummy column for soil moisture labels
m3_t2_weather <- create_dummy_col(m3_t2_weather)


m3_t2_p2 <- ggplot(m3_t2_weather, aes(x = temp_m_t2Unscaled , y = predicted, colour = dummy)) +
  geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), linewidth = 1) +
  geom_rug(data = subset(dcbindScaled, scientific =="Salamandra salamandra"), aes(x = temp_m_t2, y = 0), sides = "b", alpha = 0.5,
           position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
  #  geom_ribbon(aes(x = temp_d_t2Unscaled, ymin = conf.low, ymax = conf.high, fill = dummy), alpha = 0.2, colour = NA, show.legend = F) +
  labs(title =  bquote(paste(italic(t))[(-60~days)]), linetype = "Soil moisture") +
  ylab("Fire salamander Bsal prevalence (%)") +
  xlab(expression("Temperature (°C)")) + 
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_viridis(option = "G", discrete = T, begin = 0.8, end = 0.3) +
  scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.3)) + 
  ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + guides(colour = guide_legend("Soil moisture"))

m3_t2_p2 
ggsave("m3_t2_weather.tif", m3_t2_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

# 3 Panel Graph
m3_p2_combined <- m2_p2_combined <- ((m3_t2_p2 + labs(caption = NULL, x = NULL) + theme(legend.position = "none")) | 
                                     (m3_t1_p2 + labs(caption = NULL, y = NULL)) | 
                                     (m3_t0_p2 + labs(caption = NULL, y = NULL, x = NULL) + theme(legend.position = "none"))) + 
                                      plot_annotation(tag_levels = "A", 
                                      caption = "Soil moisture ranged from 5.42-5.52 (kg/m"^2~"), 6.16-6.22 (kg/m"^2~"), and 6.89-6.92 (kg/m"^2~") for the Low, Medium, and High categories respectively. Timepoints above each graph indicate the time from the initial observation.",
                                      theme = theme(plot.caption = element_text(size = 12, hjust = 0)))

m3_p2_combined
ggsave("m3_weather_combined.tif", m3_p2_combined, device = "tiff", scale = 2, width = 2600, height = 1500, units = "px", 
       path = file.path(dir, figpath), dpi = 300)


# Remove saved objects from the global environment to speed up processing
rm(m3_p2_combined, m3_t0_p1, m3_t0_p2, m3_t0_rich, m3_t0_weather, m3_t1_p2, 
   m3_t1_weather,m3_t2_p2, m3_t2_weather)



#### 4. Fatality model ####################################################
# Remove any instances of NA within the fatal column & weather vars columns, as well as I. alpestris
d_fatal <- d %>%
  tidyr::drop_na(any_of(c(31, 40:45))) %>%
  subset(scientific != "Ichthyosaura alpestris") %>%
# Scale relevant vars
  mutate_at(c("tavg_wc", "tmin_wc", "tmax_wc", "prec_wc", "bio1_wc", "bio12_wc", "bio1_wc"), 
            ~(scale(., center = T, scale = T %>% as.numeric)))
# bio1 == annual mean temp
# bio12 == annual precip

##      4a.  Fatality of fire salamanders given an interaction between average monthly temperature (tavg) and if Bsal has ever been detected at a site. 
m4 <- glmmTMB(fatal ~ bio1_wc*prev_above_0 + (1|Site),
                 family = "binomial",
                 data = filter(d, scientific == "Salamandra salamandra"),
                 control = glmmTMBControl(optimizer = optim,
                                          optArgs = list(method = "BFGS")))

summary(m4)
Anova(m4)


##      4b. Plots
# bio1 - all data
m4_predict <- ggpredict(m4, terms = c("bio1_wc [all]", "prev_above_0")) %>%
  rename("bio1_wc" = "x",
         "BsalDetected" = "group") %>%
  mutate(bio1_wc = as.numeric(as.character(bio1_wc)),
         BsalDetected = as.factor(ifelse(BsalDetected == 0, "Bsal ( - )", "Bsal ( + )")),
         # Convert scaled prediction to original data scale
         bio1_wcUnscaled = (bio1_wc * as.numeric(attr(d_fatal$bio1_wc, "scaled:scale")) + as.numeric(attr(d_fatal$bio1_wc, "scaled:center"))))
 

m4_plot <- ggplot(m4_predict, aes(x = sMoist_dUnscaled , y = predicted, colour = BsalDetected)) +
  geom_line(aes(linetype = BsalDetected), linewidth = 1) +
#  geom_rug(data = dcbindScaled, aes(x = richness, y = 0), sides = "b", alpha = 0.5, 
#           position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
  labs(linetype = "Status") +
#  geom_ribbon(aes(x = tavgUnscaled, ymin = conf.low, ymax = conf.high, fill = dummy), alpha = 0.2, colour = NA, show.legend = F) +
  ylab("Fire salamander mortality (%)") +
  xlab(expression("Mean annual temperature (°C)")) + 
  scale_linetype_manual(values = c("solid", "longdash")) +
  scale_color_manual(values = c("black", "#C23113")) +
  scale_x_continuous(labels = seq(17, 23, 2), breaks = seq(17, 23, 2), limits = c(17, 23)) +  
  scale_y_continuous(labels = scales::percent, limits = c(0, .5)) +
  ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + guides(colour = guide_legend("Status"))

m4_plot

ggsave("m4_fatality.tif", m4_plot, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
       path = file.path(dir, figpath), dpi = 300)

