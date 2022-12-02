require(pacman)
require(extrafont)
#remotes::install_version("Rttf2pt1", version = "1.3.8") # install this version, latest ver. not compatible
require(Rttf2pt1)
#extrafont::font_import() # load fonts before ggplot2
extrafont::loadfonts(device = "win", quiet = T) # plot fonts
pacman::p_load(tidyverse,
               glmmTMB, # glmmTMB()
               car, # Anova()
               DHARMa, # simulateResiduals(), testZeroInflation(), testDispersion()
               lme4, # lm()
               MASS, # negative binomial models
               sjPlot, # plot_model()
               ggeffects,
               ggtext,
               hrbrthemes, # plot colors
               stars, # spatiotemporal data handling
               raster, # raster data handling
               sf, # vector data handling
               data.table, # data wrangling
               patchwork, # arranging figures
               ggsignif, # adds labels to significant groups
               tigris, # county border
               colorspace, # color scale
               viridis, # arranging figures
               ggspatial # north arrow and scale bar
)

## Set working directory
setwd('C:/Users/alexi/OneDrive/Documents/01_GradSchool/_DissertationWork/Chapter4/03_code')

## Load csv files
d <- read.csv("bsalData_clean.csv", header = T, encoding = "UTF-8")
dcbind <- read.csv("bsalData_cbind.csv", header = T, encoding = "UTF-8")

## Set plot theme
ak_theme <- theme_ipsum() +
  theme(axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 24, hjust = 0.5, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 24,hjust = 0.5, margin = margin(t = 0, r = 15, b = 0, l = 5)),
        axis.title = element_text(size = 28, face = "plain"),
        plot.title = element_markdown(hjust = c(0, -1), size = 32, face = "plain"),
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


## Include Pelophylax perezi data  (5 rows) into Pelophylax sp.
d$scientific <- gsub(d$scientific, pattern = "Pelophylax perezi",
                     replacement = "Pelophylax sp.")

# log transform + scale vars
d <- d %>%
  mutate(logsppAbun = log(sppAbun),
         logsiteAbun = log(siteAbun),
         temp_t0_scaled = scale(temp_date),
         temp_t1_scaled = scale(temp_date_t1),
         temp_t2_scaled = scale(temp_date_t2),
         soilMoisture_t0_scaled = scale(soilMoisture_date),
         soilMoisture_t1_scaled = scale(soilMoisture_date_t1),
         soilMoisture_t2_scaled = scale(soilMoisture_date_t2),
         bio1_scaled = scale(bio1),
         bio12_scaled = scale(bio12),
         scientific = as.factor(scientific))

dcbind <- dcbind %>%
  mutate(logsppAbun = log(sppAbun),
         logsiteAbun = log(siteAbun),
         temp_t0_scaled = scale(temp_date),
         temp_t1_scaled = scale(temp_date_t1),
         temp_t2_scaled = scale(temp_date_t2),
         soilMoisture_t0_scaled = scale(soilMoisture_date),
         soilMoisture_t1_scaled = scale(soilMoisture_date_t1),
         soilMoisture_t2_scaled = scale(soilMoisture_date_t2),
         bio1_scaled = scale(bio1),
         bio12_scaled = scale(bio12),
         scientific = as.factor(scientific))



# Remove non-native species
dcbind_alpestris <- subset(dcbind, scientific != "Ichthyosaura alpestris")
d_alpestris <- subset(d, scientific != "Ichthyosaura alpestris")

# Remove Jaime's data 
d_noJB <- subset(d, collectorList != "Jaime Bosch")
dcbind_noJB <- subset(dcbind, collectorList != "Jaime Bosch")


##### Mean species abundance by site; susceptibility of each spp. is noted as well. ####
##    This figure is based on the raw data, not based on any model. (Not sure how 
##    useful this figure actually is)
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


abun <-ggplot(descriptive, aes(x = Site, y = scientific, size = meanSppAbun, colour = susceptibility)) +
  geom_point(alpha=0.5) +
  scale_size_area(name = "Mean Species Abundance", limits = c(1,36), breaks = seq(0, 30, 10)) +
  scale_colour_manual(name = "Susceptibility",values = c("#8bd3c7", "#f8ae5d", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible")) +
  scale_x_discrete(limits = factor(c(0:400)), breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 
                                                         225, 250, 275, 300, 325, 350, 375, 400),
                   labels = c("0", "", "", "", "100", "", "", "", "200", "", "", "", "300", "", "", "", "400")) +
  ylab("Species") +
  xlab("Site #") +
  guides(color = guide_legend(override.aes = list(size = 12))) + # increase size of 'susceptibility' legend icons
  labs(caption = c("Abundance of individual species at each site. Outliers (*e.g.*, instances of a single mass die-off) were removed to better highlight data. 
  Size of each point indicates the site abundance (*i.e.*, how many species total there are at a site for all sampling occurrences). 
  Color indicates the<br>susceptibility level of each species: Resistant (blue) = No to low infection and no clinical signs of disease; Tolerant (orange) = low infection loads with 
  low or variable mortality; and Susceptible (red) = High infection loads resulting in consistently high mortality.")) + 
  ak_theme
abun  


#### Simple model using all data to see how abundant each species is at each site. ####
## log Spp Abundance ~ Scientific + (1|Site)
model1a <- glmmTMB(logsppAbun ~  scientific + (1|Site),
                   data = d,
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))
summary(model1a)
Anova(model1a)

m1a <- ggpredict(model1a, terms = "scientific")

p1a <-ggplot(m1a, aes(x , predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  ylab("Predicted ln(Species Abundance)") +
  xlab("Species") + scale_x_discrete(expand = expansion(mult = c(0, 0.1), add = c(1, 0))) +
  ak_theme 
p1a


model1b <- glmmTMB(richness ~  scientific + (1|Site),
                   data = d,
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))
summary(model1b)
Anova(model1b)

m1b <- ggpredict(model1b, terms = "scientific")

p1b <-ggplot(m1b, aes(x , predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  ylab("Predicted Species Richness") + 
  xlab("") + scale_x_discrete(labels = NULL) +
  ak_theme
p1b

p1combined <- (p1a|p1b) + plot_layout(guides = "collect")
p1combined

#### Model to see how disease prevalence (both Bd and Bsal) at each site is influenced by species presence. ####
## Bsal only model had issues converging
model2 <- glmmTMB(cbind(YesDisease, NoDisease) ~  scientific + whichDisease + (1|Site),
                  data = dcbind, family = "binomial",
                  control = glmmTMBControl(optimizer = optim,
                                           optArgs = list(method = "BFGS")))
summary(model2)
Anova(model2)

m2 <- ggpredict(model2, terms = c("scientific", "whichDisease"))

## Check model residuals
m2_sim <- simulateResiduals(model2)
testResiduals(m2_sim, plot = T)
testDispersion(m2_sim, plot = T) # overdispersed
testZeroInflation(m2_sim, plot = T)
testOutliers(m2_sim, type = "bootstrap", plot = T)
plot(m2_sim)

p2 <- ggplot(m2, aes(x , (predicted*100))) +
  geom_point() +
  #  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  ylab("Disease Prevalence (%)") +
  xlab("Species") + ak_theme + theme(axis.title.x = element_text(size = 24, hjust = 0.9),
                                     axis.title.y = element_text(size = 24, hjust = 0.9))
p2


prev <-ggplot(m2, aes(x = (predicted*100), y = x)) +
  geom_point(alpha=0.5) +
  ylab("Species") +
  xlab("Disease Prevalence") +
  ak_theme
prev

#### Models to test whether the most susceptible hosts (susceptibility score 3) are the most abundant ####
##   and whether the least susceptible or resistant hosts (susceptibility score 1) are rare hosts.  

## T0 (Weather data from the date of sample observation)
model3a <- glmmTMB(logsppAbun ~  as.factor(susceptibility) + scale(temp_date)*scale(soilMoisture_date) + (1|Site),
                   data = d,
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))
print(summary(model3a))
print(Anova(model3a))
m3a <- ggpredict(model3a, terms = c("susceptibility", "temp_date[5, 15, 25]", "soilMoisture_date[1, 3, 6]"))


## T-1 (Weather data from one month prior (i.e., 30 days) to the date of sample observation) 
model3b <- glmmTMB(logsppAbun ~  as.factor(susceptibility) + scale(temp_date_t1)*scale(soilMoisture_date_t1) + (1|Site),
                   data = d,
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))
print(summary(model3b))
print(Anova(model3b))
m3b <- ggpredict(model3b, terms = c("susceptibility", "temp_date_t1[5, 15, 25]", "soilMoisture_date_t1[1, 3, 6]"))


## T-2 (Weather data from two months prior (i.e., 30 days) to the date of sample observation) 
model3c <- glmmTMB(logsppAbun ~  as.factor(susceptibility) + scale(temp_date_t2)*scale(soilMoisture_date_t2) + (1|Site),
                   data = d,
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))
print(summary(model3c))
print(Anova(model3c))
m3c <- ggpredict(model3c, terms = c("susceptibility", "temp_date_t2[5, 15, 25]", "soilMoisture_date_t2[1, 3, 6]"))


## Abundance ~ Susceptibility + Temp*Precip 
model3d <- glmmTMB(logsppAbun ~  as.factor(susceptibility) + scale(tavg)*scale(prec) + (1|Site),
                   data = d,
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))
print(summary(model3d))
print(Anova(model3d))
#m3d <- ggpredict(model3d, terms = c("susceptibility", "bio1[5, 10, 15]", "bio12[70, 85, 100]"))
m3d <- ggpredict(model3d, terms = c("susceptibility", "tavg[5, 10, 15]", "prec[5, 7, 9]"))


## Model 3 Plots
p3a <- ggplot(m3a, aes(x, predicted, colour = group, shape = facet)) +
  geom_jitter(size = 4, alpha = 0.85, position = position_jitter(width = 0.2, height = 0.2, seed = 123)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, position = position_jitter(width = 0.2, height = 0.2, seed = 123)) +
  scale_shape_manual(name = bquote("Soil Moisture (kg/m"^2~")"), 
                     values = c(15, 16, 17)) +
  scale_colour_manual(name = expression("Temperature (°C)"),
                      values = c("#032349", "#f7c331", "#c4001f")) +
  labs(title = bquote("Time T"[(0)])) +
  ylab("ln(Species Abundance)") +
  ylim(-1.7, 4) +
  xlab("Susceptibility Level") +
  scale_x_discrete(labels = c("Low","Med","High"),
                   limits = factor(c(1:3))) +
  guides(color = guide_legend(override.aes = list(size = 5))) + ak_theme 
p3a

p3b <- ggplot(m3b, aes(x, predicted, colour = group, shape = facet)) +
  geom_jitter(size = 4, alpha = 0.85, position = position_jitter(width = 0.2, height = 0.2, seed = 123)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, position = position_jitter(width = 0.2, height = 0.2, seed = 123)) +
  scale_shape_manual(name = bquote("Soil Moisture (kg/m"^2~")"), 
                     values = c(15, 16, 17)) +
  scale_colour_manual(name = expression("Temperature (°C)"),
                      values = c("#032349", "#f7c331", "#c4001f")) +
  labs(title = bquote("Time T"[(-1)])) +
  ylab("") +
  ylim(-1.7, 4) +
  xlab("Susceptibility Level") +
  scale_x_discrete(labels = c("Low","Med","High"),
                   limits = factor(c(1:3))) +
  guides(color = guide_legend(override.aes = list(size = 5))) + ak_theme
p3b

p3c <- ggplot(m3c, aes(x, predicted, colour = group, shape = facet)) +
  geom_jitter(size = 4, alpha = 0.85, position = position_jitter(width = 0.2, height = 0.2, seed = 123)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, position = position_jitter(width = 0.2, height = 0.2, seed = 123)) +
  scale_shape_manual(name = bquote("Soil Moisture (kg/m"^2~")"), 
                     values = c(15, 16, 17)) +
  scale_colour_manual(name = expression("Temperature (°C)"),
                      values = c("#032349", "#f7c331", "#c4001f")) +
  labs(title = bquote("Time T"[(-2)])) +
  ylab("") +
  ylim(-1.7, 4) +
  xlab("Susceptibility Level") +
  scale_x_discrete(labels = c("Low","Med","High"),
                   limits = factor(c(1:3))) +
  guides(color = guide_legend(override.aes = list(size = 5))) + ak_theme
p3c

p3d <- ggplot(m3d, aes(x, predicted, colour = group, shape = facet)) +
  geom_jitter(size = 4, alpha = 1, position = position_jitter(width = 0.2, height = 0.2, seed = 123)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, position = position_jitter(width = 0.2, height = 0.2, seed = 123)) +
  scale_shape_manual(name = expression("Precipitation (cm)"), 
                     values = c(18, 21, 22)) +
  scale_colour_manual(name = expression("Temperature (°C)"),
                      values = c("#032349", "#7cb4c3", "#f7c331")) +
  labs(title = "Climatic Variables (Monthly Average)") +
  scale_y_continuous(labels = c("0.5"," 1.5", "2.5"),
                     breaks = c(0.5, 1.5, 2.5))+
  ylab("ln(Species Abundance)") +
  xlab("Susceptibility Level") +
  scale_x_discrete(labels = c("Low","Med","High"),
                   limits = factor(c(1:3))) +
  guides(color = guide_legend(override.aes = list(size = 5))) + ak_theme
p3d



weather_combined <- p3a|p3b|p3c
combined <- (weather_combined + plot_layout(guides = "collect") & theme(legend.position = "bottom"))/p3d
combined



##### Bsal Detected Full Model (Excluding I. alpestris) #####
## Full model at time T0 (Weather data from the date of sample observation)
model4a <- glmmTMB(BsalDetected ~ logsiteAbun*richness + scale(temp_date)*scale(soilMoisture_date) +
                     (1|scientific),
                   family = "binomial",
                   data = d_alpestris,
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))

summary(model4a)
Anova(model4a)

## Check model residuals
m4a_sim <- simulateResiduals(model4a)
testResiduals(m4a_sim, plot = T)
testDispersion(m4a_sim, plot = T)
testOutliers(m4a_sim, type = "bootstrap", plot = T)
plot(m4a_sim)

## Plot
p4a <-plot_model(model4a, type = "eff", terms = c("richness", "logsiteAbun[1, 2, 4]"),
                 axis.title = c("Host Richness", "Bsal Prevalence (%)"),
                 legend.title = c("ln(Site Level Host Abundance)"),
                 title = bquote("Time T"[(0)]),
                 colors = "bw",
                 ci.lvl = NA) + 
  scale_x_continuous(labels = seq(0, 10, 1),
                     breaks = seq(0, 10, 1)) +
  ak_theme
p4a 

p4a.2 <- plot_model(model4a, type = "eff", terms = c("temp_date", "soilMoisture_date"),
                    axis.title = c("Temperature (°C)", "Bsal prevalence (%)"),
                    legend.title = bquote("Soil Moisture (kg/m"^2~")"),
                    title = bquote("Time T"[(0)]),
                    show.data=FALSE,
                    colors = "bw",
                    ci.lvl = NA) + 
  scale_x_continuous(labels = seq(-10, 25, 5),
                     breaks = seq(-10, 25, 5)) +
  scale_y_continuous(limits = c(0, 0.03),
                     breaks = seq(0, 0.03, 0.01),
                     labels = scales::percent_format(accuracy = 1)) +
  ak_theme
p4a.2

p4a_combined <- p4a/p4a.2
p4a_combined


model4b <- glmmTMB(BsalDetected ~ logsiteAbun*richness + scale(temp_date_t1)*scale(soilMoisture_date_t1) +
                     (1|scientific),
                   family = "binomial",
                   data = d_alpestris,
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))

summary(model4b)
Anova(model4b)

## Check model residuals
m4b_sim <- simulateResiduals(model4b)
testResiduals(m4b_sim, plot = T)
testDispersion(m4b_sim, plot = T)
testOutliers(m4b_sim, type = "bootstrap", plot = T)
plot(m4b_sim)

## Plot
p4b <-plot_model(model4b, type = "eff", terms = c("richness", "logsiteAbun[1, 2, 4]"),
                 axis.title = c("Host Richness", "Bsal Prevalence (%)"),
                 legend.title = c("ln(Site Level Host Abundance)"),
                 title = bquote("Time T"[(-1)]),
                 colors = "bw",
                 ci.lvl = NA) + 
  scale_x_continuous(labels = seq(0, 10, 1),
                     breaks = seq(0, 10, 1)) +
  scale_y_continuous(limits = c(0, 0.04),
                     breaks = seq(0, 0.04, 0.01), 
                     labels = scales::percent_format(accuracy = 1)) +
  ak_theme
p4b 

p4b.2 <- plot_model(model4b, type = "eff", terms = c("temp_date_t1", "soilMoisture_date_t1"),
                    axis.title = c("Temperature (°C)", "Bsal prevalence (%)"),
                    legend.title = bquote("Soil Moisture (kg/m"^2~")"),
                    title = bquote("Time T"[(-1)]),
                    show.data=FALSE,
                    colors = "bw",
                    ci.lvl = NA) + 
  scale_x_continuous(labels = seq(-10, 30, 10),
                     breaks = seq(-10, 30, 10)) +
  scale_y_continuous(limits = c(0, 0.15),
                     breaks = seq(0, 0.15, 0.05),
                     labels = scales::percent_format(accuracy = 1)) +
  ak_theme
p4b.2

p4b_combined <- p4b/p4b.2
p4b_combined


model4c <- glmmTMB(BsalDetected ~ logsiteAbun*richness + scale(temp_date_t2)*scale(soilMoisture_date_t2) +
                     (1|scientific),
                   family = "binomial",
                   data = d_alpestris,
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))

summary(model4c)
Anova(model4c)

## Check model residuals
m4c_sim <- simulateResiduals(model4c)
testResiduals(m4c_sim, plot = T)
testDispersion(m4c_sim, plot = T)
testOutliers(m4c_sim, type = "bootstrap", plot = T)
plot(m4c_sim)

## Plot
p4c <-plot_model(model4c, type = "eff", terms = c("richness", "logsiteAbun"),
                 axis.title = c("Host Richness", "Bsal Prevalence (%)"),
                 legend.title = c("ln(Site Level Host Abundance)"),
                 title = bquote("Time T"[(-1)]),
                 colors = "bw",
                 ci.lvl = NA) + 
  scale_x_continuous(labels = seq(0, 10, 1),
                     breaks = seq(0, 10, 1)) +
  scale_y_continuous(limits = c(0, 0.04),
                     breaks = seq(0, 0.04, 0.01),
                     labels = scales::percent_format(accuracy = 1)) +
  ak_theme
p4c 

p4c.2 <- plot_model(model4c, type = "eff", terms = c("temp_date_t2", "soilMoisture_date_t2"),
                    axis.title = c("Temperature (°C)", "Bsal prevalence (%)"),
                    legend.title = bquote("Soil Moisture (kg/m"^2~")"),
                    title = bquote("Time T"[(-2)]),
                    show.data=FALSE,
                    colors = "bw",
                    ci.lvl = NA) + 
  scale_x_continuous(labels = seq(-10, 30, 10),
                     breaks = seq(-10, 30, 10)) +
  scale_y_continuous(limits = c(0.005, 0.03),
                     breaks = seq(0.005, 0.03, 0.005),
                     labels = scales::percent_format(accuracy = 0.1)) +
  ak_theme
p4c.2
p4c_combined <- p4c/p4c.2
p4c_combined


#### Only Species with Bsal* no Site as random -- Run w/ species that have ever had Bsal. ####
model5 <- glmmTMB(BsalDetected ~ logsiteAbun*richness + scale(temp_date)*scale(soilMoisture_date) +
                    (1|scientific),
                  family = "binomial",
                  data = subset(d_alpestris, BsalDetected == 1),
                  control = glmmTMBControl(optimizer = optim,
                                           optArgs = list(method = "BFGS")))

summary(model5)
Anova(model5)
m5_sim <- simulateResiduals(model5)
testResiduals(m5_sim, plot = T)

p5a <-plot_model(
  model4, 
  type = "pred", 
  terms = c("richness", "logsiteAbun"),
  axis.title = c("Host richness", "Bsal prevalence across all salamander species"),
  legend.title = c("Ln site-level host abundance"),
  title = "",
  colors = "bw",
  ci.lvl = NA
)
p5a

p5b <- plot_model(
  modelb, 
  type = "pred", 
  terms = c("temp_date", "soilMoist_date"),
  axis.title = c("Temperature (C)", "Bsal prevalence across all salamander species"),
  legend.title=c("Soil Moisture (kg/m^2"),
  title = "",
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
p5b


#### Fatality models with Fire Salamanders only. ####
# With weather data
model6a <- glmmTMB(fatal ~ diseaseDetected*scale(temp_date),
                   family = "binomial",
                   data = subset(d, scientific =="Salamandra salamandra"),
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))

summary(model6a)
Anova(model6a)
m6a_sim <- simulateResiduals(model6a)
testResiduals(m6a_sim, plot = T)

# With climate data (annual trends)
model6b <- glmmTMB(fatal ~ diseaseDetected*scale(bio1),
                   family = "binomial",
                   data = subset(d, scientific =="Salamandra salamandra"),
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))

summary(model6b)
Anova(model6b)
m6b_sim <- simulateResiduals(model6b)
testResiduals(m6b_sim, plot = T)

# With climate data (monthly trends)
model6c <- glmmTMB(fatal ~ diseaseDetected*scale(tavg),
                   family = "binomial",
                   data = subset(d, scientific =="Salamandra salamandra"),
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))

summary(model6c)
Anova(model6c)
m6c_sim <- simulateResiduals(model6c)
testResiduals(m6c_sim, plot = T)

## Model 6 plots
# Weather data
p6a <- plot_model(
  model6a, 
  type = "pred", 
  terms = c("temp_date", "diseaseDetected"),
  axis.title = c("Temperature (C)", "Fire salamander fatality prevalence"),
  legend.title=c("Disease present"),
  title = "",
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
p6a

# Average annual temp.
p6b <- plot_model(
  model6b, 
  type = "pred", 
  terms = c("bio1", "diseaseDetected"),
  axis.title = c("Avg. Annual Temperature (C)", "Fire salamander fatality prevalence"),
  legend.title=c("Disease present"),
  title = "",
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
p6b

# Average monthly temp.
p6b <- plot_model(
  model6c, 
  type = "pred", 
  terms = c("tavg", "diseaseDetected"),
  axis.title = c("Avg. Monthly Temperature (C)", "Fire salamander fatality prevalence"),
  legend.title=c("Disease present"),
  title = "",
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
p6b



#### Cbind full model with "scientific" as random effect ####
cbind_m1 <- glmmTMB(cbind(YesBsal, NoBsal) ~ logsiteAbun*richness + scale(temp_date_t1)*scale(soilMoisture_date_t1) +
                      (1|scientific),
                    family = "binomial",
                    data = dcbind_alpestris,
                    control = glmmTMBControl(optimizer = optim,
                                             optArgs = list(method = "BFGS")))

summary(cbind_m1)
Anova(cbind_m1)
cbind_m1_sim <- simulateResiduals(cbind_m1)
testResiduals(cbind_m1_sim, plot = T)

cb_p1a <-plot_model(
  cbind_m1, 
  type = "pred", 
  terms = c("richness", "logsiteAbun"),
  axis.title = c("Host richness", "Bsal prevalence across all salamander species"),
  legend.title = c("Ln site-level host abundance"),
  title = "",
  colors = "bw",
  ci.lvl = NA
)
cb_p1a

cb_p1b <- plot_model(
  cbind_m1, 
  type = "pred", 
  terms = c("temp_date_t1", "soilMoisture_date_t1"),
  axis.title = c("Temperature (C)", "Bsal prevalence across all salamander species"),
  legend.title=c("Soil Moisture (kg/m^2)"),
  title = "",
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
cb_p1b


#### Cbind model on fire salamanders only. ####
## With weather data
cbind_m2a <- glmmTMB(cbind(YesBsal, NoBsal) ~ logsiteAbun*richness +
                       scale(temp_date)*scale(soilMoisture_date),
                     family = "binomial",
                     data = subset(dcbind, scientific =="Salamandra salamandra"),
                     control = glmmTMBControl(optimizer = optim,
                                              optArgs = list(method = "BFGS")))

summary(cbind_m2a)
Anova(cbind_m2a)
cbind_m2a_sim <- simulateResiduals(cbind_m2a)
testResiduals(cbind_m2a_sim, plot = T)


## With climate data (annual trends)
cbind_m2b <- glmmTMB(cbind(YesBsal, NoBsal) ~ logsiteAbun*richness +
                       scale(bio1)*scale(bio12),
                     family = "binomial",
                     data = subset(dcbind, scientific =="Salamandra salamandra"),
                     control = glmmTMBControl(optimizer = optim,
                                              optArgs = list(method = "BFGS")))

summary(cbind_m2b)
Anova(cbind_m2b)
cbind_m2b_sim <- simulateResiduals(cbind_m2b)
testResiduals(cbind_m2b_sim, plot = T)

## With climate data (monthly trends)
cbind_m2c <- glmmTMB(cbind(YesBsal, NoBsal) ~ logsiteAbun*richness +
                       scale(tavg)*scale(prec),
                     family = "binomial",
                     data = subset(dcbind, scientific =="Salamandra salamandra"),
                     control = glmmTMBControl(optimizer = optim,
                                              optArgs = list(method = "BFGS")))

summary(cbind_m2c)
Anova(cbind_m2c)
cbind_m2c_sim <- simulateResiduals(cbind_m2c)
testResiduals(cbind_m2c_sim, plot = T)


## Fire salamander only Cbind plots (cbind_m2 models)
cbp2a <- plot_model(
  cbind_m2a, 
  type = "pred", 
  terms = c("temp_date", "soilMoisture_date"),
  axis.title = c("Temperature (C)", "Bsal prevalence in fire salamanders"),
  legend.title=c("Soil Moisture (kg/m^2)"),
  title = "",
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
cbp2a

cbp2b <- plot_model(
  cbind_m2b, 
  type = "pred", 
  terms = c("bio1", "bio12"),
  axis.title = c("Avg. Annual Temperature (C)", "Bsal prevalence in fire salamanders"),
  legend.title=c("Avg. Annual Precip (cm)"),
  title = "",
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
cbp2b

cbp2c <- plot_model(
  cbind_m2c, 
  type = "pred", 
  terms = c("tavg", "prec"),
  axis.title = c("Avg. Monthly Temperature (C)", "Bsal prevalence in fire salamanders"),
  legend.title=c("Avg. Monthly Precip (cm)"),
  title = "",
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
cbp2c



#### Cbind fatality models with Fire Salamanders only. ####
# With weather data
cbind_m3a <- glmmTMB(fatal ~ cbind(YesBsal, NoBsal)*scale(temp_date)*scale(soilMoisture_date),
                     family = "binomial",
                     data = subset(dcbind, scientific =="Salamandra salamandra"),
                     control = glmmTMBControl(optimizer = optim,
                                              optArgs = list(method = "BFGS")))


summary(cbind_m3a)
Anova(cbind_m3a)
cbind_m3a_sim <- simulateResiduals(cbind_m3a)
testResiduals(cbind_m3a_sim, plot = T)

# With climate data (average annual temp)
cbind_m3b <- glmmTMB(fatal ~ YesBsal*scale(bio1)*scale(bio12),
                     family = "binomial",
                     data = subset(dcbind, scientific =="Salamandra salamandra"),
                     control = glmmTMBControl(optimizer = optim,
                                              optArgs = list(method = "BFGS")))


summary(cbind_m3b)
Anova(cbind_m3b)
cbind_m3b_sim <- simulateResiduals(cbind_m3b)
testResiduals(cbind_m3b_sim, plot = T)

# With climate data (average monthly temp)
cbind_m3c <- glmmTMB(fatal ~ YesBsal*scale(tavg)*scale(prec),
                     family = "binomial",
                     data = subset(dcbind, scientific =="Salamandra salamandra"),
                     control = glmmTMBControl(optimizer = optim,
                                              optArgs = list(method = "BFGS")))


summary(cbind_m3c)
Anova(cbind_m3c)
cbind_m3c_sim <- simulateResiduals(cbind_m3c)
testResiduals(cbind_m3c_sim, plot = T)

## Cbind Model 3 
cbp3a <- plot_model(
  cbind_m3a, 
  type = "pred", 
  terms = c("temp_date", "YesBsal", "soilMoisture_date"),
  axis.title = c("Temperature (C)", "Fire salamander fatality prevalence"),
  legend.title=c("Bsal present"),
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
cbp3a

cbp3b <- plot_model(
  cbind_m3b, 
  type = "pred", 
  terms = c("bio1", "bio12", "YesBsal"),
  axis.title = c("Avg. Annual Temperature (C)", "Fire salamander fatality prevalence"),
  legend.title=c("Bsal present"),
  title = "",
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
cbp3b

cbp3c <- plot_model(
  cbind_m3c, 
  type = "pred", 
  terms = c("tavg", "YesBsal"),
  axis.title = c("Avg. Monthly Temperature (C)", "Fire salamander fatality prevalence"),
  legend.title=c("Bsal present"),
  title = "",
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
cbp3c


















































# preferred model
model2cz <- glmmTMB(diseaseDetected ~ logsiteAbun*richness + scale(bio1)*scale(bio12cm) +                    (1|scientific),
                    family = "binomial",
                    data = d_alpestris,
                    control = glmmTMBControl(optimizer = optim,
                                             optArgs = list(method = "BFGS")))

summary(model2cz)
Anova(model2cz)

p2cz <-plot_model(
  model2cz, 
  type = "pred", 
  terms = c("richness", "logsiteAbun"),
  axis.title = c("Host richness", "Bsal prevalence across all salamander species"),
  legend.title = c("Ln site-level host abundance"),
  title = "",
  colors = "bw",
  ci.lvl = NA
)
p2cz

p3cz <- plot_model(
  model2cz, 
  type = "pred", 
  terms = c("bio1", "bio12cm"),
  axis.title = c("Temperature (C)", "Bsal prevalence across all salamander species"),
  legend.title=c("Annual precip. (cm)"),
  title = "",
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
p3cz


## preferred model 2
model2ecbind <- glmmTMB(cbind(YesBsal, NoBsal) ~ logsiteAbun*richness + (bio1)*(bio12cm) +                                                  (1|scientific),
                        family = "binomial",
                        data = dcbind_alpestris,
                        control = glmmTMBControl(optimizer = optim,
                                                 optArgs = list(method = "BFGS")))

summary(model2ecbind)
Anova(model2ecbind)

p2ecbind <-plot_model(
  model2ecbind, 
  type = "pred", 
  terms = c("richness", "logsiteAbun"),
  axis.title = c("Host richness", "Bsal prevalence across all salamander species"),
  legend.title = c("Ln site-level host abundance"),
  title = "",
  show.data =FALSE,
  colors = "bw",
  ci.lvl = NA
)
p2ecbind

p3ecbind <- plot_model(
  model2ecbind, 
  type = "pred", 
  terms = c("bio1", "bio12cm"),
  axis.title = c("Temperature (C)", "Bsal prevalence across all salamander species"),
  legend.title=c("Annual precip. (cm)"),
  title = "",
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
p3ecbind


## Fire salamanders only
model3.salamandra <- glmmTMB(cbind(YesBsal, NoBsal) ~ logsiteAbun*richness + scale(bio1)*scale(bio12cm),
                             family = "binomial",
                             data = subset(dcbind, species=="salamandra"),
                             control = glmmTMBControl(optimizer = optim,
                                                      optArgs = list(method = "BFGS")))

summary(model3.salamandra)
Anova(model3.salamandra)


p1.salamandra <- plot_model(
  model3.salamandra, 
  type = "pred", 
  terms = c("bio1", "bio12cm"),
  axis.title = c("Temperature (C)", "Bsal prevalence in fire salamanders"),
  legend.title=c("Annual precip.(cm)"),
  title = "",
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
p1.salamandra








summary(model3.salamandra)
Anova(model3.salamandra)


p1.salamandra <- plot_model(
  model3.salamandra, 
  type = "pred", 
  terms = c("bio1", "bio12cm"),
  axis.title = c("Temperature (C)", "Bsal prevalence in fire salamanders"),
  legend.title=c("Annual precip.(cm)"),
  title = "",
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
p1.salamandra




