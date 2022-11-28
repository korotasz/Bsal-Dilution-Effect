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
        plot.title = element_text(hjust = 0.5, size = 32, face = "plain"),
        plot.margin = margin(6, 12, 2, 2, "pt"), 
        legend.position = "bottom", legend.text = element_text(size = 14),
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

# log transform vars
d$logsppAbun <- log(d$sppAbun)
d$logsiteAbun <- log(d$siteAbun)
dcbind$logsppAbun <- log(dcbind$sppAbun)
dcbind$logsiteAbun <- log(dcbind$siteAbun)
d$scientific <- as.factor(d$scientific)

# Remove non-native species
dcbind_alpestris <- subset(dcbind, species != "alpestris")
d_alpestris <- subset(d, species != "alpestris")

# Remove Jaime's data 
d_noJB <- subset(d, collectorList != "Jaime Bosch")
dcbind_noJB <- subset(dcbind, collectorList != "Jaime Bosch")



#### Simple model using all data to see how abundant each species is at each site. ####
## log Spp Abundance ~ Scientific + (1|Site)
model1 <- glmmTMB(logsppAbun ~  scientific + (1|Site),
                  data = d,
                  control = glmmTMBControl(optimizer = optim,
                         optArgs = list(method = "BFGS")))
summary(model1)
Anova(model1)

m1 <- ggpredict(model1, terms = "scientific")

p1 <-ggplot(m1, aes(x , predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  ylab("ln(Species Abundance)") +
  xlab("Species") + ak_theme + theme(axis.title.x = element_text(size = 24, hjust = 0.9),
                                     axis.title.y = element_text(size = 24, hjust = 0.9))
p1

#### Model to see how disease prevalence (both Bd and Bsal) at each site is influenced by species presence. ####
## Bsal only model had issues converging
model2 <- glmmTMB(diseaseDetected ~  scientific + (1|Site),
                  data = d, family = "binomial",
                  control = glmmTMBControl(optimizer = optim,
                                           optArgs = list(method = "BFGS")))
summary(model2)
Anova(model2)

m2 <- ggpredict(model2, terms = "scientific")

p2 <- ggplot(m2, aes(x , (predicted * 100))) +
  geom_point() +
#  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  ylab("Disease Prevalence (%)") +
  xlab("Species") + ak_theme + theme(axis.title.x = element_text(size = 24, hjust = 0.9),
                                     axis.title.y = element_text(size = 24, hjust = 0.9))
p2

#### Models to test whether the most susceptible hosts (susceptibility score 3) are the most abundant ####
##   and whether the least susceptible or resistant hosts (susceptibility score 1) are rare hosts.  

## T0 (Weather data from the date of sample observation)
model3a <- glmmTMB(logsppAbun ~  as.factor(susceptibility) + scale(temp_date)*scale(soilMoisture_date) + (1|Site),
                   data = d,
                   control = glmmTMBControl(optimizer = optim,
                         optArgs = list(method = "BFGS")))
summary(model3a)
Anova(model3a)
m3a <- ggpredict(model3a, terms = c("susceptibility", "temp_date[5, 15, 25]", "soilMoisture_date[1, 3, 6]"))

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


## T-1 (Weather data from one month prior (i.e., 30 days) to the date of sample observation) 
model3b <- glmmTMB(logsppAbun ~  as.factor(susceptibility) + scale(temp_date_t1)*scale(soilMoisture_date_t1) + (1|Site),
                   data = d,
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))
summary(model3b)
Anova(model3b)
m3b <- ggpredict(model3b, terms = c("susceptibility", "temp_date_t1[5, 15, 25]", "soilMoisture_date_t1[1, 3, 6]"))

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


## T-2 (Weather data from two months prior (i.e., 30 days) to the date of sample observation) 
model3c <- glmmTMB(logsppAbun ~  as.factor(susceptibility) + scale(temp_date_t2)*scale(soilMoisture_date_t2) + (1|Site),
                   data = d,
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))
summary(model3c)
Anova(model3c)
m3c <- ggpredict(model3c, terms = c("susceptibility", "temp_date_t2[5, 15, 25]", "soilMoisture_date_t2[1, 3, 6]"))

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

## Abundance ~ Susceptibility + Temp*Precip 
model3d <- glmmTMB(logsppAbun ~  as.factor(susceptibility) + scale(tavg)*scale(prec) + (1|Site),
                   data = d,
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))
summary(model3d)
Anova(model3d)
#m3d <- ggpredict(model3d, terms = c("susceptibility", "bio1[5, 10, 15]", "bio12[70, 85, 100]"))
m3d <- ggpredict(model3d, terms = c("susceptibility", "tavg[5, 10, 15]", "prec[5, 7, 9]"))

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

## Full model at time T-1 (Weather data from one month prior (i.e., 30 days) to the date of sample observation)
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


## Full model at time T-2 (Weather data from one month prior (i.e., 60 days) to the date of sample observation)
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
                    title = bquote("Time T"[(-1)]),
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





model2

































































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




