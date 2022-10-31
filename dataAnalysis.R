library(dplyr)
library(tidyverse)
library(glmmTMB) # glmmTMB()
library(car) # Anova()
library(DHARMa) # simulateResiduals(), testZeroInflation(), testDispersion()
library(sjPlot) # plot_model()
library(ggplot2)
library(lme4)


setwd('E:/01_GradSchool/_DissertationWork/Chapter4/03_code')

d <- read.csv('bsal_analysis.csv', header = T, encoding = 'UTF-8')

## Define plot theme
ak_theme <- theme_minimal() +
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5)),
        axis.title = element_text(size = 24, face = "plain"),
        plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),
        legend.position = "bottom", legend.text = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'))


## Include Pelophylax perezi data  (5 rows) into Pelophylax sp.
d$scientific <- gsub(d$scientific, pattern = "Pelophylax perezi",
                     replacement = "Pelophylax sp.")

# log transform vars
d$logsppAbun <- log(d$sppAbun)
d$logsiteAbun <- log(d$siteAbun)

# Exclude non-native spp.
dcbind_alpestris<-subset(d, species!="alpestris")
d_alpestris<-subset(d, species!="alpestris")


# Model for how host susceptibility is impacted by species abundance
m1 <- glmmTMB(logsppAbun ~  as.factor(susceptibility) + (temp)*(soilmoist) + (1|Site),
                  data = d, control = glmmTMBControl(optimizer = optim,
                                                     optArgs = list(method = "BFGS")))
summary(model1)
Anova(model1)

# plot
p1 <- plot_model(
  model1, 
  type = "pred", 
  terms = c("susceptibility"),
  axis.title = c("Bsal susceptibility score", "Natural log of host abundance"),
  title = "",
  colors = "bw",
  ci.lvl = 0.95,
)
p1

# including random effects
m2 <- glmmTMB(diseaseDetected ~ logsiteAbun*richness + (temp)*(precip) + 
                (1|Site) + (1|scientific),
                  family = "binomial",
                  data = d_alpestris,
                  control = glmmTMBControl(optimizer = optim,
                                           optArgs = list(method = "BFGS")))

summary(model2)
Anova(model2)

# plot
p2 <-plot_model(
  model2, 
  type = "pred", 
  terms = c("richness", "logsiteAbun"),
  axis.title = c("Host richness", "Bsal prevalence across all salamander species"),
  legend.title = c("Ln site-level host abundance"),
  title = "",
  colors = "bw",
  ci.lvl = NA
)
p2

p3 <- plot_model(
  model2, 
  type = "pred", 
  terms = c("bio1", "bio12cm"),
  axis.title = c("Temperature (C)", "Bsal prevalence across all salamander species"),
  legend.title=c("Annual precip. (cm)"),
  title = "",
  show.data=FALSE,
  colors = "bw",
  ci.lvl = NA
)
p3

# Only spp. with Bsal
bsalOnly <- d %>%
  dplyr::select(BsalDetected == TRUE)






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




