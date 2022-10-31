library(tidyr)
library(lme4)	# glmer() function
library(car) # Anova() function
library(sjPlot) #plot_model() function


setwd('C:/Users/alexi/OneDrive/Documents/01_GradSchool/_Dissertation work/Chapter4/03_code')

d <- read.csv("bsal_analysis.csv", header = T, encoding = "UTF-8")


## Cbind model
## rescale/convert relevant vars
d$logsppAbun <- log(d$sppAbun)
d$logsiteAbun <- log(d$siteAbun)
d$NoBsal <- as.factor(d$NoBsal)
d$YesBsal <- as.factor(d$YesBsal)
d$susceptibility <- as.factor(d$susceptibility)
d$yearCollected <- as.factor(d$yearCollected)

m1 <- glmer(cbind(YesBsal, NoBsal) ~ logsppAbun*richness + susceptibility +
              (1|Site) + (1|scientific),
            family = "binomial", 
            data = d, na.action = na.omit)
Anova(m1)
plot_model(
  m1, 
  type = "pred", 
  terms = c("richness", "logsppAbun"),
  colors = "bw",
  ci.lvl = 0.95
)



## bio1 = annual mean temp 
## bio12 = annual precip

m2 <- glmer(cbind(YesBsal, NoBsal) ~ logsiteAbun*richness + susceptibility + bio12 +
            (1|Site) + (1|scientific),
            family = "binomial", 
            data = d, na.action = na.omit, 
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

Anova(m2)
plot_model(
  m2, 
  type = "pred", 
  terms = c("richness", "logsiteAbun"),
  colors = "bw",
  ci.lvl = 0.95
)


m3 <- glmer(cbind(YesBsal, NoBsal) ~ richness + scale(bio1)*scale(bio12) +
              (1|Site) + (1|scientific),
            family = "binomial", 
            data = subset(d, species!= "alpestris"), na.action = na.omit, 
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

Anova(m3)
plot_model(
  m3, 
  type = "pred", 
  terms = c("bio1", "bio12"),
  colors = "bw", 
  ci.lvl = 0.95
)


model <- glmer.nb(siteAbun ~ richness + susceptibility + (1|Site) + (1|scientific), 
              data = d)
Anova(model)
plot_model(
  model, 
  type = "pred", 
  terms = c("susceptibility"),
  colors = "bw",
  ci.lvl = 0.95
)






