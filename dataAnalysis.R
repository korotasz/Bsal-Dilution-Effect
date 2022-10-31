library(dplyr)
library(tidyverse)      
library(corrplot)
library(glmmTMB)
library(lme4)
library(sjPlot)
library(DHARMa)
library(ggplot2)        
library(viridis)        
library(extrafont)
library(hrbrthemes)


setwd('C:/Users/alexi/OneDrive/Documents/01_GradSchool/_Dissertation work/Chapter4/03_code')

d <- read.csv("bsalprev_final.csv", header = T, encoding = "UTF-8")

#################################
## Model fitting and selection ##
#################################
## Include Pelophylax perezi data  (5 rows) into Pelophylax sp.
d$scientific <- gsub(d$scientific, pattern = "Pelophylax perezi",
                     replacement = "Pelophylax sp.")

temp <- d %>%
  group_by(scientific, Site) %>%
  dplyr::summarize(count = n())



## Community assembly visualization 
#library(remotes)
#remotes::install_version("Rttf2pt1", version = "1.3.8")
#extrafont::font_import()
#font_import()
#loadfonts(device = "win")

# spp. richness
temp <-d %>%
  group_by(richness, scientific, Site) %>%
  dplyr::summarize(count = n()) %>%
  mutate(freq = (round(count/sum(count), 3))) %>% 
  arrange(richness)


ggplot(temp, aes(x = richness, y = scientific, size=freq, colour=scientific)) +
  geom_jitter(alpha=0.45) +
  scale_size_continuous(range = c(1,10)) +
  scale_colour_viridis(option="plasma", discrete = TRUE) +
  ylab("Species") +
  xlab("Species richness") +
  scale_x_discrete(breaks = c("1","2","3","4","5","6","7","8","9"), 
                   labels = c("1","2","3","4","5","6","7","8","9"),
                   limits = c("1","2","3","4","5","6","7","8","9")) +
  theme_ipsum(axis_title_size = 14) +
  theme(legend.position = "none")
# need to add scale bar for frequency at each site (size of spheres), 
# but it is roughly the same as Piet's

# spp. abundance
temp2 <- d %>%
  dplyr::select(Site, scientific, individualCount) # subset relevant data
temp2 <- aggregate(individualCount ~ scientific+Site, temp2, sum)
names(temp2)[names(temp2) == 'individualCount'] <- 'Abundance'
meanAbun <- temp2%>%
  mutate(logAbun = log(Abundance)) %>%
  group_by(scientific) %>%
  dplyr::summarise(meanAbun = mean(logAbun), 
                   sd = sd(logAbun), 
                   se = sd/sqrt(n())) %>%
  ungroup()

ggplot(meanAbun, aes(x = scientific, y = meanAbun)) +
  geom_bar(aes(fill = scientific), stat = "identity", alpha=0.3) +
  geom_errorbar(aes(ymin = meanAbun-se, ymax=meanAbun+se), 
                width=0.7, alpha=1, size=0.5) +
  coord_flip()+
  scale_colour_viridis(option="plasma", discrete = TRUE) +
  scale_fill_viridis(option="plasma", discrete = TRUE) +
  ylab("Log species abundance") +
  xlab("Species") +
  theme_ipsum(axis_title_size = 14) +
  theme(legend.position = "none")

## Rank abundance plot
temp2 <- temp2 %>%
  pivot_wider(names_from = scientific,
              values_from = Abundance, values_fill = 0) 

temp2 <- as.data.frame(temp2)
temp2 <- temp2[,-1]

xr <- rankabundance(temp2)

par(mfrow=c(2,2))
p1 <- rankabunplot(xr, addit=F, labels="", scale = "logabun", scaledx = T,
                   specnames=1)
p2 <- rankabunplot(xr, addit=F, labels="", scale = "logabun", scaledx = T,
                   specnames=2)
p3 <- rankabunplot(xr, addit=F, labels="", scale = "logabun", scaledx = T,
                   specnames=3)
p4 <- rankabunplot(xr, addit=F, labels="", scale = "logabun", scaledx = T,
                   specnames=4)


## Model for Bsal presence
d$logsppAbun <- log(d$sppAbun)
d$logsiteAbun <- log(d$siteAbun)
d$diseaseDetected <- as.factor(d$diseaseDetected)
d$susceptibility <- as.factor(d$susceptibility)
d$yearCollected <- as.factor(d$yearCollected)



m1a <- glmmTMB(diseaseDetected ~ logsiteAbun*richness + susceptibility +
                 (1|Site) + (1|scientific),
               family = "binomial",
               data = d,
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))

summary(m1a)
Anova(m1a)
plot_model(m1a, type = "int")
plot_model(
  m1a, 
  type = "pred", 
  terms = c("richness", "logsiteAbun"), 
  colors = "bw",
  ci.lvl = NA
)

m1b <- glmmTMB(diseaseDetected ~ logsppAbun*richness + susceptibility +
                 (1|Site) + (1|scientific),
               family = "binomial",
               data = d,
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))

summary(m1b)
Anova(m1b)
plot_model(m1b, type = "int")
plot_model(
  m1b, 
  type = "pred", 
  terms = c("richness", "logsiteAbun"), 
  colors = "bw",
  ci.lvl = NA
)


################################################################################
# See how many T. marmoratus there are and where they're from
temp <- d %>%
  group_by(scientific, country) %>%
  dplyr::summarize(count = n())

# new df without T. marmoratus from Spain
d2 <- d %>%
  dplyr::filter(scientific != "Triturus marmoratus" | country != "Spain") 



m2a <- glmmTMB(diseaseDetected ~ logsiteAbun*richness + susceptibility +
                 (1|Site) + (1|scientific),
               family = "binomial",
               data = subset(d2),
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))

summary(m2a)
Anova(m2a)
plot_model(
  m2a, 
  type = "pred", 
  terms = c("richness", "logsiteAbun"), 
  colors = "bw",
  ci.lvl = NA
)



m2b <- glmmTMB(diseaseDetected ~ logsppAbun*richness + susceptibility +
                 (1|Site) + (1|scientific),
               family = "binomial",
               data = subset(d2),
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))

summary(m2b)
Anova(m2b)
plot_model(
  m2b, 
  type = "pred", 
  terms = c("richness", "logsiteAbun"), 
  colors = "bw",
  ci.lvl = NA
)


plot_model(
  m2b, 
  type = "pred", 
  terms = c("susceptibility", "logsppAbun"), 
  colors = "bw",
  ci.lvl = NA
)

################################################################################
## Table showing species at each site -- some sites could be biased towards 
## more susceptible species
siterich <- d2 %>%
  group_by(Site, richness, scientific, diseaseDetected) %>%
  dplyr::summarize(count = n()) %>%
  filter(scientific == "Ichthyosaura alpestris") # Is categorized as "tolerant" (2)
View(siterich)

test <- glmmTMB(diseaseDetected ~ logsppAbun*richness + susceptibility,
                family = "binomial",
                data = subset(d, species!="alpestris"),
                control = glmmTMBControl(optimizer = optim,
                                         optArgs = list(method = "BFGS")))

summary(test)
Anova(test)
plot_model(
  test, 
  type = "pred", 
  terms = c("richness", "logsppAbun"), 
  colors = "bw",
  ci.lvl = NA
)


m5 <- glmmTMB(diseaseDetected ~ logsppAbun*richness + susceptibility,
              family = "binomial",
              data = subset(d, species!="alpestris"),
              control = glmmTMBControl(optimizer = optim,
                                       optArgs = list(method = "BFGS")))

summary(test)
Anova(test)
plot_model(
  test, 
  type = "pred", 
  terms = c("richness", "logsppAbun"), 
  colors = "bw",
  ci.lvl = NA
)



# Confidence bands are still wildly large
# backtransform model
# subset to center 80th percentile; clip bottom&top 5/10%


# Including some climatic variables
m3 <- glmmTMB(diseaseDetected ~ logsiteAbun*richness + bio1*bio12 + susceptibility +
                (1|Site) + (1|scientific),
              family = "binomial",
              data = subset(d, species!="alpestris"),
              control = glmmTMBControl(optimizer = optim,
                                       optArgs = list(method = "BFGS")))
## bio1 = annual mean temp 
## bio12 = annual precip


summary(m3)
Anova(m3)

plot_model(
  m3, 
  type = "pred", 
  terms = c("bio1", "bio12"),
  colors = "bw",
  ci.lvl = 0.95
)

plot_model(
  m3, 
  type = "pred", 
  terms = c("richness", "logsiteAbun"),
  colors = "bw",
  ci.lvl = NA
)

tab_model(nosus)



# convert bio12 from mm to cm
d$bio12 <- (d$bio12*.1)
nosus <- glmmTMB(diseaseDetected ~ logsiteAbun*richness + bio1*bio12 +
                   (1|Site) + (1|scientific),
                 family = "binomial",
                 data = subset(d, species!="alpestris"),
                 control = glmmTMBControl(optimizer = optim,
                                          optArgs = list(method = "BFGS")))
## bio1 = annual mean temp 
## bio12 = annual precip


summary(nosus)
Anova(nosus)

#simulationOutput <- simulateResiduals(fittedModel = nosus)
#plot(simulationOutput)
#testZeroInflation(simulationOutput)
#testDispersion(simulationOutput)

## drop susceptibility 
## pretty graphs
## print ANOVA 
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



p1 <- plot_model(
  nosus, 
  type = "pred", 
  terms = c("bio1", "bio12"),
  colors = "bw",
  ci.lvl = NA
)
p1

p2 <-plot_model(
  nosus, 
  type = "pred", 
  terms = c("richness", "logsiteAbun"),
  colors = "bw",
  ci.lvl = NA
)
p2


## Bio1 and Bio12
plot1 <- p1 + geom_line(aes(x = x, y = predicted, color = group, linetype = group), size = 1.5) +
  scale_color_manual(labels = c("7.46", "8.73", "10"),
                     values = c("#b8bea3","#677f6e", "#193840")) +
  labs(x = expression(paste("Annual mean temperature", " (°C)")), y = expression(paste(italic("Bsal"), " prevalence (%)")), title = NULL) +
  scale_y_continuous(breaks = c(0, 0.02, 0.04, 0.06, 0.08), labels = scales::percent) +
  ak_theme +
  guides(color = guide_legend('Annual mean precipitation (cm)'), linetype = guide_legend('Annual mean precipitation (cm)'))

plot1


## Richness and log(site abundance)
scientific_10 <- function(x) {ifelse(x==0, "0", parse(text=gsub("[+]", "", 
                                                                gsub("e", " %*% 10^", 
                                                                     scientific_format()(x)))))}

plot2 <- p2 + geom_line(aes(x = x, y = predicted, color = group, linetype = group), size = 1.5) +
  scale_color_manual(labels = c("1.8", "2.87", "3.93"),
                     values = c("#f9c459","#ff9265", "#c8355e")) +
  labs(x = "Richness", y = expression(paste(italic("Bsal"), " prevalence (%)")), title = NULL) +
  scale_y_continuous(limits = c(0, 0.00003),
                     labels = scientific_10) +
  ak_theme +
  guides(color = guide_legend('Log(site abundance)'), linetype = guide_legend('Log(site abundance)'))

plot2












# new df with only fire salamanders
d2 <- d %>%
  dplyr::filter(scientific == "Salamandra salamandra")



m4 <- glmmTMB(diseaseDetected ~ logsiteAbun*richness + bio1*bio12 + susceptibility +
                (1|Site),
              family = "binomial",
              data = d2,
              control = glmmTMBControl(optimizer = optim,
                                       optArgs = list(method = "BFGS")))




m4_plot <- plot_model(
  m3, 
  type = "pred", 
  terms = c("richness"),
  colors = "bw",
  ci.lvl = 
)

### Graphs
ak_theme <- theme_minimal() +
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5)),
        axis.title = element_text(size = 24, face = "plain"),
        plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),
        legend.position = "bottom", legend.text = element_text(size = 12),
        legend.title = element_text(size = 16, face = "bold"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'))

plot1 <- m3_plot + geom_line(aes = richness) +
  ylab("Bsal prevalence") +
  xlab("Richness") +
  ak_theme
plot1

plot2 <- 
  
  
  
  
  
  ggsave(file = "DEHPlot.png", plot = plot1, dpi = 300, path = 'C:/Users/alexi/OneDrive/Documents/01_GradSchool/_Dissertation work/Chapter1/03_output')



m4 <- glmmTMB(diseaseDetected ~ logsiteAbun*richness + bio1*bio12 + susceptibility +
                (1|Site) + (1|scientific),
              family = "binomial",
              data = subset(d2, species!="alpestris"),
              control = glmmTMBControl(optimizer = optim,
                                       optArgs = list(method = "BFGS")))



summary(m4)
Anova(m4)
plot_model(
  m4, 
  type = "pred", 
  terms = c("richness", "logsiteAbun"),
  colors = "bw",
  ci.lvl = NA
)


m5 <- glmmTMB(diseaseDetected ~ logsiteAbun*richness + bio1*bio12 + susceptibility,
              family = "binomial",
              data = subset(d2, species!="alpestris"),
              control = glmmTMBControl(optimizer = optim,
                                       optArgs = list(method = "BFGS")))



summary(m5)
Anova(m5)
plot_model(
  m4, 
  type = "pred", 
  terms = c("richness", "logsiteAbun"),
  colors = "bw",
  ci.lvl = NA
)









########################
## Variable selection ##
########################
## Subset climate vars from larger dataset
climatevars <- d %>%
  dplyr::select(tmin, tmax, prec, bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, 
                bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19) 

## Rescale climate vars between 0 and 1 to test for collinearity
climatevars <- apply(climatevars, MARGIN = 2, rescale)

cor <- cor(climatevars, method = "pearson")
corrplot(cor, method = "circle", type = "lower",
         tl.col = "black", tl.srt = 45,
         cl.ratio = 0.2, col = COL2("BrBG"))

testcor <- cor.mtest(cor, conf.level = 0.95) # include p-value
posCor <- corrplot(cor, p.mat = testcor$p, sig.level = 0.05, insig = "blank",
                   method = "circle", type = "lower",
                   tl.col = "black", tl.srt = 45,
                   addCoef.col = "black", number.cex = 0.8,
                   cl.ratio = 0.2, col = COL2("BrBG"))

## As you can see, there are many highly correlated vars.
## I plan to drop anything correlated above a 0.8 rsq value when 
## I can get my model to run without convergence issues.



## Model for Bsal load data (Spain only)
#spain <- d[which(d$country == "Spain"),]

#spain$Site <- as.factor(spain$Site)
#spain$susceptibility <- as.factor(spain$susceptibility)

#spain$logabun <- log(spain$abundance)

#spain %>%
#  group_by(scientific) %>%
#  dplyr::summarize(count = n()) 

# H. meridionalis was causing an issue as a fixed effect, ended up dropping it.

#m1 <- glmmTMB(quantityDetected ~ logabun + susceptibility + 
#                (1|Site) + (1|scientific),
#               family = "nbinom2", ziformula=~1,
#               data = spain,
#              control = glmmTMBControl(optimizer = optim,
#                                       optArgs = list(method = "BFGS")))
#summary(m1)

