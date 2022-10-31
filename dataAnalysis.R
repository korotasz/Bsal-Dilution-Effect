library(tidyverse)      
library(reshape2)       
library(plyr)
library(corrplot)
library(glmmTMB)
library(boot)
library(scales)
library(BiodiversityR)
library(ggplot2)        
library(viridis)        
library(hrbrthemes)
library(extrafont)


# library(bblme) not availble for this version of R

setwd('C:/Users/alexi/OneDrive/Documents/01_Grad.School/_Dissertation work/BsalData/03_code')

d <- read.csv("bsalprev_final.csv", header = T, encoding = "UTF-8")


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
d$logabun <- log(d$abundance)
d$logrich <- log(d$richness)
d$susceptibility <- as.factor(d$susceptibility)


m1 <- glmmTMB(diseaseDetected ~ logabun*richness + susceptibility +
               (1|Site) + (1|scientific),
               family = "nbinom2", ziformula=~1,
               data = d,
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))

summary(m1)

m1.diag <- glm.diag(m1) ##?? not working
glm.diag.plots(m1, m1.diag)


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

