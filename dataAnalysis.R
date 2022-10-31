library(dplyr)
library(tidyverse)
library(glmmTMB) # glmmTMB()
library(car) # Anova()
library(DHARMa) # simulateResiduals(), testZeroInflation(), testDispersion()
library(sjPlot) # plot_model()
library(ggplot2)



setwd('C:/Users/alexi/OneDrive/Documents/01_GradSchool/_Dissertation work/Chapter4/03_code')

d <- read.csv("bsalprev_final.csv", header = T, encoding = "UTF-8")



## Include Pelophylax perezi data  (5 rows) into Pelophylax sp.
d$scientific <- gsub(d$scientific, pattern = "Pelophylax perezi",
                     replacement = "Pelophylax sp.")

# log transform vars
d$logsppAbun <- log(d$sppAbun)
d$logsiteAbun <- log(d$siteAbun)

# make disease a factor
d$diseaseDetected <- as.factor(d$diseaseDetected)


## model
## bio1 = mean annual temp (C) 
## bio12 = mean annual precip (cm)
model1 <- glmmTMB(diseaseDetected ~ logsiteAbun*richness + bio1*bio12 +
                   (1|Site) + (1|scientific),
                 family = "binomial",
                 data = subset(d, species!="alpestris"),
                 control = glmmTMBControl(optimizer = optim,
                                          optArgs = list(method = "BFGS")))

summary(model1)
Anova(model1)
tab_model(model1)



## Check for zero-inflation and overdispersion
simulationOutput <- simulateResiduals(fittedModel = model1)
plot(simulationOutput)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)

## Define theme
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


## mean annual temp & mean annual precip
p1 <- plot_model(
  model1, 
  type = "pred", 
  terms = c("bio1", "bio12"),
  colors = "bw",
  ci.lvl = NA
)
p1

plot1 <- p1 + geom_line(aes(x = x, y = predicted, color = group, linetype = group), size = 1.5) +
  scale_color_manual(labels = c("74.63", "87.29", "99.95"),
                     values = c("#b8bea3","#677f6e", "#193840")) +
  labs(x = expression(paste("Annual mean temperature", " (°C)")), 
       y = expression(paste(italic("Bsal"), " prevalence (%)")), 
       title = NULL) +
#  scale_y_continuous(labels = scales::percent) +
  guides(color = guide_legend('Annual mean precipitation (cm)'), 
         linetype = guide_legend('Annual mean precipitation (cm)')) +
  ak_theme

plot1



## Richness and log(site abundance)
p2 <-plot_model(
  model1, 
  type = "pred", 
  terms = c("richness", "logsiteAbun"),
  colors = "bw",
  ci.lvl = NA
)
p2

#scientific_10 <- function(x) {ifelse(x==0, "0", parse(text=gsub("[+]", "", 
#                                                      gsub("e", " %*% 10^", 
#                                                      scales::scientific_format()(x)))))}

plot2 <- p2 + geom_line(aes(x = x, y = predicted, color = group, linetype = group), size = 1.5) +
  scale_color_manual(labels = c("1.8", "2.87", "3.93"),
                     values = c("#f9c459","#ff9265", "#c8355e")) +
  labs(x = "Richness", y = expression(paste(italic("Bsal"), " prevalence (%)")), title = NULL) +
#  scale_y_continuous(labels = scales::percent) +
  guides(color = guide_legend('Log(site abundance)'), linetype = guide_legend('Log(site abundance)')) +
  ak_theme

plot2


ggsave(file = "climaticVars.png", plot = plot1, dpi = 300, path = 'C:/Users/alexi/OneDrive/Documents/01_GradSchool/_Dissertation work/Chapter1/03_output')
ggsave(file = "sppRichness.png", plot = plot2, dpi = 300, path = 'C:/Users/alexi/OneDrive/Documents/01_GradSchool/_Dissertation work/Chapter1/03_output')




## Print Anova table
Anovatable <- Anova(model1)
Anovatable
















# new df with only fire salamanders
fs <- d %>%
  dplyr::filter(scientific == "Salamandra salamandra")





