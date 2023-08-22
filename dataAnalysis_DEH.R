#remotes::install_version("Rttf2pt1", version = "1.3.8") # install this version, latest ver. not compatible
#remotes::install_github("gorkang/html2latex") # convert sjPlot::tab_model() hmtl table to tex and pdf in .Rmd docs
#extrafont::font_import("C:/Windows/Fonts") # load fonts before ggplot2; only need to do this once
require(pacman)
require(rstudioapi) # Set working directory to current file location
require(extrafontdb)
require(extrafont) 
extrafont::loadfonts(device = "all", quiet = T) # plot fonts 


#### Visualization Packages ####
pckgs <- c("ggsignif", # adds labels to significant groups
             "ggtext", # for text type/arrangements w/ ggplot2
           "Rttf2pt1", # to use with the extrafont package
           "ggthemes", # contains 'scales', 'themes', and 'geoms' packages
          "grDevices", # saves high quality svg, pdf, and ps files
           "graphics", # dependency of grDevices
            "cowplot", # arranging plots/figs
          "gridExtra", # arranging plots/figs
          "patchwork", # arranging plots/figs
         "hrbrthemes", # plot colors
       "RColorBrewer", # plot colors
            "viridis", # plot colors
             "scales", # plot customization
           "eurostat", # obtain spatial data from europe
            "geodata", # obtain geographic data (world map)
              "ggmap", # creates maps
          "ggspatial", # north arrow and scale bar
            "mapproj", # apply map projection
         "scatterpie", # add pie charts to maps
             "ggpubr", # prepares plots to be ready for publication
          "latex2exp", # allows use of LaTeX in R
               "glue", # allows concatenation of LaTeX and R syntax
             "sjPlot", # plot_model(), tab_model()
               "ragg", # converts plots to tiff files
          "htmltools", # visualizes model outputs as html tables
           "webshot2", # converts html files to png
             "magick", # image_read() -- needed for cowplot::draw_image
              "Hmisc", # binconf() provides CI of binomial distribution
#### Analysis Specific Packages ####
          "tidyverse", # data wrangling/manipulation
            "glmmTMB", # glmmTMB()
            "emmeans", # lsmeans()
                "car", # Anova()
             "DHARMa", # simulateResiduals(), testZeroInflation(), testDispersion() 
              "MuMIn", # model.sel()
          "ggeffects", # ggpredict()
               "epiR", # calculate prevalence & CIs
             "sjmisc"  # data and variable transformation
)

## Load packages
pacman::p_load(pckgs, update = FALSE, character.only = T)


## Set working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
csvpath <- (path.expand("/csvFiles"))
figpath <- (path.expand("figures"))
setwd(file.path(dir, csvpath))

## Set plot theme 
ak_theme <- theme_ipsum() +
  theme(axis.text.x = element_text(size = 26),
        axis.title.x = element_text(size = 34, hjust = 0.5, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0), 
                                    face = "plain"),
        axis.text.y = element_text(size = 26, face = "italic"),
        axis.title.y = element_text(size = 34, hjust = 0.5, 
                                    margin = margin(t = 0, r = 15, b = 0, l = 5), 
                                    face = "plain"),
        axis.ticks.length = unit(.25, "cm"),
        axis.ticks = element_blank(),
        plot.tag = element_text(size = 36, face = "bold"),
        plot.title = element_text(size = 42, hjust = 0.5, face = "plain"),
        plot.subtitle = element_markdown(size = 12, face = "plain"),
        plot.margin = margin(1, 1, 1.5, 1.2, "cm"), 
        plot.caption = element_markdown(hjust = 0, size = 14, face = "plain"),
        plot.caption.position = "plot",
        legend.position = "top", 
        legend.key.size = unit(2,"cm"), 
        legend.text.align = 1,
        legend.text = element_text(size = 28, hjust = -1),
        legend.title = element_text(size = 28, face = "bold"), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.spacing.y = unit(1.5,"cm"),
        strip.text = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.line = element_line(color = 'black'))


#### Load csv files ####
d <- read.csv("bsalData_clean.csv", header = T, encoding = "UTF-8")
dcbind <- read.csv("bsalData_cbind.csv", header = T, encoding = "UTF-8")



# log transform vars
d <- d %>%
  mutate(logsppAbun = log(sppAbun),
         logsiteAbun = log(siteAbun),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
  relocate(c(logsppAbun, logsiteAbun), .after = sppAbun) 


dcbind <- dcbind %>%
  mutate(logsppAbun = log(sppAbun),
         logsiteAbun = log(siteAbun),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
  relocate(c(logsppAbun, logsiteAbun), .after = sppAbun) 

## Data prep for cbind models
# Scale relevant vars
dcbindScaled <- dcbind %>%
  mutate_at(c("temp_d", "temp_m", "temp_m_t1", "temp_m_t2",
              "sMoist_d", "sMoist_m", "sMoist_m_t1", "sMoist_m_t2",
              "precip_m", "precip_m_t1", "precip_m_t2",
              "bio1_wc", "bio12_wc", "tavg_wc", "prec_wc"), 
            ~(scale(., center = T, scale = T %>% as.numeric)))


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
## NEED TO FIX!! Not working as of R v 4.3.1
create_dummy_col <- function(df){
  values <- c("Low", "Med", "High")
  keys <-  unique(df[,8])
  index <- setNames(as.list(values), keys)

  df$dummy <- dplyr::recode(as.list(df[,8]), !!!index)

  # df$dummy <- dplyr::mutate(df = as.factor(dplyr::recode(df[,6], !!!index)))

  return(df)
}


#### 1) Descriptive Figures ####################################################
## Figure 1. Data distribution map
obs <- d %>%
  dplyr::select(country, ADM0, decimalLatitude, decimalLongitude, diseaseTested,
                BsalDetected, BdDetected, individualCount) %>%
  plyr::mutate(BsalDetected = as.factor(dplyr::recode(BsalDetected,
                                                      "1" = "Bsal positive", 
                                                      "0" = "Bsal negative"))) %>%
  arrange(BsalDetected)

# Summarise number of observations from each country
sampSize <- obs %>%
  group_by(country, BsalDetected) %>%
  summarise(n = n())

# Get the coordinates of each country
country_lookup <- read.csv("countries.csv", stringsAsFactors = F)
names(country_lookup)[1] <- "country_code"

# Combine data
labs <- merge(x = sampSize, y = country_lookup, 
              by.x = "country", by.y = "name", all.x = T)

# obtain world map
worldmap <- map_data("world")

Euro_map <- ggplot() +
  geom_map(data = worldmap, map = worldmap,
           aes(map_id = region), col = "white", fill = "#B2BEB5") +
  scale_x_continuous(limits = c(-13, 15)) +
  scale_y_continuous(limits = c(35, 60)) +
  geom_point(data = obs, aes(x = decimalLongitude, y = decimalLatitude, 
                             fill = BsalDetected, shape = BsalDetected),
             alpha = 0.3, size = 4, stroke = 1, color = "gray30") +
  scale_fill_manual(values = c("gray40", "#C23113")) +
  scale_shape_manual(values = c(21, 24)) +
  coord_fixed() +
  labs(x = "Longitude", y = "Latitude") +
  geom_richtext(data = labs, aes(longitude, latitude, group = country, label = paste("n<sub>obs</sub> =", n)), 
                nudge_y = -0.25,stat = "identity", size = 8, fill = NA, label.color = NA, na.rm = FALSE, show.legend = NA) +
  ak_theme + theme(legend.title = element_blank(),
                   legend.position = "top",
                   legend.spacing = unit(1, "cm"), # Space legend labels
                   legend.key.size = unit(1,"cm"),
                   legend.text.align = 0,
                   legend.text = element_text(size = 28, hjust = 0),
                   axis.text.x = element_text(size = 28),
                   axis.title.x = element_text(size = 42, hjust = 0.5, 
                                               margin = margin(t = 10, r = 0, b = 0, l = 0), 
                                               face = "plain"),
                   axis.text.y = element_text(size = 28, face = "plain"),
                   axis.title.y = element_text(size = 42, hjust = 0.5, 
                                               margin = margin(t = 0, r = 15, b = 0, l = 5), 
                                               face = "plain"),) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

Euro_map

# ggsave("Euro_map.pdf", Euro_map, device = cairo_pdf, path = file.path(dir, figpath),
#        width = 2000, height = 2000, scale = 2, units = "px", dpi = 300, limitsize = F)



#### 2) Testing assumptions of the dilution effect hypothesis ##################
##      2a. Hosts differ in their reservoir competence.
prev <- d %>%
  group_by(scientific) %>%
  mutate(ncas_Bsal = sum(BsalDetected == 1), # number of pos. Bsal cases
         npop = sum(individualCount)) %>% # pop size (total # individuals/spp.)
  drop_na(date) %>%
  ungroup() %>%
  mutate(Bsal_prev = (ncas_Bsal/npop)*100) %>% # prevalence as a percentage
  dplyr::select(scientific, susceptibility, ncas_Bsal, Bsal_prev, npop) %>%
  unique() %>%
  dplyr::mutate(row_id = row_number()) %>%
  relocate(row_id, .before = scientific)


# Use binconf function to sample binomial distribution and get CIs for prevalence
bsal_ci <- binconf(prev$ncas_Bsal, prev$npop, method = "exact", return.df = T)

deu <- sampSize %>%
  filter(country == "Germany") %>%
  plyr::mutate(BsalDetected = as.factor(dplyr::recode(BsalDetected,
                                                      "Bsal positive" = "Pos", "Bsal negative" = "Neg"))) %>%
  pivot_wider(names_from = BsalDetected, values_from = n) %>%
  mutate(pop = sum(Pos, Neg),
         prev = round((Pos/pop)*100, 2))
print(deu$prev)

esp <- sampSize %>%
  filter(country == "Spain") %>%
  plyr::mutate(BsalDetected = as.factor(dplyr::recode(BsalDetected,
                                                      "Bsal positive" = "Pos", "Bsal negative" = "Neg"))) %>%
  pivot_wider(names_from = BsalDetected, values_from = n) %>%
  mutate(pop = sum(Pos, Neg),
         prev = round((Pos/pop)*100, 2))
print(esp$prev)


# Convert binconf point estimates + CIs to percentages for plotting
bsal_ci <-  bsal_ci %>%
  plyr::mutate(PointEst = round((PointEst*100), 1),
               Lower = round((Lower*100), 1),
               Upper = round((Upper*100), 1)) %>%
  dplyr::mutate(row_id = row_number()) %>%
  relocate(row_id, .before = PointEst)


# Join with original 'prev' dataset, so we can plot actual vs expected
prev <- prev %>% 
  left_join(., bsal_ci, by = "row_id")

prev_binconf_plot <- ggplot(prev, aes(scientific, sapply(PointEst, FUN = function(x) ifelse(x == 0.0, round(x, 0), x)), 
                                          colour = susceptibility,
                                          label = paste(PointEst,"%"))) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.5, linewidth = 1) +
  geom_text(aes(colour = susceptibility, x = (as.numeric(scientific) + 0.05), y = Upper + 5),
            size = 6, fontface = "bold", alpha = 0.75) +
  coord_flip(clip = "off") +
  ylab("Disease prevalence (%)") +
  xlab("Species") + 
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  scale_y_continuous(labels = seq(0, 60, 20),
                     breaks = seq(0, 60, 20),
                     limits = c(0, 67)) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(1, 0.25))) +
  ak_theme + theme(axis.text.y = element_text(face = "italic"),
                   legend.title = element_blank()) 

prev_binconf_plot

ggsave("prev_binconf_plot.pdf", prev_binconf_plot, device = cairo_pdf, path = file.path(dir, figpath),
         width = 2600, height = 2000, scale = 1.5, units = "px", dpi = 300, limitsize = F)


##      2b. The most susceptible species are also the most abundant, while the 
##          least susceptible species are the least abundant.
model_2b <- glmmTMB(logsppAbun ~ scientific + (1|Site),
               data = d,
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))
summary(model_2b)
Anova(model_2b)


# Calculate observed avg
spavg <- d %>%
  dplyr::select(scientific, sppAbun, Site) # subset relevant data
spavg <- aggregate(sppAbun ~ scientific, spavg, mean) # aggregate by Site, & spp. and summarise
names(spavg)[names(spavg) == 'sppAbun'] <- 'avg_sppAbun'

textcol <- d %>%
  dplyr::select(scientific, susceptibility, Site) %>% # subset relevant data
  mutate(Site = as.factor(Site)) %>%
  group_by(scientific) %>%
  mutate(No.Sites = length(unique(Site))) %>%
  ungroup() %>%
  dplyr::select(!Site) %>%
  unique() %>%
  left_join(., spavg, by = "scientific")


m2b_predict <- ggpredict(model_2b, terms = "scientific") %>%
  dplyr::rename("scientific" = "x",
         "logsppAbun" = "predicted",
         "expectedAbun" = "group") %>%
  left_join(., textcol, by = "scientific") %>%
  plyr::mutate(expectedAbun = exp(logsppAbun),
               conf.low = exp(conf.low),
               conf.high = exp(conf.high),
               susceptibility = as.factor(susceptibility))


# xhat <- TeX(r"($\hat{X}_{\textit{a}} =)") ## LaTeX formula: $\hat{X}_{\textit{a}} = ## THIS ALSO WORKS, BUT WILL TRY USING LATEX EXP BELOW
TeXlabl <- glue::glue("$\\textbf{\\hat{X}_{\\textit{a}}}}}=", .open = "{{") # THIS WORKS
xhat <- latex2exp::TeX(TeXlabl, output = "expression")

# Create color coded labels for graph annotation
# Resistant
df1 <- m2b_predict %>%
  filter(susceptibility == "1")

# Tolerant
df2 <- m2b_predict %>% 
  filter(susceptibility == "2") 

# Susceptible
df3 <- m2b_predict %>% 
  filter(susceptibility == "3") 


m2b_plot <- ggplot(m2b_predict, aes(x = scientific, label = round(expectedAbun,0))) +
  geom_point(aes(y = expectedAbun, colour = susceptibility), size = 5) +
  # geom_point(aes(y = avg_sppAbun colour = susceptibility), size = 5, shape = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = susceptibility), width = 0.5, linewidth = 1) +
  geom_text(aes(colour = susceptibility, x = (as.numeric(scientific) + 0.05), y = conf.high + 3), 
            size = 6, fontface = "bold", alpha = 0.75) +
  annotate(geom = "text", x = (as.numeric(df_m2_1$scientific) + 0.1), y = (df_m2_1$conf.high + 1.5),
           label = paste(xhat), parse = TRUE, size = 6, color = "#548078", alpha = 0.75) +
  annotate(geom = "text", x = (as.numeric(df_m2_2$scientific) + 0.1), y = (df_m2_2$conf.high + 1.5),
           label = paste(xhat), parse = TRUE, size = 6, color = "#E3A630", alpha = 0.75) +
  annotate(geom = "text", x = (as.numeric(df_m2_3$scientific) + 0.1), y = (df_m2_3$conf.high + 1.5),
           label = paste(xhat), parse = TRUE, size = 6, color = "#b30000", alpha = 0.75) +
  coord_flip(clip = "off") +
  ylab("Abundance") +
  xlab("Species") + 
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  scale_y_continuous(labels = seq(0, 20, 5),
                     breaks = seq(0, 20, 5),
                     limits = c(0, 22)) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(1, 0.25))) +
  ak_theme + theme(axis.text.y = element_text(face = "italic"),
                   legend.title = element_blank()) 


m2b_plot

ggsave("fig2b.pdf", m2b_plot, device = cairo_pdf, path = file.path(dir, figpath),
          width = 2000, height = 1400, scale = 2, units = "px", dpi = 300, limitsize = F)



##      2c. Host abundance and susceptibility in our dataset.
model_2c <- glmmTMB(logsppAbun ~ susceptibility + (1|Site),
               data = d,
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))
summary(model_2c)
Anova(model_2c)


m2c_predict <- ggpredict(model_2c, terms = c("susceptibility")) %>%
  dplyr::rename("susceptibility" = "x",
       "group" = "group") %>%
  mutate(susceptibility = as.factor(susceptibility),
         predicted = exp(as.numeric(predicted)),
         conf.high = exp(as.numeric(conf.high)),
         conf.low = exp(as.numeric(conf.low)))

m2c_rug <- d %>%
  group_by(susceptibility) %>%
  summarise(n = n())

m2c_predict <- merge(m2c_predict, m2c_rug)


m2c_plot <- ggplot(m2c_predict, aes(susceptibility, predicted, color = susceptibility)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 1) +
  geom_richtext(aes(y = (conf.high + 1), label = paste0("n<span style = 'font-size:15pt'><sub>*obs*</sub> </span>= ", n)), 
                alpha = 0.75, size = 8, label.size = NA, fill = NA, fontface = "bold", show.legend = F) +
  annotate("text", x = 3, y = 6.75, label = "***", size = 10, fontface = "bold", 
           colour = "#b30000") +
  annotate("text", x = 0.65, y = 9, label = "C", size = 12, fontface = "bold", 
           colour = "black") +
#  stat_compare_means() +
  scale_x_discrete(labels = c("Resistant", "Tolerant", "Susceptible")) +
  ylab("Species abundance") + 
  xlab("Susceptibility level") +
  scale_y_continuous(limits = c(0,9),
                   breaks = seq(0, 8, 2)) +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") + 
  ak_theme + theme(axis.text.y = element_text(size = 26, face = "plain"),)

m2c_plot


ggsave("fig2c.pdf", m2c_plot, device = cairo_pdf, path = file.path(dir, figpath),
          width = 2000, height = 1300, scale = 2, units = "px", dpi = 300, limitsize = F)


## Create guide to force a common legend
commonLegend <- guides(fill = guide_legend(override.aes = list(color = c("#548078", "#E3A630", "#b30000"),
                                                    shape = c(16, 16, 16), 
                                                    size = c(2, 2, 2),
                                                    alpha = c(1, 1, 1))))

## Create combined plot for manuscript
fig2a <- prev_binconf_plot + labs(caption = NULL) + 
                     theme(plot.tag.position = c(0.96, 0.92),
                           plot.margin = margin(.5, .75, .5, .75, "cm"),
                           axis.ticks.length = unit(.25, "cm"),
                           axis.ticks = element_blank(),
                           axis.text.y = element_text(size = 28),
                           axis.text.x = element_text(size = 24),
                           axis.title.y = element_blank(),
                           axis.title.x = element_text(size = 30)) +
                      commonLegend

fig2b <- m2b_plot + labs(caption = NULL) + 
                       theme(axis.text.y = element_blank(),
                             axis.title.y = element_blank(),
                             axis.title.x = element_text(size = 30),
                             axis.text.x = element_text(size = 24),
                             axis.ticks.length = unit(.25, "cm"),
                             axis.ticks = element_blank(),
                             plot.margin = margin(.5, .75, .5, .5, "cm"),
                             plot.tag.position = c(0.92, 0.92)) +
                       commonLegend

fig2c <- m2c_plot + labs(caption = NULL) + 
                      theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                            plot.margin = margin(.75, 2, .75, 0, "cm"),
                            axis.ticks.length.y = unit(.25, "cm"),
                            axis.ticks = element_blank(),
                            axis.title.y = element_text(margin = margin(r = -500)),
                            axis.title.x = element_text(size = 34),
                            axis.text.y = element_text(face = "plain"),
                            plot.tag.position = c(0.33, 0.92)) +
                      commonLegend


fig2ab <-(fig2a | fig2b) + plot_layout(guides = "collect", heights = c(20, 16)) +
  plot_annotation(tag_levels = "A") & theme(legend.position = "top",
                                            legend.box.margin = margin(0, 1, 1, 1, "cm"),
                                            legend.text = element_text(margin = margin(r = 1, unit = "cm")),
                                            legend.title = element_blank())
fig2ab

ggsave("fig2ab.pdf", fig2ab, device = cairo_pdf, path = file.path(dir, figpath),
        width = 2800, height = 1400, scale = 2, units = "px", dpi = 300, limitsize = F)

fig2combined <- ((fig2a | fig2b)/fig2c) + plot_layout(guides = "collect", heights = c(20, 16)) +
                plot_annotation(tag_levels = "A") & theme(legend.position = "top",
                                                          legend.box.margin = margin(0, 1, 1, 1, "cm"),
                                                          legend.text = element_text(margin = margin(r = 1, unit = "cm")),
                                                          legend.title = element_blank())
fig2combined

# ggsave("fig2_combined.pdf", fig2combined, device = cairo_pdf, path = file.path(dir, figpath),
#        width = 2800, height = 2600, scale = 2, units = "px", dpi = 300, limitsize = F)

## Code to see how many sites each species are found at in each country
# d %>%
#   dplyr::select(country, species, Site) %>%
#   filter(country == "Spain" & species == "marmoratus") %>%
#   unique() #%>%
#   nrow()


#### 3. Cbind models testing the Dilution Effect Hypothesis ####################
# Drop rows with NA vals in weather data
dcbindScaled <- dcbind %>% 
  tidyr::drop_na(., any_of(c(21:36))) %>%
  filter(country == "Germany" | country == "Spain")

##      3a. Cbind model including all salamander spp. --------------------------
m_all <- glmmTMB(cbind(nPos_all, nNeg_all) ~ richness*logsiteAbun +
                    temp_d*sMoist_d + (1|scientific),
                  data = dcbindScaled, family = "binomial", na.action = "na.fail",
                  control = glmmTMBControl(optimizer = optim,
                                           optArgs = list(method = "BFGS")))

summary(m_all)
Anova(m_all)

# m_all2 <- glmmTMB(cbind(nPos_all, nNeg_all) ~ richness*logsiteAbun +
#                    temp_d*sMoist_d + (1|scientific) + (1|principalInvestigator),
#                  data = dcbindScaled, family = "binomial", na.action = "na.fail",
#                  control = glmmTMBControl(optimizer = optim,
#                                           optArgs = list(method = "BFGS")))
# 
# summary(m_all2)
# Anova(m_all2)

# anova(m_all, m_all2) # models are virtually the same, going with the simpler model to be parsimonious.


#### Clean model outputs
## ABUNDANCE x RICHNESS
tab_model(m_all, show.obs = T, collapse.ci = T,
          show.icc = F, show.ngroups = F, show.re.var = F,
          rm.terms = c("temp_d", "sMoist_d", "temp_d:sMoist_d"),
          dv.labels = "All species model",
          string.pred = "Terms",
          string.p = "P-Value",
          show.p = T,
          pred.labels = nicelabs,
          file = file.path(dir, figpath, "m_all.html"))


# take html file and make .png file
webshot2::webshot(file.path(dir, figpath, "m_all.html"),
        file.path(dir, figpath, "m_all.png"),
        vwidth = 365, vheight = 500)




##      Prevalence by Abundance & Richness Plots for 'All spp.' model
m_all_predict <- ggpredict(m_all,  terms = c("richness", "logsiteAbun")) %>%
  dplyr::rename("richness" = "x",
         "logsiteAbun" = "group") %>%
  plyr::mutate(richness = as.numeric(as.character(richness)),
               logsiteAbun = as.numeric(as.character(logsiteAbun)),
               # Convert scaled prediction to original data scale:
               siteAbun = as.factor(round(exp(as.numeric(logsiteAbun)), 0)))


m_all_plot <- ggplot(m_all_predict, aes(x = richness, y = predicted, 
                                        colour = siteAbun)) +
  geom_line(aes(linetype = siteAbun), linewidth = 1) +
  geom_rug(data = dcbindScaled, aes(x = richness, y = 0), sides = "b", 
           alpha = 0.5, position = position_jitter(width = 0.4, height = 0.1), 
           inherit.aes = F, na.rm = T) +
  labs(x = "Species richness",
       y = "Bsal prevalence (%)",
       # title = "All spp. model", 
       linetype = "Site-level abundance") +
  #  geom_ribbon(aes(x = richness, ymin = conf.low, ymax = conf.high, 
  #              fill = siteAbun), alpha = 0.2, colour = NA, show.legend = F) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_grey(start = 0.8, end = 0.2) +
  scale_x_continuous(labels = seq(0, 10, 1), 
                     breaks = seq(0, 10, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = seq(0, .05, 0.01), 
                     limits = c(0, 0.05),
                     minor_breaks = seq(0, 0.05, 0.01)) + 
  ak_theme + theme(plot.tag.position = c(0.96, 0.9),
                   legend.title = element_blank(),
                   axis.text.y = element_text(face = "plain")) + 
  guides(colour = guide_legend("Site-level abundance"))

m_all_plot
# ggsave("m_all_plot.pdf", m_all_plot, device = cairo_pdf, path = file.path(dir, figpath),
#        width = 1250, height = 1500, scale = 2, units = "px", dpi = 300, limitsize = F)

# ggsave("m_all_plot.tif", m_all_plot, device = "tiff", scale = 2, 
#        width = 1500, height = 1000, units = "px", 
#        path = file.path(dir, figpath), dpi = 300)


##      3b. Cbind model for fire salamanders only ------------------------------
## Subset data even further to only include Fire Salamanders
FSdata <- dcbindScaled %>%
  filter(scientific == "Salamandra salamandra")

m_FS <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~  richness + logsiteAbun + temp_d*sMoist_d + (1|scientific),
                data = FSdata, family = "binomial",
                control = glmmTMBControl(optimizer = optim, 
                                         optArgs = list(method = "BFGS")))


summary(m_FS)
Anova(m_FS)


# m_FS2 <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~  richness + logsppAbun + temp_d*sMoist_d + (1|scientific) + (1|principalInvestigator),
#                  data = FSdata, family = "binomial", na.action = "na.fail",
#                  control = glmmTMBControl(optimizer = optim,
#                                           optArgs = list(method = "BFGS")))
# 
# summary(m_FS2)
# Anova(m_FS2)
# 
# 
# anova(m_FS, m_FS2) # same as the "all spp. model" -- simpler model is just as effective at accounting for variance




#### Clean model outputs
## ABUNDANCE x RICHNESS
tab_model(m_FS, show.obs = T, collapse.ci = T,
          show.icc = F, show.ngroups = F, show.re.var = F,
          rm.terms = c("temp_d", "sMoist_d", "temp_d:sMoist_d"),
          dv.labels = "Fire salamander model",
          string.pred = "Terms",
          string.p = "P-Value",
          show.p = T,
          pred.labels = nicelabs,
          file = file.path(dir, figpath, "m_FS.html"))


# take html file and make .png file
webshot2::webshot(file.path(dir, figpath, "m_FS.html"),
                  file.path(dir, figpath, "m_FS.png"),
                  vwidth = 365, vheight = 500)



# # take html file and make .png file
# webshot(file.path(dir, figpath, "m_FS_weather.html"),file.path(dir, figpath, "m_FS_weather.png"))


##      Prevalence by Abundance & Richness Plots for 'fire salamander' model
m_FS_predict <- ggpredict(m_FS,  terms = c("richness", "logsiteAbun")) %>%
  dplyr::rename("richness" = "x",
         "logsiteAbun" = "group") %>%
  plyr::mutate(richness = as.numeric(as.character(richness)),
               logsiteAbun = as.numeric(as.character(logsiteAbun)),
               # Convert scaled prediction to original data scale:
               siteAbun = as.factor(round(exp(as.numeric(logsiteAbun)), 0)))

m_FS_plot <- ggplot(m_FS_predict, aes(x = richness , y = predicted, colour = siteAbun)) +
  geom_line(aes(linetype = siteAbun), linewidth = 1) +
  geom_rug(data = FSdata, aes(x = richness, y = 0), sides = "b", alpha = 0.5,
           position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
  labs(linetype = "Site-level abundance",
       # title =  "Fire salamander model", 
       y = "Bsal prevalence (%)",
       x = "Species richness") +
  # geom_ribbon(aes(x = richness, ymin = conf.low, ymax = conf.high, fill = sppAbun), alpha = 0.2, colour = NA, show.legend = F) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_grey(start = 0.8, end = 0.2) +
  scale_x_continuous(labels = seq(0, 10, 1), 
                     breaks = seq(0, 10, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = seq(0, 0.12, 0.04), 
                     limits = c(0, 0.12),
                     minor_breaks = seq(0, 0.12, 0.01)) +
  ak_theme + theme(plot.tag.position = c(0.96, 0.9),
                   legend.title = element_blank(),
                   axis.text.y = element_text(face = "plain")) + 
  guides(colour = guide_legend("Site-level abundance"))


m_FS_plot
# ggsave("m_FS_plot.pdf", m_FS_plot, device = cairo_pdf, path = file.path(dir, figpath),
#        width = 1250, height = 1500, scale = 2, units = "px", dpi = 300, limitsize = F)

# ggsave("m_FS_plot.pdf", m_FS_plot, device = cairo_pdf, scale = 2, width = 1920, height = 1080, units = "px",
#        path = file.path(dir, figpath), dpi = 300)


## Create combined plot for manuscript
fig3a <- m_all_plot + labs(caption = NULL, y = NULL, x = NULL) + 
  theme(plot.tag.position = c(0.95, 0.95),
        plot.margin = margin(.5, .75, .5, .5, "cm"),
        legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill = alpha ("white", 0.75), color = NA),
        legend.box = "horizontal",
        legend.key.size = unit(1,"cm"),
        legend.text = element_text(margin = margin(r = 1, unit = "cm"), size = 24),
        legend.title = element_blank()) +
  guides(shape = guide_legend(label.position = "left"),
         linetype = guide_legend(nrow = 1))

fig3a

fig3b <- m_FS_plot + labs(caption = NULL, y = NULL) + 
  theme(plot.tag.position = c(0.95, 0.95),
        plot.margin = margin(.5, .75, .5, .5, "cm"),
        legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill = alpha ("white", 0.75), color = NA),
        legend.box = "horizontal",
        legend.key.size = unit(1,"cm"),
        legend.text = element_text(margin = margin(r = 1, unit = "cm"), size = 24),
        legend.title = element_blank()) +
  guides(shape = guide_legend(label.position = "left"),
         linetype = guide_legend(nrow = 1))
fig3b



# ## Vertical plots* 
# #-- need to alter x and y labs in fig3a/3b if switching between h and v plots
FS_imgpath <- str_c("C:/Development/Chapter-4-Analyses/csvFiles/firesalamander.png") # add image to 3b

fig3ab_v <- (fig3a / fig3b) + plot_annotation(tag_levels = "A")

fig3ab_v_combined <- ggdraw(fig3ab_v) + 
  draw_label("Bsal prevalence (%)", x = 0, y = 0.5, angle = 90,
             size = 34, fontfamily = "Arial") + 
  draw_image(image = FS_imgpath, x = 0.35, y = -0.15, scale = 0.25) +
  ak_theme + theme(axis.line = element_blank())
fig3ab_v_combined


ggsave("modelPlots_vertical.pdf", fig3ab_v_combined, device = cairo_pdf, scale = 2, 
       width = 1750, height = 2400, units = "px",
       path = file.path(dir, figpath), dpi = 300)


## Horizontal plots


fig3ab_h <- (fig3a | fig3b) + plot_annotation(tag_levels = "A")

fig3ab_h_combined <- ggdraw(fig3ab_h) + 
  draw_label("Species richness", x = 0.5, y = 0, angle = 0,
             size = 34, fontfamily = "Arial") + 
  draw_image(image = FS_imgpath, x = 0.4, y = 0.22, scale = 0.25) +
  ak_theme + theme(axis.line = element_blank())
fig3ab_h_combined


ggsave("modelPlots_horizontal.pdf", fig3ab_h_combined, device = cairo_pdf, scale = 2, 
       width = 2400, height = 1250, units = "px",
       path = file.path(dir, figpath), dpi = 300)


# ##      3c. Prevalence by Temperature & Soil Moisture Plots for T0, T-1, T-2 (Fire Salamanders Only)
# # T0
# m3_t0_weather <- ggpredict(m3_t0, terms = c("temp_d [all]", "sMoist_d"))%>%
#   rename("temp_d" = "x",
#          "sMoist_d" = "group") %>%
#   mutate(temp_d = as.numeric(as.character(temp_d)),
#          sMoist_d = as.numeric(as.character(sMoist_d)),
#          # Convert scaled prediction to original data scale:
#          temp_dUnscaled = (temp_d * as.numeric(attr(dcbindScaled$temp_d, "scaled:scale")) + 
#                                 as.numeric(attr(dcbindScaled$temp_d, "scaled:center"))),
#          sMoistUnscaled = as.factor((round(sMoist_d * as.numeric(attr(dcbindScaled$sMoist_d, "scaled:scale")) +
#                                                    as.numeric(attr(dcbindScaled$sMoist_d, "scaled:center")), 2)))) 
# # Create dummy column for soil moisture labels
# m3_t0_weather <- create_dummy_col(m3_t0_weather)
# 
# 
# m3_t0_p2 <- ggplot(m3_t0_weather, aes(x = temp_dUnscaled , y = predicted, colour = dummy)) +
#   geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), linewidth = 1) +
#   geom_rug(data = subset(dcbindScaled, scientific =="Salamandra salamandra"), aes(x = temp_d, y = 0), sides = "b", alpha = 0.5,
#            position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
#   #  geom_ribbon(aes(x = temp_dUnscaled, ymin = conf.low, ymax = conf.high, fill = dummy), alpha = 0.2, colour = NA, show.legend = F) +
#   labs(title =  bquote(paste(italic(t))[(0~days)]), linetype = "Soil moisture") +
#   ylab("Fire salamander Bsal prevalence (%)") +
#   xlab(expression("Temperature (°C)")) + 
#   scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
#   scale_color_viridis(option = "G", discrete = T, begin = 0.8, end = 0.3) +
#   scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) + 
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.3)) + 
#   ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + guides(colour = guide_legend("Soil moisture"))
# 
# m3_t0_p2 
# ggsave("m3_t0_weather.tif", m3_t0_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
#        path = file.path(dir, figpath), dpi = 300)
# 
# # T-1
# m3_t1_weather <- ggpredict(m3_t1, terms = c("temp_m_t1 [all]", "sMoist_m_t1"))%>%
#   rename("temp_m_t1" = "x",
#          "sMoist_m_t1" = "group") %>%
#   mutate(temp_m_t1 = as.numeric(as.character(temp_m_t1)),
#          sMoist_m_t1 = as.numeric(as.character(sMoist_m_t1)),
#          # Convert scaled prediction to original data scale:
#          temp_m_t1Unscaled = (temp_m_t1 * as.numeric(attr(dcbindScaled$temp_m_t1, "scaled:scale")) + 
#                                    as.numeric(attr(dcbindScaled$temp_m_t1, "scaled:center"))),
#          sMoistUnscaled = as.factor((round(sMoist_m_t1 * as.numeric(attr(dcbindScaled$sMoist_m_t1, "scaled:scale")) +
#                                                    as.numeric(attr(dcbindScaled$sMoist_m_t1, "scaled:center")), 2)))) 
# # Create dummy column for soil moisture labels
# m3_t1_weather <- create_dummy_col(m3_t1_weather)
# 
# 
# m3_t1_p2 <- ggplot(m3_t1_weather, aes(x = temp_m_t1Unscaled , y = predicted, colour = dummy)) +
#   geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), linewidth = 1) +
#   geom_rug(data = subset(dcbindScaled, scientific =="Salamandra salamandra"), aes(x = temp_m_t1, y = 0), sides = "b", alpha = 0.5,
#            position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
#   #  geom_ribbon(aes(x = temp_m_t1Unscaled, ymin = conf.low, ymax = conf.high, fill = dummy), alpha = 0.2, colour = NA, show.legend = F) +
#   labs(title =  bquote(paste(italic(t))[(-30~days)]), linetype = "Soil moisture") +
#   ylab("Fire salamander Bsal prevalence (%)") +
#   xlab(expression("Temperature (°C)")) + 
#   scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
#   scale_color_viridis(option = "G", discrete = T, begin = 0.8, end = 0.3) +
#   scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) + 
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.3)) + 
#   ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + guides(colour = guide_legend("Soil moisture"))
# 
# m3_t1_p2 
# ggsave("m3_t1_weather.tif", m3_t1_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
#        path = file.path(dir, figpath), dpi = 300)
# 
# 
# # T-2
# m3_t2_weather <- ggpredict(m3_t2, terms = c("temp_m_t2 [all]", "sMoist_m_t2"))%>%
#   rename("temp_m_t2" = "x",
#          "sMoist_m_t2" = "group") %>%
#   mutate(temp_m_t2 = as.numeric(as.character(temp_m_t2)),
#          sMoist_m_t2 = as.numeric(as.character(sMoist_m_t2)),
#          # Convert scaled prediction to original data scale:
#          temp_m_t2Unscaled = (temp_m_t2 * as.numeric(attr(dcbindScaled$temp_m_t2, "scaled:scale")) + 
#                                    as.numeric(attr(dcbindScaled$temp_m_t2, "scaled:center"))),
#          sMoistUnscaled = as.factor((round(sMoist_m_t2 * as.numeric(attr(dcbindScaled$sMoist_m_t2, "scaled:scale")) +
#                                                    as.numeric(attr(dcbindScaled$sMoist_m_t2, "scaled:center")), 2)))) 
# # Create dummy column for soil moisture labels
# m3_t2_weather <- create_dummy_col(m3_t2_weather)
# 
# 
# m3_t2_p2 <- ggplot(m3_t2_weather, aes(x = temp_m_t2Unscaled , y = predicted, colour = dummy)) +
#   geom_line(aes(linetype = factor(dummy, levels  = c("Low", "Med", "High"))), linewidth = 1) +
#   geom_rug(data = subset(dcbindScaled, scientific =="Salamandra salamandra"), aes(x = temp_m_t2, y = 0), sides = "b", alpha = 0.5,
#            position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
#   #  geom_ribbon(aes(x = temp_d_t2Unscaled, ymin = conf.low, ymax = conf.high, fill = dummy), alpha = 0.2, colour = NA, show.legend = F) +
#   labs(title =  bquote(paste(italic(t))[(-60~days)]), linetype = "Soil moisture") +
#   ylab("Fire salamander Bsal prevalence (%)") +
#   xlab(expression("Temperature (°C)")) + 
#   scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
#   scale_color_viridis(option = "G", discrete = T, begin = 0.8, end = 0.3) +
#   scale_x_continuous(labels = seq(-10, 30, 10), breaks = seq(-10, 30, 10), limits = c(-10, 30)) + 
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.3)) + 
#   ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + guides(colour = guide_legend("Soil moisture"))
# 
# m3_t2_p2 
# ggsave("m3_t2_weather.tif", m3_t2_p2, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
#        path = file.path(dir, figpath), dpi = 300)
# 
# # 3 Panel Graph
# m3_p2_combined <- m2_p2_combined <- ((m3_t2_p2 + labs(caption = NULL, x = NULL) + theme(legend.position = "none")) | 
#                                      (m3_t1_p2 + labs(caption = NULL, y = NULL)) | 
#                                      (m3_t0_p2 + labs(caption = NULL, y = NULL, x = NULL) + theme(legend.position = "none"))) + 
#                                       plot_annotation(tag_levels = "A", 
#                                       caption = "Soil moisture ranged from 5.42-5.52 (kg/m"^2~"), 6.16-6.22 (kg/m"^2~"), and 6.89-6.92 (kg/m"^2~") for the Low, Medium, and High categories respectively. Timepoints above each graph indicate the time from the initial observation.",
#                                       theme = theme(plot.caption = element_text(size = 12, hjust = 0)))
# 
# m3_p2_combined
# ggsave("m3_weather_combined.tif", m3_p2_combined, device = "tiff", scale = 2, width = 2600, height = 1500, units = "px", 
#        path = file.path(dir, figpath), dpi = 300)
# 
# 
# # Remove saved objects from the global environment to speed up processing
# rm(m3_p2_combined, m3_t0_p1, m3_t0_p2, m3_t0_rich, m3_t0_weather, m3_t1_p2, 
#    m3_t1_weather,m3_t2_p2, m3_t2_weather)
# 
# 
# 
# #### 4. Fatality model ####################################################
# # Remove any instances of NA within the fatal column & weather vars columns, as well as I. alpestris
# d_fatal <- d %>%
#   tidyr::drop_na(any_of(c(31, 40:45))) %>%
#   subset(scientific != "Ichthyosaura alpestris") %>%
# # Scale relevant vars
#   mutate_at(c("tavg_wc", "tmin_wc", "tmax_wc", "prec_wc", "bio1_wc", "bio12_wc", "bio1_wc"), 
#             ~(scale(., center = T, scale = T %>% as.numeric)))
# # bio1 == annual mean temp
# # bio12 == annual precip
# 
# ##      4a.  Fatality of fire salamanders given an interaction between average monthly temperature (tavg) and if Bsal has ever been detected at a site. 
# m4 <- glmmTMB(fatal ~ bio1_wc*prev_above_0 + (1|Site),
#                  family = "binomial",
#                  data = filter(d, scientific == "Salamandra salamandra"),
#                  control = glmmTMBControl(optimizer = optim,
#                                           optArgs = list(method = "BFGS")))
# 
# summary(m4)
# Anova(m4)
# 
# 
# ##      4b. Plots
# # bio1 - all data
# m4_predict <- ggpredict(m4, terms = c("bio1_wc [all]", "prev_above_0")) %>%
#   rename("bio1_wc" = "x",
#          "BsalDetected" = "group") %>%
#   mutate(bio1_wc = as.numeric(as.character(bio1_wc)),
#          BsalDetected = as.factor(ifelse(BsalDetected == 0, "Bsal ( - )", "Bsal ( + )")),
#          # Convert scaled prediction to original data scale
#          bio1_wcUnscaled = (bio1_wc * as.numeric(attr(d_fatal$bio1_wc, "scaled:scale")) + as.numeric(attr(d_fatal$bio1_wc, "scaled:center"))))
#  
# 
# m4_plot <- ggplot(m4_predict, aes(x = sMoist_dUnscaled , y = predicted, colour = BsalDetected)) +
#   geom_line(aes(linetype = BsalDetected), linewidth = 1) +
# #  geom_rug(data = dcbindScaled, aes(x = richness, y = 0), sides = "b", alpha = 0.5, 
# #           position = position_jitter(width = 0.4, height = 0.1), inherit.aes = F, na.rm = T) +
#   labs(linetype = "Status") +
# #  geom_ribbon(aes(x = tavgUnscaled, ymin = conf.low, ymax = conf.high, fill = dummy), alpha = 0.2, colour = NA, show.legend = F) +
#   ylab("Fire salamander mortality (%)") +
#   xlab(expression("Mean annual temperature (°C)")) + 
#   scale_linetype_manual(values = c("solid", "longdash")) +
#   scale_color_manual(values = c("black", "#C23113")) +
#   scale_x_continuous(labels = seq(17, 23, 2), breaks = seq(17, 23, 2), limits = c(17, 23)) +  
#   scale_y_continuous(labels = scales::percent, limits = c(0, .5)) +
#   ak_theme + theme(plot.tag.position = c(0.96, 0.9)) + guides(colour = guide_legend("Status"))
# 
# m4_plot
# 
# ggsave("m4_fatality.tif", m4_plot, device = "tiff", scale = 2, width = 1920, height = 1080, units = "px", 
#        path = file.path(dir, figpath), dpi = 300)
# 
