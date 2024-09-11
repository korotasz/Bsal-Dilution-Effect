## GETTING STARTED -------------------------------------------------------------
## Please review the program version requirements in the .README document associated with this GitHub repository.
## 1. Make sure you have the correct version of R (4.3.3 "Angel Food Cake") loaded for this session.

## 2. Load 'renv'
require(renv)

## 3. Restore project dependencies from the renv lockfile ('renv.lock'). You should get a message that
##    no issues have been found and that the project is in a consistent state.
# renv::restore()

## If step 3 does give an error, try running:
# renv::init(repos = "https://packagemanager.posit.co/cran/2023-10-13") # (should install the correct versions of maptools, rgdal, and sp)

## Packages --------------------------------------------------------------------
## These packages need to be loaded first (commented out pckgs only need to be run once)
# remotes::install_version("Rttf2pt1", version = "1.3.8") # install this version, latest ver. not compatible
# remotes::install_github("gorkang/html2latex") # convert sjPlot::tab_model() hmtl table to tex and pdf in .Rmd docs
# remotes::install_github("ddsjoberg/gtsummary")
#    devtools::install_github("larmarange/broom.helpers") ## used by gtsummary
#    devtools::install_github("cran/aod") # need at least v. 1.3.3
#    devtools::install_github("insightsengineering/cardx")# need at least v. 0.2.0.9008
## As of 2024-04-04, there are issues with patchwork and ggplot2 that require specific pull requests to resolve:
# remotes::install_github("thomasp85/patchwork")
# remotes::install_github("tidyverse/ggplot2", ref = remotes::github_pull("5592"))

## As of 2024-04-04, the Matrix package (a dependency of glmmTMB) is throwing errors
##  (Matrix v. 1.6-5) and must be reverted to Matrix v. 1.6-1.1 to work
# remotes::install_version("Matrix", version = "1.6-1.1")

require(pacman)
extrafont::loadfonts(device = "all", quiet = T)

### Visualization Packages------------------------------------------------------
pckgs <- c("ggsignif", # adds labels to significant groups
             "ggpubr", # stat_compare_means()
            "ggbreak", # create axis breaks in ggplots
             "gtools", # signif. value styling
             "ggtext", # for text type/arrangements w/ ggplot2
           "Rttf2pt1", # to use with the extrafont package
           "ggthemes", # contains 'scales', 'themes', and 'geoms' packages
          "grDevices", # saves high quality svg, pdf, and ps files
            "cowplot", # arranging plots/figs
          "gridExtra", # arranging plots/figs
          "patchwork", # arranging plots/figs
       "multcompView", # cld.emmGrid()
          "latex2exp", # allows use of LaTeX in R
               "glue", # allows concatenation of LaTeX and R syntax
             "sjPlot", # plot_model(), tab_model()
               "ragg", # converts plots to tiff files
          "flextable", # create tables compatible with Word
          "gtsummary", # better package for creating tables from glmmTMB objects
        "broom.mixed", # required to create flextable/gtsummary objects from mixed model outputs
      "broom.helpers", # required for gtsummary
           "reporter", # streamlines adding special characters to labels ## PROBABY DELETE
              "cardx", # to add chisquare vals to model summary outputs in gtsummary
          # "webshot", # save kable tables
             "magick", # image_read() -- needed for cowplot::draw_image
### Analysis Packages --------------------------------------------------------
          "tidyverse", # data wrangling/manipulation
             "sjmisc", # data and variable transformation
            "glmmTMB", # glmmTMB()
           "multcomp", # glht()
            "emmeans", # lsmeans()
                "car", # Anova()
             "DHARMa", # simulateResiduals(), testZeroInflation(), testDispersion()
              "MuMIn", # model.sel()
          "ggeffects", # ggpredict()
               "epiR", # calculate prevalence & CIs
              "binom", # binom.bayes()
              "Hmisc" # binconf()
)

## Load packages
pacman::p_load(pckgs, character.only = T)
#### IF RENV CANNOT INSTALL/LOAD PACKAGES, USE CODE BELOW TO NAVIGATE TO OTHER .libPaths() OUTSIDE OF PROJECT.
## Home computer:
# renv::hydrate(packages = c(pckgs, "pacman"), sources = c("C:/Users/alexi/AppData/Local/R/win-library/4.3", "C:/Program Files/R/R-4.3.1/library"))
## Work computer
# renv::hydrate(packages = c(pckgs, "pacman"), sources = c("C:/Users/Alexis/AppData/Local/R/win-library/4.3", "C:/Program Files/R/R-4.3.1/library"))


## Functions -------------------------------------------------------------------
flip <- function(data) {
  new <- data[rev(rownames(data)), ]
  rownames(new) <- NULL
  new
}

# a function to round p-values and add stars to gtsummary objects
style_pvalue_stars <- function(x) {
  dplyr::case_when(
    x < 0.001 ~ paste0(style_pvalue(x), "***"),
    x < 0.01 ~ paste0(style_pvalue(x), "**"),
    x < 0.05 ~ paste0(style_pvalue(x), "*"),
    TRUE ~ style_pvalue(x)
  )
}


## File paths ------------------------------------------------------------------
dir <- rstudioapi::getActiveProject()
analysis <- file.path(dir, path.expand("02_dataAnalyses"))
outputs <- file.path(dir, path.expand("03_outputs"))
figpath <- file.path(outputs, path.expand("figures"))
tblpath <- file.path(outputs, path.expand("tables"))
shppath <- file.path(dir, path.expand("01_dataCleaning/shapefiles"))

## Plot and table stylings -----------------------------------------------------
## Load fonts from grDevices
grDevices::pdfFonts()

ak_theme <- hrbrthemes::theme_ipsum(base_family = "Segoe UI Light") +
  theme(axis.text.x = element_text(size = 32),
        axis.title.x = element_text(size = 38, hjust = 0.5,
                                    margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    face = "plain"),
        axis.text.y = element_text(size = 32, face = "plain"),
        axis.title.y = element_text(size = 38, hjust = 0.5,
                                    margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    face = "plain"),
        axis.ticks.length = unit(.25, "cm"),
        axis.ticks = element_blank(),
        plot.tag = element_text(size = 42, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_markdown(size = 12, face = "plain"),
        # plot.margin = margin(1, 1, 1.5, 1.2, "cm"),
        plot.caption = element_markdown(hjust = 1, size = 14, face = "plain"),
        plot.caption.position = "plot",
        legend.position = "top",
        legend.key.size = unit(2,"cm"),
        legend.text = element_text(size = 32, hjust = -1),
        legend.title = element_text(size = 38, face = "bold"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.spacing.y = unit(1.5,"cm"),
        strip.text = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.line = element_line(color = 'black'))

set_flextable_defaults(
  font.family = "Segoe UI Light",
  font.size = 11,
  font.color = "black",
  text.align = "center",
  line_spacing = 1,
  cs.family = "Segoe UI Light",
  digits =3,
  pct_digits = 3,
  split = T,
  # keep_with_next = NULL,
  # tabcolsep = NULL,
  post_process_docx = T
)

nicelabs <- list(locality_rich = "Locality richness",
                 logsiteAbun = "log(Site abundance)",
                 temp_d = paste0("Temperature (\u00b0C)"),
                 sMoist_d = paste0("Soil moisture (kg\u00b7m\u00b2)"),
                 "locality_rich*logsiteAbun" = "Locality richness:log(Site abundance)",
                 "locality_rich:logsiteAbun" = "Locality richness:log(Site abundance)",
                 "temp_d*sMoist_d" = "Temperature:Soil moisture",
                 "temp_d:sMoist_d" = "Temperature:Soil moisture")


chisqlab <- paste0(symbol("chi"),"\u00b2")

## Read in .csv files and prep data --------------------------------------------
setwd(analysis)

## only data I am confident about
d <- read.csv("BsalData_OK.csv", header = T, encoding = "UTF-8") %>%
  filter(continent == "Europe") %>%
  #   Retain sites that have ever had a Bsal+, regardless of when the first positive at that site was
  filter(!(is.na(dateFirstPositive))) %>%
  #   transform vars
  mutate(logsppAbun = log(sppAbun + 1),
         logsiteAbun = log(siteAbun + 1),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
  relocate(c(logsppAbun, logsiteAbun), .after = sppAbun)


## 'dcbind' dataset is what will be used to run our models
dcbind <- read.csv("Bsal_cbind_ok.csv", header = T, encoding = "UTF-8") %>%
  filter(continent == "Europe") %>%
  #   Retain sites that have ever had a Bsal+, regardless of when the first positive at that site was
  filter(!(is.na(dateFirstPositive))) %>%
  # transform vars
  mutate(logsppAbun = log(sppAbun + 1),
         logsiteAbun = log(siteAbun + 1),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
  relocate(c(logsppAbun, logsiteAbun), .after = sppAbun) %>%
## Data prep for cbind models: drop rows with NA vals in weather data & scale relevant vars
#   bio1_wc = "annual mean temperature" | bio12_wc = annual precipitation
  tidyr::drop_na(., any_of(c("temp_d", "sMoist_d", "tmin_wc:bio1_wc", "bio12_wc"))) %>%
  mutate_at(c("temp_d", "sMoist_d",
              "bio1_wc", "bio12_wc", "tavg_wc", "prec_wc"),
            ~(scale(., center = T, scale = T %>% as.numeric))) %>%
  mutate(year = year(date))

## > Test for outliers in dataset ----------------------------------------------
df <- read.csv("BsalData_OK.csv", header = T, encoding = "UTF-8")
hist(df$sppAbun,
     xlab = "sppAbun",
     breaks = sqrt(nrow(df)))

boxplot(df$sppAbun,
        ylab = "sppAbun")

# obtain row # of outliers in a vector
out <- boxplot.stats(df$sppAbun)$out

# extract outliers to their own data frame for inspection
out_ind <- which(df$sppAbun %in% out)

check <- d[out_ind, ]

# remove outliers from larger data set
df <-  df %>%
  filter(!(sppAbun %in% out))

## prep data the same as 'd'
d_subset <- df %>%
  filter(continent == "Europe") %>%
  # Retain sites that have ever had a Bsal+, regardless of when the first positive at that site was
  filter(!(is.na(dateFirstPositive))) %>%
  # we still want to log-transform the same variables to get a more normal distribution
  mutate(logsppAbun = log(sppAbun + 1),
         logsiteAbun = log(siteAbun + 1),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
  relocate(c(logsppAbun, logsiteAbun), .after = sppAbun)

hist(d_subset$sppAbun,
     xlab = "sppAbun",
     breaks = sqrt(nrow(d)))

boxplot(d_subset$sppAbun,
        ylab = "sppAbun")

hist(d_subset$logsppAbun,
     ylab = "logsppAbun")


## > Use this information to make new cbind data set ---------------------------
### > Summarise BsalDetected observations in data set --------------------------
BsalCount_all <- df %>%
  subset(., select = c(country, Site, posSite, date, scientific, BsalDetected, individualCount)) %>%
  mutate(BsalDetected = case_when(BsalDetected == "1" ~ "nPos_all",
                                  BsalDetected == "0" ~ "nNeg_all")) %>%
  group_by(country, Site, posSite, date, scientific, BsalDetected) %>%
  summarise(n = sum(individualCount)) %>%
  pivot_wider(names_from = c(BsalDetected),
              id_cols = c(country, Site, posSite, date, scientific),
              values_from = n, values_fill = 0)

## Tally for fire salamanders only
BsalCount_FS_only <- df %>%
  subset(., select = c(country, Site, posSite, date, scientific, BsalDetected, individualCount)) %>%
  filter(scientific == "Salamandra salamandra") %>%
  mutate(BsalDetected = case_when(BsalDetected == "1" ~ "nPos_FS",
                                  BsalDetected == "0" ~ "nNeg_FS")) %>%
  group_by(country, Site, posSite, date, scientific, BsalDetected) %>%
  summarise(n = sum(individualCount)) %>%
  pivot_wider(names_from = c(BsalDetected),
              id_cols = c(country, Site, posSite, date, scientific),
              values_from = n, values_fill = 0)

## Tally excluding fire salamanders
BsalCount_noFS <- df %>%
  subset(., select = c(country, Site, posSite, date, scientific, BsalDetected, individualCount)) %>%
  filter(scientific != "Salamandra salamandra") %>%
  mutate(BsalDetected = case_when(BsalDetected == "1" ~ "nPos_noFS",
                                  BsalDetected == "0" ~ "nNeg_noFS")) %>%
  group_by(country, Site, posSite, date, scientific, BsalDetected) %>%
  summarise(n = sum(individualCount)) %>%
  pivot_wider(names_from = c(BsalDetected),
              id_cols = c(country, Site, posSite, date, scientific),
              values_from = n, values_fill = 0)

### > Summarise fatal observations in dataset ----------------------------------
fatalCount_all <- df %>%
  subset(., select = c(country, Site, posSite, date, scientific, fatal, individualCount)) %>%
  mutate(fatal = case_when(fatal == "1" ~ "nDead_all",
                           fatal == "0" ~ "nAlive_all",
                           is.na(fatal) ~ "nFatalUnk_all")) %>%
  group_by(country, Site, posSite, date, scientific, fatal) %>%
  summarise(n = sum(individualCount)) %>%
  pivot_wider(names_from = c(fatal),
              id_cols = c(country, Site, posSite, date, scientific),
              values_from = n, values_fill = 0)

fatalCount_FS_only <- df %>%
  subset(., select = c(country, Site, posSite, date, scientific, fatal, individualCount)) %>%
  filter(scientific == "Salamandra salamandra") %>%
  mutate(fatal = case_when(fatal == "1" ~ "nDead_FS",
                           fatal == "0" ~ "nAlive_FS",
                           is.na(fatal) ~ "nFatalUnk_FS")) %>%
  group_by(country, Site, posSite, date, scientific, fatal) %>%
  summarise(n = sum(individualCount)) %>%
  pivot_wider(names_from = c(fatal),
              id_cols = c(country, Site, posSite, date, scientific),
              values_from = n, values_fill = 0)

fatalCount_noFS <- df %>%
  subset(., select = c(country, Site, posSite, date, scientific, fatal, individualCount)) %>%
  filter(scientific != "Salamandra salamandra") %>%
  mutate(fatal = case_when(fatal == "1" ~ "nDead_noFS",
                           fatal == "0" ~ "nAlive_noFS",
                           is.na(fatal) ~ "nFatalUnk_noFS")) %>%
  group_by(country, Site, posSite, date, scientific, fatal) %>%
  summarise(n = sum(individualCount)) %>%
  pivot_wider(names_from = c(fatal),
              id_cols = c(country, Site, posSite, date, scientific),
              values_from = n, values_fill = 0)

### > Combine it all -----------------------------------------------------------
dcbind_subset <- df %>%
  subset(., select = c(continent:country, ADM1:siteAbun, genus:susceptibility,
                       establishmentMeans:dataConfidence)) %>%
  # subset(., select = c(country:susceptibility, establishmentMeans:bio19_wc, principalInvestigator:dataConfidence)) %>%
  left_join(BsalCount_all, by = c("country", "Site", "posSite", "date", "scientific")) %>%
  left_join(fatalCount_all, by = c("country", "Site", "posSite", "date", "scientific")) %>%
  left_join(BsalCount_FS_only, by = c("country", "Site", "posSite", "date", "scientific")) %>%
  left_join(fatalCount_FS_only, by = c("country", "Site", "posSite", "date", "scientific")) %>%
  left_join(BsalCount_noFS, by = c("country", "Site", "posSite", "date", "scientific")) %>%
  left_join(fatalCount_noFS, by = c("country", "Site", "posSite", "date", "scientific")) %>%
  mutate_at(c(66:80), ~replace(., is.na(.), 0)) %>%
  relocate(c(nNeg_all:nDead_noFS), .after = populationTrend) %>%
  distinct()


dcbind_subset <- with(dcbind_subset, dcbind_subset[order(Site, scientific), ])

rm(BsalCount_all, BsalCount_FS_only, BsalCount_noFS, fatalCount_all, fatalCount_FS_only, fatalCount_noFS,
   out, out_ind, df, check)

dcbind_subset <- dcbind_subset %>%
  filter(continent == "Europe") %>%
  #   Retain sites that have ever had a Bsal+, regardless of when the first positive at that site was
  filter(!(is.na(dateFirstPositive))) %>%
  # transform vars
  mutate(logsppAbun = log(sppAbun + 1),
         logsiteAbun = log(siteAbun + 1),
         scientific = as.factor(scientific),
         susceptibility = as.factor(susceptibility)) %>%
  relocate(c(logsppAbun, logsiteAbun), .after = sppAbun) %>%
  ## Data prep for cbind models: drop rows with NA vals in weather data & scale relevant vars
  #   bio1_wc = "annual mean temperature" | bio12_wc = annual precipitation
  tidyr::drop_na(., any_of(c("temp_d", "sMoist_d", "tmin_wc:bio1_wc", "bio12_wc"))) %>%
  mutate_at(c("temp_d", "sMoist_d",
              "bio1_wc", "bio12_wc", "tavg_wc", "prec_wc"),
            ~(scale(., center = T, scale = T %>% as.numeric))) %>%
  mutate(year = year(date))

dcbind_pos <- dcbind_subset %>%
  filter((scientific != "Triturus anatolicus" | scientific != "Pelophylax sp." & country == "Germany")) %>%
  filter(posSite == 1)


## II. Testing assumptions of the dilution effect hypothesis -------------------
### a. Hosts differ in their reservoir competence. -----------------------------
sampSize <- d_subset %>%
  filter(continent == "Europe" & posSite == 1) %>%
  subset(., select = c(country, Site, scientific, susceptibility, BsalDetected, individualCount)) %>%
  plyr::mutate(BsalDetected = as.factor(dplyr::recode(BsalDetected,
                                                      "1" = "nPos",
                                                      "0" = "nNeg"))) %>%
  group_by(country, Site, scientific, BsalDetected) %>%
  mutate(n = sum(individualCount)) %>%
  dplyr::select(!(individualCount)) %>%
  unique() %>%
  pivot_wider(names_from = BsalDetected, id_cols = c(country, Site, scientific, susceptibility), values_from = n) %>%
  mutate(nPos = case_when(is.na(nPos) ~ 0,
                          TRUE ~ nPos),
         nNeg = case_when(is.na(nNeg) ~ 0,
                          TRUE ~ nNeg)) %>%
  group_by(country, scientific, Site) %>%
  mutate(sppTotal = sum(nPos, nNeg)) %>%
  ungroup() %>%
  group_by(country, Site) %>%
  mutate(siteTotal = sum(nPos, nNeg)) %>%
  ungroup()


## > Observed prevalence by species
prev <- sampSize %>%
  subset(., select = c(country, scientific, susceptibility, nPos, sppTotal)) %>%
  group_by(country, scientific) %>%
  mutate(ncas_Bsal = sum(nPos), # number of observed Bsal+ cases
         npop = sum(sppTotal)) %>% # pop size (total # individuals/spp.)
  ungroup() %>%
  mutate(Bsal_prev = (ncas_Bsal/npop)*100) %>% # prevalence as a percentage
  subset(., select = c(country, scientific, susceptibility, ncas_Bsal, Bsal_prev, npop)) %>%
  unique() %>%
  filter(!(npop < 10)) %>%
  dplyr::mutate(row_id = row_number()) %>%
  relocate(row_id, .before = country)


# Use binom.bayes() function to sample binomial distribution and get CIs for prevalence
bsal_bayesci <- binom::binom.bayes(x = prev$ncas_Bsal, n = prev$npop, type = "highest", tol = 1e-9, maxit = 1000)

# Convert bayesian estimates + CIs to percentages for plotting
bsal_bayesci <-  bsal_bayesci %>%
  plyr::mutate(mean = round((mean*100), 1),
               lower = round((lower*100), 1),
               upper = round((upper*100), 1)) %>%
  dplyr::mutate(row_id = row_number()) %>%
  relocate(row_id, .before = method)


# Join with original 'prev' dataset, so we can plot actual vs expected
prev <- prev %>%
  left_join(., bsal_bayesci, by = "row_id") %>%
  plyr::arrange(., scientific)

##### > Figure 2a --------------------------------------------------------------
# make dummy df for labels
dummy <- prev %>%
  subset(., select = c(country, scientific, susceptibility, mean, lower, upper))

fig2a <- ggplot(prev, aes(scientific, sapply(mean, FUN = function(x) ifelse(x == 0.0, round(x, 0), x)),
                                    label = paste(mean,"%"))) +
  geom_point(aes(colour = susceptibility,
                 shape = country,
                 group = country), position = position_dodge2(width = 0.9), size = 5) +
  geom_errorbar(aes(ymin = lower, ymax = upper,
                    colour = susceptibility,
                    group = country), position = position_dodge(width = 0.9), width = 0.5, linewidth = 1, show.legend = F) +
  geom_text(dummy, mapping =  aes(x = scientific, y = upper + 6, colour = susceptibility, group = country),
            position = position_dodge2(width = 0.9, preserve = "single"),
            size = 6.75, fontface = "bold", alpha = 0.75, show.legend = F) +
  labs(x = NULL,
       y = "Disease prevalence (%)") +
  coord_flip(clip = "off") +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  scale_y_continuous(labels = seq(0, 80, 20),
                     breaks = seq(0, 80, 20),
                     limits = c(0, 85)) +
  # scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(0.5, 0.025)),
  #                  limits = rev) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5)),
                   limits = rev) +
    ak_theme + theme(axis.text.y = element_text(face = "italic"),
                   legend.position = "top",
                   legend.key.size = unit(1, "cm"),
                   legend.box = "vertical",
                   legend.spacing.y = unit(0.01, "cm"),
                   legend.text = element_text(margin = margin(l = 0.1, r = 0.75, unit = "cm"), size = 24),
                   legend.title = element_blank(),
                   plot.margin = margin(0.25, 1, 0.25, 1, unit = "cm")) +
  guides(colour = guide_legend(override.aes = list(color = c("#548078", "#E3A630", "#b30000"),
                                                   shape = c(15, 15, 15),
                                                   size = c(10, 10, 10))),
         shape = guide_legend(order = 1,
                              override.aes = list(size = c(7, 7))))

fig2a

# ggsave("fig2a_bayes.pdf", fig2a, device = cairo_pdf, path = figpath,
#        width = 2700, height = 2000, scale = 1.5, units = "px", dpi = 300, limitsize = F)
#
rm(bsal_bayesci, prev, sampSize, dummy)

### b. The most susceptible species are the most abundant ----------------------
## Make sure to remove observations associated with a site when it was negative even if it later became positive
d <- d %>%
  filter(posSite == 1)

dcbind <- dcbind %>%
  filter(posSite == 1)

d_subset <- d_subset %>%
  filter((scientific != "Triturus anatolicus" | scientific != "Pelobates cultripes" & country == "Germany")) %>%
  filter(posSite == 1)

#### i. Including fire salamanders --------------------------------------------
m2b <- glmmTMB(logsppAbun ~ scientific + (1|Site) + (1|associatedReferences),
               data = d_subset,
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))



summary(m2b) # glmm overview
Anova(m2b)
# simulationOutput <- simulateResiduals(m2b)
# plot(simulationOutput)



##### > Tables -----------------------------------------------------------------
# tab_model(m2b)
m2b_tbl <- gtsummary::tbl_regression(m2b, exponentiate = T,
                                          intercept = F, label = nicelabs) %>%
  add_glance_source_note(label = list(sigma = "\u03c3"),
                         include = c(nobs, AIC, df.residual)) %>%
  bold_p() %>%
  modify_header(label = "**Variable**", estimate = "**OR**") %>%
  modify_footnote(estimate ~ "OR = Odds Ratio") %>%
  add_significance_stars(pattern = "{p.value}{stars}", hide_se = T, hide_p = F) %>%
  # add stars to model p-val
  modify_fmt_fun(estimate ~ style_pvalue_stars,
                 rows = row_type == "glance_statistic" & label == "p-value")

m2b_tbl


# using glht for multcomps.
m2b_post.hoc <- glht(m2b, linfct = mcp(scientific = "Tukey"), alternative = "greater") %>%
  broom::tidy() %>%
  dplyr::select(-(term)) %>%
  dplyr::mutate(signif = gtools::stars.pval(adj.p.value)) %>%
  flextable(theme_fun = theme(booktabs)) %>%
  set_header_labels(., contrast = "Species",
                    null.value = "Null",
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "Statistic",
                    adj.p.value = "Adjusted p-value",
                    signif = "") %>%
  add_header_row(., values = "Tukey's Multiple Comparisons - All species", colwidths = 7) %>%
  footnote(., i = 2, j = 6, inline = T, ref_symbols = c("1"), part = "header",
           value = as_paragraph("*p<0.05; **p<0.01; ***p<0.001")) %>%
  set_formatter_type(fmt_double = "%#.3f") %>%
  flextable::align(., align = "center", part = "all") %>%
  flextable::align(j = "adj.p.value", align = "right") %>%
  flextable::align(j = "signif", align = "left") %>%
  padding(padding.right = 0, j = 'adj.p.value', part = "all") %>%
  padding(padding.left = 0, j = "signif", part  = "all") %>%
  autofit() %>%
  bold(bold = TRUE, part = "header") %>%
  color(i = ~ adj.p.value >= 0.05,
        j = c("contrast", "null.value", "estimate", "std.error",
              "statistic", "adj.p.value", "signif"),
        color = "gray") %>%
  bold(bold = T,
       i = ~ adj.p.value < 0.05,
       j = c("contrast", "null.value", "estimate", "std.error",
             "statistic", "adj.p.value", "signif"))


m2b_post.hoc


##### > Figure 2b --------------------------------------------------------------
# Subset observed abundance
obs_abun <- d_subset %>%
  filter(scientific != "Triturus anatolicus" | scientific != "Pelobates cultripes" & country != "Germany") %>%
  dplyr::select(scientific, susceptibility, sppAbun, Site) %>% # subset relevant data
  rename(obs_abun = sppAbun) %>%
  mutate(Site = as.factor(Site)) %>%
  group_by(scientific, Site) %>%
  unique() %>%
  ungroup()


m2b_predict <- ggpredict(m2b, terms = "scientific") %>%
  dplyr::rename("scientific" = "x",
                "logsppAbun" = "predicted",
                "expectedAbun" = "group") %>%
  left_join(., obs_abun, by = "scientific") %>%
  plyr::mutate(expectedAbun = exp(logsppAbun - 1),
               conf.low = exp(conf.low - 1),
               conf.high = exp(conf.high - 1))


# xhat <- TeX(r"($\hat{X}_{\textit{a}} =)") ## LaTeX formula: $\hat{X}_{\textit{a}} = ## THIS ALSO WORKS, BUT WILL TRY USING LATEX EXP BELOW
TeXlabl <- glue::glue("$\\textbf{\\hat{x}_{\\textit{a}}}}}=", .open = "{{") # THIS WORKS
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

fig2b <- ggplot(m2b_predict, aes(x = scientific, y = expectedAbun,
                                    color = susceptibility,
                                    label = round(expectedAbun, 0)),
                   shape = 19, size = 4.5) +
  geom_point(shape = 19, size = 4.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = susceptibility), width = 0.5, linewidth = 1,
                show.legend = F) +
  geom_jitter(aes(x = scientific,
                            y = obs_abun,
                            group = scientific,
                            colour = susceptibility), shape = 21, size = 2, alpha = 0.5) +
  # geom_text(aes(colour = susceptibility, x = (as.numeric(scientific) + 0.05), y = conf.high + 3),
  #           size = 6, fontface = "bold", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df1$scientific) + 0.1), y = (df1$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#548078", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df2$scientific) + 0.1), y = (df2$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#E3A630", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df3$scientific) + 0.1), y = (df3$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#b30000", alpha = 0.75) +
  coord_flip(clip = "off") +
  labs(x = "",
       y = "Abundance",
       caption = "Figure displays all species from our dataset.") +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  scale_y_continuous(labels = seq(0, 60, 10),
                     breaks = seq(0, 60, 10),
                     limits = c(0, 60)) + # all outliers removed
  # scale_y_break(c(64, 84)) +
  # scale_y_break(c(95, 110)) +
  # scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(0.5, 0.025)),
  #                  limits = rev) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5)),
                   limits = rev) +
  ak_theme + theme(axis.text.y = element_text(face = "italic"),
                   legend.position = element_blank(),
                   plot.margin = margin(0.25, 1, 0.25, 1, unit = "cm"))

fig2b

# ggsave("fig2b_all.pdf", fig2b, device = cairo_pdf, path = figpath,
#        width = 2700, height = 2000, scale = 1.5, units = "px", dpi = 300, limitsize = F)
#
rm(df1, df2, df3, obs_abun, m2b, m2b_predict, TeXlabl, xhat)
#### ii. Excluding fire salamanders -------------------------------------------
m2b_noFS <- glmmTMB(logsppAbun ~ scientific + (1|Site) + (1|associatedReferences),
                    data = filter(d_subset, scientific != "Salamandra salamandra"),
                    control = glmmTMBControl(optimizer = optim,
                                             optArgs = list(method = "BFGS")))


summary(m2b_noFS) # glmm overview


##### > Tables -----------------------------------------------------------------
m2b_noFS_tbl <- gtsummary::tbl_regression(m2b_noFS, exponentiate = T,
                                     intercept = F, label = nicelabs) %>%
  add_glance_source_note(label = list(sigma = "\u03c3"),
                         include = c(nobs, AIC, df.residual)) %>%
  bold_p() %>%
  modify_header(label = "**Variable**", estimate = "**OR**") %>%
  modify_footnote(estimate ~ "OR = Odds Ratio") %>%
  add_significance_stars(pattern = "{p.value}{stars}", hide_se = T, hide_p = F) %>%
  # add stars to model p-val
  modify_fmt_fun(estimate ~ style_pvalue_stars,
                 rows = row_type == "glance_statistic" & label == "p-value")

m2b_noFS_tbl

m2b_noFS_post.hoc <- glht(m2b_noFS, linfct = mcp(scientific = "Tukey"), alternative = "greater") %>%
  broom::tidy() %>%
  dplyr::select(-(term)) %>%
  dplyr::mutate(signif = gtools::stars.pval(adj.p.value)) %>%
  flextable(theme_fun = theme(booktabs)) %>%
  set_header_labels(., contrast = "Species",
                    null.value = "Null",
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "Statistic",
                    adj.p.value = "Adjusted p-value",
                    signif = "") %>%
  add_header_row(., values = "Tukey's Multiple Comparisons - No fire salamanders", colwidths = 7) %>%
  footnote(., i = 2, j = 6, inline = T, ref_symbols = c("1"), part = "header",
           value = as_paragraph("*p<0.05; **p<0.01; ***p<0.001")) %>%
  set_formatter_type(fmt_double = "%#.3f") %>%
  flextable::align(., align = "center", part = "all") %>%
  flextable::align(j = "adj.p.value", align = "right") %>%
  flextable::align(j = "signif", align = "left") %>%
  padding(padding.right = 0, j = 'adj.p.value', part = "all") %>%
  padding(padding.left = 0, j = "signif", part  = "all") %>%
  autofit() %>%
  bold(bold = TRUE, part = "header") %>%
  color(i = ~ adj.p.value >= 0.05,
        j = c("contrast", "null.value", "estimate", "std.error",
              "statistic", "adj.p.value", "signif"),
        color = "gray") %>%
  bold(bold = T,
       i = ~ adj.p.value < 0.05,
       j = c("contrast", "null.value", "estimate", "std.error",
             "statistic", "adj.p.value", "signif"))

m2b_noFS_post.hoc

## Combine model2b 'All spp' tables with model2a 'no FS' tables
# dir.create(tblpath)
model2tbls <- tbl_merge(tbls = list(m2b_tbl, m2b_noFS_tbl),
                        tab_spanner = c("**All species**", "**No fire salamanders**")) %>%
  as_gt() %>%
  gt::gtsave(., filename = "model2b_tbls.docx", path = tblpath)

## Combine model2b Tukey HSD tables
save_as_docx(
  "Model 2b Tukey's HSD - All species" = m2b_post.hoc,
  "Model 2b Tukey's HSD - No fire salamanders" = m2b_noFS_post.hoc,
  path = file.path(tblpath, "/model2b_TukeyHSD.docx")
)

rm(m2b_post.hoc, m2b_noFS_post.hoc, m2b_tbl, m2b_noFS_tbl, model2tbls)
##### > Supp. Figure 2b --------------------------------------------------------
# Subset observed abundance
obs_abun <- d_subset %>%
  filter(scientific != "Salamandra salamandra") %>%
  dplyr::select(scientific, susceptibility, sppAbun, Site) %>% # subset relevant data
  rename(obs_abun = sppAbun) %>%
  mutate(Site = as.factor(Site)) %>%
  group_by(scientific, Site) %>%
  unique() %>%
  ungroup()


m2b_noFS_predict <- ggpredict(m2b_noFS, terms = "scientific") %>%
  dplyr::rename("scientific" = "x",
                "logsppAbun" = "predicted",
                "expectedAbun" = "group") %>%
  left_join(., obs_abun, by = "scientific") %>%
  plyr::mutate(expectedAbun = exp(logsppAbun - 1),
               conf.low = exp(conf.low - 1),
               conf.high = exp(conf.high - 1))

# Create color coded labels for graph annotation
# Resistant
df1 <- m2b_noFS_predict %>%
  filter(susceptibility == "1")

# Tolerant
df2 <- m2b_noFS_predict %>%
  filter(susceptibility == "2")

# Susceptible
df3 <- m2b_noFS_predict %>%
  filter(susceptibility == "3")

fig2b_noFS <- ggplot(m2b_noFS_predict, aes(x = scientific, y = expectedAbun,
                                              color = susceptibility,
                                              label = round(expectedAbun, 0))) +
  geom_point(shape = 19, size = 4.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = susceptibility),
                width = 0.5, linewidth = 1,show.legend = F) +
  geom_jitter(aes(x = scientific,
                  y = obs_abun,
                  group = scientific,
                  colour = susceptibility), shape = 21, size = 2, alpha = 0.5) +
  # geom_text(aes(colour = susceptibility, x = (as.numeric(scientific) + 0.05), y = conf.high + 3),
  #           size = 6, fontface = "bold", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df1$scientific) + 0.1), y = (df1$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#548078", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df2$scientific) + 0.1), y = (df2$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#E3A630", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df3$scientific) + 0.1), y = (df3$conf.high + 1.5),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#b30000", alpha = 0.75) +
  coord_flip(clip = "off") +
  labs(x = "",
       y = "Abundance",
       caption = "Figure displays all species from our dataset, excluding fire salamanders.") +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  scale_y_continuous(labels = seq(0, 60, 10),
                     breaks = seq(0, 60, 10),
                     limits = c(0, 60)) + # all outliers removed
  # scale_y_break(c(64, 84)) +
  # scale_y_break(c(95, 110)) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(0.5, 0.025)),
                   limits = rev) +
  ak_theme + theme(axis.text.y = element_text(face = "italic"),
                   legend.position = "top",
                   legend.key.size = unit(1, "cm"),
                   legend.box = "vertical",
                   legend.spacing.y = unit(0.01, "cm"),
                   legend.text = element_text(margin = margin(l = 0.1, r = 0.75, unit = "cm"), size = 24),
                   legend.title = element_blank(),
                   plot.margin = margin(0.25, 1, 0.25, 1, unit = "cm")) +
  guides(colour = guide_legend(override.aes = list(color = c("#548078", "#E3A630", "#b30000"),
                                                   shape = c(15, 15, 15),
                                                   size = c(10, 10, 10))))



fig2b_noFS

# ggsave("fig2b_noFS.pdf", fig2b_noFS, device = cairo_pdf, path = figpath,
#        width = 2700, height = 2000, scale = 1.5, units = "px", dpi = 300, limitsize = F)

rm(df1, df2, df3, obs_abun, m2b_noFS, fig2b_noFS, m2b_noFS_predict)

### c. Host abundance and susceptibility in our dataset ------------------------
#### i. Including fire salamanders --------------------------------------------
m2c <- glmmTMB(logsppAbun ~ susceptibility + (1|Site) + (1|associatedReferences),
               data = d_subset,
               control = glmmTMBControl(optimizer = optim,
                                        optArgs = list(method = "BFGS")))
summary(m2c) # glmm overview
Anova(m2c)
# simulationOutput <- simulateResiduals(m2c)
# plot(simulationOutput, quantreg = TRUE)
#
# simulationOutput <- recalculateResiduals(simulationOutput, group = d$scientific)


##### > Tables -----------------------------------------------------------------
m2c_tbl <- gtsummary::tbl_regression(m2c, exponentiate = T,
                                        intercept = F, label = nicelabs) %>%
  add_glance_source_note(label = list(sigma = "\u03c3"),
                         include = c(nobs, AIC, df.residual)) %>%
  bold_p() %>%
  modify_header(label = "**Variable**", estimate = "**OR**") %>%
  modify_footnote(estimate ~ "OR = Odds Ratio") %>%
  add_significance_stars(pattern = "{p.value}{stars}", hide_se = T, hide_p = F) %>%
  # add stars to model p-val
  modify_fmt_fun(estimate ~ style_pvalue_stars,
                 rows = row_type == "glance_statistic" & label == "p-value")

m2c_tbl


# using glht for multcomps.
m2c_post.hoc <- glht(m2c, linfct = mcp(susceptibility = "Tukey"), alternative = "greater") %>%
  broom::tidy() %>%
  dplyr::select(-(term)) %>%
  dplyr::mutate(signif = gtools::stars.pval(adj.p.value)) %>%
  flextable(theme_fun = theme(booktabs)) %>%
  set_header_labels(., contrast = "Species",
                    null.value = "Null",
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "Statistic",
                    adj.p.value = "Adjusted p-value",
                    signif = "") %>%
  add_header_row(., values = "Tukey's Multiple Comparisons - All species", colwidths = 7) %>%
  footnote(., i = 2, j = 6, inline = T, ref_symbols = c("1"), part = "header",
           value = as_paragraph("*p<0.05; **p<0.01; ***p<0.001")) %>%
  set_formatter_type(fmt_double = "%#.3f") %>%
  flextable::align(., align = "center", part = "all") %>%
  flextable::align(j = "adj.p.value", align = "right") %>%
  flextable::align(j = "signif", align = "left") %>%
  padding(padding.right = 0, j = 'adj.p.value', part = "all") %>%
  padding(padding.left = 0, j = "signif", part  = "all") %>%
  autofit() %>%
  bold(bold = TRUE, part = "header") %>%
  color(i = ~ adj.p.value >= 0.05,
        j = c("contrast", "null.value", "estimate", "std.error",
              "statistic", "adj.p.value", "signif"),
        color = "gray") %>%
  bold(bold = T,
       i = ~ adj.p.value < 0.05,
       j = c("contrast", "null.value", "estimate", "std.error",
             "statistic", "adj.p.value", "signif"))

m2c_post.hoc

##### > Figure 2c --------------------------------------------------------------
## post-hoc test for multiple comparison of means
post.hoc <- glht(m2c, linfct = mcp(susceptibility = "Tukey"),
                 alternative = "greater")
fortify(post.hoc)

mcLabs <- glht(m2c, linfct = mcp(susceptibility = "Tukey"),
               alternative = "greater") %>%
  broom::tidy() %>%
  dplyr::select(-(term)) %>%
  flip(.) %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  subset(., select = c(group1, group2, adj.p.value)) %>%
  # add letters for comparison
  mutate(cld = cld(post.hoc, decreasing = T)$mcletters$Letters) %>%
  mutate(signif = gtools::stars.pval(adj.p.value)) %>%
  mutate(signif = case_when(signif == " " ~ "n.s.",
                            TRUE ~ signif))

## Summarise data for labels
n <- d_subset %>%
  group_by(susceptibility) %>%
  summarise(n = n())

m2c_predict <- ggpredict(m2c, terms = c("susceptibility")) %>%
  dplyr::rename("susceptibility" = "x",
                "group" = "group") %>%
  mutate(susceptibility = as.factor(susceptibility),
         predicted = exp(as.numeric(predicted - 1)),
         conf.high = exp(as.numeric(conf.high - 1)),
         conf.low = exp(as.numeric(conf.low - 1))) %>%
  drop_na(.) %>%
  left_join(., n, by = "susceptibility")


fig2c <- ggplot(m2c_predict, aes(susceptibility, predicted, color = susceptibility)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 1) +
  stat_pvalue_manual(filter(mcLabs, !(signif == "n.s.")), y.position = 9, label = "signif",
                     step.increase = 0.1, label.size = 10,bracket.size = 1) +
  stat_pvalue_manual(filter(mcLabs, (signif == "n.s.")), y.position = 5, label = "signif",
                     label.size = 10, bracket.size = 1) +
  # geom_richtext(aes(y = (conf.high + 0.5), label = paste0("n<span style = 'font-size:15pt'><sub>*obs*</sub> </span>= ", n)),
  #               alpha = 0.75, size = 8, label.size = NA, fill = NA, fontface = "bold", show.legend = F) +
  # annotate("text", x = 3, y = 4.25, label = "***", size = 10, fontface = "bold",
  #          colour = "#b30000") +
  # annotate("text", x = 0.65, y = 9, label = "C", size = 12, fontface = "bold",
  #          colour = "black") +
  scale_x_discrete(labels = c("Resistant", "Tolerant", "Susceptible")) +
  labs(x = "Susceptibility category",
       y = "Species abundance",
       caption = "All species at at our sites.") +
  scale_y_continuous(limits = c(0, 10.5),
                     breaks = seq(0, 10, 2)) +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", # 9 spp
                                 "#E3A630", # 15 spp
                                 "#b30000"),# 5 spp
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  ak_theme + theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, unit = "cm"),
                   legend.position = element_blank())


fig2c


# ggsave("fig2c.pdf", fig2c, device = cairo_pdf, path = figpath,
#           width = 2000, height = 1300, scale = 1.5, units = "px", dpi = 300, limitsize = F)

rm(m2c, m2c_predict, mcLabs, n, post.hoc)
#### ii. Excluding fire salamanders -------------------------------------------
m2c_noFS <- glmmTMB(logsppAbun ~ susceptibility + (1|Site) + (1|associatedReferences),
                    data = filter(d_subset, scientific != "Salamandra salamandra"),
                    control = glmmTMBControl(optimizer = optim,
                                             optArgs = list(method = "BFGS")))
summary(m2c_noFS) # glmm overview


##### > Tables -----------------------------------------------------------------
m2c_noFS_tbl <- gtsummary::tbl_regression(m2c_noFS, exponentiate = T,
                                     intercept = F, label = nicelabs) %>%
  add_glance_source_note(label = list(sigma = "\u03c3"),
                         include = c(nobs, AIC, df.residual)) %>%
  bold_p() %>%
  modify_header(label = "**Variable**", estimate = "**OR**") %>%
  modify_footnote(estimate ~ "OR = Odds Ratio") %>%
  add_significance_stars(pattern = "{p.value}{stars}", hide_se = T, hide_p = F) %>%
  # add stars to model p-val
  modify_fmt_fun(estimate ~ style_pvalue_stars,
                 rows = row_type == "glance_statistic" & label == "p-value")

m2c_noFS_tbl

# using glht for multcomps.
m2c_noFS_post.hoc <- glht(m2c_noFS, linfct = mcp(susceptibility = "Tukey"), alternative = "greater") %>%
  broom::tidy() %>%
  dplyr::select(-(term)) %>%
  dplyr::mutate(signif = gtools::stars.pval(adj.p.value)) %>%
  flextable(theme_fun = theme(booktabs)) %>%
  set_header_labels(., contrast = "Species",
                    null.value = "Null",
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "Statistic",
                    adj.p.value = "Adjusted p-value",
                    signif = "") %>%
  add_header_row(., values = "Tukey's Multiple Comparisons - No fire salamanders", colwidths = 7) %>%
  footnote(., i = 2, j = 6, inline = T, ref_symbols = c("1"), part = "header",
           value = as_paragraph("*p<0.05; **p<0.01; ***p<0.001")) %>%
  set_formatter_type(fmt_double = "%#.3f") %>%
  flextable::align(., align = "center", part = "all") %>%
  flextable::align(j = "adj.p.value", align = "right") %>%
  flextable::align(j = "signif", align = "left") %>%
  padding(padding.right = 0, j = 'adj.p.value', part = "all") %>%
  padding(padding.left = 0, j = "signif", part  = "all") %>%
  autofit() %>%
  bold(bold = TRUE, part = "header") %>%
  color(i = ~ adj.p.value >= 0.05,
        j = c("contrast", "null.value", "estimate", "std.error",
              "statistic", "adj.p.value", "signif"),
        color = "gray") %>%
  bold(bold = T,
       i = ~ adj.p.value < 0.05,
       j = c("contrast", "null.value", "estimate", "std.error",
             "statistic", "adj.p.value", "signif"))

m2c_noFS_post.hoc

## Combine model2c 'All spp' tables with model 2c 'no FS' tables
# dir.create(tblpath)
model2ctbls <- tbl_merge(tbls = list(m2c_tbl, m2c_noFS_tbl),
                        tab_spanner = c("**All species**", "**No fire salamanders**")) %>%
  as_gt() %>%
  gt::gtsave(., filename = "model2c_tbls.docx", path = tblpath)

## Combine model2b Tukey HSD tables
save_as_docx(
  "Model 2c Tukey's HSD - All species" = m2c_post.hoc,
  "Model 2c Tukey's HSD - No fire salamanders" = m2c_noFS_post.hoc,
  path = file.path(tblpath, "/model2c_TukeyHSD.docx")
)

rm(m2c_noFS_post.hoc, m2c_noFS_tbl, m2c_post.hoc, m2c_tbl, model2ctbls)
##### > Supp Figure 2c ---------------------------------------------------------

## post-hoc test for multiple comparison of means
post.hoc <- glht(m2c_noFS, linfct = mcp(susceptibility = "Tukey"),
                 alternative = "greater")
fortify(post.hoc)

mcLabs <- glht(m2c_noFS, linfct = mcp(susceptibility = "Tukey"),
               alternative = "greater") %>%
  broom::tidy() %>%
  dplyr::select(-(term)) %>%
  flip(.) %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  subset(., select = c(group1, group2, adj.p.value)) %>%
  # add letters for comparison
  mutate(cld = cld(post.hoc, decreasing = T)$mcletters$Letters) %>%
  mutate(signif = gtools::stars.pval(adj.p.value)) %>%
  mutate(signif = case_when(signif == " " ~ "n.s.",
                            TRUE ~ signif))

## Summarise data for labels
n <- d_subset %>%
  filter(!(scientific == "Salamandra salamandra")) %>%
           group_by(susceptibility) %>%
           summarise(n = n())

m2c_noFS_predict <- ggpredict(m2c_noFS, terms = c("susceptibility")) %>%
  dplyr::rename("susceptibility" = "x",
                         "group" = "group") %>%
  mutate(susceptibility = as.factor(susceptibility),
                  predicted = exp(as.numeric(predicted - 1)),
                  conf.high = exp(as.numeric(conf.high - 1)),
                  conf.low = exp(as.numeric(conf.low - 1))) %>%
  drop_na(.) %>%
  left_join(., n, by = "susceptibility")


fig2c_noFS <- ggplot(m2c_noFS_predict, aes(susceptibility, predicted, color = susceptibility)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 1) +
  stat_pvalue_manual(filter(mcLabs, !(signif == "n.s.")), y.position = 15.75, label = "signif",
                     step.increase = 0.1, label.size = 10,bracket.size = 1) +
  stat_pvalue_manual(filter(mcLabs, (signif == "n.s.")), y.position = 6, label = "signif",
                     label.size = 10, bracket.size = 1) +
  # geom_richtext(aes(y = (conf.high + 0.5), label = paste0("n<span style = 'font-size:15pt'><sub>*obs*</sub> </span>= ", n)),
  #               alpha = 0.75, size = 8, label.size = NA, fill = NA, fontface = "bold", show.legend = F) +
  # annotate("text", x = 3, y = 4.25, label = "***", size = 10, fontface = "bold",
  #          colour = "#b30000") +
  # annotate("text", x = 0.65, y = 9, label = "C", size = 12, fontface = "bold",
  #          colour = "black") +
  scale_x_discrete(labels = c("Resistant", "Tolerant", "Susceptible")) +
  labs(x = "Susceptibility level",
       y = "Species abundance",
       caption = "All species in the dataset (except for fire salamanders) and all observations from both continents were retained in these analyses.") +
  scale_y_continuous(limits = c(0, 18),
                     breaks = seq(0, 18, 2)) +
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", # 9 spp
                                 "#E3A630", # 15 spp
                                 "#b30000"),# 5 spp
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  ak_theme + theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, unit = "cm"),
                   legend.position = element_blank())


fig2c_noFS

# ggsave("fig2c_noFS.pdf", fig2c_noFS, device = cairo_pdf, path = figpath,
#           width = 2000, height = 1300, scale = 2, units = "px", dpi = 300, limitsize = F)

rm(m2c, m2c_noFS, m2c_noFS_predict, fig2c_noFS, mcLabs, n, post.hoc)

## Figure 2. ------------------------------------------------------------------
## Create guide to force a common legend
commonLegend <- cowplot::get_legend(fig2a) +
  guides(fill = guide_legend(override.aes = list(color = c("#548078", "#E3A630", "#b30000"),
                                                    shape = c(16, 16, 16),
                                                    size = c(2, 2, 2),
                                                    alpha = c(1, 1, 1))))

## Create combined plot for manuscript
fig2a_tag <- fig2a + labs(caption = NULL) +
  theme(plot.tag.position = c(0.96, 0.86)) +
                      commonLegend

fig2b_tag <- fig2b + labs(caption = NULL) +
                       theme(axis.title.y = element_blank(),
                             axis.text.y = element_blank(),
                             plot.tag.position = c(0.92, 0.86)) +
                       commonLegend


fig2c_tag <- fig2c + labs(caption = NULL) +
                      theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                            plot.margin = margin(.75, 2, .75, 0, "cm"),
                            axis.text.x = element_text(size = 32),
                            axis.title.x = element_text(size = 40, hjust = 0.5,
                                                        margin = margin(t = 10, r = 0, b = 0, l = 0),
                                                        face = "plain"),
                            axis.text.y = element_text(size = 32, face = "plain"),
                            axis.title.y = element_text(size = 42, hjust = 0.5,
                                                        margin = margin(t = 0, r = 15, b = 0, l = 5),
                                                        face = "plain"),
                            plot.tag.position = c(0.15, 0.94))

fig2c_tag <- (plot_spacer() + fig2c_tag + plot_spacer()) +
  plot_layout(widths = c(0.01, 1, 0.01),
              heights = 1,
              guides = "collect")


fig2ab <- (fig2a_tag + plot_spacer() + fig2b_tag) +
  plot_layout(widths = c(1, -0.0025, 1),
              heights = 1,
              guides = "collect",
              axes = "collect_x") &
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),
        legend.position = "top",
        legend.box.margin = margin(0.25, 1, 0.25, 1, "cm"),
        legend.text = element_text(margin = margin(r = 0.5, unit = "cm")),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Segoe UI Semibold", face = "bold", size = 42))

fig2ab


fig2combined <- (fig2ab/plot_spacer()/fig2c_tag) +
  plot_layout(guides = "collect",
              widths = c(1,0.01, 3.5),
              heights = c(10, 0.01, 10)) +
                plot_annotation(tag_levels = "A") & theme(legend.position = "top",
                                                          legend.direction = "horizontal",
                                                          legend.spacing.y = unit(0.01, "cm"),
                                                          legend.box.margin = margin(0, 1, 0.25, 1, "cm"),
                                                          legend.text = element_text(margin = margin(l = 0.1, r = 0.75, unit = "cm"), size = 24),
                                                          legend.title = element_blank(),
                                                          plot.tag = element_text(family = "Segoe UI Semibold", face = "bold", size = 42))


fig2combined

ggsave("fig2_combined.pdf", fig2combined, device = cairo_pdf, path = figpath,
       width = 2800, height = 2600, scale = 2, units = "px", dpi = 300, limitsize = F)

rm(fig2a, fig2a_tag, fig2ab, fig2b, fig2b_tag, fig2c, fig2c_tag, fig2combined)
## III. Cbind models testing the dilution dffect hypothesis --------------------
#### i. 'All spp.' (Including fire salamanders) --------------------------------
##### > Locality richness (From collaborators) ---------------------------------
###### > Model selection -------------------------------------------------------
# all_lr <- glmmTMB(cbind(nPos_all, nNeg_all) ~ locality_rich*logsiteAbun +
#                     temp_d*sMoist_d + (1|country) + (1|scientific) + (1|associatedReferences),
#                      data = dcbind_pos,
#                      family = "binomial", na.action = "na.fail",
#                      control = glmmTMBControl(optimizer = optim,
#                                               optArgs = list(method = "BFGS")))
#
# all_lr.2 <- glmmTMB(cbind(nPos_all, nNeg_all) ~ locality_rich*logsiteAbun +
#                       temp_d*sMoist_d + (1|country) + (1|scientific),
#                     data = dcbind_pos,
#                     family = "binomial", na.action = "na.fail",
#                     control = glmmTMBControl(optimizer = optim,
#                                              optArgs = list(method = "BFGS")))
#
# all_lr.3 <- glmmTMB(cbind(nPos_all, nNeg_all) ~ locality_rich*logsiteAbun +
#                       temp_d*sMoist_d + (1|scientific) + (1|associatedReferences),
#                     data = dcbind_pos,
#                     family = "binomial", na.action = "na.fail",
#                     control = glmmTMBControl(optimizer = optim,
#                                              optArgs = list(method = "BFGS")))
#
# all_lr.4<- glmmTMB(cbind(nPos_all, nNeg_all) ~ locality_rich*logsiteAbun +
#                       temp_d*sMoist_d + (1|scientific/country) + (1|associatedReferences),
#                     data = dcbind_pos,
#                     family = "binomial", na.action = "na.fail",
#                     control = glmmTMBControl(optimizer = optim,
#                                              optArgs = list(method = "BFGS")))
#
# all_lr.5 <- glmmTMB(cbind(nPos_all, nNeg_all) ~ locality_rich*logsiteAbun +
#                       temp_d*sMoist_d + (1|scientific/country),
#                     data = dcbind_pos,
#                     family = "binomial", na.action = "na.fail",
#                     control = glmmTMBControl(optimizer = optim,
#                                              optArgs = list(method = "BFGS")))
#
#
# model.sel(all_lr, all_lr.2, all_lr.3, all_lr.4, all_lr.5)
# rm(all_lr, all_lr.2, all_lr.3, all_lr.4, all_lr.5)
###### > Model Checking/Diagnostics --------------------------------------------
all_lr.3 <- glmmTMB(cbind(nPos_all, nNeg_all) ~ locality_rich*logsiteAbun +
                      temp_d*sMoist_d + (1|scientific) + (1|associatedReferences),
                    data = dcbind_pos,
                    family = "binomial", na.action = "na.fail",
                    control = glmmTMBControl(optimizer = optim,
                                             optArgs = list(method = "BFGS")))

summary(all_lr.3) # richness:logsiteAbun and soil moisture are highly significant
Anova(all_lr.3) # soil moisture is n.s.



resid <- simulateResiduals(all_lr.3)
testResiduals(resid)
# plotResiduals(resid)
# testOutliers(resid, type = "bootstrap") # n.s.
# testDispersion(resid) # n.s.
# testQuantiles(resid) # significant deviation, but may be ok
# testZeroInflation(resid) # n.s.


## Need to subset unique lat/lon vals to test for spatial autocorrelation
coords <- dcbind_pos %>%
  subset(., select = c(Site, Lat, Lon)) %>%
  distinct(Site, Lat, Lon, .keep_all = T) %>%
  group_by(Site) %>%
  mutate(Lat = mean(Lat), Lon = mean(Lon)) %>%
  distinct() %>%
  ungroup()

# need to use recalculateResiduals() function because we have multiple observations per location
recalc.resid <- recalculateResiduals(resid, group = coords$Site)

testSpatialAutocorrelation(recalc.resid, x = coords$Lon, y = coords$Lat)
# DHARMa Moran's I test for distance-based autocorrelation
#
# data:  recalc.resid
# observed = -0.099234, expected = -0.020833, sd = 0.045536, p-value = 0.08512
# alternative hypothesis: Distance-based autocorrelation

## No spatial autocorrelation! cool.

rm(resid, recalc.resid, coords)
###### > Tables ----------------------------------------------------------------
m3_all_tbl <- all_lr.3 %>%
  broom::tidy() %>%
  filter(!row_number() %in% c(1, 8:9)) %>%
  mutate(OR = exp(estimate)) %>%
  relocate(OR, .before = p.value) %>%
  left_join(., (Anova(all_lr.3) %>%
                  rownames_to_column(., "term")), by = "term") %>%
  dplyr::select(-c(effect, component, group, estimate, std.error, statistic, p.value,
                   Df)) %>%
  rename(Terms = term,
         pval = `Pr(>Chisq)`) %>%
  mutate(signif = gtools::stars.pval(pval),
         Terms = case_when(Terms == "locality_rich" ~ "Locality richness",
                           Terms == "logsiteAbun" ~ "log(Site abundance)",
                           Terms == "temp_d" ~ paste0("Temperature (\u00b0C)"),
                           Terms == "sMoist_d" ~ paste0("Soil moisture (kg\u00b7m\u00b2)"),
                           Terms == "locality_rich:logsiteAbun" ~ "Locality richness:log(Site abundance)",
                           Terms == "temp_d:sMoist_d" ~ "Temperature:Soil moisture")) %>%
  flextable(theme_fun = theme(booktabs)) %>%
  set_header_labels(., Chisq = chisqlab,
                    pval = "p-value",
                    signif = "") %>%
  add_header_lines(., values = "cbind Model - All species") %>%
  footnote(., i = 2, j = 2,
           value = as_paragraph("OR = Odds Ratio"),
           ref_symbols = "1",
           part = "header") %>%
  footnote(., i = 2, j = 4,
           value = as_paragraph("*p<0.05; **p<0.01; ***p<0.001"),
           ref_symbols = "2",
           part = "header") %>%
  set_formatter_type(fmt_double = "%#.3f") %>%
  flextable::align(j = c("OR", "Chisq"), align = "center", part = "all") %>%
  flextable::align(j = c("Terms", "pval"), align = "right", part = "all") %>%
  flextable::align(j = "signif", align = "left") %>%
  flextable::align(i = 1, j = NULL, align = "center", part = "header") %>%
  flextable::align(., align = "left", part = "footer") %>%
  padding(padding.right = 0, j = 'pval', part = "all") %>%
  padding(padding.left = 0, j = "signif", part  = "all") %>%
  autofit() %>%
  bold(bold = TRUE, part = "header") %>%
  color(i = ~ pval >= 0.05, j = c("Terms", "OR", "Chisq", "pval", "signif"),
        color = "gray") %>%
  bold(bold = T,
       i = ~ pval < 0.05,
       j = c("Terms", "OR", "Chisq", "pval", "signif"))


m3_all_tbl

###### > Plot ------------------------------------------------------------------
m3a_predict <- ggpredict(all_lr.3,  terms = c("locality_rich", "logsiteAbun")) %>%
# m3a_predict <- ggpredict(all_lr.3,  terms = c("locality_rich", "logsiteAbun [1.6094, 2.7726, 3.5835]")) %>% # 1st quartile, median, 3rd quartile
# m3a_predict <- ggpredict(all_lr.3,  terms = c("locality_rich", "logsiteAbun [0.6931, 2.5935, 4.8040]")) %>% # min, mean, max
  dplyr::rename("locality_rich" = "x",
                "logsiteAbun" = "group") %>%
  plyr::mutate(locality_rich = as.numeric(as.character(locality_rich)),
               logsiteAbun = as.numeric(as.character(logsiteAbun)),
               # Convert scaled prediction to original data scale:
               siteAbun = as.factor(round(exp(as.numeric(logsiteAbun - 1)), 0)))



m3a <- ggplot(m3a_predict, aes(x = locality_rich, y = predicted, linetype = siteAbun, colour = siteAbun)) +
  geom_line(aes(linetype = siteAbun, colour = siteAbun), linewidth = 1) +
  # geom_ribbon(aes(ymin = conf.low, ymax = conf.high,
  #                 colour = siteAbun,
  #                 linetype = siteAbun), outline.type = "both", alpha = 0.1, show.legend = F) +
  geom_rug(data = dcbind_pos, aes(x = locality_rich, y = 0), sides = "b",
           alpha = 0.5, position = position_jitter(width = 0.4, height = 0.1),
           inherit.aes = F, na.rm = T) +
  labs(x = "Locality richness",
       y = "Bsal prevalence (%)") +
  geom_label(label = "Europe", size = 8, colour = "gray20", label.padding = unit(0.25, "lines"),
             x = 9, y = 0.055, alpha = 0.75) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_grey(start = 0.8, end = 0.2) +
  scale_x_continuous(labels = seq(0, 6, 1),
                     breaks = seq(0, 6, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, .6, 0.2),
                     limits = c(0, .65),
                     minor_breaks = seq(0, 0.6, 0.1)) +
  ak_theme + theme(plot.tag.position = c(0.96, 0.9),
                   axis.text.y = element_text(face = "plain")) +
  guides(colour = guide_legend("Site-level abundance", title.position = "top", title.hjust = 0.5),
         linetype = guide_legend("Site-level abundance", title.position = "top", title.hjust = 0.5))

m3a
# ggsave("m3a_all.pdf", m3a, device = cairo_pdf, path = figpath,
#        width = 1250, height = 1500, scale = 2, units = "px", dpi = 300, limitsize = F)

#### ii. 'All spp.' (Excluding fire salamanders) -------------------------------
##### > Locality richness (From collaborators) ---------------------------------
###### > Model selection -------------------------------------------------------
# noFS_lr <- all_lr <- glmmTMB(cbind(nPos_noFS, nNeg_noFS) ~ locality_rich*logsiteAbun +
#                       temp_d*sMoist_d + (1|country) + (1|scientific) + (1|associatedReferences),
#                        data = subset(dcbind_pos, scientific != "Salamandra salamandra"),
#                        family = "binomial", na.action = "na.fail",
#                        control = glmmTMBControl(optimizer = optim,
#                                                 optArgs = list(method = "BFGS")))
#
#
# noFS_lr.2 <- glmmTMB(cbind(nPos_noFS, nNeg_noFS) ~ locality_rich*logsiteAbun +
#                       temp_d*sMoist_d + (1|country) + (1|scientific),
#                     data = subset(dcbind_pos, scientific != "Salamandra salamandra"),
#                     family = "binomial", na.action = "na.fail",
#                     control = glmmTMBControl(optimizer = optim,
#                                              optArgs = list(method = "BFGS")))
#
# noFS_lr.3 <- glmmTMB(cbind(nPos_noFS, nNeg_noFS) ~ locality_rich*logsiteAbun +
#                       temp_d*sMoist_d + (1|scientific) + (1|associatedReferences),
#                     data = subset(dcbind_pos, scientific != "Salamandra salamandra"),
#                     family = "binomial", na.action = "na.fail",
#                     control = glmmTMBControl(optimizer = optim,
#                                              optArgs = list(method = "BFGS")))
#
# noFS_lr.4 <- glmmTMB(cbind(nPos_noFS, nNeg_noFS) ~ locality_rich*logsiteAbun +
#                       temp_d*sMoist_d + (1|scientific/country) + (1|associatedReferences),
#                     data = subset(dcbind_pos, scientific != "Salamandra salamandra"),
#                     family = "binomial", na.action = "na.fail",
#                     control = glmmTMBControl(optimizer = optim,
#                                              optArgs = list(method = "BFGS")))
#
# noFS_lr.5 <- glmmTMB(cbind(nPos_noFS, nNeg_noFS) ~ locality_rich*logsiteAbun +
#                       temp_d*sMoist_d + (1|scientific/country),
#                     data = subset(dcbind_pos, scientific != "Salamandra salamandra"),
#                     family = "binomial", na.action = "na.fail",
#                     control = glmmTMBControl(optimizer = optim,
#                                              optArgs = list(method = "BFGS")))
#
#
# model.sel(noFS_lr, noFS_lr.2, noFS_lr.3, noFS_lr.4, noFS_lr.5)
# rm(noFS_lr, noFS_lr.2, noFS_lr.3, noFS_lr.4, noFS_lr.5)

###### > Model Checking/Diagnostics --------------------------------------------
noFS_lr.3 <- glmmTMB(cbind(nPos_noFS, nNeg_noFS) ~ locality_rich*logsiteAbun +
                       temp_d*sMoist_d + (1|scientific) + (1|associatedReferences),
                     data = subset(dcbind_pos, scientific != "Salamandra salamandra"),
                     family = "binomial", na.action = "na.fail",
                     control = glmmTMBControl(optimizer = optim,
                                              optArgs = list(method = "BFGS")))

summary(noFS_lr.3)
Anova(noFS_lr.3) # richness:logsiteAbun and soil moisture are highly significant

resid <- simulateResiduals(noFS_lr.3)
testResiduals(resid)
# plotResiduals(resid)
# testOutliers(resid, type = "bootstrap") # n.s.
# testDispersion(resid) # n.s.
# testQuantiles(resid) # deviation n.s.
# testZeroInflation(resid) # n.s.


## Need to subset unique lat/lon vals to test for spatial autocorrelation
coords <- dcbind_pos %>%
  filter(scientific != "Salamandra salamandra") %>%
  # filter(scientific != "Salamandra salamandra" & country == "Germany") %>%
  subset(., select = c(Site, Lat, Lon)) %>%
  distinct(Site, Lat, Lon, .keep_all = T) %>%
  group_by(Site) %>%
  mutate(Lat = mean(Lat), Lon = mean(Lon)) %>%
  distinct() %>%
  ungroup()

# need to use recalculateResiduals() function because we have multiple observations per location
recalc.resid <- recalculateResiduals(resid, group = coords$Site)

testSpatialAutocorrelation(recalc.resid, x = coords$Lon, y = coords$Lat)
## I tested for spatial autocorrelation both including and excluding Spain,
## in case the distance between our single Spain site and the rest of the sites in Germany
## were giving us a biased result

# DHARMa Moran's I test for distance-based autocorrelation
# (INCLUDING SPAIN OBSERVATIONS)
# data:  recalc.resid
# observed = 0.050168, expected = -0.028571, sd = 0.062917, p-value = 0.2108
# alternative hypothesis: Distance-based autocorrelation

# DHARMa Moran's I test for distance-based autocorrelation -- Germany data only
# (GERMANY ONLY)
# data:  recalc.resid
# observed = -0.015942, expected = -0.029412, sd = 0.064805, p-value = 0.8353
# alternative hypothesis: Distance-based autocorrelation

## No spatial autocorrelation! cool.

rm(resid, recalc.resid, coords)

###### > Tables ----------------------------------------------------------------
m3_noFS_tbl <- noFS_lr.3 %>%
  broom::tidy() %>%
  filter(!row_number() %in% c(1, 8:9)) %>%
  mutate(OR = exp(estimate)) %>%
  relocate(OR, .before = p.value) %>%
  left_join(., (Anova(noFS_lr.3) %>%
                  rownames_to_column(., "term")), by = "term") %>%
  dplyr::select(-c(effect, component, group, estimate, std.error, statistic, p.value,
                   Df)) %>%
  rename(Terms = term,
         pval = `Pr(>Chisq)`) %>%
  mutate(signif = gtools::stars.pval(pval),
         Terms = case_when(Terms == "locality_rich" ~ "Locality richness",
                           Terms == "logsiteAbun" ~ "log(Site abundance)",
                           Terms == "temp_d" ~ paste0("Temperature (\u00b0C)"),
                           Terms == "sMoist_d" ~ paste0("Soil moisture (kg\u00b7m\u00b2)"),
                           Terms == "locality_rich:logsiteAbun" ~ "Locality richness:log(Site abundance)",
                           Terms == "temp_d:sMoist_d" ~ "Temperature:Soil moisture")) %>%
  flextable(theme_fun = theme(booktabs)) %>%
  set_header_labels(., Chisq = chisqlab,
                    pval = "p-value",
                    signif = "") %>%
  add_header_lines(., values = "cbind Model - No fire salamanders") %>%
  footnote(., i = 2, j = 2,
           value = as_paragraph("OR = Odds Ratio"),
           ref_symbols = "1",
           part = "header") %>%
  footnote(., i = 2, j = 4,
           value = as_paragraph("*p<0.05; **p<0.01; ***p<0.001"),
           ref_symbols = "2",
           part = "header") %>%
  set_formatter_type(fmt_double = "%#.3f") %>%
  flextable::align(j = c("OR", "Chisq"), align = "center", part = "all") %>%
  flextable::align(j = c("Terms", "pval"), align = "right", part = "all") %>%
  flextable::align(j = "signif", align = "left") %>%
  flextable::align(i = 1, j = NULL, align = "center", part = "header") %>%
  flextable::align(., align = "left", part = "footer") %>%
  padding(padding.right = 0, j = 'pval', part = "all") %>%
  padding(padding.left = 0, j = "signif", part  = "all") %>%
  autofit() %>%
  bold(bold = TRUE, part = "header") %>%
  color(i = ~ pval >= 0.05, j = c("Terms", "OR", "Chisq", "pval", "signif"),
        color = "gray") %>%
  bold(bold = T,
       i = ~ pval < 0.05,
       j = c("Terms", "OR", "Chisq", "pval", "signif"))


m3_noFS_tbl
###### > Plot ------------------------------------------------------------------
m3_noFS_predict <- ggpredict(noFS_lr.3,  terms = c("locality_rich", "logsiteAbun")) %>%
  # m3b_noFS_predict <- ggpredict(noFS_lr.3,  terms = c("locality_rich", "logsiteAbun [1.9459, 2.8332, 3.6376]")) %>% # 1st quartile, median, 3rd quartile
  # m3b_noFS_predict <- ggpredict(noFS_lr.3,  terms = c("locality_rich", "logsiteAbun [0.6931, 2.7482, 4.8040]")) %>% # min, mean, max
  dplyr::rename("locality_rich" = "x",
                "logsiteAbun" = "group") %>%
  plyr::mutate(locality_rich = as.numeric(as.character(locality_rich)),
               logsiteAbun = as.numeric(as.character(logsiteAbun)),
               # Convert scaled prediction to original data scale:
               siteAbun = as.factor(round(exp(as.numeric(logsiteAbun - 1)), 0)))



m3a_noFS <- ggplot(m3_noFS_predict, aes(x = locality_rich, y = predicted, linetype = siteAbun, colour = siteAbun)) +
  geom_line(aes(linetype = siteAbun, colour = siteAbun), linewidth = 1) +
  # geom_ribbon(aes(ymin = conf.low, ymax = conf.high,
  #                 colour = siteAbun,
  #                 linetype = siteAbun), outline.type = "both", alpha = 0.1, show.legend = F) +
  geom_rug(data = dcbind_pos, aes(x = locality_rich, y = 0), sides = "b",
           alpha = 0.5, position = position_jitter(width = 0.4, height = 0.1),
           inherit.aes = F, na.rm = T) +
  labs(x = "Locality richness",
       y = "Bsal prevalence (%)") +
  geom_label(label = "Europe", size = 8, colour = "gray20", label.padding = unit(0.25, "lines"),
             x = 9, y = 0.055, alpha = 0.75) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_grey(start = 0.8, end = 0.2) +
  scale_x_continuous(labels = seq(0, 6, 1),
                     breaks = seq(0, 6, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, .5, 0.1),
                     limits = c(0, .5),
                     minor_breaks = seq(0, 0.5, 0.05)) +
  ak_theme + theme(plot.tag.position = c(0.96, 0.9),
                   axis.text.y = element_text(face = "plain")) +
  guides(colour = guide_legend("Site-level abundance", title.position = "top", title.hjust = 0.5),
         linetype = guide_legend("Site-level abundance", title.position = "top", title.hjust = 0.5))

m3a_noFS
# ggsave("m3a_noFS.pdf", m3a_noFS, device = cairo_pdf, path = figpath,
#        width = 1250, height = 1500, scale = 2, units = "px", dpi = 300, limitsize = F)

rm(m3a_predict, m3_noFS_predict, noFS, noFS_lr.3)
#### iii. 'Fire salamanders only' ----------------------------------------------
##### > Locality richness (From collaborators) ---------------------------------
###### > Model selection -------------------------------------------------------
# FS_lr <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~ locality_rich*logsiteAbun +
#                       temp_d*sMoist_d + (1|country) + (1|scientific) + (1|associatedReferences),
#                        data = subset(dcbind_pos, scientific == "Salamandra salamandra"),
#                        family = "binomial", na.action = "na.fail",
#                        control = glmmTMBControl(optimizer = optim,
#                                                 optArgs = list(method = "BFGS")))
#
#
# FS_lr.2 <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~ locality_rich*logsiteAbun +
#                       temp_d*sMoist_d + (1|country) + (1|scientific),
#                     data = subset(dcbind_pos, scientific == "Salamandra salamandra"),
#                     family = "binomial", na.action = "na.fail",
#                     control = glmmTMBControl(optimizer = optim,
#                                              optArgs = list(method = "BFGS")))
#
# FS_lr.3 <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~ locality_rich*logsiteAbun +
#                      temp_d*sMoist_d + (1|country) + (1|associatedReferences),
#                    data = subset(dcbind_pos, scientific == "Salamandra salamandra"),
#                    family = "binomial", na.action = "na.fail",
#                    control = glmmTMBControl(optimizer = optim,
#                                             optArgs = list(method = "BFGS")))
#
# FS_lr.4 <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~ locality_rich*logsiteAbun +
#                      temp_d*sMoist_d + (1|scientific) + (1|associatedReferences),
#                    data = subset(dcbind_pos, scientific == "Salamandra salamandra"),
#                    family = "binomial", na.action = "na.fail",
#                    control = glmmTMBControl(optimizer = optim,
#                                             optArgs = list(method = "BFGS")))
#
# FS_lr.5 <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~ locality_rich*logsiteAbun +
#                      temp_d*sMoist_d + (1|associatedReferences),
#                    data = subset(dcbind_pos, scientific == "Salamandra salamandra"),
#                    family = "binomial", na.action = "na.fail",
#                    control = glmmTMBControl(optimizer = optim,
#                                             optArgs = list(method = "BFGS")))
#
# FS_lr.6 <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~ locality_rich*logsiteAbun +
#                       temp_d*sMoist_d + (1|scientific/country) + (1|associatedReferences),
#                     data = subset(dcbind_pos, scientific == "Salamandra salamandra"),
#                     family = "binomial", na.action = "na.fail",
#                     control = glmmTMBControl(optimizer = optim,
#                                              optArgs = list(method = "BFGS")))
#
# FS_lr.7 <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~ locality_rich*logsiteAbun +
#                       temp_d*sMoist_d + (1|scientific/country),
#                     data = subset(dcbind_pos, scientific == "Salamandra salamandra"),
#                     family = "binomial", na.action = "na.fail",
#                     control = glmmTMBControl(optimizer = optim,
#                                              optArgs = list(method = "BFGS")))
#
#
# model.sel(FS_lr, FS_lr.2, FS_lr.3, FS_lr.4, FS_lr.5, FS_lr.6, FS_lr.7)
# rm(FS_lr, FS_lr.2, FS_lr.3, FS_lr.4, FS_lr.5, FS_lr.6, FS_lr.7)
###### > Model Checking/Diagnostics --------------------------------------------
FS_lr.5 <- glmmTMB(cbind(nPos_FS, nNeg_FS) ~ locality_rich*logsiteAbun +
                     temp_d*sMoist_d + (1|associatedReferences),
                   data = subset(dcbind_pos, scientific == "Salamandra salamandra"),
                   family = "binomial", na.action = "na.fail",
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs = list(method = "BFGS")))


summary(FS_lr.5)
Anova(FS_lr.5) # locality_rich:logsiteAbun and soil moisture are n.s.
# main effects of locality_rich, logsiteAbun, and sMoist_d are very significant

resid <- simulateResiduals(FS_lr.5)
testResiduals(resid)
# plotResiduals(resid)
# testOutliers(resid, type = "bootstrap") # n.s.
# testDispersion(resid) # n.s.
# testQuantiles(resid) # very wonky.
# testZeroInflation(resid) # n.s.


## Need to subset unique lat/lon vals to test for spatial autocorrelation
coords <- dcbind_pos %>%
  filter(scientific == "Salamandra salamandra") %>%
  # filter(scientific == "Salamandra salamandra" & country == "Germany") %>%
  subset(., select = c(Site, Lat, Lon)) %>%
  distinct(Site, Lat, Lon, .keep_all = T) %>%
  group_by(Site) %>%
  mutate(Lat = mean(Lat), Lon = mean(Lon)) %>%
  distinct() %>%
  ungroup()

# need to use recalculateResiduals() function because we have multiple observations per location
recalc.resid <- recalculateResiduals(resid, group = coords$Site)

testSpatialAutocorrelation(recalc.resid, x = coords$Lon, y = coords$Lat)
## I tested for spatial autocorrelation both including and excluding Spain,
## in case the distance between our single Spain site and the rest of the sites in Germany
## were giving us a biased result

# DHARMa Moran's I test for distance-based autocorrelation
# (INCLUDING SPAIN OBSERVATIONS)
# data:  recalc.resid
# observed = -0.076639, expected = -0.034483, sd = 0.063388, p-value = 0.506
# alternative hypothesis: Distance-based autocorrelation

# DHARMa Moran's I test for distance-based autocorrelation -- Germany data only
# (GERMANY ONLY)
# data:  recalc.resid
# observed = -0.005374, expected = -0.035714, sd = 0.065634, p-value = 0.6439
# alternative hypothesis: Distance-based autocorrelation

## No spatial autocorrelation! cool.

rm(resid, recalc.resid, coords)

###### > Tables ----------------------------------------------------------------
## Table for model output
m3_FS_tbl <- FS_lr.5 %>%
  broom::tidy() %>%
  filter(!row_number() %in% c(1, 8)) %>%
  mutate(OR = exp(estimate)) %>%
  relocate(OR, .before = p.value) %>%
  left_join(., (Anova(FS_lr.5) %>%
                  rownames_to_column(., "term")), by = "term") %>%
  dplyr::select(-c(effect, component, group, estimate, std.error, statistic, p.value,
                   Df)) %>%
  rename(Terms = term,
         pval = `Pr(>Chisq)`) %>%
  mutate(signif = gtools::stars.pval(pval),
         Terms = case_when(Terms == "locality_rich" ~ "Locality richness",
                           Terms == "logsiteAbun" ~ "log(Site abundance)",
                           Terms == "temp_d" ~ paste0("Temperature (\u00b0C)"),
                           Terms == "sMoist_d" ~ paste0("Soil moisture (kg\u00b7m\u00b2)"),
                           Terms == "locality_rich:logsiteAbun" ~ "Locality richness:log(Site abundance)",
                           Terms == "temp_d:sMoist_d" ~ "Temperature:Soil moisture")) %>%
  flextable(theme_fun = theme(booktabs)) %>%
  set_header_labels(., Chisq = chisqlab,
                    pval = "p-value",
                    signif = "") %>%
  add_header_lines(., values = "cbind Model - Fire salamanders only") %>%
  footnote(., i = 2, j = 2,
           value = as_paragraph("OR = Odds Ratio"),
           ref_symbols = "1",
           part = "header") %>%
  footnote(., i = 2, j = 4,
           value = as_paragraph("*p<0.05; **p<0.01; ***p<0.001"),
           ref_symbols = "2",
           part = "header") %>%
  set_formatter_type(fmt_double = "%#.3f") %>%
  flextable::align(j = c("OR", "Chisq"), align = "center", part = "all") %>%
  flextable::align(j = c("Terms", "pval"), align = "right", part = "all") %>%
  flextable::align(j = "signif", align = "left") %>%
  flextable::align(i = 1, j = NULL, align = "center", part = "header") %>%
  flextable::align(., align = "left", part = "footer") %>%
  padding(padding.right = 0, j = 'pval', part = "all") %>%
  padding(padding.left = 0, j = "signif", part  = "all") %>%
  autofit() %>%
  bold(bold = TRUE, part = "header") %>%
  color(i = ~ pval >= 0.05, j = c("Terms", "OR", "Chisq", "pval", "signif"),
        color = "gray") %>%
  bold(bold = T,
       i = ~ pval < 0.05,
       j = c("Terms", "OR", "Chisq", "pval", "signif"))


m3_FS_tbl

## Combine cbind model tables
save_as_docx(
  "Model 3a - All species" = m3_all_tbl,
  "Model 3b - No fire salamanders" = m3_noFS_tbl,
  "Model 3c - Fire salamanders only" = m3_FS_tbl,
  path = file.path(tblpath, "/cbind_models.docx")
)

# rm(m3_all_tbl, m3_noFS_tbl, m3_FS_tbl)

###### > Plot ------------------------------------------------------------------
m_FS_predict <- ggpredict(FS_lr.5,  terms = c("locality_rich", "logsiteAbun")) %>%
  # m_FS_predict <- ggpredict(FS_lr.5,  terms = c("locality_rich", "logsiteAbun [1.9459, 2.8332, 3.6376]")) %>% # 1st quartile, median, 3rd quartile
  # m_FS_predict <- ggpredict(FS_lr.5,  terms = c("locality_rich", "logsiteAbun [0.6931, 2.7482, 4.8040]")) %>% # min, mean, max
  dplyr::rename("locality_rich" = "x",
                "logsiteAbun" = "group") %>%
  plyr::mutate(locality_rich = as.numeric(as.character(locality_rich)),
               logsiteAbun = as.numeric(as.character(logsiteAbun)),
               # Convert scaled prediction to original data scale:
               siteAbun = as.factor(round(exp(as.numeric(logsiteAbun - 1)), 0)))


m3b_FS <- ggplot(m_FS_predict, aes(x = locality_rich, y = predicted, linetype = siteAbun, colour = siteAbun)) +
  geom_line(aes(linetype = siteAbun, colour = siteAbun), linewidth = 1) +
  # geom_ribbon(aes(ymin = conf.low, ymax = conf.high,
  #                 colour = siteAbun,
  #                 linetype = siteAbun), outline.type = "both", alpha = 0.1, show.legend = F) +
  geom_rug(data = subset(dcbind_pos, scientific == "Salamandra salamandra"), aes(x = locality_rich, y = 0), sides = "b",
           alpha = 0.5, position = position_jitter(width = 0.4, height = 0.1),
           inherit.aes = F, na.rm = T) +
  labs(x = "Locality richness",
       y = "Bsal prevalence (%)") +
  geom_label(label = "Europe", size = 8, colour = "gray20", label.padding = unit(0.25, "lines"),
             x = 9, y = 0.055, alpha = 0.75) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
  scale_color_grey(start = 0.8, end = 0.2) +
  scale_x_continuous(labels = seq(0, 6, 1),
                     breaks = seq(0, 6, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.2),
                     limits = c(0, 1),
                     minor_breaks = seq(0, 1, 0.1)) +
  ak_theme + theme(plot.tag.position = c(0.96, 0.9),
                   axis.text.y = element_text(face = "plain")) +
  guides(colour = guide_legend("Site-level abundance", title.position = "top", title.hjust = 0.5),
         linetype = guide_legend("Site-level abundance", title.position = "top", title.hjust = 0.5))

m3b_FS

# ggsave("m3b_FS.pdf", m3b_FS, device = cairo_pdf, path = figpath,
#        width = 1250, height = 1500, scale = 2, units = "px", dpi = 300, limitsize = F)

rm(nicelabs, m_FS_predict, FS_lr.5)
## Figure 3. ------------------------------------------------------------------
## Create combined plot for manuscript
fig3a <- m3a + labs(caption = NULL, y = NULL, x = NULL) +
  theme(plot.tag.position = c(0.95, 0.95),
        plot.tag = element_text(face = "bold"),
        plot.margin = margin(.5, .75, .5, .5, "cm"),
        legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill = alpha ("white", 0.75), color = NA),
        legend.box = "horizontal",
        legend.key.size = unit(1,"cm"),
        legend.text = element_text(margin = margin(r = 1, unit = "cm"), size = 24),
        legend.title = element_text(face = "plain",  size = 28),
        axis.title.y = element_text(size = 42, hjust = 0.5,
                                    margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    face = "plain"),
        axis.title.x = element_text(size = 38, hjust = 0.5,
                                    margin = margin(t = 0.1, r = 0, b = 0, l = 5),
                                    face = "plain")) +
  guides(linetype = guide_legend(nrow = 1, "Site-level abundance", title.position = "top", title.hjust = 0.5))

fig3a

fig3b <- m3b_FS + labs(caption = NULL, y = NULL) +
  theme(plot.tag.position = c(0.95, 0.95),
        plot.tag = element_text(face = "bold"),
        plot.margin = margin(.5, .75, .5, .5, "cm"),
        legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill = alpha ("white", 0.75), color = NA),
        legend.box = "horizontal",
        legend.key.size = unit(1,"cm"),
        legend.title = element_text(face = "plain", size = 28),
        legend.text = element_text(margin = margin(r = 1, unit = "cm"), size = 24),
        axis.title.y = element_text(size = 42, hjust = 0.5,
                                    margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    face = "plain"),
        axis.title.x = element_text(size = 38, hjust = 0.5,
                                    margin = margin(t = 0.1, r = 0, b = 0, l = 5),
                                    face = "plain")) +
  guides(linetype = guide_legend(nrow = 1, "Site-level abundance", title.position = "top", title.hjust = 0.5))
fig3b


# ## Vertical plots*
# #-- need to alter x and y labs in fig3a/3b if switching between h and v plots
# http://127.0.0.1:29403/graphics/70691af6-0ed8-4077-8e3e-00e9c65965ad.png

fs_img <- image_read("firesalamander.png") # add image to 3b

fig3ab_v <- (fig3a / fig3b) +
  plot_annotation(tag_levels = "A")

fig3ab_v

fig3ab_v_combined <- ggdraw(fig3ab_v, xlim = c(-0.15, 1)) +
  draw_label("Bsal prevalence (%)", x = -0.11, y = 0.55, angle = 90,
             size = 44, fontfamily = "Segoe UI Light") +
  draw_image(image = fs_img, x = 0.32, y = -0.05, scale = 0.17) +
  ak_theme + theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))


fig3ab_v_combined


ggsave("modelPlots_vertical.pdf", fig3ab_v_combined, device = cairo_pdf, scale = 2,
       width = 2000, height = 2400, units = "px",
       path = figpath, dpi = 300)


## Horizontal plots


# fig3ab_h <- (fig3a | fig3b) + plot_annotation(tag_levels = "A")
#
# fig3ab_h_combined <- ggdraw(fig3ab_h) +
#   draw_label("Species richness", x = 0.5, y = 0, angle = 0,
#              size = 34, fontfamily = "Arial") +
#   draw_image(image = fs_img, x = 0.4, y = 0.22, scale = 0.25) +
#   ak_theme + theme(axis.line = element_blank())
# fig3ab_h_combined
#
#
# ggsave("modelPlots_horizontal.pdf", fig3ab_h_combined, device = cairo_pdf, scale = 2,
#        width = 2400, height = 1250, units = "px",
#        path = file.path(dir, figpath), dpi = 300)




## IV. Cbind models with other measures of richness ----------------------------
### i. Including fire salamanders ----------------------------------------------
##### > Relative richness (derived from dataset) -------------------------------
# all_EU_RR <- glmmTMB(cbind(nPos_all, nNeg_all) ~ richness*logsiteAbun +
#                        temp_d*sMoist_d + (1|country) + (1|scientific),
#                      data = filter(dcbind_subset, continent == "Europe"),
#                      family = "binomial", na.action = "na.fail",
#                      control = glmmTMBControl(optimizer = optim,
#                                               optArgs = list(method = "BFGS")))
#
# summary(all_EU_RR)
# Anova(all_EU_RR) # richness:logsiteAbun and soil moisture are highly significant
#
# resid <- simulateResiduals(all_EU_RR)
# testResiduals(resid) # Outlier test significant (p = 0.01716)
# testOutliers(resid, type = "bootstrap")
# testQuantiles(resid)
# testZeroInflation(resid)
#
# ## Need to subset unique lat/lon vals to test for spatial autocorrelation
# coords <- dcbindScaled %>%
#   filter(continent == "Europe") %>%
#   distinct(Site, Lat, Lon, .keep_all = T) %>%
#   group_by(Site) %>%
#   mutate(Lat = mean(Lat), Lon = mean(Lon)) %>%
#   distinct() %>%
#   ungroup()
#
# recalc.resid <- recalculateResiduals(resid, group = coords$Site)
#
# testSpatialAutocorrelation(recalc.resid, x = coords$Lon, y = coords$Lat)
# DHARMa Moran's I test for distance-based autocorrelation
#
# data:  recalc.resid
# observed = 0.0697829, expected = -0.0034247, sd = 0.0216687, p-value = 0.0007289
# alternative hypothesis: Distance-based autocorrelation

###### *Data are spatially autocorrelated -- trying to correct
## -> Tried running all Europe data in spatial-autocorrelation corrected model.
#     Not only did it make the model worse, but it also did not correct the spatial-
#     Autocorrelation issue.
## -> Then tried separating data by country, but Spain only had 9 distinct sites,
#     which was not enough for a robust model.
## -> I then created a model using only the Germany data, initially not correcting
#     for spatial-autocorrelation to see if we still have a spatial-autocorrelation
#     issue if it's just one country (284 distinct sites).
#
#
# ## Create dummy df with only Germany data.
# tmp <- dcbindScaled %>%
#   filter(country == "Germany")
#
# ## model is fit by adding the pos term in
# all_EU_RR2 <- glmmTMB(cbind(nPos_all, nNeg_all) ~ richness*logsiteAbun +
#                         temp_d*sMoist_d + (1|scientific),
#                       data = tmp, family = "binomial", na.action = "na.fail",
#                       control = glmmTMBControl(optimizer = optim,
#                                                optArgs = list(method = "BFGS")))
#
#
# summary(all_EU_RR2)
# Anova(all_EU_RR2) # Soil moisture and logsppAbun are significant
#
# resid2 <- simulateResiduals(all_EU_RR2)
# testResiduals(all_EU_RR2)
# testQuantiles(all_EU_RR2)
# testZeroInflation(all_EU_RR2)
#
# ## Need to subset unique lat/lon vals to test for spatial autocorrelation
# coords <- tmp %>%
#   distinct(Site, Lat, Lon, .keep_all = T) %>%
#   group_by(Site) %>%
#   mutate(Lat = mean(Lat), Lon = mean(Lon)) %>%
#   distinct() %>%
#   ungroup()
#
# recalc.resid2 <- recalculateResiduals(resid2, group = coords$Site)
# testSpatialAutocorrelation(recalc.resid2, x = coords$Lon, y = coords$Lat)
#
# DHARMa Moran's I test for distance-based autocorrelation
#
# data:  recalc.resid2
# observed = 0.0204701, expected = -0.0035336, sd = 0.0211485, p-value = 0.2564
# alternative hypothesis: Distance-based autocorrelation
##### > IUCN Richness estimate --------------------------------------------------
# EU_iucnR <- glmmTMB(cbind(nPos_all, nNeg_all) ~ iucn_rich*logsiteAbun +
#                        temp_d*sMoist_d + (1|country) + (1|scientific),
#                      data = filter(dcbindScaled_conf, continent == "Europe"),
#                     family = "binomial", na.action = "na.fail",
#                     control = glmmTMBControl(optimizer = optim,
#                                              optArgs = list(method = "BFGS")))
#
# summary(EU_iucnR)
# Anova(EU_iucnR)
#
# resid <- simulateResiduals(EU_iucnR)
# testResiduals(resid) # Outlier test significant (p = 0.01716)
# testOutliers(resid, type = "bootstrap")
# testQuantiles(resid)
# testZeroInflation(resid)
#
# ## Need to subset unique lat/lon vals to test for spatial autocorrelation
# coords <- dcbindScaled %>%
#   filter(continent == "Europe") %>%
#   distinct(Site, Lat, Lon, .keep_all = T) %>%
#   group_by(Site) %>%
#   mutate(Lat = mean(Lat), Lon = mean(Lon)) %>%
#   distinct() %>%
#   ungroup()
#
# recalc.resid <- recalculateResiduals(resid, group = coords$Site)
#
# testSpatialAutocorrelation(recalc.resid, x = coords$Lon, y = coords$Lat)
#
# ##  Prevalence by Abundance & Richness Plots for 'All spp.' model
# EU_iucnR_pred <- ggpredict(EU_iucnR,  terms = c("iucn_rich", "logsiteAbun")) %>%
#   dplyr::rename("richness" = "x",
#                 "logsiteAbun" = "group") %>%
#   plyr::mutate(richness = as.numeric(as.character(richness)),
#                logsiteAbun = as.numeric(as.character(logsiteAbun)),
#                # Convert scaled prediction to original data scale:
#                siteAbun = as.factor(round(exp(as.numeric(logsiteAbun)), 0)))
#
#
# EU_iucnR_plot <- ggplot(EU_iucnR_pred, aes(x = richness, y = predicted, linetype = siteAbun, colour = siteAbun)) +
#   geom_line(aes(linetype = siteAbun, colour = siteAbun), linewidth = 1) +
#   geom_rug(data = dcbindScaled, aes(x = iucn_rich, y = 0), sides = "b",
#            alpha = 0.5, position = position_jitter(width = 0.4, height = 0.1),
#            inherit.aes = F, na.rm = T) +
#   labs(x = "IUCN Richness est.",
#        y = "Bsal prevalence (%)") +
#   # geom_ribbon(aes(x = richness, ymin = conf.low, ymax = conf.high,
#   #             fill = siteAbun), alpha = 0.2, colour = NA, show.legend = F) +
#   geom_label(label = "Europe", size = 8, colour = "gray20", label.padding = unit(0.25, "lines"),
#              x = 9, y = 0.055, alpha = 0.75) +
#   scale_linetype_manual(values = c("solid", "longdash", "twodash")) +
#   scale_color_grey(start = 0.8, end = 0.2) +
#   scale_x_continuous(limits = c(0, 17),
#                      labels = seq(0, 15, 1),
#                      breaks = seq(0, 15, 1)) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1),
#                      breaks = seq(0, .10, 0.02),
#                      limits = c(0, 0.10),
#                      minor_breaks = seq(0, 0.10, 0.02)) +
#   ak_theme + theme(plot.tag.position = c(0.96, 0.9),
#                    axis.text.y = element_text(face = "plain")) +
#   guides(colour = guide_legend("Site-level abundance", title.position = "top", title.hjust = 0.5),
#          linetype = guide_legend("Site-level abundance", title.position = "top", title.hjust = 0.5))
#
# EU_iucnR_plot
#
##### > Relative richness + no FS prevalence ------------------------------------
# ## Fire salamanders are still accounted for in site abundance and richness calculations
# all_RR_noFS <- glmmTMB(cbind(nPos_all_noFS, nNeg_all_noFS) ~ richness*logsiteAbun +
#                          temp_d*sMoist_d + (1|scientific),
#                        data = filter(dcbindScaled_conf, scientific != "Salamandra salamandra"),
#                        family = "binomial", na.action = "na.fail",
#                        control = glmmTMBControl(optimizer = optim,
#                                                 optArgs = list(method = "BFGS")))
#
# summary(all_RR_noFS)
# Anova(all_RR_noFS)
#
# resid_noFS <- simulateResiduals(all_RR_noFS)
# testResiduals(resid_noFS)
# testQuantiles(resid_noFS)
# testZeroInflation(resid_noFS)
#
# # Test for spatial autocorrelation
# recalc.resid_noFS <- recalculateResiduals(resid_noFS, group = coords$Site)
# testSpatialAutocorrelation(recalc.resid_noFS, x = coords$Lon, y = coords$Lat)
# # not spatially autocorrelated
#
#### > IUCN Richness estimate + no FS prevalence -------------------------------
# all_iucnR_noFS <- glmmTMB(cbind(nPos_all_noFS, nNeg_all_noFS) ~ iucn_rich*logsiteAbun +
#                             temp_d*sMoist_d + (1|scientific),
#                           data = filter(dcbindScaled_conf, scientific != "Salamandra salamandra"),
#                           family = "binomial", na.action = "na.fail",
#                           control = glmmTMBControl(optimizer = optim,
#                                                    optArgs = list(method = "BFGS")))
#
# summary(all_iucnR_noFS)
# Anova(all_iucnR_noFS)
#
# resid_iucnR_noFS <- simulateResiduals(all_iucnR_noFS)
# testResiduals(resid_iucnR_noFS)
# testQuantiles(resid_iucnR_noFS)
# testZeroInflation(resid_iucnR_noFS)
#
# # Test for spatial autocorrelation
# recalc.resid_iucnR_noFS <- recalculateResiduals(resid_iucnR_noFS, group = coords$Site)
# testSpatialAutocorrelation(recalc.resid_iucnR_noFS, x = coords$Lon, y = coords$Lat)
