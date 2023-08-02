
prev_cbind_plot <- ggplot(m2a_predict, aes(scientific, sapply(Bsal_prev, FUN = function(x) ifelse(x == 0, 0.1, x)), 
                                           colour = susceptibility,
                                           label = paste(sprintf(Bsal_prev, fmt = '%#.1f'), "%"))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.5) +
  geom_text(aes(colour = susceptibility, x = (as.numeric(scientific) + 0.05), y = conf.high + 10), 
            size = 6, fontface = "bold", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df1$scientific) + 0.1), y = (df1$conf.high + 3),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#548078", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df2$scientific) + 0.1), y = (df2$conf.high + 3),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#E3A630", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df3$scientific) + 0.1), y = (df3$conf.high + 3),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#b30000", alpha = 0.75) +
  coord_flip(clip = "off") +
  ylab("Disease prevalence (%)") +
  xlab("Species") + 
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  scale_y_continuous(labels = seq(0, 100, 20),
                     breaks = seq(0, 100, 20),
                     limits = c(0, 110)) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(1, 0.25))) +
  ak_theme + theme(axis.text.y = element_text(face = "italic"),
                   legend.title = element_blank()) 


prev_cbind_plot




# Resistant
df1_p <- prev %>%
  filter(susceptibility == "1")

# Tolerant
df2_p <- prev %>% 
  filter(susceptibility == "2") 

# Susceptible
df3_p <- prev %>% 
  filter(susceptibility == "3") 


prev_epiR_plot <- ggplot(prev, aes(scientific, sapply(est_prev, FUN = function(x) ifelse(x == 0, 0.1, x)), 
                                   colour = susceptibility,
                                   label = paste(sprintf(est_prev, fmt = '%#.1f'), "%"))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width = 0.5) +
  geom_text(aes(colour = susceptibility, x = (as.numeric(scientific) + 0.05), y = upperCI + 6), 
            size = 6, fontface = "bold", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df1_p$scientific) + 0.1), y = (df1_p$upperCI + 3),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#548078", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df2_p$scientific) + 0.1), y = (df2_p$upperCI + 3),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#E3A630", alpha = 0.75) +
  # annotate(geom = "text", x = (as.numeric(df3_p$scientific) + 0.1), y = (df3_p$upperCI + 3),
  #          label = paste(xhat), parse = TRUE, size = 6, color = "#b30000", alpha = 0.75) +
  coord_flip(clip = "off") +
  ylab("Disease prevalence (%)") +
  xlab("Species") + 
  scale_colour_manual(name = "Susceptibility",
                      values = c("#548078", "#E3A630", "#b30000"),
                      labels = c("Resistant", "Tolerant", "Susceptible"),
                      guide = "none") +
  scale_y_continuous(labels = seq(0, 60, 20),
                     breaks = seq(0, 60, 20),
                     limits = c(0, 71)) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.01), add = c(1, 0.25))) +
  ak_theme + theme(axis.text.y = element_text(face = "italic"),
                   legend.title = element_blank()) 


prev_epiR_plot



## Create guide to force a common legend
commonLegend <- guides(fill = guide_legend(override.aes = list(color = c("#548078", "#E3A630", "#b30000"),
                                                               shape = c(16, 16, 16), 
                                                               size = c(2, 2, 2),
                                                               alpha = c(1, 1, 1)),))

## Create combined plot for manuscript
fig2a <- prev_cbind_plot + labs(caption = NULL, x = NULL, title = "Cbind model output") + 
  theme(plot.tag.position = c(0.96, 0.9),
        plot.margin = margin(.5, .75, .5, .75, "cm"),
        axis.ticks.length = unit(.25, "cm"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  commonLegend

fig2b <- prev_epiR_plot + labs(caption = NULL, x = NULL, title = "epiR model output") + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 24),
        axis.ticks.length = unit(.25, "cm"),
        axis.ticks = element_blank(),
        plot.margin = margin(.5, .75, .5, .5, "cm"),
        plot.tag.position = c(0.92, 0.9)) +
  commonLegend


fig2ab <-(fig2a | fig2b) + plot_layout(guides = "collect", heights = c(20, 16)) +
  plot_annotation(tag_levels = "A") & theme(legend.position = "top",
                                            legend.box.margin = margin(0, 1, 1, 1, "cm"),
                                            legend.text = element_text(margin = margin(r = 1, unit = "cm")),
                                            legend.title = element_blank())
fig2ab

fig2ab_combined <- ggdraw(fig2ab) + 
  draw_label("Disease prevalence (%)", x = 0.65, y = 0, angle = 0,
             size = 34, fontfamily = "Arial") + 
  ak_theme + theme(axis.line = element_blank())
fig2ab_combined

ggsave("est_prev_plots.pdf", fig2ab_combined, device = cairo_pdf, path = file.path(dir, figpath),
        width = 2800, height = 1400, scale = 2, units = "px", dpi = 300, limitsize = F)


tmp <- d %>%
  group_by(scientific) %>%
  mutate(ncas_Bsal = sum(BsalDetected == 1), # number of pos. Bsal cases
         npop = sum(individualCount)) %>% # pop size (total # individuals/spp.)
  drop_na(date) %>%
  ungroup() %>%
  mutate(p = round((ncas_Bsal/npop), 2),
         q = round((1-p), 2)) %>% # prevalence as a percentage
  dplyr::select(scientific, susceptibility, ncas_Bsal, p, q, npop) %>%
  unique()

View(tmp)
