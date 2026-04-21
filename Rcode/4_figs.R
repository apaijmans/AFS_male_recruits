# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 4_figs.R
#
# Purpose: Final aesthetics of the fig panels and combining them
# to create figs 1 and 2
#
# -----------------------------------------------------------

library(here)
library(readxl)
library(tidyverse)
library(ggpubr)
library(cowplot)
#library(magick)
#library(MoMAColors)

colblue <- "#C9E8FF" 
coltan <- "#B0925B" 

coldarkb <- "#9bd5ff"
coldarkt <- "#907645"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Figure 1: Male counts  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~ Panel a: map
# Produced by the Mapping and Geographic Information Centre, British Antarctic Survey, 2025
# Data from the South Georgia GIS (accessed 2025)
BImap <- "./Figs/2559_Bird Island v2.png"

# Create a ggplot object that features only the image.
map_panel <- ggdraw() + draw_image(BImap) #+ theme(plot.margin=unit(c(0,0,0,0), "points"))

#map_panel


#~~ Panel b: number of captures
p.recap <- readRDS(here("Figs", "hist_n_captures_all_males.rds"))

p.recap_new <- p.recap + 
  scale_fill_manual(values = colblue) + 
  scale_color_manual(values = "black") + 
  theme_pubr() +
  theme(legend.position="none") +
  scale_x_continuous(breaks=1:10)

p.recap_new


#~~ Panel c: number of sampled adult males on beach
p.males <- readRDS(here("Figs", "n_obs_males_per_year.rds"))

p.males_new <- p.males + scale_x_continuous("Year", breaks = c(1995, 2005, 2015)) + theme_pubr()

p.males_new


#~~ Panel d: SAM vs year
p.sam_year <- readRDS(here("Figs", "SAM_year_reg_line.rds"))

p.sam_year_new <- p.sam_year + theme_pubr()

p.sam_year_new



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Combine and save figure 1  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

fig1 <- plot_grid(
  map_panel, labels = c('(a)'), label_fontface = "bold", label_size = 12,
  plot_grid(p.recap_new +
              coord_cartesian(ylim = c(0, 630)), #+ theme(axis.text=element_text(size=10.5), axis.title=element_text(size=11)),
            
            p.males_new + 
              coord_cartesian(ylim = c(40, 210)) +
              guides(colour = guide_legend(position = "inside"),
                     linetype   = guide_legend(position = "inside"),
                     fill  = guide_legend(position = "inside")), #+ theme(axis.text=element_text(size=10.5), axis.title=element_text(size=11)),
            #legend.text=element_text(size=8.5),
            #legend.background = element_blank(),
            #legend.position.inside = c(0.35, 0.15)
            #p.sex_ratio_new, 
            p.sam_year_new, #+ theme(axis.text=element_text(size=10.5), axis.title=element_text(size=11)),
            #mod.dtSAM.year + theme(legend.position="none", axis.text=element_text(size=12), axis.title=element_text(size=12)),
            ncol=1,
            labels = c('(b)', '(c)', '(d)'), label_fontface = "bold", label_size = 12, align = "v"),
  rel_widths = c(1, 0.5)) 

# fig1

cowplot::save_plot(here("Figs", "fig1_n_male_counts.jpg"), fig1, base_width = 10, base_height = 8, dpi = 300)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Figure 2: Load figures  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~ Fig 2a: Barplot recruitment per birth year for all samples genotyped at 39 loci
bar_rec_all <- readRDS(here("Figs", "barplot_males_per_year_39loci_all_years_unfiltered.rds"))


#~~ Fig 2b: Histogram age (all 106 recruited males)
p.age <- readRDS(here("Figs", "age_first_recapture.rds"))


#~~ Fig 2c: Temporal model prediction plot (based on the filtered dataset)
p.BY <- readRDS(here("Figs", "Mod_pred_status_birthyear_temporal_model.rds"))


#~~ Fig 2d: Full model forest plot (based on the filtered dataset)
p.mod <- readRDS(here("Figs", "Forest_plot_recruitment_39loci_SAM.rds"))


#~~ Fig 2e: Full model prediction plot SAM (based on the filtered dataset)
p.SAM <- readRDS(here("Figs", "Mod_pred_status_SAM.rds"))


#~~ Fig 2f: Swarmplot recruitment birth mass (based on the filtered dataset)
beeswarm_mass <- readRDS(here("Figs", "boxplot_status_mass_beeswarm.rds"))


#~~ Fig 2g: Swarmplot recruitment sMLH (based on the filtered dataset)
beeswarm_sMLH <- readRDS(here("Figs", "boxplot_status_sMLH_beeswarm.rds"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Adjust figure aesthetics  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~ Panel a: All genotyped male pups 39 loci
bar_rec_new <- bar_rec_all + 
  scale_color_manual(values = c("black", "black"), labels=c('Non-recruited', 'Recruited')) + 
  scale_fill_manual(values = c(colblue, coltan), labels=c('Non-recruited', 'Recruited')) +
  ylab("No. of genotyped male pups") +
  coord_cartesian(ylim = c(0, 280)) +
  theme(legend.position="none")

bar_rec_new


#~~ Panel b: Age distribution recruitment
p.age_new <- p.age + 
  scale_fill_manual(values = colblue) + 
  scale_color_manual(values = "black") + 
  xlab("Age at recruitment") +
  theme_pubr() +
  theme(legend.position="none") +
  coord_cartesian(ylim = c(0, 45)) 

p.age_new


#~~ Panel c: Temporal model predictions
p.BY_new <- p.BY +
  scale_fill_manual(values = c(colblue, coltan)) +
  scale_color_manual(values = c(coldarkb, coldarkt)) +
  theme_pubr() +
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.y.right = element_blank()) +
  xlab("Birth year")  +
  ylab("sMLH") +
  guides(size = "none", color = "none", fill = "none")

p.BY_new


#~~ Panel d: Full model effect sizes 
p.mod_new <- p.mod + theme(legend.position="none")

p.mod_new


#~~ Panel e: Full model predictions SAM
p.SAM_new <- p.SAM +
  scale_fill_manual(values = c(colblue, coltan)) +
  scale_color_manual(values = c(coldarkb, coldarkt)) + 
  xlab("SAM birth year") +
  theme_pubr() +
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.y.right = element_blank()) +
  guides(size = "none", color = "none", fill = "none")

p.SAM_new


#~~ Panel f: Birth mass distribution recruits vs non recruits
beeswarm_mass_new <- beeswarm_mass +
  scale_fill_manual(values = c(colblue, coltan)) + 
  scale_color_manual(values = c(coldarkb, coldarkt)) + 
  theme_pubr()+
  xlab("") + 
  ylab("Birth mass") +
  scale_x_discrete(labels=c("Non-recruited","Recruited")) +
  theme(legend.position="none")

beeswarm_mass_new


#~~ Panel g: sMLH distribution recruits vs non recruits
beeswarm_new <- beeswarm_sMLH +
  scale_fill_manual(values = c(colblue, coltan)) + 
  scale_color_manual(values = c(coldarkb, coldarkt)) + 
  theme_pubr()+
  xlab("") + 
  ylab("sMLH") +
  scale_x_discrete(labels=c("Non-recruited","Recruited")) +
  theme(legend.position="none")

beeswarm_new



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Combine and save figure 2  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Extract horizontal legend n individuals
legend_p <- ggpubr::get_legend(
  p.BY +
    guides(color = "none", fill = "none") +
    guides(shape = guide_legend(override.aes = list(size = 4))) +
    theme_pubr() +
    theme(legend.justification="left",
          legend.box.just = "bottom")
)

# Extract horizontal legend status
legend_p2 <- ggpubr::get_legend(
  beeswarm_mass +
    scale_color_manual("Status", values = c(colblue, coltan),
                       aesthetics = c("color", "fill"),
                       labels=c('Non-recruited', 'Recruited')) +
    theme_pubr() +
    theme(legend.justification="right",
          legend.box.just = "bottom")
)

# get current margins: theme_get()$plot.margin


#~~ Combine panels fig 2
all_plots <- plot_grid(
  bar_rec_new + theme(axis.text=element_text(size=9.5), plot.margin=unit(c(5.5,5.5,5.5,6), "points")), #  
  plot_grid(p.age_new + theme(plot.margin=unit(c(5.5,5.5,5.5,10), "points")), 
            p.BY_new + theme(axis.text=element_text(size=9.5)),
            p.mod_new, 
            nrow = 1, labels = c('(b)', '(c)', '(d)'), label_fontface = "bold"), 
  plot_grid(p.SAM_new + theme(plot.margin=unit(c(5.5,5.5,5.5,2), "points")), 
            beeswarm_mass_new,
            beeswarm_new,
            nrow = 1, labels = c('(e)', '(f)', '(g)'), label_fontface = "bold"),
  plot_grid(legend_p, legend_p2, nrow = 1),
  nrow = 4,
  rel_heights = c(1.3, 1, 1, .15),
  labels = c("(a)", "", ""), 
  label_fontface = "bold"
)

all_plots

save_plot(here("Figs", "fig2_male_recruitment_SAM_mass.jpg"), all_plots, base_width = 10, base_height = 10, dpi = 300)



# #~~ Combine forest plots year and SAM models n males
# 
# # Load model figs
# 
# mod.SAM <- readRDS(here("Figs", "Forest_plot_n_males_SAM.rds"))
# 
# mod.year <- readRDS(here("Figs", "Forest_plot_n_males_year.rds"))
# 
# # Make scale the same for all 3 models
# 
# mod.SAM_new <- mod.SAM +
#   coord_cartesian(xlim = c(0.60, 1.15))
# 
# mod.year_new <- mod.year +
#   coord_cartesian(xlim = c(0.60, 1.15))
# 
# # Combine figures
# mod_all <- plot_grid(
#   mod.year_new + xlab("") + theme(axis.text=element_text(size=10.5),
#                                   axis.title=element_text(size=11),
#                                   legend.position="none"),
#   
#   mod.SAM_new + xlab("") + theme(axis.text=element_text(size=10.5),
#                                  axis.title.y=element_text(size=11),
#                                  legend.position="none"),
#   nrow = 3, align = "v",
#   labels = c('(d)', '(e)', '(f)'), label_fontface = "bold", label_size = 12)
# 
# mod_all
# 
# cowplot::save_plot(here("Figs", "Forest_plot_n_male_SAM_year.jpg"), mod_all, base_width = 10, base_height = 8, dpi = 300)
#