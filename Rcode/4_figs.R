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
p.sam_year <- readRDS(here("Figs", "SAM_year_reg_line_1995-2021.rds"))

p.sam_year_new <- p.sam_year + scale_x_continuous("Year", breaks = c(1995, 2005, 2015)) + theme_pubr()

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

#~~ Fig 2b: Apparent survival split by SAM (negative/neutral/high)
p_S <- readRDS(here("Figs", "p_S_model.rds"))

#~~ Fig 2c: Histogram age (all 106 recruited males)
p.age <- readRDS(here("Figs", "age_first_recapture.rds"))

#~~ Fig 2d: recruitment probability over time split by age
p_R <- readRDS(here("Figs", "p_R_model.rds"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Adjust figure aesthetics  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~ Panel a: All genotyped male pups 39 loci
bar_rec_new <- bar_rec_all + 
  scale_color_manual(values = c("black", "black"), labels=c('Non-recruited', 'Recruited')) + 
  scale_fill_manual(values = c(colblue, coltan), labels=c('Non-recruited', 'Recruited')) +
  scale_x_discrete(breaks=seq(1995,2020,by=5))+
  ylab("No. of genotyped male pups") +
  coord_cartesian(ylim = c(0, 280)) +
  theme(legend.position="none")

bar_rec_new


#~~ Panel c: Age distribution recruitment
p.age_new <- p.age + 
  scale_fill_manual(values = coltan) + 
  scale_color_manual(values = "black") + 
  xlab("Age at recruitment") +
  theme_pubr() +
  theme(legend.position="none") +
  coord_cartesian(ylim = c(0, 45)) 

p.age_new



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Combine and save figure 2  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# get current margins: theme_get()$plot.margin

#~~ Combine panels fig 2
# Apply theme tweaks
pa <- bar_rec_new + theme(axis.text = element_text(size = 11),
                          axis.title = element_text(size = 13))
pc <- p.age_new + theme(axis.text = element_text(size = 11),
                        axis.title = element_text(size = 13))
pb <- p_S + theme(axis.text = element_text(size = 11), 
                  axis.title = element_text(size = 13),
                  strip.text = element_text(size = 11),
                  legend.title=element_text(size=13), 
                  legend.text=element_text(size=11),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
pd <- p_R + theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13),
                  legend.title=element_text(size=13), 
                  legend.text=element_text(size=11),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),)

# Pass 1: left edges of a, b, c
al <- align_plots(pa, pb, pc, align = "v", axis = "l")

# Pass 2: right edges of a, b, d (reusing the left-aligned versions)
ar <- align_plots(al[[1]], al[[2]], pd, align = "v", axis = "r")

row_cd <- plot_grid(al[[3]], ar[[3]],
                     nrow = 1, labels = c("(c)", "(d)"),
                     rel_widths = c(0.5, 1),
                     label_fontface = "bold")

all_plots <- plot_grid(
  ar[[1]], ar[[2]], row_cd, #legends,
  ncol = 1,
  rel_heights = c(1.15, 1, 1), #, 0.15),
  labels = c("(a)", "(b)", ""),
  label_fontface = "bold"
)

all_plots

save_plot(here("Figs", "fig2_male_recruitment_mark_recapture.jpg"), all_plots, base_width = 10, base_height = 12, dpi = 300)



# al <- align_plots(pa, pb, pc, pd, pe, pf, align = "v", axis = "lr")
# 
# row_bcd <- plot_grid(al[[2]], al[[3]], al[[4]],
#                      nrow = 1, labels = c("(b)", "(c)", "(d)"),
#                      label_fontface = "bold")
# 
# row_ef <- plot_grid(al[[5]], al[[6]],
#                     nrow = 2, labels = c("(e)", "(f)"),
#                     label_fontface = "bold")
# 
# all_plots <- plot_grid(
#   al[[1]], row_bcd, row_ef,
#   ncol = 1,
#   rel_heights = c(1.15, 0.9, 2),
#   labels = c("(a)", "", ""),
#   label_fontface = "bold"
# )
# 
# all_plots
# save_plot(here("Figs", "fig2_test.jpg"), all_plots, base_width = 10, base_height = 12, dpi = 300)

