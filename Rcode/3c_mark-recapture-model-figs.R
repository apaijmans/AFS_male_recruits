# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 3c_mark-recapture-model-figs.R
#
# Purpose: This script takes the results from the mark-recapture 
# analysis (see "AFSMaleRecruitment.Rmd") and generates table 2
# and figure panels 2b and d.
# NB Final aesthetics of the figures is done in "4_figs.R"
#
# -----------------------------------------------------------

library(here)
library(tidyverse)
library(gt)

# Load results from mark-recapture analysis
load(here("Data", "external", "AFSMaleRecruitment_new.RData"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Mark-recapture framework: best model table  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


model_summary <- coef(mod.25)

mod_table <- model_summary |>
  gt(rownames_to_stub = TRUE) |>
  fmt_number(columns = c(Estimate, se, lcl, ucl), decimals = 3) |>
  cols_merge(
    columns = c(lcl, ucl),
    pattern = "{1}; {2}"
  ) |>
  cols_label(Estimate = "Beta estimate", se = "SE", lcl = "95% CI")
  
gtsave(mod_table, here("Tables", "table_best_model.docx"))
  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Mark-recapture framework: apparent survival fig  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

blue1 <- c("#C9E8FF") # light blue
blue2 <- c("#9bd5ff") # mid blue
blue3 <- c("#1F78B8")

gglayer_theme <- list(
  #geom_point(shape = 22, size = 4),
  theme_minimal(),
  theme(axis.line.x = element_line(colour = 'black', linetype='solid'),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        axis.text = element_text(colour = 'black'),
        plot.title = element_text(size = rel(1)))
)

# Sam in different panels
p_S <- ggplot(data=S1313)+
  geom_line(aes(x=Age,y=estimate,colour=MLH),position=position_dodge(width=0.3),linewidth=.8)+
  geom_linerange(aes(ymin=lowCI,ymax=highCI,x=Age,colour=MLH),position=position_dodge(width=0.3), alpha = 0.6)+   # ,alpha=1,show.legend=FALSE,colour="olivedrab")+
  geom_point(aes(x=Age,y=estimate,colour=MLH),size=2.5,position=position_dodge(width=0.3))+ #  ,show.legend=FALSE,colour="olivedrab")+
  facet_wrap(~SAM, axes = "all_y") +
  ylab("Apparent survival")+
  xlab("Age")+
  scale_colour_manual(
                    name="sMLH",
                    values = c("low" = blue1, "mid" = blue2, "high" = blue3),
                    breaks=c("low", "mid", "high"),
                    labels=c("Low", "Mid", "High")) +
  scale_x_continuous(breaks=seq(0,14,by=2),minor_breaks=seq(0,15,by=1),limits=c(-0.5,14))+
  scale_y_continuous(breaks=seq(0,1,by=0.1),minor_breaks=seq(0,1,by=0.05)) +  #,limits=c(0.1,0.55))+
  gglayer_theme

p_S

#ggsave(here("Figs", "p_S_model.jpg"), height = 3, width = 7)
saveRDS(p_S, here("Figs", "p_S_model.rds"))


# # sMLH in different panels
# labels <- c(low = "Low sMLH", mid = "Mid sMLH", high = "High sMLH")
# 
# p_S <- ggplot(data=S1313)+
#   geom_line(aes(x=Age,y=estimate,colour=SAM),position=position_dodge(width=0.3),linewidth=0.8)+
#   geom_linerange(aes(ymin=lowCI,ymax=highCI,x=Age,colour=SAM), position=position_dodge(width=0.3), alpha = 0.6)+   # ,alpha=1,show.legend=FALSE,colour="olivedrab")+
#   geom_point(aes(x=Age,y=estimate,colour=SAM),size=2.5,position=position_dodge(width=0.3))+ #  ,show.legend=FALSE,colour="olivedrab")+
#   facet_wrap(~ MLH, labeller=labeller(MLH = labels)) +
#   ylab("Apparent survival")+xlab("Age")+
#   scale_colour_manual(
#     name="Birth year SAM",
#     values = c("negative SAM" = col3, "neutral SAM" = col2, "positive SAM" = col1),
#     breaks=c("negative SAM", "neutral SAM", "positive SAM"),
#     labels=c("negative", "neutral", "positive")) +
#   scale_x_continuous(breaks=seq(0,14,by=2),minor_breaks=seq(0,15,by=1),limits=c(-0.5,14))+
#   scale_y_continuous(breaks=seq(0,1,by=0.1),minor_breaks=seq(0,1,by=0.05)) +   #,limits=c(0.1,0.55))++
#   gglayer_theme 
# p_S


brown0 <- "#D4BC8C"
brown1 <- "#B0925B"  # lighter brown
brown2 <- "#907645"  # darker brown
brown3 <- "#765F36"
brown4 <- "#5E4B28"
brown5 <- "#3A2E17"

shapes <- c(16,  # filled circle
            17,  # filled triangle up
            8,   # *
            18,  # filled diamond
            15,  # filled square
            25)  # fillable triangle down

p_R <- ggplot(data=Psi.df)+
  geom_line(aes(x=year,y=estimate,colour=Age),position=position_dodge(width=0.35),linewidth=1)+
  geom_linerange(aes(ymin=lowCI,ymax=highCI,x=year,colour=Age),position=position_dodge(width=0.35), alpha = 0.6)+   # ,alpha=1,show.legend=FALSE,colour="olivedrab")+
  geom_point(aes(x=year,y=estimate,colour=Age, fill = Age, shape = Age),size=2.5,position=position_dodge(width=0.35))+ #  ,show.legend=FALSE,colour="olivedrab")+
  ylab("Recruitment probability")+
  xlab("Year")+
  scale_colour_manual(
    name="Age",
    values = c("7" = brown0, "8" = brown1, "9" = brown2, "10" = brown3, "11" = brown4, "12" = brown5)) +
  scale_fill_manual(values = c("7" = brown0, "8" = brown1, "9" = brown2, "10" = brown3, "11" = brown4, "12" = brown5)) +
  scale_shape_manual(values = shapes) +
  scale_x_continuous(breaks=seq(2010,2020,by=5),minor_breaks=seq(2006,2021,by=1)) +
  ylim(0.0, 0.63) + # make a bit extra space at the top so that the labels do not overlap
  gglayer_theme 

p_R

saveRDS(p_R, here("Figs", "p_R_model.rds"))

#ggsave(here("Figs", "p_R_model.jpg"), height = 3, width = 8)
