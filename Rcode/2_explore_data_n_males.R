# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 2_explore_data_n_males.R
#
# Purpose: This script creates some of the panels for fig 1:
# - male annual counts (fig 1c and table s1)
# - number of recaptures (fig 1b).
# NB Final aesthetics of the figures is done in "4_figs.R"
#
# -----------------------------------------------------------


library(here)
library(readxl)
library(tidyverse)
#library(MoMAColors)
library(ggpubr)



#~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Male annual counts  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~ Load data
n_males <- readxl::read_excel(here("Data", "processed", "total_n_males.xlsx")) # genetically sampled unique males

obs_males <- readxl::read_excel(here("Data", "processed", "n_observed_males.xlsx")) # all observed males AND observed and sampled unique males combined

str(obs_males)


#~~ Observed vs tissue sampled males per year
tot_males <- full_join(n_males, obs_males, by = "SamplingYear") %>%
  rename(GenMales = TotalMales, AllMales = n)

tot_males[which(is.na(tot_males$GenMales)), "GenMales"] <- 0 # no males were tissue sampled in 1998/1999

colSums(tot_males)["GenMales"]/colSums(tot_males)["AllMales"] # 77% of males are tissue sampled across all years


#~~ Percentage decline
(tot_males[which(tot_males$SamplingYear == 1995), "AllMales"]-tot_males[which(tot_males$SamplingYear == 2021), "AllMales"]) / tot_males[which(tot_males$SamplingYear == 1995), "AllMales"] * 100
# decline is 61% in 27 years (1995 until 2021)


#~~ Make table for suppl with gt (table S1)
library(gt)

tot_males %>%
  mutate(Per = round(GenMales/AllMales*100, digits = 0)) %>%
  select(Year = SamplingYear, "n total" = AllMales, "n genotyped" = GenMales, "% sampled" = Per) %>%
  gt() %>% 
  tab_header(
    title = "Adult males",
    subtitle = ""
  ) %>% 
  gtsave(filename = "Tables/sampled_males.docx")

# Plot number of tissue sampled adult males per year
n.males <- ggplot(tot_males %>% pivot_longer(c(AllMales, GenMales), names_to = "sampled", values_to = "n"), 
                  aes(x = SamplingYear, y = n, group = sampled)) + 
  #geom_smooth(method = "lm", colour = "black", fill = "darkgrey", alpha = 0.8) +
  geom_point(aes(col = sampled)) +
  geom_line(aes(col = sampled)) +
  scale_color_manual(labels = c("N sampled", "N sampled + observed"), values = c("darkgrey", "black")) +
  labs(x = "Year", y = "Male count", color = "")
#ggpubr::stat_cor(method = "pearson",  show.legend = FALSE, label.y.npc = "top")

n.males

#ggsave(here("Figs", "n_sampled_vs_observed_males_per_year.jpg"), height = 3.5, width = 4)


#~~ Annual counts of all (observed only+observed and sampled) males (fig 1c)
male_plot <- ggpubr::ggscatter(tot_males, x = "SamplingYear", y = "AllMales",
                               add = "reg.line",
                               conf.int = TRUE,
                               #cor.coef = TRUE,
                               cor.method = "pearson",
                               na.rm = TRUE,
                               add.params = list(fill = "darkgrey")) + # Customize reg. line) +
  #ggpubr::stat_regline_equation(aes(label =  after_stat(rr.label)), label.y = 20) + # Adds R2
  labs(x="Year", y= "Male counts")

male_plot

saveRDS(male_plot, here("Figs", "n_obs_males_per_year.rds"))



#~~~~~~~~~~~~~~~~~~~~~~#
#  Male recaptures  ####
#~~~~~~~~~~~~~~~~~~~~~~#

#~~ Load data
males <- readxl::read_excel(here("Data", "processed", "all_males_wrangled.xlsx"))

nrow(males %>% distinct(Group)) # total number of genetically sampled unique males: 1217

#~~ Number of years an individual was captured (fig 1b)
p.recap <- ggplot(males %>% distinct(Group, YearsAshore) %>% mutate(dummy = "1"), 
                  aes(x = YearsAshore, fill = dummy, color = dummy)) +
  geom_histogram(binwidth = 1) +
  xlab("Number of captures") +
  ylab("Count") +
  theme_minimal() 

p.recap

# Save fig, make pretty in script "4_figs.R"
saveRDS(p.recap, here("Figs", "hist_n_captures_all_males.rds"))

# Mean number of seasons ashore? 2
colMeans(males %>% distinct(Group, YearsAshore) %>% select(YearsAshore))

# Explore mean/median recaptures (not counting multiple captures per year)
males %>% 
  distinct(Group, YearsAshore) %>%
  summarise(av = mean(YearsAshore, na.rm = T),
            md = median(YearsAshore, na.rm = T),
            mn = min(YearsAshore, na.rm = T),
            mx = max(YearsAshore, na.rm = T))
# # A tibble: 1 × 4
# av    md    mn    mx
# <dbl> <dbl> <dbl> <dbl>
#   1  2.01     2     1     9
