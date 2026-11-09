# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 3b_stats_recruitment.R
#
# Purpose: This script tests whether there is an cohort effect 
# on recruitment age before and after 2009.
# The glm testing what factors affect recruitment has been removed
# and replaced with a mark-recapture analysis (see "AFSMaleRecruitment.Rmd").
# This script creates panels fig 2a and c.
# NB Final aesthetics of the figures is done in "4_figs.R"
#
# -----------------------------------------------------------


library(here)
library(readxl)
library(tidyverse)
library(lme4)
library(lmerTest) 
library(DHARMa)
library(sjPlot)
library(ggeffects)
library(ggpubr)


#~~ Load data
gen_males <- readxl::read_excel(here("Data", "processed", "males_sMLH_fitness.xlsx")) %>%
  group_by(uniqueID) %>%
  arrange(Group, .by_group = TRUE) %>%
  ungroup() 

names(gen_males)

max(gen_males$Group, na.rm = T) # 106 pup-male recaptures

gen_males %>% distinct(uniqueID, .keep_all = T) %>% group_by(PupSex) %>% tally() # 3946 males

gen_males %>% 
  mutate(status = ifelse(is.na(status) | status =="BeachDead", "NonRecruited", status)) %>%
  distinct(uniqueID, .keep_all = T) %>%
  group_by(status) %>% 
  tally()

# status           n
# <chr>        <int>
# 1 NonRecruited  3840
# 2 Recruited      106

gen_males <- gen_males %>%
  mutate(status = ifelse(is.na(status) | status =="BeachDead", "NonRecruited", status)) %>%
  mutate(Status = ifelse(status =="Recruited", 1, 0)) %>%
  select(-c(SAMSamplingYear, dtSAMmismatch)) # drop these as they are not used


#~~ Keep only pups genotyped at 39 loci
pups39 <- gen_males %>%
  filter(!grepl("^AGM", PupTissueID)) %>%
  distinct(uniqueID, .keep_all = T) %>%
  filter(!is.na(sMLH_39msat)) 

str(pups39)

# Mean # of loci
pups39 %>% distinct(uniqueID, gaps_39msat) %>% mutate(n_loci = 39 - gaps_39msat) %>%
  summarise(mean_loci = mean(n_loci))

pups39 %>% 
  group_by(Status) %>% 
  tally()
# Status     n
# <dbl> <int>
# 1      0  3757
# 2      1   106
3757+106 # in total 3863 pups genotyped at 39 loci

#~~ Filter pups genotyped at 39 loci for year
# The latest observed age at recruitment was 13, so cohorts born up to 2008 had reached age 13 by 2021.
pups39 %>% 
  #filter(Status == 1) %>%
  summarize(pre2009 = sum(PupBirthyear < 2009),
            up2009 = sum(PupBirthyear >= 2009))
# 1517 pups < 2009 and genotyped at 39 loci
# 2346 pups >= 2009 and genotyped at 39 loci

pups39 %>% 
  filter(PupBirthyear < 2009) %>% 
  group_by(Status) %>% 
  tally()
# Status     n
# <dbl> <int>
# 1      0  1442
# 2      1    75
# Total pups: 1517

# The mean observed age at recruitment was 9, so cohorts born up to 2012 had reached age 9 by 2021.
pups39 %>% 
  #filter(Status == 1) %>%
  summarize(pre2013 = sum(PupBirthyear < 2013),
            up2013 = sum(PupBirthyear >= 2013))
# 2301 pups < 2013 and genotyped at 39 loci
# 1562 pups >= 2013 and genotyped at 39 loci

pups12 <- pups39 %>% filter(PupBirthyear < 2013)

pups12 %>% 
  group_by(Status) %>% 
  tally()
# Status     n
# <dbl> <int>
# 1      0  2195
# 2      1   106 # still 106 so we actually have no recruits born > 2012

# Filter for year but nothing else
gen_males %>%
  filter(!grepl("^AGM", PupTissueID)) %>%
  distinct(uniqueID, .keep_all = T) %>% 
  filter(PupBirthyear < 2009) %>% 
  group_by(Status) %>% 
  tally()

# # A tibble: 2 x 2
# Status     n
# 1      0  1517
# 2      1    75

(75/(75+1517)) *100
# Recruitment rate: 4.7% (4.711055)


#~~ Check for collinearity
melted_cors <- reshape::melt(pups12 %>% 
                               select(sMLH_39msat, PupWeight, PupBirthyear, FirstRecaptureYear, YearsAshore, SAMBirthyear) %>% 
                               mutate_all(as.numeric) %>% 
                               cor(use = 'pair'))

ggplot(data = melted_cors, aes(x=X1, y=X2, fill=value)) + 
  geom_tile() +
  geom_text(aes(label = round(value, 3)), size = 3) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") 
# FirstRecaptureYear and BirthYear highly correlated. 
# But FirstRecaptureYear is not in the model, so no problem

#~~ Check for collinearity including only parameters included in model
melted_cors <- reshape::melt(pups12 %>% 
                               select(sMLH_39msat, PupWeight, PupBirthyear, SAMBirthyear) %>% 
                               mutate_all(as.numeric) %>% 
                               cor(use = 'pair'))

ggplot(data = melted_cors, aes(x=X1, y=X2, fill=value)) + 
  geom_tile() +
  geom_text(aes(label = round(value, 3)), size = 3) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") 
# SAMBirthYear and PupBirthYear highest corrlation 0.257

#~~ Cleaned df
pups12.complete <- pups12 %>%
  select(Status, sMLH_39msat, PupBirthyear, PupWeight, SAMBirthyear, uniqueID, status) %>%
  drop_na

pups12.complete %>% group_by(Status) %>% tally()
# Status     n
# 1      0  1680
# 2      1    87



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Cohort effect recruitment age  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Including all recruits, regardless of missing fitness data such as birth mass
age_males <- gen_males %>% filter(!is.na(FirstRecaptureAge)) %>%
  distinct(uniqueID, FirstRecaptureAge, PupBirthyear, FirstRecaptureYear)

# Recruitment year and birth year are (obvisously) highly correlated:
summary(lm(FirstRecaptureYear ~ scale(PupBirthyear),
           data = age_males))

age_males %>% group_by(PupBirthyear) %>% tally
# up to 2012
age_males %>% group_by(FirstRecaptureYear) %>% tally
# up to 2021

#~~ Effect birth year (cohort) on recruitment age including all years
ggplot(age_males, aes(PupBirthyear, FirstRecaptureAge) ) +
  geom_point()

# Model assumptions
testDispersion(lm(FirstRecaptureAge ~ scale(PupBirthyear),
                          data = age_males))
plotQQunif(lm(FirstRecaptureAge ~ scale(PupBirthyear),
              data = age_males))
plotResiduals(lm(FirstRecaptureAge ~ scale(PupBirthyear),
                 data = age_males)) 

# Model summary
summary(lm(FirstRecaptureAge ~ scale(PupBirthyear),
           data = age_males))
# not sig trend indicating recruitment age changed over the study period


#~~ Effect birth year (cohort) on recruitment age grouping data before and after 2009 and comparing groups
ggplot(age_males %>% mutate(group = ifelse(PupBirthyear < 2009, "before", "after")), 
       aes(group, FirstRecaptureAge) ) +
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.05))

car::leveneTest(data = age_males %>% 
                  mutate(group = ifelse(PupBirthyear < 2009, "before", "after")), 
                FirstRecaptureAge ~ group, 
                center = mean)
# p = 0.03 so different variances between groups (as expected)

# T-test comparing groups assuming unequal variances (as shown above)
t.test(FirstRecaptureAge ~ group, data = age_males %>% 
         mutate(group = ifelse(PupBirthyear < 2009, "before", "after")), var.equal = F)
# p = 0.566; no sig difference in pups born before 2009 and after 2009



#~~~~~~~~~~~~~~~~~~~~~#
#  Raw data plots  ####
#~~~~~~~~~~~~~~~~~~~~~#


#~~ Number of all genotyped recruits vs nonrecruits (all individuals genotyped at 39 loci)(fig 2a)
bar_rec_all_years_unfiltered <- ggbarplot(pups39 %>% 
                                            select(Status, sMLH_39msat, PupWeight, PupBirthyear, dtSAMBirthyear, uniqueID, status) %>%
                                            filter(!is.na(sMLH_39msat)) %>%
                                            group_by(PupBirthyear, status) %>% 
                                            tally(), 
                                          "PupBirthyear", "n",
                                          fill = "status", 
                                          color = "status",
                                          xlab = "Birth year",
                                          ylab = "Number of male pups genotyped at 39 loci",
                                          legend.title = "Status" )
# Get n to add in fig
text.df <- pups39 %>% 
  select(Status, sMLH_39msat, PupWeight, PupBirthyear, dtSAMBirthyear, uniqueID, status) %>%
  filter(!is.na(sMLH_39msat)) %>%
  group_by(PupBirthyear, status) %>% 
  tally() %>%
  left_join(pups39 %>% 
              select(Status, sMLH_39msat, PupWeight, PupBirthyear, dtSAMBirthyear, uniqueID, status) %>%
              filter(!is.na(sMLH_39msat)) %>%
              group_by(PupBirthyear) %>% 
              tally() %>% rename(n_tot = n) ) %>%
  mutate(ypos = ifelse(status == "NonRecruited", n_tot + 10, n + 10))

# Add n to fig
bar_rec_all_years_unfiltered <- bar_rec_all_years_unfiltered +
  geom_text(data=text.df,aes(x=as.factor(PupBirthyear),y=ypos,label=n))

bar_rec_all_years_unfiltered

# Save fig 2a
saveRDS(bar_rec_all_years_unfiltered, here("Figs", "barplot_males_per_year_39loci_all_years_unfiltered.rds"))

#~~ Percentage recruits per year
# Pups genotyped at 39 loci
pups39 %>%
  filter(!is.na(sMLH_39msat)) %>%
  select(uniqueID, PupBirthyear, status) %>%
  distinct(uniqueID, .keep_all = T) %>%
  group_by(PupBirthyear, status) %>%
  tally() %>%
  mutate(Total = sum(n), PercentRecruits = n/Total) %>%
  filter(status == "Recruited") %>%
  ggplot(aes(x = PupBirthyear, y = PercentRecruits, fill = PupBirthyear < 2009)) +
  geom_bar(stat = "identity", width = 1) +
  xlab("Birth year") +
  ylab("Percentage recruits") +
  # Color incomplete recruitment histories grey
  scale_fill_manual(values = c(`FALSE` = "grey", `TRUE` = "grey35"), guide = "none") +
  # Absolute numbers above the bars: recruits / total pups
  geom_text(aes(label = paste0(n, "/", Total)),
            angle = 90, hjust = -0.15, size = 3) +
  # Extra space at the top so the labels aren't cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  theme_pubr() +
  theme(legend.position="none")

save_plot(here("Figs", "percentage_recruits_per_year.jpg"), width = 12, height = 6, dpi = 300)


#~~ Age at recruitment (fig 2c)
#(add dummy column to allow saving and editing figure colors post hoc in "4_figs.R")

# Here we can include all recruits, regardless of missing fitness data such as birth mass
pups12 %>% 
  group_by(Status) %>% 
  tally()
# Status     n
# <dbl> <int>
# 1      0  2195
# 2      1   106

p.age <- ggplot(pups12 %>% filter(status == "Recruited") %>% mutate(dummy = "1"), 
                aes(x = FirstRecaptureAge, fill = dummy, color = dummy)) +
  geom_histogram(binwidth = 1) +
  xlab("Age at first recapture") +
  ylab("Count") +
  theme_minimal() 

p.age

# Save fig 2c
saveRDS(p.age, here("Figs", "age_first_recapture.rds"))

#~~ Same histogram but removing the last 13 season to avoid underestimating recruitment age
# Here we can include all recruits, regardless of missing fitness data such as birth mass

pups12 %>% 
  filter(PupBirthyear < 2009) %>%
  group_by(Status) %>% 
  tally()
# Status     n
# <dbl> <int>
# 1      0  1442
# 2      1    75

p.age2009 <- ggplot(pups12 %>% 
                  filter(status == "Recruited") %>% 
                  filter(PupBirthyear < 2009) %>% 
                  mutate(dummy = "1"), 
                aes(x = FirstRecaptureAge, fill = dummy, color = dummy)) +
  geom_histogram(binwidth = 1) +
  xlab("Age at first recapture") +
  ylab("Count") +
  theme_minimal() 

p.age2009

coltan <- "#B0925B" 

cowplot::plot_grid(p.age + 
  scale_fill_manual(values = coltan) + 
  scale_color_manual(values = "black") + 
  xlab("Age at recruitment") +
  ggtitle("All birth years") +
  theme_pubr() +
  theme(legend.position="none") +
  coord_cartesian(ylim = c(0, 40)), 

p.age2009 + 
  scale_fill_manual(values = coltan) + 
  scale_color_manual(values = "black") + 
  xlab("Age at recruitment") +
  ggtitle("Birthyear < 2009") +
  theme_pubr() +
  theme(legend.position="none") +
  coord_cartesian(ylim = c(0, 40)) 
)

save_plot(here("Figs", "age_first_recapture_comparison.jpg"), width = 12, height = 6, dpi = 300)
