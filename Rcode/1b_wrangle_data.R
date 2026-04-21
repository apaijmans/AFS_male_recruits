# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 1b_wrangle_data.R
#
# Purpose: This script contains thee main parts.
#
# In the first part, the male genetic recapture data
# is used to calculate the number of observed males per year
# ("total_n_males.xlsx", further used in script "2_explore_data_n_males.R").
#
# In the second part, the male genetic recapture data is used
# to calculate variables such as first/last year of recapture 
# age at recapture, skippers etc. This df is saved as
# "all_males_wrangled.xlsx" and further used in script 
# "2_explore_data_n_males.R".
#
# The third part uses only the natal genetic recaptures (ie
# pups that were recaptured as an adult male), adds all other male pups (ie
# male pups that were not later recaptured as an adult)
# and adds fitness data and sMLH. Finally, the SAM values are added 
# to the datafame and the SAM figure (fig 1d) is created and saved.
# This df is then saved as: "males_sMLH_fitness.xlsx# and further
# used in script "3b_stats_interaction_SAM.R".
#
# -----------------------------------------------------------


library(here)
library(readxl)
library(tidyverse)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Load male genetic recapture data  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~ Code to load the all_genotypes dataframe, but taking only the columns needed to extract recaptured males
AFS <- read_excel(here("Data", "raw", "all_msat_genotypes_uniqueID.xlsx"),
  guess_max = 15000) %>%
  select(uniqueID, SampleID, dummyID, Pup, Male, Best_genotype, Matches)

nrow(AFS %>% filter(Pup == 1) %>% distinct(uniqueID)) # 8580
nrow(AFS %>% filter(Male == 1)%>% distinct(dummyID)) # 2696

#~~ Filter all males and pups that were resampled as adult males
# This includes all adult males and male pups but only if they were recaptured as an adult male
males <- AFS %>%
  filter(Male == 1 | 
           (Pup == 1 & grepl(".*AGM.*", Matches)))
# 2920

str(males)

# Add a group ID to each group of males
males <- males %>%
  group_by(uniqueID) %>%
  mutate(Group = cur_group_id()) %>%
  ungroup() %>%
  arrange(Group)

max(males$Group) # 1217 pup-male and male-male recaptures, and adult males sampled only once

# Check which groups have either none or multiple samples identified as best_genotype (ie sum best_genotype is either > 1 or < 1)
males %>% group_by(Group) %>% summarise(Best_gen = sum(as.numeric(Best_genotype))) %>% filter(Best_gen != 1)
# 0, so all is good

# Fix sampling year
males <- males %>% 
  separate(SampleID , c("AG", "SamplingYear"), sep = "(?<=[A-Za-z])(?=[0-9])", remove = F) %>%
  mutate(pre = ifelse(grepl("^9", .$SamplingYear), 19, 20)) %>%
  mutate(SamplingYear = paste(pre, SamplingYear, sep = "")) %>%
  mutate(SamplingYear = substr(SamplingYear, 1, 4)) %>%
  mutate(SamplingYear = as.numeric(SamplingYear)) %>%
  select(-c(pre, AG)) %>%
  mutate(PupBirthyear = ifelse(grepl("^AGP", SampleID), SamplingYear, NA)) %>%
  group_by(Group) %>%
  fill(PupBirthyear, .direction = "downup") %>%
  ungroup() %>%
  mutate(SamplingAge = SamplingYear - PupBirthyear) %>%
  mutate(SamplingYear = ifelse(dummyID == "M1", 2007, SamplingYear)) %>% # M1 was sampled in 2006/07 (and most likely AGM06089)
  mutate(SamplingYear = ifelse(dummyID == "L-R-EAR", 2002, SamplingYear)) %>% # L-R-EAR was sampled in 2001/02, found in ledger
  mutate(SamplingYear = ifelse(dummyID == "O-3SP-RUMP", 2002, SamplingYear)) # O-3SP-RUMP was probably sampled in 2001/02, done on the same plate as all other experimental males from 2001

males %>% filter(is.na(SamplingYear)) # none without a sampling year


#~~ How many times was each male recaptured?
males %>% 
  filter(!grepl(".*AGP.*", SampleID)) %>% # remove pups
  group_by(Group) %>%
  tally() %>%
  summarise(mean_recap = mean(n, na.rm = T),
            min_recap = min(n, na.rm = T), 
            max_recap = max(n, na.rm = T))
# mean: 2, min: 1, max: 14

# How many times was each male recaptured within seasons and across seasons?
within_year <- males %>% 
  filter(!grepl(".*AGP.*", SampleID)) %>% # remove pups
  group_by(SamplingYear, Group) %>%
  tally() %>%
  ungroup() %>%
  summarise(
    total_captures      = sum(n),
    recaptured_within   = sum(n[n > 1]),  # captures beyond the 1st
    pct_recapture_within = round(recaptured_within / total_captures * 100, 1)
  )

round(sum(within_year$recaptured_within)/sum(within_year$total_captures)* 100, 1)
# summing up over all years 22.5 % of individuals was recaptured within the same year

across_year <- males %>% 
  filter(!grepl(".*AGP.*", SampleID)) %>% # remove pups
  distinct(SamplingYear, Group) %>% # removes all duplicates within a year
  group_by(Group) %>%
  tally() %>%
  ungroup() %>%
  summarise(
    total_individuals    = n(), # total unique animals
    total_captures      = sum(n),
    recaptured_across   = sum(n[n > 1]),  # captures beyond the 1st
    pct_recapture_across = round(recaptured_across / total_captures * 100, 1)
  )

across_year
# 75% was recaptured across years



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Pt 1: Obtain annual count unique adult genotyped males  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~ Calculate total number of gen sampled unique males per year
n_males <- males %>% 
  filter(!grepl(".*AGP.*", SampleID)) %>% # remove pups
  distinct(uniqueID, SamplingYear) %>% 
  group_by(SamplingYear) %>% 
  tally() %>%
  complete(SamplingYear = min(SamplingYear, na.rm = T):max(SamplingYear, na.rm = T)) %>%
  rename(TotalMales = n)

#males <- left_join(males, n_males)


#~~ Save dataframe: total n genetically sampled unique males per year 
# Used in 2_explore_data_n_males.R
n_males2 <- n_males %>%
  mutate(SamplingYear = SamplingYear + 1) # Breeding seasons are referred to by the year in which they ended

openxlsx::write.xlsx(n_males2, here("Data", "Processed", "total_n_males.xlsx"))  # only genetically sampled males



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Pt 2: Male recapture variables  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Note: based on genetic recaptures, not taken into account visual observations

# Age at first recapture and age at last recapture
first_recap_age <- males %>% 
  distinct(Group, SamplingAge) %>% 
  group_by(Group) %>%
  filter(SamplingAge != 0) %>% 
  slice_min(SamplingAge) %>%
  rename(FirstRecaptureAge = SamplingAge) %>%
  ungroup()

males <- full_join(males, first_recap_age)

last_recap_age <- males %>% 
  distinct(Group, SamplingAge) %>% 
  group_by(Group) %>%
  filter(SamplingAge != 0) %>% 
  slice_max(SamplingAge) %>%
  rename(LastRecaptureAge = SamplingAge) %>%
  ungroup()

males <- full_join(males, last_recap_age)

# First recapture year and last recapture year
first_recap <- males %>% 
  filter(!grepl(".*AGP.*", SampleID)) %>% # remove pups
  distinct(Group, SamplingYear) %>% 
  group_by(Group) %>%
  slice_min(SamplingYear) %>%
  rename(FirstRecaptureYear = SamplingYear) %>%
  ungroup()

males <- full_join(males, first_recap)

last_recap <- males %>% 
  distinct(Group, SamplingYear) %>% 
  group_by(Group) %>%
  slice_max(SamplingYear) %>%
  rename(LastRecaptureYear = SamplingYear) %>%
  ungroup()

males <- full_join(males, last_recap)

# Calculate survival (ie last recapture year - first recapture year)
males <- males %>%
  mutate(LastRecap_FirstRecap = LastRecaptureYear - FirstRecaptureYear) 

# Calculate tenure duration (ie years captured ashore as an ADULT male ie ignoring pup year)
male_ten <- males %>%
  filter(!grepl(".*AGP.*", SampleID)) %>% # remove pups
  group_by(Group) %>%
  summarise(n(),
            n_distinct(SamplingYear)) %>% # take years in which individuals were sampled multiple times as only 1
  select(Group, YearsAshore = `n_distinct(SamplingYear)`) %>% 
  ungroup()

males <- full_join(males, male_ten %>% distinct(Group, YearsAshore))


#~~ Save dataframe: 
# for each recaptured male: year of first recapture, year of last recapture, years ashore (tenure duration)
# Used in 2_explore_data_n_males.R  

# Breeding seasons are referred to by the year in which they ended
males2 <- males %>%
  mutate(SamplingYear = SamplingYear + 1) %>% 
  mutate(PupBirthyear = PupBirthyear + 1) %>%
  mutate(FirstRecaptureYear = FirstRecaptureYear + 1) %>%
  mutate(LastRecaptureYear = LastRecaptureYear + 1)

openxlsx::write.xlsx(males2, here("Data", "Processed", "all_males_wrangled.xlsx"))  



#~~~~~~~~~~~~~~~~~~~~#
#  Male skippers  ####
#~~~~~~~~~~~~~~~~~~~~#


#~~  Explore years skipped during tenure
skipped_years <- males %>%
  filter(!grepl("^AGP", SampleID)) %>%
  group_by(Group) %>%
  arrange(SamplingYear, .by_group = T) %>% 
  mutate(Diff = SamplingYear - lag(SamplingYear))

max(skipped_years$Diff, na.rm = T)
# Max skipped year 5: one male was sampled in 1995 and then again in 2000 but not in between

skipped_years %>% group_by(Group, Diff) %>% tally() %>% filter(!is.na(Diff) & Diff != 0 & Diff != 1) %>% filter(n>1)
# 11 males that skipped 1 year, twice (eg seen in 2005, then 2007, then 2009)

skipped_years %>% group_by(Group, Diff) %>% tally() %>% filter(!is.na(Diff) & Diff != 0 & Diff != 1)  %>% group_by(Group) %>% tally() %>% filter(n>1)
# plus 1 male that skipped 1 year and then 2 years (1994-97-99)

skipped_years %>% distinct(Group, Diff) %>% group_by(Diff) %>% tally()
# Diff     n
# <dbl> <int>
# 1     0   251 # These are individuals that were sampled more than once in the same year
# 2     1   567 # These are NOT skipped years, but simply returns the next year (eg a male seen in 1994 and then in 1995 will have a difference of 1)
# 3     2    76 # So this would be males that skipped 1 year. Eg seen in 1994 and 1996, but not 1995
# 4     3    16 # Skipped 2 years
# 5     4    11 # Skipped 3 years
# 6     5     1 # Skipped 4 years
# 7    NA  1217 # these are both the first row within a group but also any groups that consist of only 1 sample (ie not recaptured individuals)

# Who are the skippers?
skippers <- males %>%
  filter(!grepl("^AGP", SampleID)) %>%
  group_by(Group) %>%
  arrange(SamplingYear, .by_group = T) %>%
  complete(SamplingYear = min(SamplingYear):max(SamplingYear), fill = list(SampleID = "no_sample")) %>% # Fill in missing years
  filter(any(SampleID=="no_sample")) %>%
  ungroup()

nrow(skippers %>% distinct(Group))
# 103 males that skipped at least 1 year of tenure between first appearance and last appearance
nrow(skippers %>% distinct(Group))/nrow(males %>% distinct(Group)) *100
# ie 8.5% of males skipped at least one year


# 103 or 104 individuals skipping a year?
# t <- skipped_years %>% distinct(Group, Diff) %>% filter(!is.na(Diff) & Diff != 0 & Diff != 1)
# 
# t %>% filter(Group %in% unique(.[["Group"]][duplicated(.[["Group"]])])) 
# 
# which(!t$Group %in% skippers$Group)
# 103, 1 male skipped 1 year and then 2 years, so was counted double in the number of years skipped overview



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Pt 3: keep only natal males  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Keep only males that were sampled as a pup and recaptured at least once
male_recaptures <- males %>%
  group_by(uniqueID) %>%
  filter(any(grepl("^AGP", SampleID))) %>%
  # add a new group ID to each group
  mutate(Group = cur_group_id()) %>%
  ungroup() %>%
  arrange(Group)

max(male_recaptures$Group) # 106 pup-male recaptures

# Check which groups have either none or multiple samples identified as best_genotype (ie sum best_genotype is either > 1 or < 1)
male_recaptures %>% group_by(Group) %>% summarise(Best_gen = sum(as.numeric(Best_genotype))) %>% filter(Best_gen != 1)
# zero, so all good

# Keep only relevant columns
male_recaptures <- male_recaptures %>% 
  distinct(Group, 
           SampleID = dummyID, 
           SamplingYear, 
           PupBirthyear, 
           SamplingAge, 
           YearsAshore, 
           FirstRecaptureYear, 
           LastRecaptureYear, 
           FirstRecaptureAge, 
           LastRecaptureAge)


#~~ Skippers among natal males
skippers <- male_recaptures %>%
  filter(!grepl("^AGP", SampleID)) %>%
  group_by(Group) %>%
  arrange(SamplingYear, .by_group = T) %>%
  complete(SamplingYear = min(SamplingYear):max(SamplingYear), fill = list(SampleID = "no_sample")) %>% # Fill in missing years
  filter(any(SampleID=="no_sample")) %>%
  ungroup()

nrow(skippers %>% distinct(Group))
# 10 males that skipped at least 1 year of tenure between first appearance and last appearance
nrow(skippers %>% distinct(Group))/nrow(male_recaptures %>% distinct(Group)) *100
# ie 9.4% of males skipped at least one year


skipped_years <- male_recaptures %>%
  filter(!grepl("^AGP", SampleID)) %>%
  group_by(Group) %>%
  arrange(SamplingYear, .by_group = T) %>% 
  mutate(Diff = SamplingYear - lag(SamplingYear))

max(skipped_years$Diff, na.rm = T)
# max 2, so that means 1 years skipped between tenure

# nrow(skipped_years %>% 
#        group_by(Group) %>% 
#        filter(any(Diff == 3)) %>%
#        distinct(Group))

nrow(skipped_years %>% 
       group_by(Group) %>% 
       filter(any(Diff == 2)) %>%
       distinct(Group))
# 10 males skipped 1 year between tenure

# ggplot(skipped_years %>% filter(Diff > 0)) + # Diff = 0 is a result of resampling in the same year
#   geom_histogram(aes(x = Diff), binwidth = 1)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Add sMLH and phenotypic data  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~ Load sMLH data for male pups
sMLH_msats <- read_excel(here("Data", "raw", "sMLH_msats_male_pups.xlsx"), guess_max = 4000)

# There are duplicate IDs, but that is because the tissue ID is kept in there
# Per unique ID the best genotype is used to calculate the sMLH
janitor::get_dupes(sMLH_msats %>% filter(!is.na(uniqueID)), uniqueID) 
nrow(janitor::get_dupes(sMLH_msats %>% filter(!is.na(uniqueID)), uniqueID) %>% distinct(uniqueID)) # 73 sample IDs that are duplicated

nrow(sMLH_msats %>% distinct(uniqueID, sMLH_9msat, gaps_9msat, sMLH_39msat, gaps_39msat))
# 3995


#~~ Load phenotypic data male pups
# Parameters of interest: status, birth year, weight, coord pupping
male_pups <- read_excel(here("Data", "raw", "male_pups_pheno_data.xlsx"), guess_max = 9000)

janitor::get_dupes(male_pups %>% filter(!is.na(uniqueID)), uniqueID) # duplicate tissue IDs, usually phenotypic data is the same, but sometimes there are differences...


#~~ Check differences in phenotypic data across duplicates
dups <- male_pups %>% filter(uniqueID %in% unique(.[["uniqueID"]][duplicated(.[["uniqueID"]])])) %>% 
  filter(!is.na(uniqueID))

# Identify individuals with mismatching phenotypic data
dups %>% 
  mutate(Status = ifelse(status == "Recruited", 1,
                         ifelse(status == "NonRecruited", 2,
                                ifelse(status == "BeachDead", 3, 4)))) %>%
  group_by(uniqueID) %>% 
  summarize(stts = diff(Status),
            born = diff(PupBirthyear),
            mass = diff(PupWeight),
            coord = diff(as.numeric(CoordPupping))) %>%
  mutate(sum = born + mass + coord) %>%
  filter(sum!=0) %>%
  ungroup()
# none, so that is good


#~~ Expand pupping coordination to all individuals with the same uniqueID
male_pups <- male_pups%>% 
  group_by(uniqueID) %>%
  fill(CoordPupping, .direction = "downup") %>% 
  ungroup() 

nrow(male_pups %>% filter(!is.na(uniqueID)) %>% distinct(uniqueID))
# 3995 obs

pups <- full_join(male_pups, sMLH_msats, by = c("PupTissueID" = "SampleID", "uniqueID"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Combine fitness data with recaptured pups  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~ Join with recaptures
all_males <- full_join(pups, male_recaptures %>% rename(PupTissueID = SampleID), by = "PupTissueID")

# Check birthyear
nrow(all_males %>% 
       mutate(check = ifelse(!is.na(PupBirthyear.x) & !is.na(PupBirthyear.y) & PupBirthyear.x != PupBirthyear.y, "WRONG", NA)) %>% 
       filter(check == "WRONG"))
# zero

all_males <- all_males %>%
  mutate(PupBirthyear = coalesce(PupBirthyear.x, PupBirthyear.y)) %>%
  select(PupTissueID, uniqueID, status, PupBirthyear, PupSex, PupWeight, CoordPupping, 
         sMLH_9msat, sMLH_39msat, gaps_39msat, Group, SamplingYear, SamplingAge, 
         FirstRecaptureAge, LastRecaptureAge, FirstRecaptureYear, LastRecaptureYear,YearsAshore)

# Duplicates?
all_males %>% filter(PupTissueID %in% unique(.[["PupTissueID"]][duplicated(.[["PupTissueID"]])])) %>% arrange(PupTissueID)
# only P00150, which is correct

# Change status to recruited for recaptured pups
all_males$status[all_males$PupTissueID %in% male_recaptures$SampleID] <- "Recruited"

all_males %>% filter(!is.na(uniqueID)) %>% distinct(uniqueID, .keep_all = T) %>% group_by(status) %>% tally()
# status           n
# <chr>        <int>
# 1 BeachDead      708
# 2 NonRecruited    34
# 3 Recruited      106
# 4 NA            3147


# Check sex
#which(all_males$PupSex[all_males$PupTissueID %in% male_recaptures$SampleID] == "F") # No recaptured pups identified as females
#which(is.na(all_males$PupSex[all_males$PupTissueID %in% male_recaptures$SampleID])) # but many missing sex

# Add/Change sex to male for recaptured pups
all_males$PupSex[all_males$PupTissueID %in% male_recaptures$SampleID] <- "M"

all_males <- all_males %>%
  group_by(uniqueID) %>%
  fill(status, .direction = "downup") %>% 
  fill(PupSex, .direction = "downup") %>% 
  ungroup()

all_males %>% filter(!is.na(uniqueID)) %>% distinct(uniqueID, .keep_all = T) %>% group_by(PupSex) %>% tally()
# PupSex     n
#   1 M       3995

all_males %>% 
  filter(!is.na(uniqueID)) %>% 
  filter(!is.na(sMLH_9msat) | !is.na(sMLH_39msat)) %>% 
  distinct(uniqueID, .keep_all = T) %>% 
  group_by(PupSex) %>% 
  tally()
# 3946

# Keep only samples genotyped for 9 and/or 39 loci
# max 21-22% missing data (ie max 2 missing loci out of 9, max 8 missing loci out of 39)

gen_males <- all_males %>%
  filter(!is.na(sMLH_9msat) | !is.na(sMLH_39msat))

gen_males %>%
  filter(!is.na(sMLH_39msat)) %>%
  filter(status == "Recruited") %>%
  distinct(uniqueID, PupBirthyear, status) %>%
  group_by(PupBirthyear) %>%
  tally()

overview <- gen_males %>%
  filter(!is.na(sMLH_39msat)) %>%
  #filter(!is.na(sMLH_9msat)) %>%
  mutate(status = ifelse(is.na(status) | status =="BeachDead", "NonRecruited", status)) %>%
  group_by(PupBirthyear, status) %>%
  tally() %>%
  group_by(PupBirthyear) %>%
  filter(any(status=="Recruited")) %>%
  ungroup() #%>%
# group_by(PupBirthyear) %>%
# tally()

gen_males %>% filter(!is.na(uniqueID)) %>% distinct(uniqueID, .keep_all = T) %>% group_by(PupSex) %>% tally()
# PupSex     n
#  1 M       3946



#~~~~~~~~~~~~~~~~~~~~#
#  Add SAM index  ####
#~~~~~~~~~~~~~~~~~~~~#

sam_index <- read.table(here("data", "raw", "newsam.1957.2007.seas.txt"), header = T) %>%
  rownames_to_column("year") %>%
  mutate(year = as.numeric(year)) %>%
  mutate(annual_mean = as.numeric(ANN)) %>%
  select(year, annual_mean)

#~~ Make figure showing SAM index over time (fig 1d)
sam_plot <- ggpubr::ggscatter(sam_index, x = "year", y = "annual_mean",
                              add = "reg.line",
                              conf.int = TRUE,
                              #cor.coef = TRUE,
                              cor.method = "pearson",
                              na.rm = TRUE,
                              add.params = list(fill = "darkgrey")) +
  #ggpubr::stat_regline_equation(aes(label =  after_stat(rr.label)), label.y = 3.5) + # Adds R2
  labs(x="Year", y= "SAM index\n(annual mean)")

sam_plot

saveRDS(sam_plot, here("Figs", "SAM_year_reg_line.rds"))

# Simple LM
summary(lm(sam_index$annual_mean ~ sam_index$year))

# Year and SAM are correlated. Create detrended SAM variable (= residuals from the lm on SAM)
detrend <- function(x) resid(lm(x ~ seq(x)))

sam_index$detrended_annual_mean <- detrend(sam_index$annual_mean) # where you pass annual SAM values from the entire time series

# # Did detrending work? Yes
# ggpubr::ggscatter(sam_index, x = "year", y = "detrended_annual_mean",
#                   add = "reg.line",
#                   conf.int = TRUE,
#                   cor.coef = TRUE,
#                   cor.method = "pearson",
#                   na.rm = TRUE) +
#   ggpubr::stat_regline_equation(aes(label =  after_stat(rr.label)), label.y = 3)


# Antarctic fur seals are born mostly in Dec. I used the annual mean from the year of birth, 
# (eg mostly affecting prenatal lifestage and first month of life) rather than the 1st year of life 

gen_males <- gen_males %>%
  left_join(sam_index, by = c("PupBirthyear" = "year")) %>%
  rename(SAMBirthyear = annual_mean, dtSAMBirthyear = detrended_annual_mean) %>%
  left_join(sam_index, by = c("SamplingYear" ="year")) %>%
  rename(SAMSamplingYear = annual_mean, dtSAMSamplingYear = detrended_annual_mean) %>%
  mutate(dtSAMmismatch = abs(dtSAMSamplingYear - dtSAMBirthyear))

# n_males <- n_males %>%
#   mutate(year = SamplingYear - 1) %>%
#   left_join(sam_index, by = "year") %>%
#   rename(SAMSamplingYear = annual_mean, dtSAMSamplingYear = detrended_annual_mean) %>%
#   select(-year)

nrow(gen_males %>% filter(!is.na(uniqueID)) %>% distinct(uniqueID))
#3946 ( = 3840+106)


#~~ Save dataframe
# Breeding seasons are referred to by the year in which they ended
gen_males2 <- gen_males %>%
  mutate(PupBirthyear = PupBirthyear + 1) %>%
  mutate(SamplingYear = SamplingYear + 1) %>% 
  mutate(FirstRecaptureYear = FirstRecaptureYear + 1) %>%
  mutate(LastRecaptureYear = LastRecaptureYear + 1)

gen_males2 %>% filter(!is.na(uniqueID)) %>% distinct(uniqueID, .keep_all = T) %>% group_by(status) %>% tally()

gen_males2 %>% 
  mutate(status = ifelse(is.na(status) | status =="BeachDead", "NonRecruited", status)) %>%
  distinct(uniqueID, .keep_all = T) %>%
  group_by(status) %>% 
  tally()
# status           n
# <chr>        <int>
# 1 NonRecruited  3840
# 2 Recruited      106

openxlsx::write.xlsx(gen_males2, here("Data", "Processed", "males_sMLH_fitness.xlsx"))         
