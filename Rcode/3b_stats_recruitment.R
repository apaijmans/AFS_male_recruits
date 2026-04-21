# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 3b_stats_recruitment.R
#
# Purpose: This script runs the GLMs used to test the effect of 
# various predictors on male recruitment. Since SAM and year are strongly 
# correlated, two models were made:
# - one testing the effect of year on male recruitment ("temporal model") and 
# - one testing the effect of SAM and other predictors on male recruitment ("full model").
# It also tests whether there is an cohort effect on recruitment age
# before and after 2009.
# This script creates tables 2a and b (separately, later combined in the 
# MS) and all panels for fig 2.
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


#~~ Filter for year
pups39 %>% 
  #filter(Status == 1) %>%
  summarize(pre2013 = sum(PupBirthyear < 2013),
            up2013 = sum(PupBirthyear >= 2013))
# 2301 pups < 2013
# 1562 pups >= 2013

pups11 <- pups39 %>% filter(PupBirthyear < 2013)

pups11 %>% 
  group_by(Status) %>% 
  tally()
# Status     n
# <dbl> <int>
# 1      0  2195
# 2      1   106

# Filter for year but nothing else
gen_males %>%
  filter(!grepl("^AGM", PupTissueID)) %>%
  distinct(uniqueID, .keep_all = T) %>% 
  filter(PupBirthyear < 2013) %>% 
  group_by(Status) %>% 
  tally()

# # A tibble: 2 x 2
# Status     n
# 1      0  2273
# 2      1   106

(106/2273) *100
# Recruitment rate: 4.7% (4.66)


#~~ Check for collinearity
melted_cors <- reshape::melt(pups11 %>% 
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


#~~ Cleaned df
pups11.complete <- pups11 %>%
  select(Status, sMLH_39msat, PupBirthyear, PupWeight, SAMBirthyear, uniqueID, status) %>%
  drop_na

pups11.complete %>% group_by(Status) %>% tally()
# Status     n
# 1      0  1680
# 2      1    87



#~~~~~~~~~~~~~~~~~~~~~#
#  Temporal model  ####
#~~~~~~~~~~~~~~~~~~~~~#


#~~ Model testing temporal effect male recruitment success
m1 <- glm(Status ~ scale(PupBirthyear),
          family=binomial, 
          data = as.data.frame(pups11.complete))


#~~ Model assumptions
testDispersion(m1)
plotQQunif(m1)
plotResiduals(m1) 
# All look good

summary(m1)

temp_model <- m1



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Temporal model: table 2a  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#tab_model(temp_model)


#~~ Table 2a
print(tab_model(temp_model,
                pred.labels = c(`(Intercept)` = "Intercept", `scale(PupBirthyear)` = "Year"),
                dv.labels = "Male recruitment",
                #transform = NULL, #transform = NULL gives Log-Odds, transformed gives Odds
                show.stat=T, 
                string.stat = "z value",
                file = here("Tables", "Table_recruitment_temporal_model.html")))


#~~ Turn saved htlm table into a docx using pandoc
# NB pandoc can be downloaded here: https://github.com/jgm/pandoc/releases
pandoc  <- normalizePath(here("Tools", "pandoc.exe"))
input   <- normalizePath(here("Tables", "Table_recruitment_temporal_model.html"))
output  <- normalizePath(here("Tables", "Table_recruitment_temporal_model.docx"), mustWork = FALSE) # file doesn't exist yet, therefore mustWork = F

cmd <- sprintf('"%s" "%s" -o "%s"', pandoc, input, output)
#cat(cmd)
system(cmd)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Temporal model: model predictions year incl raw data (fig 2c)  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# predict() function to calculate predicted values from the model. Remember that to
# use the predict() function, you need to provide a model and an empty data frame that has starter
# values each of the predictors in your model
# You have a multivariate regression, so you need to vary one variable and hold others at their mean (ie), 
# this is called marginal effect.

# Use ggeffects to calculate predicted values as well as confidence intervals

# Calculate total number and percentage of recruits for inclusion in figure
n_recs <- pups11.complete %>% 
  select(uniqueID, PupBirthyear, status) %>%
  distinct(uniqueID, .keep_all = T) %>%
  group_by(PupBirthyear, status) %>%
  tally() %>%
  mutate(PercentRecruits = n/sum(n))


#~~ Recruitment per birth year
predict_response(temp_model, "PupBirthyear")

gge_year <- as.data.frame(ggpredict(temp_model, "PupBirthyear"))

pred_year <- left_join(pups11.complete, gge_year, by = c("PupBirthyear" = "x"))

pred_year %>%
  distinct(PupBirthyear, predicted) %>%
  arrange(PupBirthyear)

pred_year <- left_join(pred_year, n_recs)

pred_year %>% group_by(n) %>% tally()


#~~ Plot model predictions for recruitment probability per year (fig 2c)
p.BY <- ggplot(pred_year, aes(x = PupBirthyear)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, x = PupBirthyear),
              fill = "darkgrey", alpha = 0.4) +
  geom_line(aes(y = predicted), linewidth = 1) +
  geom_point(aes(y = Status/6, size = n, fill = factor(Status), color = factor(Status)), shape=21) + 
  labs(x = "Birth year", size="Number of\nindividuals") +
  scale_y_continuous(
    # Features of the first axis
    name = "Recruitment probability",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . * 6, breaks=c(0, 1), labels=c("Non recruits", "Recruits"))) +
  theme_minimal() +
  scale_size(breaks = c(5, 15, 50, 100))

p.BY
#ggsave(here("Figs", "Recruitment_birthyear_model_predicitions_n.jpg"), height = 4, width = 4)

saveRDS(p.BY, here("Figs", "Mod_pred_status_birthyear_temporal_model.rds"))


#~~ Calculate odds for change of recruitment
exp(temp_model$coefficient)

# When the odds value is smaller than 1, we have to take the inverse value (i.e. 1 divided by the odds) to facilitate interpretation.
1/exp(temp_model$coefficient["scale(PupBirthyear)"])

# We can also subtract 1 from the odds value to obtain a percentage:
( (1/exp(temp_model$coefficient["scale(PupBirthyear)"])) - 1 ) * 100
# So there is a 59% decrease in probability of recruitment with a one-unit increase in birth year (58.98618) 



#~~~~~~~~~~~~~~~~~#
#  Full model  ####
#~~~~~~~~~~~~~~~~~#


#~~ Full model environmental and genetic factors driving male recruitment success
m1 <- glm(Status ~ scale(sMLH_39msat) * scale(SAMBirthyear) # Test whether under bad SAM, heterozygous individuals are more likely to recruit
          + scale(PupWeight) * scale(SAMBirthyear), # Test whether pups that are born heavier in a bad year are more likely to recruit
          family=binomial, 
          data = as.data.frame(pups11.complete))

#~~ Model assumptions
testDispersion(m1)
plotQQunif(m1)
plotResiduals(m1) 
# All look good

summary(m1)

final_model <- m1



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Full model: forest plot (fig 2d)  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#plot_model(final_model)

gglayer_theme <- list(
  geom_point(shape = 22, size = 4),
  theme_minimal(),
  theme(axis.line.x = element_line(colour = 'black', linetype='solid'),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        axis.text.y = element_text(colour = 'black'),
        plot.title = element_text(size = rel(1)))
)

# Color non sig effects , 
col1 = "#C9E8FF"
# Color sig effects
col2 = "#B0925B" #"#ea4f88"  # "#872ca2"

plot_label <- c(
  `(Intercept)` = "Intercept",
  `scale(sMLH_39msat)` = "sMLH",
  `scale(PupWeight)` = "Birth mass",
  `scale(SAMBirthyear)` = "SAM\nbirth year",
  `scale(sMLH_39msat):scale(SAMBirthyear)` = "sMLH *\nSAM\nbirth year",
  `scale(SAMBirthyear):scale(PupWeight)` = "Birth mass *\nSAM\nbirth year")

order_terms <- c(2, 3, 5, 1, 4)

forest_data <- as.data.frame(exp(cbind(final_model$coefficients, confint(final_model)))) %>% 
  mutate(Predictor = row.names(.)) %>%
  select(Predictor, OddsR = V1, CI_low = "2.5 %", CI_high = "97.5 %") %>%
  mutate(sig = ifelse(CI_low <= 1 & CI_high >= 1, "no", "yes"))

#forest_data$sig[4] <- "yes" # birth year is borderline significant, p value is 0.049 something but CI is JUST overlapping 1.

forest_plot <- ggplot(forest_data[-1,], aes(OddsR, Predictor, fill = sig)) +
  geom_vline(xintercept = 1) +
  geom_linerange(aes(xmin = CI_low, xmax = CI_high), colour = "black") + 
  geom_point() +
  scale_y_discrete(limits = rev(forest_data$Predictor[-1][order_terms]), labels = plot_label) +
  scale_fill_manual(values = c("no" = col1, "yes" = col2)) +
  labs(x = "Odds Ratios", y = "") +
  gglayer_theme

forest_plot

# ggsave(here("Figs", "Defence_Forest_plot_recruitment_SAM.jpg"), height = 3, width = 4)

saveRDS(forest_plot, here("Figs", "Forest_plot_recruitment_39loci_SAM.rds"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Full model: table model output (table 2b)  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#tab_model(final_model)

order_terms <- c(1, order_terms + 1)


#~~ Table 2b
print(tab_model(final_model,
                pred.labels = plot_label,
                #title = "Recruitment males",
                dv.labels = "Male recruitment",
                order.terms = order_terms,
                #transform = NULL, #transform = NULL gives Log-Odds, transformed gives Odds
                show.stat=T, 
                string.stat = "z value",
                file = here("Tables", "Table_recruitment_model_SAM.html")))


#~~ Turn saved htlm table into a docx using pandoc
pandoc  <- normalizePath(here("Tools", "pandoc.exe"))
input   <- normalizePath(here("Tables", "Table_recruitment_model_SAM.html"))
output  <- normalizePath(here("Tables", "Table_recruitment_model_SAM.docx"), mustWork = FALSE)

cmd <- sprintf('"%s" "%s" -o "%s"', pandoc, input, output)
#cat(cmd)
system(cmd)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Full model: model predictions SAM incl raw data (fig 2e)  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Calculate total number and percentage of recruits for inclusion in figure
# n_recs <- pups11.complete %>% 
#   select(uniqueID, PupBirthyear, status) %>%
#   distinct(uniqueID, .keep_all = T) %>%
#   group_by(PupBirthyear, status) %>%
#   tally() %>%
#   mutate(PercentRecruits = n/sum(n))

# already done above


#~~ Recruitment vs SAM
gge_SAM <- as.data.frame(ggpredict(final_model, "SAMBirthyear"))

# sprintf("%.16f", gge_SAM$x)
# sprintf("%.16f", pups11.complete$dtSAMBirthyear)
# For some weird reason, two values for dtSAM are slightly different compared to the dataframe...
# Rounding to 3 decimals to make sure they merge properly

gge_SAM <- gge_SAM %>% mutate(x = round(x, digits = 3))

pred_SAM <- left_join(pups11.complete %>% mutate(SAMBirthyear = round(SAMBirthyear, digits = 3)), gge_SAM, by = c("SAMBirthyear" = "x"))

pred_SAM %>%
  distinct(SAMBirthyear, predicted) %>%
  arrange(SAMBirthyear)

pred_SAM <- left_join(pred_SAM, n_recs)

pred_SAM %>% group_by(SAMBirthyear) %>% tally()

pred_SAM %>% group_by(n) %>% tally()

# View(pred_SAM %>%
#   distinct(dtSAMBirthyear, predicted, status, n) %>%
#   arrange(dtSAMBirthyear))


#~~ Plot model predictions for recruitment probability per SAM (fig 2e)
p.SAM <- ggplot(pred_SAM, aes(x = SAMBirthyear)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, x = SAMBirthyear),
              fill = "darkgrey", alpha = 0.4) +
  geom_line(aes(y = predicted), linewidth = 1) +
  geom_point(aes(y = Status/7, size = n, fill = factor(Status), color = factor(Status)), shape=21) + 
  labs(x = "mean SAM index in birth year", size="Number of individuals") +
  scale_y_continuous(
    # Features of the first axis
    name = "Recruitment probability",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . * 7, breaks=c(0, 1), labels=c("Non recruits", "Recruits"))) +
  theme_minimal() +
  scale_size(breaks = c(5, 15, 50, 100)) #+ 
#guides(size="none", color = "none")

p.SAM

#ggsave(here("Figs", "Recruitment_dtSAMbirthyear_model_predicitions.jpg"), height = 4, width = 4)

saveRDS(p.SAM, here("Figs", "Mod_pred_status_SAM.rds"))


#~~ Percentage of change
# Calculate odds for change of recruitment
exp(final_model$coefficient)

# When the odds value is smaller than 1, we have to take the inverse value (i.e. 1 divided by the odds) to facilitate interpretation.
#1/exp(final_model$coefficient["scale(PupBirthyear)"])
1/exp(final_model$coefficient["scale(SAMBirthyear)"])

# This means that a one-unit increase in SAM decreases the likelihood of recruitment by 1.57. 

# We can also subtract 1 from the odds value to obtain a percentage:
( (1/exp(final_model$coefficient["scale(SAMBirthyear)"])) - 1 ) * 100
# So there is a 56% decrease in probability of recruitment with a one-unit increase in SAM index (56.50738)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Cohort effect recruitment age  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Including all recruits, regardless of missing fitness data such as birth mass
age_males <- gen_males %>% filter(!is.na(FirstRecaptureAge)) %>%
  distinct(uniqueID, FirstRecaptureAge, PupBirthyear, FirstRecaptureYear)

# Recruitment year and birth year are highly correlated:
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
# not sig


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

saveRDS(bar_rec_all_years_unfiltered, here("Figs", "barplot_males_per_year_39loci_all_years_unfiltered.rds"))


#~~ Age at recruitment (fig 2b)
#(add dummy column to allow saving and editing figure colors post hoc in "4_figs.R")

# Here we can include all recruits, regardless of missing fitness data such as birth mass
p.age <- ggplot(pups11 %>% filter(status == "Recruited") %>% mutate(dummy = "1"), 
                aes(x = FirstRecaptureAge, fill = dummy, color = dummy)) +
  geom_histogram(binwidth = 1) +
  xlab("Age at first recapture") +
  ylab("Count") +
  theme_minimal() 

p.age

saveRDS(p.age, here("Figs", "age_first_recapture.rds"))


#~~ Recruitment vs birth mass (fig 2f)
# In this figure the filtered data was used (same data as for model)
beeswarm_mass <- ggplot(pups11.complete, aes(x=status, y=PupWeight)) +
  ggbeeswarm::geom_quasirandom(size = 2, aes(color = status, fill = status), shape=21) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA, fill = "darkgrey") 

beeswarm_mass

saveRDS(beeswarm_mass, here("Figs", "boxplot_status_mass_beeswarm.rds"))


#~~ Recruitment vs sMLH (fig 2g)
# In this figure the filtered data was used (same data as for model)
beeswarm_sMLH <- ggplot(pups11.complete, aes(x=status, y=sMLH_39msat)) +
  ggbeeswarm::geom_quasirandom(size = 2, aes(color = status, fill = status), shape=21) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA, fill = "darkgrey") 

beeswarm_sMLH

saveRDS(beeswarm_sMLH, here("Figs", "boxplot_status_sMLH_beeswarm.rds"))
