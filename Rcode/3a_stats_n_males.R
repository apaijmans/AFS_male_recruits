# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 3a_stats_n_males.R
#
# Purpose: This script runs the GLMs used to test the effect of 
# year and SAM on male counts. Since SAM and year are strongly 
# correlated, two models were made:
# - one testing the effect of year on male counts and 
# - one testing the effect of SAM on male counts.
# This script creates tables 1a and b (separately, 
# later combined in the MS).
#
# -----------------------------------------------------------

library(here)
library(readxl)
library(tidyverse)
library(MASS) # For GAM
library(mgcv) # For GAM
library(DHARMa)
library(sjPlot)
library(ggeffects)
library(ggpubr)


# Prep theme and cols for figures
gglayer_theme <- list(
  geom_point(shape = 22, size = 4),
  theme_minimal(),
  theme(axis.line.x = element_line(colour = 'black', linetype='solid'),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        axis.text.y = element_text(colour = 'black'),
        plot.title = element_text(size = rel(1)))
)

# Color non sig effects 
col1 = "#C9E8FF"
# Color sig effects
col2 = "#B0925B"


#~~ Load data
n_males <- readxl::read_excel(here("Data", "processed", "n_observed_males.xlsx"))

str(n_males)

n_males <- n_males %>%
  rename(TotalMales = n)

complete_males <- n_males %>% drop_na() # nothing gets dropped



#~~~~~~~~~~~~~~~~~#
#  Year model  ####
#~~~~~~~~~~~~~~~~~#


#~~ Negative binomial GLM only including year
mYear <- glm.nb(TotalMales ~ scale(SamplingYear), 
                data = complete_males)

#~~ Model assumptions
testDispersion(mYear)
plotQQunif(mYear)
plotResiduals(mYear) 
# Looks OK

summary(mYear)


#~~ Try if GAM is a better fit
gam.year <- gam(TotalMales ~ s(scale(SamplingYear)),  
                family = nb(),
                data = complete_males)

gam.year$family$getTheta(TRUE)

gam.year <- gam(TotalMales ~ s(scale(SamplingYear)), 
                family = negbin(18),
                data = complete_males)

gam.check(gam.year)
# A bit more difficult to assess, 
# but QQ plot looks OK, as well as the histogram and residual plots

summary(gam.year)


#~~ Compare GLM and GAM against empirical data
par(mfrow=c(1,2))

pred.glm <- predict(mYear, se = TRUE, type = "response")
plot(complete_males$SamplingYear, complete_males$TotalMales, 
     xlab = "Year",
     ylab = "Male count",
     main = "Model predictions GLM")
I1 <- order(complete_males$SamplingYear)
lines(complete_males$SamplingYear[I1], pred.glm$fit[I1], lty = 1)
lines(complete_males$SamplingYear[I1], pred.glm$fit[I1] + 2 * pred.glm$se[I1], lty = 2)
lines(complete_males$SamplingYear[I1], pred.glm$fit[I1] - 2 * pred.glm$se[I1], lty = 2)


pred.gam <- predict(gam.year, se = TRUE, type = "response")
plot(complete_males$SamplingYear, complete_males$TotalMales, 
     xlab = "Year",
     ylab = "Male count",
     main = "Model predictions GAM")
I1 <- order(complete_males$SamplingYear)
lines(complete_males$SamplingYear[I1], pred.gam$fit[I1], lty = 1)
lines(complete_males$SamplingYear[I1], pred.gam$fit[I1] + 2 * pred.gam$se[I1], lty = 2)
lines(complete_males$SamplingYear[I1], pred.gam$fit[I1] - 2 * pred.gam$se[I1], lty = 2)

dev.off()


#~~ Very similar, stick with GLM
final_model <- mYear



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Year Model: forest plot  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# sjPlot::plot_model(final_model)

plot_label <- c(
  `(Intercept)` = "Intercept",
  `scale(SamplingYear)` = "Year",
  `scale(SAMSamplingYear)` = "SAM")

order_terms <- c(1)

forest_data <- as.data.frame(exp(cbind(final_model$coefficients, confint(final_model)))) %>% 
  mutate(Predictor = row.names(.)) %>%
  dplyr::select(Predictor, IRR = V1, CI_low = "2.5 %", CI_high = "97.5 %") %>%
  mutate(sig = ifelse(CI_low < 1 & CI_high > 1, "no", "yes"))

forest_plot <- ggplot(forest_data[-1,], aes(IRR, Predictor, fill = sig)) +
  geom_vline(xintercept = 1) +
  geom_linerange(aes(xmin = CI_low, xmax = CI_high), colour = "black") + 
  geom_point() +
  scale_y_discrete(limits = rev(forest_data$Predictor[-1][order_terms]), labels = plot_label) +
  scale_fill_manual(values = c("no" = col1, "yes" = col2)) +
  labs(x = "Incidence Rate Ratios", y = "") +
  gglayer_theme

forest_plot

#ggsave(here("Figs", "Forest_plot_n_males.jpg"), as_ggplot(p1), height = 4, width = 5)

saveRDS(forest_plot, here("Figs", "Forest_plot_n_males_year.rds"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Year Model: table model output  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~ Table
print(tab_model(final_model,
                pred.labels = plot_label,
                #title = "Male counts",
                dv.labels = "Male counts",
                # dv.labels = c("(a) model incl. maternal effect",
                #               "(b) model excl. maternal effect"),
                #order.terms = c(1, 2, 4, 3, 7, 5, 6),
                #transform = NULL, # if you don't transform it shows Log-mean, if you do it shows IRRs (which are in the effect size plot)
                show.stat=T,
                string.stat = "z value",
                file = here("Tables", "Table_male_counts_model_year.html")))

#~~ Turn saved htlm table into a docx using pandoc
# NB pandoc can be downloaded here: https://github.com/jgm/pandoc/releases
pandoc  <- normalizePath(here("Tools", "pandoc.exe"))
input   <- normalizePath(here("Tables", "Table_male_counts_model_year.html"))
output  <- normalizePath(here("Tables", "Table_male_counts_model_year.docx"), mustWork = FALSE) # file doesn't exist yet, therefore mustWork = F

cmd <- sprintf('"%s" "%s" -o "%s"', pandoc, input, output)
#cat(cmd)
system(cmd)
# NB 0 is exit code for success



#~~~~~~~~~~~~~~~~#
#  SAM model  ####
#~~~~~~~~~~~~~~~~#


#~~ Negative binomial GLM only including SAM
mSAM <- glm.nb(TotalMales ~ scale(SAMSamplingYear), 
               data = complete_males)

#~~ Model assumptions
testDispersion(mSAM)
plotQQunif(mSAM)
plotResiduals(mSAM) 
# Looks OK, qq plot is not great but there are also not many observations in the raw data

summary(mSAM)

#~~ Test whether a GAM is a better fit
gam.sam <- gam(TotalMales ~ s(scale(SAMSamplingYear)),  
               family = nb(),
               data = complete_males)

gam.sam$family$getTheta(TRUE)

gam.sam <- gam(TotalMales ~ s(scale(SAMSamplingYear)), 
               family = negbin(6),
               data = complete_males)

gam.check(gam.sam)
# A bit more difficult to assess, but QQ plot looks OK-ish, histogram does not look very well...

summary(gam.sam)


#~~ Compare GLM and GAM against empirical data
par(mfrow=c(1,2))

pred.glm <- predict(mSAM, se = TRUE, type = "response")
plot(complete_males$SAMSamplingYear, complete_males$TotalMales, 
     xlab = "SAM",
     ylab = "Male count",
     main = "Model predictions GLM")
I1 <- order(complete_males$SAMSamplingYear)
lines(complete_males$SAMSamplingYear[I1], pred.glm$fit[I1], lty = 1)
lines(complete_males$SAMSamplingYear[I1], pred.glm$fit[I1] + 2 * pred.glm$se[I1], lty = 2)
lines(complete_males$SAMSamplingYear[I1], pred.glm$fit[I1] - 2 * pred.glm$se[I1], lty = 2)


pred.gam <- predict(gam.sam, se = TRUE, type = "response")
plot(complete_males$SAMSamplingYear, complete_males$TotalMales, 
     xlab = "SAM",
     ylab = "Male count",
     main = "Model predictions GAM")
I1 <- order(complete_males$SAMSamplingYear)
lines(complete_males$SAMSamplingYear[I1], pred.gam$fit[I1], lty = 1)
lines(complete_males$SAMSamplingYear[I1], pred.gam$fit[I1] + 2 * pred.gam$se[I1], lty = 2)
lines(complete_males$SAMSamplingYear[I1], pred.gam$fit[I1] - 2 * pred.gam$se[I1], lty = 2)

dev.off()


#~~ Very similar, stick with GLM
final_model <- mSAM



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  SAM Model: forest plot  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# plot_model(final_model)

plot_label <- c(
  `(Intercept)` = "Intercept",
  `scale(SamplingYear)` = "Year",
  `scale(SAMSamplingYear)` = "SAM")

order_terms <- c(1)

forest_data <- as.data.frame(exp(cbind(final_model$coefficients, confint(final_model)))) %>% 
  mutate(Predictor = row.names(.)) %>%
  dplyr::select(Predictor, IRR = V1, CI_low = "2.5 %", CI_high = "97.5 %") %>%
  mutate(sig = ifelse(CI_low < 1 & CI_high > 1, "no", "yes"))

forest_plot <- ggplot(forest_data[-1,], aes(IRR, Predictor, fill = sig)) +
  geom_vline(xintercept = 1) +
  geom_linerange(aes(xmin = CI_low, xmax = CI_high), colour = "black") + 
  geom_point() +
  scale_y_discrete(limits = rev(forest_data$Predictor[-1][order_terms]), labels = plot_label) +
  scale_fill_manual(values = c("no" = col1, "yes" = col2)) +
  labs(x = "Incidence Rate Ratios", y = "") +
  gglayer_theme

forest_plot

#ggsave(here("Figs", "Forest_plot_n_males.jpg"), as_ggplot(p1), height = 4, width = 5)

saveRDS(forest_plot, here("Figs", "Forest_plot_n_males_SAM.rds"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  SAM Model: table model output  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~ Table
print(tab_model(final_model,
                pred.labels = plot_label,
                #title = "Male counts",
                dv.labels = "Male counts",
                # dv.labels = c("(a) model incl. maternal effect",
                #               "(b) model excl. maternal effect"),
                #order.terms = c(1, 2, 4, 3, 7, 5, 6),
                #transform = NULL, # if you don't transform it shows Log-mean, if you do it shows IRRs (which are in the effect size plot)
                show.stat=T,
                string.stat = "z value",
                file = here("Tables", "Table_male_counts_model_SAM.html")))


#~~ Turn saved htlm table into a docx using pandoc
pandoc  <- normalizePath(here("Tools", "pandoc.exe"))
input   <- normalizePath(here("Tables", "Table_male_counts_model_SAM.html"))
output  <- normalizePath(here("Tables", "Table_male_counts_model_SAM.docx"), mustWork = FALSE) # file doesn't exist yet, therefore mustWork = F

cmd <- sprintf('"%s" "%s" -o "%s"', pandoc, input, output)
#cat(cmd)
system(cmd)


