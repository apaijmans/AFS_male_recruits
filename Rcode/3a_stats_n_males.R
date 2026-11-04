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
library(MASS) # For glm.nb
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

complete_males <- n_males #%>% drop_na() # nothing gets dropped



#~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Year and SAM model  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~ Negative binomial GLM including year and SAM (during the study period no significant trend between SAM and year)
mYear <- glm.nb(TotalMales ~ scale(SamplingYear) + scale(SAMSamplingYear), 
                data = complete_males)

#~~ Model assumptions
testDispersion(mYear)
plotQQunif(mYear)
plotResiduals(mYear) 
# Looks OK, last plot could be better

summary(mYear)


#~~ Try if GAM is a better fit
gam.year <- gam(TotalMales ~ s(scale(SamplingYear)) + s(scale(SAMSamplingYear)),  
                family = nb(),
                data = complete_males)

gam_theta <- gam.year$family$getTheta(TRUE)

gam.year <- gam(TotalMales ~ s(scale(SamplingYear)) + s(scale(SAMSamplingYear)), 
                family = negbin(gam_theta),
                data = complete_males)

gam.check(gam.year)
# A bit more difficult to assess, 
# but QQ plot looks OK, as well as the histogram and residual plots

summary(gam.year)


#~~ Compare GLM and GAM against empirical data
par(mfrow=c(1,2))

pred.glm <- predict(mYear, type = "link", se.fit = TRUE)

glm_df <- unique(data.frame(
  SamplingYear = complete_males$SamplingYear,
  fit   = exp(pred.glm$fit),
  lower = exp(pred.glm$fit - 1.96 * pred.glm$se.fit),
  upper = exp(pred.glm$fit + 1.96 * pred.glm$se.fit)
))
glm_df <- glm_df[order(glm_df$SamplingYear), ]

plot(complete_males$SamplingYear, complete_males$TotalMales,
     xlab = "Year", 
     ylab = "Male count",
     main = "Model predictions GLM")
lines(glm_df$SamplingYear, glm_df$fit, lty = 1)
lines(glm_df$SamplingYear, glm_df$lower, lty = 2)
lines(glm_df$SamplingYear, glm_df$upper, lty = 2)


pred.gam <- predict(gam.year, type = "link", se.fit = TRUE)

gam_df <- data.frame(
  SamplingYear = complete_males$SamplingYear,
  fit   = exp(pred.gam$fit),
  lower = exp(pred.gam$fit - 1.96 * pred.gam$se.fit),
  upper = exp(pred.gam$fit + 1.96 * pred.gam$se.fit)
)
gam_df <- unique(gam_df)                       # one row per year
gam_df <- gam_df[order(gam_df$SamplingYear), ]

plot(complete_males$SamplingYear, complete_males$TotalMales,
     xlab = "Year", ylab = "Male count",
     main = "Model predictions GAM")
lines(gam_df$SamplingYear, gam_df$fit, lty = 1)
lines(gam_df$SamplingYear, gam_df$lower, lty = 2)
lines(gam_df$SamplingYear, gam_df$upper, lty = 2)

dev.off()


#~~ Very similar, stick with GLM
final_model <- mYear



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Year and SAM Model: forest plot  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# sjPlot::plot_model(final_model)

plot_label <- c(
  `(Intercept)` = "Intercept",
  `scale(SamplingYear)` = "Year",
  `scale(SAMSamplingYear)` = "SAM")

order_terms <- c(1, 2)

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

saveRDS(forest_plot, here("Figs", "Forest_plot_n_males_year_SAM.rds"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Year and SAM Model: table model output  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


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
                file = here("Tables", "Table_male_counts_model_year_SAM.html")))

#~~ Turn saved htlm table into a docx using pandoc
# NB pandoc can be downloaded here: https://github.com/jgm/pandoc/releases
pandoc  <- normalizePath(here("Tools", "pandoc.exe"))
input   <- normalizePath(here("Tables", "Table_male_counts_model_year_SAM.html"))
output  <- normalizePath(here("Tables", "Table_male_counts_model_year_SAM.docx"), mustWork = FALSE) # file doesn't exist yet, therefore mustWork = F

cmd <- sprintf('"%s" "%s" -o "%s"', pandoc, input, output)
#cat(cmd)
system(cmd)
# NB 0 is exit code for success


# Back-transform to get percentage change per year
b <- coef(final_model)["scale(SamplingYear)"]
s <- sd(complete_males$SamplingYear)   # same data the model was fitted to
ci <- confint(final_model)["scale(SamplingYear)", ]   # profile CI for glm.nb

span <- diff(range(complete_males$SamplingYear))   # 26 (1995-2021)

# Whole period: IRR, % change, and CI
exp(span * b / s)
(exp(span * b / s) - 1) * 100 # -65% over 26 years
(exp(span * ci / s) - 1) * 100

# Per decade % change, with CI
(exp(10 * b / s) - 1) * 100       # about -33% per decade
(exp(10 * ci / s) - 1) * 100

# Per year % change, with CI
(exp(b / s) - 1) * 100       # about -4% per year
(exp(ci / s) - 1) * 100      # 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Raw data plot with model estimates  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~ Generate df with model estimates for year, SAM held at its median
yr_mean <- mean(scale(complete_males$SamplingYear))
yr_sd   <- sd(scale(complete_males$SamplingYear))
trend_df <- data.frame(
  SamplingYear_unscaled = seq(min(complete_males$SamplingYear),
                     max(complete_males$SamplingYear), length.out = 200)
)
trend_df$SamplingYear <- (trend_df$SamplingYear_unscaled - yr_mean) / yr_sd
trend_df$SAMSamplingYear  <- median(scale(complete_males$SAMSamplingYear))

p <- predict(final_model, newdata = trend_df, type = "link", se.fit = TRUE)
trend_df$fit   <- exp(p$fit)
trend_df$lower <- exp(p$fit - 1.96 * p$se.fit)
trend_df$upper <- exp(p$fit + 1.96 * p$se.fit)

ggplot(complete_males, aes(SamplingYear, TotalMales)) +
  geom_point() +
  geom_ribbon(data = trend_df,
              aes(x = SamplingYear_unscaled, ymin = lower, ymax = upper),
              fill = "darkgrey", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = trend_df, aes(x = SamplingYear, y = fit),
            linewidth = 1.5, inherit.aes = FALSE) 

#~~ Annual counts of all (observed only+observed and sampled) males (fig 1c)
male_plot <- ggpubr::ggscatter(complete_males, x = "SamplingYear", y = "TotalMales") + #,
                               #add = "reg.line",
                               #conf.int = TRUE,
                               #cor.coef = TRUE,
                               #cor.method = "pearson",
                               #na.rm = TRUE,
                               #add.params = list(fill = "darkgrey")) + # Customize reg. line) +
  #ggpubr::stat_regline_equation(aes(label =  after_stat(rr.label)), label.y = 20) + # Adds R2
  #labs(x="Year", y= "Male counts")
  
  # # Line showing full model prediction for each actual year incl that year's SAM
  # geom_line(data = glm_df, aes(x = SamplingYear, y = fit), linewidth = 1.2) +
  # geom_ribbon(data = glm_df, 
  #             aes(ymin = lower, ymax = upper, x = SamplingYear),
  #             fill = "darkgrey", 
  #             alpha = 0.4, 
  #             inherit.aes = FALSE
  #             )
  
  # Line showing year trend with SAM held at its median
  geom_line(data = trend_df, aes(x = SamplingYear, y = fit),
            linewidth = 1.2 ) + #, inherit.aes = FALSE) +
  geom_ribbon(data = trend_df,
              aes(ymin = lower, ymax = upper, x = SamplingYear_unscaled),
              fill = "darkgrey", alpha = 0.4, inherit.aes = FALSE)
   
male_plot

saveRDS(male_plot, here("Figs", "n_obs_males_per_year.rds"))
