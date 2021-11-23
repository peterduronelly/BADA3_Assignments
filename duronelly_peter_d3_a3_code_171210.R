cat("/f")
rm(list=ls())

library(gmodels)
library(erer)
library(descr)
library(arm)
library(readr)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(DataCombine)
library(stargazer)
library(mfx)
library(mlogit)
library(data.table)
library(scales)


setwd("C:/Users/peter/OneDrive/FOLDERS/CEU/Data Analysis 3/assignments/a 3")

## READ DATA

share <- read.csv("C:/Users/peter/OneDrive/FOLDERS/CEU/Data Analysis 3/data repo/mortality_oldage_eu.csv", na.strings = ".")

share <- subset(share, age>=50 & age<=80)
share <- subset(share, eduyears_mod!="." & income10g!="." & sports != ".")


share <- data.table(share)
share[, sportsfrequency := cut(sports, c(0,1,2,3,4), 
                               labels = c("more then once a week", 
                                          "once a week", "one to three times per month", "4" = "hardly ever"))]

ggplot(data = share, aes(sportsfrequency)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + theme_bw() + 
  labs(title = "Physical activies", x = "frequency of doing sports", 
       y = "frequency in the population") + scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = rel(1)))

## FINDING PATTERNS USING CROSSTABLE

CrossTable(share$deceased, share$sports, prop.chisq = FALSE, format = "SAS", chisq = TRUE)

# Chi-square refuses H0: independence of deceased & sports at any reasonable level.

## CREATE DUMMIES FOR FREQUENCY OF SPORTS ACTIVITIES

share$sports_regularly <- as.numeric(share$sports == 1)
share$sports_once_a_week <- as.numeric(share$sports == 2)
share$sports_rarely <- as.numeric(share$sports == 3)

# Deleting NAs for the dummies.
share <- share[complete.cases(share[ , 48:50]), ]

## LPM ON SPORTS

lpm_sport1 <- lm(deceased ~ sports_regularly + sports_once_a_week + sports_rarely, data = share)
lmp_sport1_coefftest <- coeftest(lpm_sport1, vcov = sandwich)

stargazer(lpm_sport1, type = "html", style = "all", single.row = T, 
          title = "Linear probability model 1", align = TRUE, out = "lpm_sport1.html")
stargazer(lmp_sport1_coefftest, type = "html", style = "all", single.row = T, 
          title = "Linear probability model 1 coefficient test", align = TRUE, out = "lmp_sport1_coefftest.html")

## ADDITIONAL VARIABLES

ggplot(data = share, aes(x=eduyears_mod, y=deceased)) +
  geom_smooth(method="loess", colour="black") + theme_bw() + 
  labs(title = "Mortality rate and education", x = "years spent on education", 
       y = "mortality rate") +
  theme(plot.title = element_text(size = rel(1)))

# Seems to be of a cubic relationship. Once you complete elementary, additional education matters, 
#     up till college.

ggplot(data = share, aes(x=income10g, y=deceased)) +
  geom_smooth(method="loess", colour="black") + theme_bw() + 
  labs(title = "Mortality rate and income level", x = "income decile", 
       y = "mortality rate") +
  theme(plot.title = element_text(size = rel(1)))

# One inflection point points to quadratic relationship. 

# Testing for relationship between sports vs income and education. 

chisq.test(share$sports, share$income10g, correct = FALSE)
chisq.test(share$sports, share$eduyears_mod, correct = FALSE)

# Plot education years distribution in the sample 

ggplot(data = share, aes(eduyears_mod)) + geom_histogram(bins = 20) + theme_bw() + 
  labs(title = "Sample distribution regarding education ", x = "Years spent on education", 
       y = "frequency") +
  theme(plot.title = element_text(size = rel(1)))

# LPM including education and income. 

lpm_sport2 <- lm(deceased ~ sports_regularly + sports_once_a_week + sports_rarely + 
                   + eduyears_mod + poly(income10g, 2), data = share)
lmp_sport2_coefftest <- coeftest(lpm_sport2, vcov = sandwich)

stargazer(lpm_sport2, type = "html", style = "all", single.row = T, 
          title = "Linear probability model 2", align = TRUE, out = "lpm_sport2.html")
stargazer(lmp_sport2_coefftest, type = "html", style = "all", single.row = T, 
          title = "Linear probability model 2 coefficient test", align = TRUE, out = "lmp_sport2_coefftest.html")
share$lpmpredict <- predict(lpm_sport2)

# Logit on frequency of sports

logit_1_coeffs <- glm(deceased ~ sports_regularly + sports_once_a_week + sports_rarely, data=share, family='binomial',  x = TRUE)
logit_1_marg <- logitmfx(formula = deceased ~ sports_regularly + sports_once_a_week + sports_rarely, data=share, atmean=FALSE)

marg <- maBina(logit_1_coeffs, x.mean = TRUE, rev.dum = TRUE, digits = 2)

stargazer(lpm_sport1, logit_1_coeffs, type = "html", style = "all", single.row = T, 
          title = "Probability models 1", align = TRUE, out = "probmodels1.html")
stargazer(lpm_sport1, marg, type = "html",  style = "all", single.row = T, 
          title = "Probability models 1", align = TRUE, out = "probmodel_marginal.html")

logit_2_coeffs <- glm(deceased ~ sports_regularly + sports_once_a_week + sports_rarely + 
                        + eduyears_mod + poly(income10g, 2), 
                      data=share, family='binomial')

logit_2_marg <- logitmfx(formula = deceased ~ sports_regularly + sports_once_a_week + 
                           sports_rarely + eduyears_mod + 
                           poly(income10g, 2), data=share, atmean=FALSE)

share$logitpredict <- predict(logit_2_coeffs, type = "response")

stargazer(lpm_sport2, logit_2_coeffs, type = "html", style = "all", single.row = T, 
          title = "Probability models 2", align = TRUE, out = "probmodels2.html")


ggplot(data = share, aes(x=lpmpredict, y=logitpredict)) +
  geom_point(colour="navy") + theme_bw() + 
  geom_abline(intercept = 0, slope = 1, size = 2) +
  labs(title = "Comparing model probabilities", x = "LPM probabilities", 
       y = "logit probabilities") + xlim(c(0, 0.16)) + ylim(c(0, 0.16)) +
  theme(plot.title = element_text(size = rel(1)))

print(logit_2_marg)

