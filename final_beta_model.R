#### Understanding the Beta regression ###


# Installing packages  ----------------------------------------------------

install.packages("betareg")
install.packages("psych")

## Loading packages 

library(car)
library(betareg)
library(psych)
library(tidyverse)
library(readxl)

# Checking multicolinearity  ----------------------------------------------

municipalities_filtered <- municipalities %>%
  filter(year >= 2010 & year <= 2016)
municipalities_filtered <- municipalities_filtered %>%
  filter(!is.na(HDI_index_FIRJAN))

municipalities_filtered$d_enrollments_high_school_full <- ifelse(municipalities_filtered$enrollments_high_school_full != 0, 1, 0)

lm_model <- lm(
  early_pregnancy_rt ~ d_enrollments_high_school_full + gdp_pc + failures_high_school + dropouts_high_school + approvals_high_school + HDI_index_FIRJAN
  + proportion_male,
  data = municipalities_filtered
)

alias(lm_model)

## Approvals in High School is causing trouble in the model because is correlated with both failures and dropouts. Let's remove it. 

lm_model_adjusted <- lm(
  early_pregnancy_rt ~ d_enrollments_high_school_full + gdp_pc + failures_high_school + dropouts_high_school + HDI_index_FIRJAN
  + proportion_male,
  data = municipalities_filtered
)

## calculating Variance Inflation Factor (VIF)

vif_values_adjusted <- vif(lm_model_adjusted)
print(vif_values_adjusted)

## All values are under 10, indicating that the variables are not correlated between each other.


# running the model -------------------------------------------------------

psych::describe(municipalities_filtered$early_pregnancy_rt, quant = c(.25, .75))
hist(municipalities_filtered$early_pregnancy_rt)

## code for the model

## running with the dummy

beta_model <- betareg(
  early_pregnancy_rt ~ d_enrollments_high_school_full + gdp_pc + failures_high_school + dropouts_high_school + HDI_index_FIRJAN
  + proportion_male | municipality + year,
  link = "logit", data = municipalities_filtered
)
summary(beta_model)

## running without the dummy

beta_model_2 <- betareg(
  early_pregnancy_rt ~ enrollments_high_school_full + gdp_pc + failures_high_school + dropouts_high_school + HDI_index_FIRJAN
  + proportion_male | municipality + year,
  link = "logit", data = municipalities_filtered
)

summary(beta_model_2)


# conclusions -------------------------------------------------------------

## The dummy is negative correlated and significant at 5%. 
## GDP is still positive correlated and significant at 5%.  
## Failures is significant at 10%, but the sign is not what we expect.
## Dropouts is very significant, but the sign is not what we expect. 
## HDI is very significant and higher the HDI, lower the early pregnancy rate.
## Proportion of males is very significant and, higher the proportion of males, 
## higher the early pregnancy rate. 


## We added the proportion of men per municipality as a control.
## Theory: the proportion of man is correlated with gdp, since man are more representative in the labor market.
## We theorize that could have a mediation between proportion of man, gdp and early pregnancy rate, which
## could be explaining the weird relationship: higher gdp pc is likely to increase early pregnancy rate. 
## For the Amazon population, man are more likely to have STD's such as HIV. Since Amazon society is very traditional,
## man and women can be very traditional, which could lead to higher levels of contraception refusal or female submission.
## Moreover, Amazon has high rates of sexual violence. Where there are more man, is more lively to have
## more sexual violence, hence, more early pregnancies. 


# References --------------------------------------------------------------

## https://repositorio.ufpa.br/handle/2011/15307 > socioeconomic profile of HIV incidence cases in the Amazon context:
## men (66.46%), mixed race (76.95%), urban residents (85.20%) and,
## heterosexuals (54.41%).

## female's submission: https://www.scielo.br/j/reben/a/6DXdX9h9wG58cB5qNtgwhrp/?format=pdf&lang=pt


# Checando o ajuste do modelo ---------------------------------------------

# beta_model

psych::describe(beta_model$fitted.values, quant = c(.25, .75))
psych::describe(municipalities_filtered$early_pregnancy_rt, quant = c(.25, .75))
psych::describe(beta_model$residuals, quant = c(.25, .75))

qqPlot(beta_model$residuals)

## meu resíduo está bem ajustado.

# beta_model_2

psych::describe(beta_model_2$fitted.values, quant = c(.25, .75))
psych::describe(municipalities_filtered$early_pregnancy_rt, quant = c(.25, .75))
psych::describe(beta_model_2$residuals, quant = c(.25, .75))

qqPlot(beta_model_2$residuals)



