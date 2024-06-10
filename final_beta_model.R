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
municipalities_filtered$d_female_enrollments_full <- ifelse(municipalities_filtered$female_enrollments_full != 0, 1, 0)


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

## running with the dummy (enrollments in high school full)

beta_model <- betareg(
  early_pregnancy_rt ~ d_enrollments_high_school_full + gdp_pc + failures_high_school + dropouts_high_school + HDI_index_FIRJAN
  + proportion_male | municipality + year,
  link = "logit", data = municipalities_filtered
)

summary(beta_model)

## The dummy is significant at 1%. The sign is expected from literature. 
## Municipalities with enrollments in full time education might experience lower 
## early pregnancy rates, compared with municipalities that don't have. 
## GDP is significant at 1%. The sign is against literature. 
## Higher GDP's are associated with higher pregnancy rates. 
## Failures are significant at 10%. The sign is against literature.
## Failing high school is likely to decrease early pregnancy rate. 
## Dropouts is significant at 0.1% - too significant. The sign is against literature. 
## Dropping out high school is likely to decrease early pregnancy rate. 
## HDI Index is significant at 0.1% - too significant. The sign is expected. 
## Higher HDI are associated with lower early pregnancy rates. 
## Proportion of males is significant at 0.1%. The sign is expected. 
## Higher the proportion of males in the municipality, higher the early pregnancy rate. 

## running without the dummy

beta_model_2 <- betareg(
  early_pregnancy_rt ~ enrollments_high_school_full + gdp_pc + failures_high_school + dropouts_high_school + HDI_index_FIRJAN
  + proportion_male | municipality + year,
  link = "logit", data = municipalities_filtered
)

summary(beta_model_2)

## The enrollments variable is significant at 0.1%. 
## Being enrolled in full time education is associated with lower pregnancy rates.
## GDP is significant at 5%. The sign is against literature. 
## Higher GDP's are associated with higher pregnancy rates. 
## Failures are not significant. Sign is expected from literature. 
## Failing high school is likely to increase early pregnancy rate. 
## Dropouts is significant at 5%. The sign is against literature. 
## Dropping out high school is likely to decrease early pregnancy rate. 
## HDI Index is significant at 5%. The sign is expected. 
## Higher HDI are associated with lower early pregnancy rates. 
## Proportion of males is significant at 0.1%. The sign is expected. 
## Higher the proportion of males in the municipality, higher the early pregnancy rate.


# other models ------------------------------------------------------------

## dummy enrollments of females

beta_model_3 <- betareg(
  early_pregnancy_rt ~ d_female_enrollments_full + gdp_pc + failures_high_school + dropouts_high_school + HDI_index_FIRJAN
  + proportion_male | municipality + year,
  link = "logit", data = municipalities_filtered
)

summary(beta_model_3)

## the dummy is not significant. 
## GDP is significant at 0.1%. 
## Failures is significant at 10%. 
## Dropouts is significant at 0.1%.
## HDI is significant at 0.1%.
# Proportion of males is significant at 0.1%. 

## This model seems biased. 

## without the dummy

beta_model_4 <- betareg(
  early_pregnancy_rt ~ female_enrollments_full + gdp_pc + failures_high_school + dropouts_high_school + HDI_index_FIRJAN
  + proportion_male | municipality + year,
  link = "logit", data = municipalities_filtered
)

summary(beta_model_4)

## The enrollments variable is significant at 0.1%. 
## Municipalities with females enrolled in full time education might experience lower 
## early pregnancy rates, compared with municipalities without. 
## GDP is significant at 5%. The sign is against literature. 
## Higher GDP's are associated with higher pregnancy rates. 
## Failures are not significant. Sign is expected from literature. 
## Failing high school is likely to increase early pregnancy rate. 
## Dropouts is significant at 5%. The sign is against literature. 
## Dropping out high school is likely to decrease early pregnancy rate. 
## HDI Index is significant at 5%. The sign is expected. 
## Higher HDI are associated with lower early pregnancy rates. 
## Proportion of males is significant at 0.1%. The sign is expected. 
## Higher the proportion of males in the municipality, higher the early pregnancy rate.

## binary FTS variable

beta_model_5 <- betareg(
  early_pregnancy_rt ~ existence_FTS + gdp_pc + failures_high_school + dropouts_high_school + HDI_index_FIRJAN
  + proportion_male | municipality + year,
  link = "logit", data = municipalities_filtered
)

summary(beta_model_5)

## The FTS variable is significant at 5%. 
## Municipalities with a full time school are more likely to have lower early pregnancy rates,
## compared to municipalities that don't have them. 
## GDP is significant at 10%. The sign is against literature. 
## Higher GDP's are associated with higher pregnancy rates. 
## Failures are significant at 5%. Sign is not expected from literature. 
## Failing high school is likely to decrease early pregnancy rate. 
## Dropouts is significant at 0.1%. The sign is against literature. 
## Dropping out high school is likely to decrease early pregnancy rate. 
## HDI Index is significant at 0.1%. The sign is expected. 
## Higher HDI are associated with lower early pregnancy rates. 
## Proportion of males is significant at 0.1%. The sign is expected. 
## Higher the proportion of males in the municipality, higher the early pregnancy rate.


# conclusions -------------------------------------------------------------



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



