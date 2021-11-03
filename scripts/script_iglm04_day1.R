library(tidyverse)

weight_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm04/main/data/weight.csv")

# linear regression model modelling weight as a function of height
M1 <- lm(weight ~ height, data = weight_df)
coef(M1)
sigma(M1)

summary(M1)
summary(M1)$coefficients

confint(M1)
confint(M1, parm = 'height')
confint(M1, parm = 'height', level = 0.9)

# model weight as a function of height and age 
M2 <- lm(weight ~ height + age, data = weight_df)
coef(M2)
sigma(M2)
summary(M2)$coefficients
confint(M2)


# binary logistic regression ----------------------------------------------

theta <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# logit or log odds of theta
odds <- theta / (1 - theta)
log(odds)

affairs_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm04/main/data/affairs.csv")

affairs_df <- mutate(affairs_df, had_affair = affairs > 0)


# binary logistic regression predicting 
# probability of having an affairs, as a function of 
# number of years married
M3 <- glm(had_affair ~ yearsmarried,
          data = affairs_df,
          family = binomial(link = 'logit')
)

coef(M3)
summary(M3)$coefficients

estimates <- coef(M3)

# what is the log odds of having an affair
# for a person with 10 years of marriage
estimates[1] + estimates[2] * 10

# what is the log odds of having an affair
# for a person with 11 years of marriage
estimates[1] + estimates[2] * 11

# let's make the logit and the ilogit functions
logit <- function(theta) log(theta/(1-theta))
ilogit <- function(phi) {1/(1 + exp(-phi))}


# what is the probability of having an affair
# for a person with 10 years of marriage
ilogit(estimates[1] + estimates[2] * 10)

# what is the probability of having an affair
# for a person with 20 years of marriage
ilogit(estimates[1] + estimates[2] * 20)

# what is the probability of having an affair
# for a person with 20 years of marriage
ilogit(estimates[1] + estimates[2] * 50)

new_df <- tibble(yearsmarried = seq(10))

predict(M3, newdata = new_df)
predict(M3, newdata = new_df, type = 'response')

library(modelr)

add_predictions(new_df, M3, var = 'predicted_log_odds')
add_predictions(new_df, M3, type = 'response', var = 'predicted_probabilities')

# odds ratio for the yearsmarried predictor
# the factor by which the odds increases for a unit change in yearsmarried
exp(estimates[2])

log_odds_10 <- estimates[1] + estimates[2] * 10
odds_10 <- exp(log_odds_10)

log_odds_11 <- estimates[1] + estimates[2] * 11
odds_11 <- exp(log_odds_11)

odds_11 / odds_10

exp(estimates[2])

summary(M3)$coefficients

confint.default(M3)

# 95% confidence interval on the odds ratio for `yearsmarried`
exp(confint.default(M3, parm = 'yearsmarried'))

summary(M3)

deviance(M3)
logLik(M3) * -2 

M4 <- glm(had_affair ~ 1,
          data = affairs_df,
          family = binomial(link = 'logit')
)
deviance(M4)

# difference of the deviance
deviance(M4) - deviance(M3)

1 - pchisq(deviance(M4) - deviance(M3), df = 1)
pchisq(deviance(M4) - deviance(M3), df = 1, lower.tail = FALSE)

# Alternatively
anova(M4, M3, test = 'Chisq')


M5 <- glm(had_affair ~ age + yearsmarried + religiousness + rating,
          data = affairs_df,
          family = binomial(link = 'logit')
)
deviance(M5)

M6 <- glm(had_affair ~ religiousness + rating,
          data = affairs_df,
          family = binomial(link = 'logit')
)
deviance(M6)

anova(M6, M5, test = 'Chisq')



# Ordinal logistic regression ---------------------------------------------

library(MASS)
library(pscl)

head(admit)

M7 <- polr(score ~ gre.quant, data = admit)
summary(M7)

admit_df_2 <- tibble(gre.quant = seq(300, 800, by = 100))

add_predictions(admit_df_2, M7, type = 'prob')

mu <- coef(M7)
M7$zeta

mu * 600

plogis(M7$zeta['1|2'], location = mu * 600)
plogis(M7$zeta['2|3'], location = mu * 600)


# Categorical logistic regression -----------------------------------------

library(nnet)

M8 <- multinom(score ~ gre.quant, data = admit)
summary(M8)

add_predictions(admit_df_2, M8, type = 'prob')
