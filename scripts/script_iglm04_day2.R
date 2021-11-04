library(tidyverse)

doctor_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm04/main/data/DoctorAUS.csv")

table(doctor_df$doctorco)

M9 <- glm(doctorco ~ sex + age,
          data = doctor_df,
          family = poisson(link = 'log')
)
estimates <- coef(M9)

# What is the rate (aka lambda, aka mean) of the Poisson distribution
# for a female aged 50?

# log of the rate for a female aged 50 is....
log_rate_female_50 <- estimates[1] + estimates[2] * 1 + estimates[3] * 0.5

# the rate for a female aged 50 is....
rate_female_50 <- exp(log_rate_female_50)

# draw samples from the Poisson dist whose rate is rate_female_50
rpois(1000, lambda = rate_female_50)
table(rpois(1000, lambda = rate_female_50))

# What is the rate (aka lambda, aka mean) of the Poisson distribution
# for a *male* aged 50?

# log of the rate for a male aged 50 is....
log_rate_male_50 <- estimates[1] + estimates[2] * 0 + estimates[3] * 0.5

# the rate for a male aged 50 is....
rate_male_50 <- exp(log_rate_male_50)

# draw samples from the Poisson dist whose rate is rate_male_50
rpois(1000, lambda = rate_male_50)
table(rpois(1000, lambda = rate_male_50))


log_rate_female_50 - log_rate_male_50

c(rate_female_50, rate_male_50)
rate_female_50 / rate_male_50

exp(estimates[2])


# inferential statistics --------------------------------------------------

summary(M9)
confint.default(M9)

# 95% CI on the factor by which the rate change as we change from men to women
exp(confint.default(M9, parm = 'sex'))


M10 <- glm(doctorco ~ sex + age + insurance,
           data = doctor_df,
           family = poisson(link = 'log')
)


deviance(M9)
deviance(M10)

deviance(M9) - deviance(M10)

pchisq(deviance(M9) - deviance(M10), df = 3, lower.tail = FALSE)

anova(M9, M10, test = 'Chisq')

# Exposure and offsets ----------------------------------------------------

insurance_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm04/main/data/Insurance.csv")


M11 <- glm(Claims ~ Age + offset(log(Holders)),
           data = insurance_df,
           family = poisson(link = 'log')
)

insurance_df_2 <- tibble(Age = unique(insurance_df$Age),
                         Holders = 1000)

predict(M11, newdata = insurance_df_2)

library(modelr)

add_predictions(insurance_df_2, M11)
add_predictions(insurance_df_2, M11, type = 'response')


# binomial count models ---------------------------------------------------

golf_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm04/main/data/golf_putts.csv")

M12 <- glm(cbind(success, attempts - success) ~ distance,
           data = golf_df,
           family = binomial(link = 'logit')
)

summary(M12)$coefficients
coef(M12)

golf_df_2 <- tibble(distance = seq(20))

add_predictions(golf_df_2, M12, var = 'log_odds_successful_putt')
add_predictions(golf_df_2, M12, type = 'response', var = 'probability_successful_putt')


add_predictions(golf_df_2, 
                M12, 
                type = 'response', 
                var = 'probability_successful_putt') %>% 
  ggplot(
    aes(x = distance, y = probability_successful_putt)
  ) + geom_line()

# Overdispersion ----------------------------------------------------------

# samples some data from a Poisson distribution
x <- rpois(1000, lambda = 100)
mean(x)/var(x)

biochem_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm04/main/data/biochemist.csv")

M13 <- glm(publications ~ 1,
           data = biochem_df,
           family = poisson(link = 'log')
)
summary(M13)$coefficients
exp(0.52)

pubs <- biochem_df$publications
c(mean(pubs), var(pubs), var(pubs)/mean(pubs))



M14 <- glm(publications ~ 1,
           data = biochem_df,
           family = quasipoisson(link = 'log')
)

summary(M14)$coefficients


# Negative binomial regression --------------------------------------------

library(MASS)

M15 <- glm.nb(publications ~ prestige,
              data = biochem_df)

summary(M15)
estimates <- coef(M15)

# predicted log of the mean of the negbin for values of 
# prestige from 1 to 5
estimates[1] + estimates[2] * seq(5)

# predicted mean of the negbin for values of 
# prestige from 1 to 5
exp(estimates[1] + estimates[2] * seq(5))
