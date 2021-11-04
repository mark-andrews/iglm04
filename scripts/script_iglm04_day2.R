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
