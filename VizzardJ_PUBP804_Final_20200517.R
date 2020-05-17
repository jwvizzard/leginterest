# James W. Vizzard
# PUBP 804
# Final Project
# 2020.05.17

# Clear the global environment
rm(list=ls())

# Load required libraries
library(ggplot2)
library(aod)
library(dplyr)
library(vcd)
library(httr)
library(RCurl)
library(COUNT)
library(pscl)
library(boot)
library(MASS)
library(ggpubr)

# Open and combine the members data frame with the distances data frame
members <- read.csv("~/Documents/Drive/James/201801-20YYMM_PhD/C_PUBP804_MultivariateStatistics/data/804_members_116.csv")
View(members)
distances <- read.csv("~/Documents/Drive/James/201801-20YYMM_PhD/C_PUBP804_MultivariateStatistics/data/116_mil_distancematrix_4.csv")
View(distances)
mil_amends <- merge(members, distances,by="district_code")
View(mil_amends)
# Export merged data frame
write.csv(mil_amends, "~/Documents/Drive/James/201801-20YYMM_PhD/C_PUBP804_MultivariateStatistics/data/mil_amends.csv")

# Rows contain 442 observations (congressional districts) in ANSI/FIPS code order
"district_code"=row.names(mil_amends)
mil_amends

# columns contain 21 variables
names(mil_amends)

# describe the data
#Means
summarise(mil_amends, Average = mean(amend_count_116_2500, na.rm = T))
summarise(mil_amends, Average = mean(distance_km, na.rm = T))

# Histogram of dependent variable
hist(mil_amends$amend_count_116_2500, breaks = "scott",
     main = "Histogram of Amendments Offered",
     xlab = "Amendments Offered")


# boxplot and scatterplot
box_data_amends <- ggplot(data = mil_amends, 
                          mapping = aes(x = distance_km, 
                                        y = amend_count_116_2500))

box_data_amends + geom_jitter(position = position_jitter(width=0.01)) +
  labs(x = "Distance to Mil Installation", y = "Amendments")

box_data_amends + geom_boxplot(aes(group = amend_count_116_2500)) +
  labs(x = "Distance to Mil Installation", y = "Amendments")

box_data_amends + geom_point(aes(x = distance_km, y = amend_count_116_2500)) +
  labs(x = "Distance to Mil Installation", y = "Amendments")

# Poisson models (single binary predictor)
x <- mil_amends$oversight
y <- mil_amends$amend_count_116_2500
amend_model1 <- glm(y ~ x, family = poisson(link = "log"))
summary(amend_model1)

# Exponentiate the coefficients
exp(amend_model1$coeff)

# Compute the rate for amendments offered for oversight committee members v. non-members
mu0 <- mean(y[x==0])
mu1 <- mean(y[x==1])
c(mu0, mu1, mu1/mu0)

# Poisson models (multiple predictors)
amend_model2 <- glm(amend_count_116_2500 ~ oversight + distance_km, family = poisson(link = "log"), data = mil_amends)
summary(amend_model2)

# Compute Pearson's R2
pr <- sum(residuals(amend_model2, type = "pearson") ^ 2)
pr / amend_model2$df.resid
amend_model2$aic / (amend_model2$df.null + 1)

COUNT::modelfit(amend_model2)

# Exponentiate the coefficients and confidence intervals
exp(coef(amend_model2))

# delta method for calculating the standard error
exp(coef(amend_model2)) * sqrt(diag(vcov(amend_model2)))
exp(confint.default(amend_model2))

# Computer marginal effect at mean
beta <- coef(amend_model2)
mover <- mean(mil_amends$oversight)
mdist <- mean(mil_amends$distance_km)
xb <- sum(beta * c(1,mover,mdist))
dfdxb <- exp(xb) * beta[3]
dfdxb

#average marginal effect
mean(mil_amends$amend_count_116_2500) * beta[3]


# Test for overdispersion/model fit
dev <- deviance(amend_model1)
df <- df.residual(amend_model1)
p_value <- 1 - pchisq(dev, df)
print(matrix(c("Deviance GOF ", " ",
               "D =", round(dev,4) ,
               "df = ", df,
               "P-value = ", p_value), byrow = T,ncol = 2))

dev <- deviance(amend_model2)
df <- df.residual(amend_model2)
p_value <- 1 - pchisq(dev, df)
print(matrix(c("Deviance GOF ", " ",
               "D =", round(dev,4) ,
               "df = ", df,
               "P-value = ", p_value), byrow = T,ncol = 2))

# Zero-Inflated Negative Binomial Model
# z_model1 <- zeroinfl(formula = amend_count_116_2500 ~ distance_km | oversight,
#                     data = mil_amends, dist = "negbin", EM = TRUE)

# z_model1(formula = count ~ child + camper | persons, data = zinb, 
         ##     dist = "negbin", EM = TRUE)

# ANOVA models
set.seed(1234, kind = NULL)
dplyr::sample_n(mil_amends, 50)

# Create categorical distance variable
mil_amends$distance_cat <- cut(mil_amends$distance_km, seq(0, 4, 1), right = FALSE, labels = c(1:4))
levels(mil_amends$distance_cat)

group_by(mil_amends, distance_cat) %>%
  summarize(
    count = n(),
    mean = mean(distance_km),
    sd = sd(distance_km)
  )

# Visualize the data
box_data_amends <- ggplot(data = mil_amends, 
                          mapping = aes(x = distance_cat, 
                                        y = amend_count_116_2500))
box_data_amends + geom_boxplot(aes(x = distance_cat, y = amend_count_116_2500)) +
  labs(x = "Distance to Mil Installation", y = "Amendments")

# Compute 1-way ANOVAs
res.aov <- aov(amend_count_116_2500 ~ distance_cat, data = mil_amends)
summary(res.aov)

oversight.aov <- aov(amend_dummy ~ oversight, data = mil_amends)
summary(oversight.aov)

dist.aov <- aov(amend_dummy ~ distance_cat, data = mil_amends)
summary(dist.aov)

#Linear regression models
ols1 <- lm(amend_count_116_2500 ~ distance_km + hasc_mem + hacd_mem + hasc_chair + hasc_rank + hacd_chair + hacd_rank, data = mil_amends)
summary(ols1) # show results

# Logit models
# Does being a member of an oversight committee affect log odds of submitting an amendment?
logit1 <- glm(amend_dummy ~ oversight, data = mil_amends)
summary(logit1)

# Does proximity to military installations affect log odds of serving on an oversight committee?
logit2 <- glm(oversight ~ distance_km, data = mil_amends)
summary(logit2)

# Does proximity to military installations affect log odds of submitting amendments?
logit3 <- glm(amend_dummy ~ distance_km, data = mil_amends)
summary(logit3)

# Combined model for distance, oversight membership, and committee leadership
logit4 <- glm(amend_dummy ~ distance_km + oversight + hasc_chair + hacd_chair + hasc_rank + hacd_rank, data = mil_amends)
summary (logit4)
