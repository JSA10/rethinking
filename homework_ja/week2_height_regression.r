

# The Questions -----------------------------------------------------------

"""
1. The weights listed below were recorded in the !Kung census, but heights were 
not recorded for these individuals. Provide predicted heights and 89% compatibility 
intervals for each of these individuals. That is, fill in the table below, 
using model-based predictions.

Individual   weight   expected height   89% interval 
1 45
2 40 
3 65 
4 31 
5 53

2. Modeltherelationshipbetweenheight(cm)andthenaturallogarithmof weight (log-kg): 
log(weight). Use the entire Howell1 data frame, all 544 rows, adults and non-adults. 

Use any model type from Chapter 4 that you think useful: an ordinary linear 
regression, a polynomial or a spline. Plot the posterior predictions against 
the raw data.

3. Plot the prior predictive distribution for the polynomial regression model in Chapter 4. 
You can modify the the code that plots the linear regression prior predictive 
distribution. 20 or 30 parabolas from the prior should suf- fice to show where
the prior probability resides. Can you modify the prior distributions of α, 
β1, and β2 so that the prior predictions stay within the biologically reasonable
outcome space? That is to say: Do not try to fit the data by hand. But do try to
keep the curves consistent with what you know about height and weight, before 
seeing these exact data.
"""

## R code 4.7
library(rethinking)
data(Howell1)
d <- Howell1


# EDA ---------------------------------------------------------------------


## R code 4.8
str( d )
# 4 variables - height, weight, age and male (1 = T, 0 = F)
# 544 observations to build predictions from 


## R code 4.9
precis( d )
# height looks normally distributed 
# weight looks like got 2 peaks 

par(mfrow=c(2,2))
purrr::map(d, hist)
# focus in and can see a big left skew in height, two peaks in weight and 
# age includes children and adults
# even amount of males and females

pairs(d)
# looks like two distinct groups with different rules: 
# 1) almost perfect linear relationship between age and height / weight when children 
# 2) adults need a different approach entirely 

## As Q is about predicting adults ages - will remove under 18s 
d2 <- d[ d$age >= 18 , ]


pairs(d2)
#install.packages("psych")
psych::pairs.panels(d2)

# age now has much less influence on height 
# height ~ weight = 0.75 correlation
# height ~ gender = 0.70 correlation
# height ~ age = -0.10 correlation (get slightly smaller as age)


# benchmark a standard lm model -----------------------------------------------

m <- lm(height ~ ., data = d2)
summary(m)
# Model explains about 69% of variation in height training data

m.1 <- lm(height ~ weight + male, data =d2)
summary(m.1)
# no meaningful improvement in model from dropping age

m.2 <-  lm(height ~ weight, data =d2)
summary(m.2)
# drop to 57% explained by losing gender from model 



# Predictions

test_q1 <- data.frame(height = 0, weight = c(45, 40, 65, 31, 33), age = 0, male = 0)

test_q1$expected_height_lm <- predict(m.2, test_q1)


# rethinking this approach ------------------------------------------------

# Using a Bayesian approach we spend a bit more time defining the model 

# height (yi) is normally distributed with mean (mu) and standard deviation (sigma) 
# yi ~ Normal(mu, sigma)

# In bayesian statistics all model parameters have a distribution 
# mu = Normal(mu_prior_mean, sigma_prior_mean)
# sigma ~ uniform(sigma_lower_bound, sigma_upper_bound)



par(mfrow = c(1,1))

# establishing a prior from experience 
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )



