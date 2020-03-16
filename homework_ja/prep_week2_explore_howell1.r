# explore howell1 data set for wk 2 assignment exercise 
# (bigger one over xmas break covering material up to lec 4)

# Some EDA to understand the data before getting into Q1---------------------------------------------------------------------


## R code 4.8
str( d )
# 4 variables - height, weight, age and male (1 = T, 0 = F)
# 544 observations to build predictions from 
# test data set in Q1 doesn't include age or gender 

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

# estimated coefficients from the model
coef(m.2)

# plot residuals against weight
plot( resid(m.2) ~ weight , data=d2 )

# Predictions

test_q1 <- data.frame(height = 0, weight = c(45, 40, 65, 31, 33), age = 0, male = 0)

test_q1$expected_height_lm <- predict(m.2, test_q1)


# Q1 rethinking this approach ------------------------------------------------

# Using a Bayesian approach we spend a bit more time defining the model 

# height (yi) is normally distributed with mean (mu) and standard deviation (sigma) 
# yi ~ Normal(mu, sigma)

# In bayesian statistics all model parameters have a distribution 
# mu = Normal(mu_prior_mean, sigma_prior_mean)
# sigma ~ uniform(sigma_lower_bound, sigma_upper_bound)



par(mfrow = c(1,1))

# establishing a prior from experience 
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )



