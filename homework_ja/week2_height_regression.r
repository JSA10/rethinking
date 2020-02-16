
## R code 4.7
library(rethinking)
data(Howell1)
d <- Howell1


# EDA ---------------------------------------------------------------------


## R code 4.8
str( d )
# 4 variables - height, weight, age and male (1 = T, 0 = F)

## R code 4.9
precis( d )
# height looks normally distributed 
# weight looks like got 2 peaks 


"""
jeromes_hist_function <- function(d, col) {
    hist(d$col)
}
jeromes_hist_function(d, height)
"""

par(mfrow=c(2,2))
hist(d$height)
hist(d$weight)
hist(d$age)
hist(d$male)

pairs(d)

install.packages("psych")
library(psych)
pairs.panels(d)


# fitting standard lm model -----------------------------------------------



m <- lm(height ~ ., data =d)
summary(m)

m.1 <- lm(height ~ weight + age, data =d)
summary(m.1)

test_weight_q1 <- c(45, 40, 65, 31, 33)

test_q1 <- data.frame(height = 0, weight = c(45, 40, 65, 31, 33), age = 0, male = 0)

m_height <- lm(height ~ weight, data = d)

test_q1$expected_height_lm <- predict(m, test_q1)

# correlation relationships between height and weeight and age to a certain extent 
# age looks to have two groups - children is pretty linear, but explodes out once 
# gets to adulthood 

curve( exp( -x^2 ) , from=-3 , to=3 )
dnorm(0,0,0.1)



