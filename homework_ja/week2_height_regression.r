

# week 2 assignment fell over xmas break and is a biggish one, covering 
# material up to lecture 4 
# Data is Howell1 data set from !Kung census

## R code 4.7
library(rethinking)
data(Howell1)
d <- Howell1

# by exploring data discovered need to focus on adults only as before adult hood
# height is determined by age 
## see "prep_week2_explore_howell1.r" for details 

d2 <- d[ d$age >= 18 , ]


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
"""

 

# process to building a statistical model using probability ---------------------

# 1 Design the model (create a data story) 
# 2 Condition on the data (update)
# 3 Evaluate the model (critique)


# modelling height  --------------------------------------------------------------


# goal - model using a normal (gaussian distribution)

# initial model of height only 
#  height_i ~ Normal(mu, sigma)

# priors: 
#   mu ~ Normal(178, 20)   
#   sigma ~ Uniform(0, 50) 



# adding a predictor variable to the model --------------------------------

#  likelihood 
#       height_i ~ Normal(mu, sigma)

#  linear model 
#       mu_i = alpha + beta(x_i, - x_bar)

#  priors 
#       alpha ~ Normal(178, 20)
#       beta ~ Normal(0, 10)
#       sigma ~ Uniform(0, 50) 



"""

2. Model the relationship between height(cm) and the natural logarithm of weight (log-kg): 
log(weight). Use the entire Howell1 data frame, all 544 rows, adults and non-adults. 

Use any model type from Chapter 4 that you think useful: an ordinary linear 
regression, a polynomial or a spline. Plot the posterior predictions against 
the raw data.
"""







"""
3. Plot the prior predictive distribution for the polynomial regression model in Chapter 4. 
You can modify the the code that plots the linear regression prior predictive 
distribution. 20 or 30 parabolas from the prior should suf- fice to show where
the prior probability resides. Can you modify the prior distributions of α, 
β1, and β2 so that the prior predictions stay within the biologically reasonable
outcome space? That is to say: Do not try to fit the data by hand. But do try to
keep the curves consistent with what you know about height and weight, before 
seeing these exact data.
"""




