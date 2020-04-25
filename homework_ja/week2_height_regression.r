

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


# modelling height alone --------------------------------------------------------------


# goal - model using a normal (gaussian distribution)

# initial model of height only 
#  height_i ~ Normal(mu, sigma)

# priors: 
#   mu ~ Normal(178, 20)   
#   sigma ~ Uniform(0, 50) 

# priors for height come from lecturer - his height

# adding a predictor variable to the model --------------------------------

#  likelihood 
#       height_i ~ Normal(mu, sigma)

#  linear model 
#       mu_i = alpha + beta(x_i, - x_bar)
# using difference between weight and the mean weight

#  priors 
#       alpha ~ Normal(178, 20)
#       beta ~ Normal(0, 10)
#       sigma ~ Uniform(0, 50) 

# explore the data
hist(d2$height)
hist(d2$weight)
plot(d2$height, d2$weight)

d2$weight_diff_mean <- d2$weight - mean(d2$weight, na.rm = TRUE)
hist(d2$weight_diff_mean)
plot(d2$height, d2$weight_diff_mean)
# review chapter as to why - doesn't look to be real difference 

# fairly normal looking data for target variable and looks to be a linear 
# relationship with weight 

# define the average weight, x-bar
xbar <- mean(d2$weight)

# define the model 
mh_w <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*( weight - xbar ) ,
        a ~ dnorm( 178 , 20 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=d2 )

# check model paramaters
precis(mh_w)

## check model covariance
round( vcov( mh_w ) , 3 )

# visually explore model posterior
post_mh_w <- extract.samples(mh_w, 1000)

post_mh_w <- extract.samples( mh_w )
post_mh_w[1:5,]

a_map <- mean(post_mh_w$a)
b_map <- mean(post_mh_w$b)
curve( a_map + b_map*(x - xbar) , add=TRUE )



# use link to look at predicted values
mu_pred <- link( mh_w )
str(mu_pred)

# take weights to make predictions from Q table 
weights_predict <- c(45, 40, 65, 31, 53)

# use link to compute mu for each sample from posterior and for each weight in table
mu_pred <- link( mh_w , data=data.frame(weight=weights_predict) )
str(mu_pred)

# summarize the distribution of mu
mu_mean <- apply( mu_pred , 2 , mean )

# note - used t to transpose the output    
mu_PI <- t(apply( mu_pred , 2 , PI , prob=0.89 ))

View(mu_mean)


# Q ANSWER = create table with predicted values for the 5 weights given
mu_summary <- bind_cols(data.frame(weights_predict), data.frame(mu_mean), data.frame(mu_PI))




## R code 4.57 - can als oplot summaries 

# plot raw data
# fading out points to make line and interval more visible
plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) )

# plot the MAP line, aka the mean mu for each weight
lines( weights_predict , mu_mean )

# plot a shaded region for 89% PI
shade( mu_PI , weights_predict )



# Q2  ---------------------------------------------------------------------



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




