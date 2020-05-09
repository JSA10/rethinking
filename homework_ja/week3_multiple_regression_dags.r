
library(rethinking)
library(tidyverse)

data(foxes)
summary(foxes)

# 116 obs of 5 variables
# 
# 116 foxes 
# group = category with 30 levels - 30 gangs of foxes
# avg_food = numeric for average food in gangs territory (area)
# groupsize = integer range - gangs range btw 2 and 8 foxes
# area = numeric for size of territory 
# weight = numeric *target variable* 

# update from answers 
# - Looking at the DAG - as there are no back door paths from area to weight, 
# -- > we only need to include area in the model.

# Q1 - simple linear regression Q->Q ---------------------------------------

"""
1. Use a model to infer the total causal influence of area on weight. Would 
increasing the area available to each fox make it heavier (healthier)? 

You might want to standardize the variables. 
Regardless, use prior predictive simulation to show that your model’s prior 
predictions stay within the possible outcome range
"""

# to predict = weight - numeric / Q
# predictor = area available to a fox (it's groups size of territory) - numeric / Q

# standardise both variables 
foxes$scaled_weight <- scale(foxes$weight)
foxes$scaled_area <- scale(foxes$area)
# note - rethinking package has a standardize function. 
#foxes$W <- standardize(foxes$weight)
#all.equal(foxes$W, foxes$scaled_weight)
# only difference seems to be the format of output 
# scale returns a numeric vector 
# standardize a matrix 

hist(foxes$weight)
# normal dist 
hist(scale(foxes$weight))

hist(foxes$area)
hist(scale(foxes$area))

plot(foxes$weight ~ foxes$area)
plot(foxes$scaled_weight ~ foxes$scaled_area)
pairs(scale(foxes))
# don't look to be too many visually strong linear relationships with weight 
# some positive correlations btw group and avgfood and avgfood and area

purrr::map(foxes, sd)
purrr::map(foxes, summary)

m_hw3.1 <- quap(
    alist(
        scaled_weight ~ dnorm( mu , sigma ) ,
        mu <- a + b*scaled_area,
        a ~ dnorm( 0 , 1 ) ,
        b ~ dnorm( 0 , 1 ) ,
        sigma ~ dexp(1 )
    ), data = foxes )

# Thought - can we be more focused with the priors std.dev?

# prior predictive simulation
# extract the prior 
set.seed(10)
prior_hw3.1 <- extract.prior(m_hw3.1)
precis(prior_hw3.1)

# create a custom axis
xseq <- c(-3,3)
# use link to extract prior predictions (lots of lines for different alpha and beta values)
mu_hw3.1 <- link( m_hw3.1, post = prior_hw3.1 , data=list(scaled_area = xseq) )
# construct custom plot
plot( NULL , xlim=xseq , ylim=xseq )
# iterate through 50 rows, plotting lines using alpha and beta paramaters on each row
for ( i in 1:50 ) lines( xseq , mu_hw3.1[i,] , col=col.alpha("black",0.3) )

"""
These lines are crazy. As in previous examples, we can do better by both 
tightening the α prior so that it sticks closer to zero. With two standardized 
variables, when predictor is zero, the expected value of the outcome should also
be zero. And the slope βN needs to be a bit tighter as well, so that it doesn’t 
regularly produce impossibly strong relationships. Here’s an attempt:
"""
x_bar <- mean(foxes$scaled_area)

m_hw3.1 <- quap(
    alist(
        scaled_weight ~ dnorm(mu, sigma) ,
        mu <- a + b*(scaled_area),
        a ~ dnorm(0 , 0.2),
        b ~ dnorm(0 , 0.5),
        sigma ~ dexp(1)
    ), data = foxes )

# in the main these lines stay within the possible outcome range
# 
# Don't have great intuition for why these priors are suitable, beyond the fact
# that as a benefit of standardising variables, it's easier to pick standard
# priors and that the expected value of the outcome should be zero when the 
# predictor is also zero

hist(rnorm(1000, 0, 0.2))
hist(rnorm(1000, 0, 0.5))


# Describe the posterior 

precis(m_hw3.1)
# doesn't appear to be a strong or precise association with area controlled and fox weight
# - evidence for lack of strength in association = (b mean close to 0) 
# - evidence for lack of precision = (sd > 3x the posterior mean)  

round( vcov( m_hw3.1 ) , 3 )
# very little covariance amongst the paramaters

# marginal posteriors and covariance
pairs(m_hw3.1)

## TERRITORY seems to have no total causal influence on weight, at least not in 
## this sample 
 
# Q2 ----------------------------------------------------------------------

"""
2. Now infer the causal impact of adding food to a territory. Would this make 
foxes heavier? Which covariates do you need to adjust for to estimate the total
causal influence of food?
"""
str(foxes)
# adopt naming strategy used by mcelreath in book 
foxes$A <- standardize(foxes$area)
foxes$AF <- standardize(foxes$avgfood)

foxes <- foxes %>% 
    dplyr::select(-c(scaled_area, scaled_weight))
pairs(foxes)
# positive correlation between area and avg food 
# neither seem to be correlated with fox weight on their own

m_hw3.2 <- quap(
    alist(
        W ~ dnorm(mu, sigma) ,
        mu <- a + bA*A + bAF*AF,
        a ~ dnorm(0 , 0.2),
        bA ~ dnorm(0 , 0.5),
        bAF ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = foxes )

precis(m_hw3.2)
# looks to be a bigger link between area now that avgfood is in the model 
# avg food and area have almost identical, but opposite impacts on weight, 
# avg food has a negative relationship with weight and area a positive one 
plot(precis(m_hw3.2))
round( vcov( m_hw3.2 ) , 3 )
# looks like some negative covariance between bA and bAF

pairs(m_hw3.2)
# negative covariance of -0.85 and quite a clear downward slope when visualised


# Q2 - answer -------------------------------------------------------------

"""
To infer the causal influence of avgfood on weight,we need to close any back-door paths. 

There are no back-door paths in the DAG. So again, just use a model with a 
single predictor. 

If you include groupsize, to block the 1 indirect path, then you won’t get the 
total causal influence of food. You’ll just get the direct influence. 

But I asked for the effect of adding food, and that would mean through all forward paths.
"""

m_hw3.2 <- quap(
    alist(
        W ~ dnorm( mu , sigma ),
        mu <- a + bAF*AF,
        a ~ dnorm(0,0.2),
        bAF ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ), data = foxes )

precis(m_hw3.2)

"""
Again nothing. Adding food does not change weight. This shouldn’t surprise you, 
if the DAG is correct, because area is upstream of avgfood.
"""
# ... spent a little more time understanding dags and strategies for 
# the four types from the lectures, from memory these are: 
# 
# the fork X <- Z -> Y ... if condition on Z, X and Y are independent 
# the pipe X -> Z -> Y ... if conditon on Z, block path between X and Y
# the collider X -> Z <- Y ... no association between X and Y until condition on Z 
# the descendent X -> Z - > Y  & Z -> K ... Conditioning on K is like conditioning 
#   on Z, but weaker. Can have descendents in pipes and colliders (+ forks?)

# in the fork and the pipe, conditioning on Z, blocks the path between X and Y 
# while with colliders and descendents, conditioning on Z opens the path between X and Y

# Conditioning = bringing the variable into the model (if available)

 
# also - go back and understand benefits and risks of multiple-regression: 
# benefits chpt5: 
#   nullify spuriours correlations 
#   reveal masked relationships 
# risks chpt6: 
#   multicollinearity 
#   collider bias
#   confounding 

# in this dataset there are two paths 
#   area -> avgfood -> weight is a pipe
#   area -> avgfood -> groupsize -> weight is another, longer pipe 


# Q3  ---------------------------------------------------------------------

"""
3. Now infer the causal impact of groupsize. Which covariates do you need to 
adjust for? 

Looking at the posterior distribution of the resulting model, what do you think 
explains these data? 

That is, can you explain the estimates for all three problems? How do they go together?
"""

# Using same strategy as solution to Q2, to estimate causal influence of a 
# predictor in a DAG with no back door paths, we just need to include a 
# single predictor - the one in question. 
# 
# However, in this case there is an open path A - > AF -> W, so we 

foxes$GS <- standardize(foxes$groupsize)

m_hw3.3 <- quap(
    alist(
        W ~ dnorm( mu , sigma ),
        mu <- a + bAF*AF + bGS*GS,
        a ~ dnorm(0,0.2),
        bAF ~ dnorm(0,0.5),
        bGS ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ), data = foxes )

precis(m_hw3.3)
# looks to be bigger impact on weight when condition on food and group size together 

# food has a positive influence on weight, controlling for group size 
# groupsize has a negative association with weight, controlling for food

# note from solutions - can use c() to group variables with the same prior:
# ...
# c(bAF, bGS) ~ dnorm(0, 0.5),
# ...

# conclusion extended in solution:
"""
... So the causal influence of group size is to reduce weight—less food for each
fox. And the direct causal influence of food is positive, of course. 

But the total causal influence of food is still nothing, since it causes larger groups. 
This is a masking effect, like in the milk energy example. 

But the causal explanation here is that more foxes move into a territory until 
the food available to each is no better than the food in a neighboring territory. 

Every territory ends up equally good/bad on average. This is known in behavioral 
ecology as an ideal free distribution.
"""


