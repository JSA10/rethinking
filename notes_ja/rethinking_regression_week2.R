
# rethinking regression ---------------------------------------------------

## R code 4.7
library(rethinking)
data(Howell1)
d <- Howell1

#rethinking EDA

dens(d$height)

# in this case distribution looks gaussian (normal), commone with height data 
# which is often the result of lots of small factors 

# ** NOTE: Eye-balling a plot to check normality isn't always a good idea:
# 1 - some normal indicators aren't visible 
# 2 - just because a variable doesn't look normal, it doesn't mean you can't use the model
# MORE ON 4.3 page 83


# DEFINE the model  -------------------------------------------------------



# h1 ~ Normal(mu, sigma)

## a note on i.i.d 
## -- assumption made in the small world of the model 
## -- not a statement about outside world as on reflection it would be very hard to 
## be sure that every point having an identical and independent probability distribution 


# add some PRIORS to complete the model 

# paramaters are mu and sigma so we need a prior Pr(mu, sigma) 
# = joint probability for all parameters 
"""
In most cases, priors are specified independently for each parameter, 
which amounts to assuming Pr(μ, σ) = Pr(μ) Pr(σ)
"""

# likelihood prior --> hi ∼ Normal(μ, σ)
# mean prior --> μ ∼ Normal(178, 20) 
# variance prior --> σ ∼ Uniform(0, 50)

# ** NOTE: the mean prior comes from richards own height and a plausible range 
# he selected from experience - 95% within +-40 of the mean (138 - 218 cm)

# in domains where there isn't such a clear physical understanding of the target 
# variable - finding a good prior will require more subtle work 


# always PLOT the priors to check how their assumption affects your model 

curve(dnorm(x, 178, 20), from = 100, to = 250)
# golem assuming avg height (not individual) almost certainly btw 140 and 220
# -> this prior carries a little info, but not a lot 

#The σ prior is a truly flat prior, a uniform one, that functions just to 
#constrain σ to have positive probability between zero and 50cm
curve(dunif(x, 0, 50), from = 0, to = 60)

"""
A standard deviation like σ must be positive, so bounding it at zero makes sense. 
How should we pick the upper bound? In this case, a standard deviation of 50cm 
would imply that 95% of individual heights lie within 100cm of the average height. 
That’s a very large range.
"""


# PRIOR PREDICTIVE SIMULATION ---------------------------------------------


# now to see what these priors imply about the distribution of individual heights 

# Priors for h, μ, and σ imply a joint prior distribution of individual heights
# By simulating from this distribution, you can observe implications about obeservable 
# height and spot bad choices. 

# How? Simulate by sampling from the prior  
# can process priors just like posteriors as each posterior is potentially a prior 
# for another model / analysis 

# R code 4.14

# sample from the joint prior probability distributio 
# taken from 1000 simulated normal distributions, using our priors about the parameters
sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )


# filter out children first as height is closely correlated with age until 
# become an adult 

d2 <- d[ d$age >= 18 , ]


# Grid approximation ------------------------------------------------------


# R code 4.16
# grid approximation 
# this technique is impractical in most cases but useful to get a sense for 
# what we are aiming for with this type of model

# calculations include some technical tricks that add little insight, so just run 
# for now so get an idea of what posterior looks like when go through quap process 
# and come back later in an endnote 

mu.list <- seq( from=150, to=160 , length.out=100 )
sigma.list <- seq( from=7 , to=9 , length.out=100 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum(
    dnorm( d2$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
    dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

par(mfrow = c(1,1))

# inspect the posterior visually as a...
# ... contour plot
contour_xyz( post$mu , post$sigma , post$prob )

# ... heat map
image_xyz( post$mu , post$sigma , post$prob )



# Sample from the grid approximation posterior -----------------------------------------------

sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$prob )

sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]
# end up with 10,000 samples, with replacement, from the posterior for the height
# data
plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )
# col.alpha is part of rethinking package - makes colours transparent which helps
# to show density 

## 
# describe the distribution of confidence in each combination of mu and sigma 
# by summarising the samples 
# think of them like data and describe them - e.g. density plots: 
## R code 4.21
dens( sample.mu )
dens( sample.sigma )

#or summarise the width with highest posterior density intervals 
## R code 4.22
HPDI( sample.mu )
HPDI( sample.sigma )



# Finding posterior with quap ---------------------------------------------

# Move onto quadratic approximation = one of great engines of applied statistics 

# Why? 
#   Handy way to quickly make inferences about shape of the posterior 
#   Posteriors peak will lie at the maximum a posteriori estimate (MAP) - we can 
#   visualise the posteriors shape using quap at this peak 

# What is quap?
#   - rethinking command takes model definition as a list and the engine 
#   computes the posterior probability at each combination of parameter values 
#   - it can then 'climb' this posterior dist to find the peak - the MAP 
#   - FINALLY it estimate the quadratic curvature are this peak to produce approximation 
#   of the posterior distribution 
# 
#NOTE - very similar to non-bayesian applications, just with priors 

library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]


## R code 4.27
## 1) DEFINE THE MODEL using R's formula syntax 
flist <- alist(
    height ~ dnorm( mu , sigma ) ,
    mu ~ dnorm( 178 , 20 ) ,
    sigma ~ dunif( 0 , 50 )
)
# note the commas at end of each line, apart fromm the last

# 2) fit the model to the data 
m4.1 <- quap( flist , data=d2 )

precis(m4.1)
"""
        mean   sd   5.5%  94.5%
mu    154.61 0.41 153.95 155.27
sigma   7.73 0.29   7.27   8.20
"""

"""
These numbers provide Gaussian approximations for each parameter’s marginal distribution. 

This means the plausibility of each value of μ, after averaging over the plausibilities 
of each value of σ, is given by a Gaussian distribution with mean 154.6 and standard 
deviation 0.4.
"""

#NOTE
# Can specify where quap starts 'climbing' from rather than using random points 

start <- list(
    mu=mean(d2$height),
    sigma=sd(d2$height)
)

m4.1 <- quap( flist , data=d2 , start=start )

# or change the 89% HDI 
precis(m4.1, prob = 0.90)

# don't use 95% as someone will compare to significance test - no real justification for 0.95 boundary

# adding a more informaive prior for mu's s.d - change to 0.1 (much tighter than 20) 
## R code 4.31
m4.2 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu ~ dnorm( 178 , 0.1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) , data=d2 )
precis( m4.2 )
# note - estimate for mu is almsot same as the prior 
# the estimate for sigma changed a lot also, despite us not changing that prior 
# WHY? Once the golem is certain the mean is 178, then it has to estimate sigma 
# conditional on that 'fact' 


# Sampling from quap ------------------------------------------------------

# How? 
# Recognize a quadratic approximation to a posterior distribution with more than 
# one parameter dimension is just a multi-dimensional gaussian distribution 

# As a consequence R calculates s.d for each paramaters and covariances for
# pairs of parameters

# Just like a mean and s.d. (or var) are sufficient to describe a 1-d gaussian 
# distribution, a list of means, and a matrix of variances and covarinaces 
# are sufficient to describe a multi-d gaussian distribution. 

## R code 4.32
## variance - covariance matrix 
vcov( m4.1 )
# tells us how each parameter relates to every other parameter in the posterior 
# distribution

# although it's slightly easier to understand: 
## R code 4.33
# 1) a vector of variances for the paramaters
diag( vcov( m4.1 ) )

#2) a correlation matrix that tells us how changes in any parameter lead to 
# correlated changes in the others
cov2cor( vcov( m4.1 ) )


# getting samples from a multi-dimensional posterior
# instead of sampling single values from a simple normal distribution, we sample 
# a vector of values from a multi-dimensional Normal distribution 

# rethinking provides a function to do just that
library(rethinking)
post <- extract.samples( m4.1 , n=1e4 )
head(post)
"""
       mu    sigma
1 154.4616 8.074704
2 154.3723 7.791929
3 154.3627 8.333492
4 154.0810 7.850571
5 154.7819 7.759078
6 154.9618 7.891039
"""
# end up with a data frame, with 10k rows and 2 cols, one for mu and sigma respectively
# each value / row is a sample from the posterior, so mean and s.d of each col will 
# be close to the MAP values from before 

# can confirm by summarising the samples
precis(post)
"""
quap posterior: 10000 samples from m4.1
        mean   sd   5.5%  94.5%    histogram
mu    154.60 0.42 153.93 155.28     ▁▁▁▅▇▂▁▁
sigma   7.73 0.29   7.27   8.19 ▁▁▁▂▅▇▇▃▁▁▁▁
"""

# compare to grid approximation model
precis(m4.1)
"""
        mean   sd   5.5%  94.5%
mu    154.61 0.41 153.95 155.27
sigma   7.73 0.29   7.27   8.20
"""

# can use plot to see how much they resemble the samples from grid approximation 
plot(post)

# these samples alsp preserve covariance between mu and sigma 


# Linear prediction  ------------------------------------------------------

# This is a Normal model, but doesn't feel like regression 
# typically we're interested in modeling how an outcome is related to another variable 

# if predictor variable has many statistical associations with the outcome variable 
# then we can use it to predict the outcome

plot(d2$height, d2$weight)
# looks to be a broadly linear relationship in adults 

## linear model strategy 

