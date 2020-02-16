"""
The preferred format is PDF or a plain text script file (.Rmd or .R). 

1. Suppose the globe tossing data had turned out to be 8 water in 15 tosses. Construct the 
posterior distribution, using grid approximation. Use the same flat prior as 
before.

2. Start over in 1, but now use a prior that is zero below p = 0.5 and a 
con- stant above p = 0.5. This corresponds to prior information that a majority 
of the Earth’s surface is water. What difference does the better prior make? 
If it helps, compare posterior distributions (using both priors) to the true 
value p = 0.7.

3. This problem is more open-ended than the others. Feel free to collabo-rate 
on the solution. Suppose you want to estimate the Earth’s proportion of water 
very precisely. Specifically, you want the 99% percentile interval of the
posterior distribution of p to be only 0.05 wide. This means the distance 
be-tween the upper and lower bound of the interval should be 0.05. How many 
times will you have to toss the globe to do this? I won’t require a precise 
answer. I’m honestly more interested in your approach.
"""

library(rethinking)

# 1 
# 8 water out of 15 tosses 


dbinom( 8 , size=15 , prob=0.5)
# That number is the relative number of ways to get eight water, holding p at 0.5 
# and N = W + L at nine. So it does the job of counting relative number of paths 
# through the garden. Change the 0.5 to any other value, to see how the value 
# changes.
## - prob close to the center (0.5) results in more ways to get 8 water, as get 
## closer to 0 and 1 the number of ways drop 

# the model: 
# W ∼ Binomial(N, p)   #where N = W + L
#  And the unobserved parameter p: p ∼ Uniform(0, 1)


# bayes theroem in word form ----------------------------------------------

# Posterior = (Probability of the data × Prior) / Average probability of the data


# different engines for computing posteriors ------------------------------

# 3 used in this book 
# (1) Grid approximation
# (2) Quadratic approximation
# (3) Markov chain Monte Carlo (MCMC)

"""
In the context of the globe tossing problem, grid approximation works extremely well. 
So let’s build a grid approximation for the model we’ve constructed so far. 

Here is the recipe:
(1) Define the grid. This means you decide how many points to use in estimating 
the posterior, and then you make a list of the parameter values on the grid.
(2) Compute the value of the prior at each parameter value on the grid.
(3) Compute the likelihood at each parameter value.
(4) Computetheunstandardizedposteriorateachparametervalue,bymultiplyingthe
prior by the likelihood.
(5) Finally, standardize the posterior, by dividing each value by the sum of all values.
In the globe tossing context, here’s the code to complete all five of these steps:
"""


# Q1 - Grid approximate 8 / 15 --------------------------------------------



# 2.3
# define grid (range of possible values for parameter of interest)
p_grid <- seq( from=0 , to=1 , length.out=20 )
hist(p_grid)

# define prior (flat - uniform prior)
prior <- rep( 1 , 20 )
hist(prior)

# compute likelihood at each value in grid (with updated values)
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
hist(likelihood)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
hist(unstd.posterior)

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
hist(posterior)

# flat prior means hist of likelihood and unstandardised posterior are identical


set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE)
precis(samples, prob = 0.99)
?precis

#The above code makes a grid of only 20 points. To display the posterior distribution now:

# 2.4 
plot( x = p_grid , y = posterior , type="b" , xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )



# Q2 - Grid approximation, lower p ----------------------------------------

"""
2. Start over in 1, but now use a prior that is zero below p = 0.5 and a 
con- stant above p = 0.5. This corresponds to prior information that a majority 
of the Earth’s surface is water. What difference does the better prior make? 
If it helps, compare posterior distributions (using both priors) to the true 
value p = 0.7.
"""




# 2.3
# define grid (range of possible values for parameter of interest)
p_grid <- seq( from=0 , to=1 , length.out=20 )

# define prior (flat - uniform prior)
#prior_2 <- ifelse( p_grid < 0.5 , 0 , 1 )
prior_2 <- c( rep( 0 , 500 ) , rep( 1 , 500 ) )

# compute likelihood at each value in grid (with updated values)
likelihood_2 <- dbinom( 8 , size=15 , prob=p_grid_2 )
hist(likelihood_2)

# compute product of likelihood and prior
unstd.posterior_2 <- likelihood_2 * prior_2
hist(unstd.posterior)

# standardize the posterior, so it sums to 1
posterior_2 <- unstd.posterior_2 / sum(unstd.posterior_2)

# flat prior means hist of likelihood and unstandardised posterior are identical
precis(posterior_2, prob = 0.99)

set.seed(100)
samples_2 <- sample( p_grid , prob=posterior_2 , size=1e4 , replace=TRUE)
precis(samples, prob = 0.99)

#The above code makes a grid of only 20 points. To display the posterior distribution now:

# 2.4 
plot( x = p_grid_2 , y = posterior_2 , type="b" , xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )

### difference? 0 probability below 0.5 & a higher probability of values btw 
### 0.5 and 0.7 than flat prior 


### answer to Q2

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- c( rep( 0 , 500 ) , rep( 1 , 500 ) )
prob_data <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- prob_data * prior
posterior <- posterior / sum(posterior)

set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE)
precis(samples)

plot( x = p_grid , y = posterior , type="b" , xlab="probability of water" , ylab="posterior probability" )
mtext( "1000 points" )
### answer is still roughly the same as abovee - richard just used a bigger grid 
### of 1000 values

# visually 
dens( samples , xlab="p" , xlim=c(0,1) , ylim=c(0,6) )
dens( samples2 , add=TRUE , lty=2 )
abline( v=0.7 , col="red" )

# 3 approach to increasing accuracy  --------------------------------------

"""
3. This problem is more open-ended than the others. Feel free to collabo-rate 
on the solution. Suppose you want to estimate the Earth’s proportion of water 
very precisely. Specifically, you want the 99% percentile interval of the
posterior distribution of p to be only 0.05 wide. This means the distance 
be-tween the upper and lower bound of the interval should be 0.05. How many 
times will you have to toss the globe to do this? I won’t require a precise 
answer. I’m honestly more interested in your approach.
"""

# understand the current range now 

posterior[19] - posterior[2]
# current 90th percentile range is 3.7

# if possible I'd:

# - compare to a benchmark 
# - iterate - try 50, 100, and then more until get closer 
# - simulate larger versions of binomial model 
# - restrict range of values using increasingly accurate priors sourced from research 


# have a read through homework solution - basically simulate different size grids, 
# calculate interval width and I think then find a way to visualise the optimal point 

# appendix - workings -----------------------------------------------------


posterior_90 <- posterior[2:19]
summary(posterior_90)
# woth 20 throws the 

boxplot(posterior)

install.packages("gmodels")
library(gmodels)
ci(posterior, confidence=0.95, alpha=1 - confidence)




