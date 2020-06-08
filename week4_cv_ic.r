
library(rethinking)
library(tidyverse)

# Qs:  "statrethinking_winter2019/homework/week04.pdf:


# Question 1 --------------------------------------------------------------


birds <- tibble::tibble(A = c(0.2, 0.8, 0.05), B = c(0.2, 0.1, 0.15), 
                        C = c(0.2, 0.05, 0.7), D = c(0.2, 0.025, 0.05), 
                        E = c(0.2, 0.025, 0.05))

# first compute the entropy of each island's bird distribution, interpret these
# entropy values 

birds_entropy <- birds %>%
    dplyr::rowwise() %>% 
    mutate(entropy = -sum(c(A,B,C,D,E) * log(c(A,B,C,D,E))))
# the highest entropy score is on island 1 which means there is the most 
# uncertainty here regarding what bird species you may come across. This makese 
# intuitive sense because there is an equal distribution across all 5 species 

# The lowest entropy score is island 2, because 80% of the birds are from species
# A, so predicting to find birds on Island 2 would be a good strategy     

# Island 3 is closer in entropy score to Island 2 and again has a single bird 
# species that dominates - 70% of Island 3 birds are from Species C


# second, use each island's bird distribution to predict the other two. 

# using solutions code as working out how to generate a matrix is melting my mind 
# this late at night

IB <- list()
IB[[1]] <- c( 0.2 , 0.2 , 0.2 , 0.2 , 0.2 )
IB[[2]] <- c( 0.8 , 0.1 , 0.05 , 0.025 , 0.025 )
IB[[3]] <- c( 0.05 , 0.15 , 0.7 , 0.05 , 0.05 )

DKL <- function(p,q) sum( p*(log(p)-log(q)) )

Dm <- matrix( NA , nrow=3 , ncol=3 )
for ( i in 1:3 ) for ( j in 1:3 ) Dm[i,j] <- DKL( IB[[j]] , IB[[i]])
round( Dm , 2 )

# due to having the highest entropy, island 1 now has the lowest divergence scores
# - this is because it knows there can be 5 species of birds in equal measure 
# and is therefore not 'surprised' by seeing any of them. 
# This enables it to make better predictions of the other islands 
#  than the others who have high concentrations of a single species

# stretch goal - get code working to apply above formula in tidyverse style
# resulting in a matrix 

"""
Dm2 <- matrix( NA , nrow=3 , ncol=3 )
for ( i in 1:3 ) for ( j in 1:3 ) Dm2[i,j] <- DKL( birds[[j,]] , birds[[i,]])
round( Dm , 2 )

birds_entropy %>% 
    mutate(q1 = sum( p*(log(p)-log(q)) ))
"""


# Question 2 --------------------------------------------------------------

# recall marriage, age and happiness collider example from chapter 6 
# run models m6.9 and m6.10 again, compare using WAIC (or LOO

"""
Suppose, just to be provocative, that an individual’s average happiness is a
trait that is determined at birth and does not change with age. However, 
happiness does influence events in one’s life. One of those events is marriage. 
Happier people are more likely to get married. 

Another variable that causally influences marriage is age: The more years you 
are alive, the more likely you are to eventually get married. 

Putting these three variables together, this is the causal model:

H -----> M <------ A

Happiness and Age both cause marriage, Marriage therefore is a collider. 

= even though there is no causal association between happiness and age, if we 
condition on marriage - include it as a predictor in a regression - then it will
induce a statistical association between age and happiness and mislead us to 
think happiness changes with age, when it is in fact constant. 

Here is the simulation design:
(1) Each year, 20 people are born with uniformly distributed happiness values.
(2) Each year, each person ages one year. Happiness does not change.
(3) At age 18, individuals can become married. The odds of marriage each year are
proportional to an individual’s happiness.
(4) Once married, an individual remains married.
(5) After age 65, individuals leave the sample. (They move to Spain.)
"""

## R code 6.22
library(rethinking)
d <- sim_happiness( seed=1977 , N_years=1000 )
precis(d)

## R code 6.23
d2 <- d[ d$age>17 , ] # only adults
d2$A <- ( d2$age - 18 ) / ( 65 - 18 )

## R code 6.24
d2$mid <- d2$married + 1
m6.9 <- quap(
    alist(
        happiness ~ dnorm( mu , sigma ),
        mu <- a[mid] + bA*A,
        a[mid] ~ dnorm( 0 , 1 ),
        bA ~ dnorm( 0 , 2 ),
        sigma ~ dexp(1)
    ) , data=d2 )
precis(m6.9,depth=2)

## R code 6.25
m6.10 <- quap(
    alist(
        happiness ~ dnorm( mu , sigma ),
        mu <- a + bA*A,
        a ~ dnorm( 0 , 1 ),
        bA ~ dnorm( 0 , 2 ),
        sigma ~ dexp(1)
    ) , data=d2 )
precis(m6.10)


# compute WAIC 
#  stepping through the overthinking: WAIC calculations on pg 221-222 (222 in book)
# set seed for reproducability and then extract samples  
set.seed(20)
post_m6.9 <- extract.samples(m6.9, n = 1000)
post_m6.10 <- extract.samples(m6.10, n = 1000)

# need log-likelihood of each observation i at each sample s from the posterior
n_samples <- 1000 
logprob_m6.9 <- sapply(1:n_samples, 
                  function(s) {
                      mu <- post_m6.9$a[s] + post_m6.9$bA[s] * d2$A
                      dnorm( d2$happiness, mu, post_m6.9$sigma[s], log = TRUE)
                  })

logprob_m6.10 <- sapply(1:n_samples, 
                  function(s) {
                      mu <- post_m6.10$a[s] + post_m6.10$bA[s] * d2$A
                      dnorm( d2$happiness, mu, post_m6.10$sigma[s], log = TRUE)
                  })
# left with 50 * 1000 matrix of log-likelihoods, obs in row and samples in cols. 

#   to compute lppd, the bayesian deviance:
#       1. avg. samples in each row, 
#       2. take log 
#       3. add all logs together. 
#   to do with precision, need to do all averaging on log scale 
#   --> log_sum_exp function provided

n_cases <- nrow(d2)
lppd_m6.9 <- sapply( 1: n_cases, function(i) log_sum_exp(logprob_m6.9[i,]) - log(n_samples))
lppd_m6.10 <- sapply( 1: n_cases, function(i) log_sum_exp(logprob_m6.10[i,]) - log(n_samples))
# typing sum(lppd_m6.9) will give lppd as defined in rethinking text

# now need penalty term pWAIC 
# this is straight forward as compute the variance across samples for each observation 
# then add these together

pWAIC_m6.9 <- sapply( 1:n_cases, function(i) var(logprob_m6.9[i,]))
pWAIC_m6.10 <- sapply( 1:n_cases, function(i) var(logprob_m6.10[i,]))
# and sum(pWAIC_m6.9) returns pWAIC as defined in text

# to compute WAIC
WAIC_m6.9 <- -2 *(sum(lppd_m6.9) - sum(pWAIC_m6.9))
WAIC_m6.10 <- -2 *(sum(lppd_m6.10) - sum(pWAIC_m6.10))


# there is a function in rethinking 
set.seed(20)
WAIC(m6.9)
WAIC(m6.10)

set.seed(20)
compare(m6.9, m6.10)

### m6.10 is close, m6.9 is out, think not defining the categorical alpha properly 
### in logprob 




# interpret
"""
Which model is ex- pected to make better predictions? Which model provides the 
correct causal inference about the influence of age on happiness? Can you explain
why the answers to these two questions disagree?
"""


