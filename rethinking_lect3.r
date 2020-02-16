


#install.packages(c("coda","mvtnorm","devtools","loo"))
#devtools::install_github("rmcelreath/rethinking")

# experimental version = v2 used in winter 2019 lectures 
#install.packages(c("devtools","mvtnorm","loo","coda"),dependencies=TRUE)
#library(devtools)
#devtools::install_github("rmcelreath/rethinking",ref="Experimental", force = TRUE)

library(rethinking)

# run once to make stan more efficient 
#options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)

data("Howell1")
d <- Howell1
precis( d )
?precis



# Prior predictive distribution -------------------------------------------

sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)

hist(prior_h)
dens(prior_h)



# Quadractic Approximation ------------------------------------------------


# create parameter distributions separately as list 
flist <- alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 20),
    sigma ~ dunif(0, 50)
)

d2 <- d[d$age >= 18, ]


m4.1 <- quap(flist, data = d2)
# quap not available in rethinking package 

precis(m4.1)

post <- extract.samples(m4.1, 1e4)
head(post)

# quap is a scaffold that forces you to learn the model 


# prior predictive distribution -------------------------------------------

# For model of height ~ weight 
# height - distributed normally, with mu predicted as a linear model and 
# sigma with uniform prior 
# mu's linear model has alpha and one beta parameter, both of which are distributed 
# normally 

set.seed(2971)

N <- 100    # 100 LINES
a <- rnorm(N, 178, 20)
b <- rnorm(N, 0, 10)
#plot(a, b, type = "l")

dens(b)

# can create better estimate of beta using log normal --> all positive values 
b <- rlnorm(1e4, 0, 1)
dens(b, xlim = c(0,5), adj = 0.1)

b <- rlnorm(N, 0, 1)

# define average weight 

xbar <- mean(d2$weight)    

# fit model 

m4.3 <- quap(
    alist(
        height ~ dnorm(mu, sigma),
        mu <- a + b * (weight - xbar),
        a ~ dnorm(178, 20),
        b ~ dlnorm(0, 1),
        sigma ~ dunif(0, 50)
    ), 
    data = d2
)

precis(m4.3)

# plot the posterior by extracting samples 

plot(height ~ weight, data = d2, col = rangi2)
post <- extract.samples(m4.3)
a_map <- mean(post$a)
b_map <- mean(post$b)
curve(a_map + b_map * (x - xbar), add = TRUE)
# curve adds plots the function line onto a preexisting plot 



# Sampling from the posterior ---------------------------------------------

# want to get uncertainty onto that graph 
# again, sample from the posterior 
# - 1 use mean and s.d. to approximate posterior 
# - 2 sample from multivariate normal distribution of paramaters 
# - 3 use samples to generate predictions that 'integrate over' the uncertainty 

post[1:5, ]
# each row is a line with alpha (y-intercept), beta (slope - can generate values of x) 
# and sigma estimates 
 
## posterior is full of lines 
## - equivalent to confidence intervals 
## - sometimes lines are better as they avoid trap of thinking the boundaries of 
## a CI mean anything - they don't 
# - generally CIs easier to interpret 


# predict mu  -------------------------------------------------------------


mu_at_50 <- post$a + post$b * (50 - xbar)
dens(mu_at_50)

# predict every mu 
# - we want a distribution for every value of x 

# define sequence of weights to make predictions from = x / horizontal axis 
weight.seq <- seq(from = 25, to = 70, by = 1)

# use link to compute mu for each sample posterior and for each weight in weight.seq

mu <- link(m4.3, data = data.frame(weight = weight.seq))
str(mu)

## link samples from the posterior, defines each series of predictor (weight) values 
## for each predictor value & for each sample from posterior, compute mu: 
## -- mu = a + b * (weight - xbar)

mu.link <- function(weight) {post$a + post$b * (weight - xbar)}

mu <- sapply(weight.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.89)

# nothing special about 95%, interested in shape, not boundaries 
## ** note - so far these intervals have been for uncertainty around the mean 
## - need to bring in sigma to generate likely intervals for all values 



