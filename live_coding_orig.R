

##
## Part 1: Introduction to NIMBLE
##

library(nimble)

code <- nimbleCode({
  for (i in 1:G) {
     ## prior for hyperparameters
     ## such gamma priors are not generally recommended, but
     ## these are the priors from the original example
     a[i] ~ dgamma(1, .001)
     b[i] ~ dgamma(1, .001)
     ##
     for (j in 1:N) {
         ## likelihood (data model)
        r[i,j] ~ dbin(p[i,j], n[i,j])
        ## latent process (random effects)
        p[i,j] ~ dbeta(a[i], b[i]) 
     }
   }
})

n <- matrix(c(13, 12, 12, 11, 9, 10, 
              9, 9, 8, 11, 8, 10, 13, 10, 12, 9, 10, 9, 10, 5, 9, 9, 13, 
              7, 5, 10, 7, 6, 10, 10, 10, 7), nrow = 2)

r <- matrix(c(13, 12, 12, 11, 9, 10, 9, 9, 8, 10, 8, 9, 
              12, 9, 11, 8, 9, 8, 9, 4, 8, 7, 11, 4, 4, 5, 5, 3, 7, 3, 7, 0), 
            nrow = 2)


## code, constants, data, inits

N <- dim(r)[2]

G <- dim(r)[1]

constants <- list(G = G, N = N, n = n)

data <- list(r = r)

inits <- list(a = c(2, 2), b = c(2, 2))



## NIMBLE model object: Rmodel



## compiled model object: Cmodel





Rmodel$p
Rmodel$calculate('a')   # log-prior density
Rmodel$getLogProb('a')

Rmodel$a <- c(3, 3)
Rmodel$getLogProb('a')
Rmodel$calculate('a')   # log-prior density

## so the calculations are reproducible
set.seed(1)

Rmodel$simulate('p')  # simulate from prior
Rmodel$p
Rmodel$getLogProb('p')  # log prob not yet updated!
Rmodel$calculate('p')   # update it
Rmodel$getLogProb('p')  # now we're good



##
## Part 2: MCMC Performance
##

## slice sampling for litters model:

## block sampling for litters model:

## block sampling with initial covariance matrices:

















