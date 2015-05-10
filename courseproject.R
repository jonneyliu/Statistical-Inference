#statistical inference course project
##Law of large numbers on Exponential distirbution

n <- 1000; means <- cumsum(rexp(n,0.2)) / (1  : n); library(ggplot2)
g <- ggplot(data.frame(x = 1 : n, y = means), aes(x = x, y = y)) 
g <- g + geom_hline(yintercept = 0) + geom_line(size = 2) 
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
g

##exponential distribution with lambda = 0.2
##mean = stdev = 1/lambda
lambda = 0.2
n = 40
hist(rexp(n,lambda))
mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(rexp(n,lambda)))
hist(mns,prob=TRUE)
geom_histogram(data=mns)
hist(mns, density=30, breaks=20, prob=TRUE, 
     xlab="x-variable", ylim=c(0, 1), 
     main="normal curve over histogram")
m = 1/lambda
ssdev = 1/lambda/sqrt(n)
curve(dnorm(x, mean=m, sd=ssdev), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")
g <- ggplot(mns, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g
ggplot() +  
  geom_histogram(data=mns, ...) +  
  geom_line(data=dnorm, ...)  


qqnorm()/qqline() 

##fitting a line
X <- c(rep(65, times=5), rep(25, times=5), rep(35, times=10), rep(45, times=4))
hist(mns, prob=TRUE)            # prob=TRUE for probabilities not counts
lines(density(mns))             # add a density estimate with defaults
lines(density(mns, adjust=2), lty="dotted")   # add another "smoother" density
stat_function(fun = dnorm, size = 2)


##lecture code for showing CLT
nosim <- 1000
cfunc <- function(x, n) sqrt(n) * (mean(x) - 3.5) / 1.71
dat <- data.frame(
  x = c(apply(matrix(sample(1 : 6, nosim * 10, replace = TRUE), 
                     nosim), 1, cfunc, 10),
        apply(matrix(sample(1 : 6, nosim * 20, replace = TRUE), 
                     nosim), 1, cfunc, 20),
        apply(matrix(sample(1 : 6, nosim * 30, replace = TRUE), 
                     nosim), 1, cfunc, 30)
  ),
  size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g
##sim 2
nosim <- 1000
cfunc <- function(x, n) 2 * sqrt(n) * (mean(x) - 0.5) 
dat <- data.frame(
  x = c(apply(matrix(sample(0:1, nosim * 10, replace = TRUE), 
                     nosim), 1, cfunc, 10),

  ),
  size = factor(rep(c(10), rep(nosim, 1))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)
g + facet_grid(. ~ size)


##posisson
lambdavals <- seq(0.005, 0.10, by = .01); nosim <- 1000
t <- 1000
coverage <- sapply(lambdavals, function(lambda){
  lhats <- rpois(nosim, lambda = lambda * t) / t
  ll <- lhats - qnorm(.975) * sqrt(lhats / t)
  ul <- lhats + qnorm(.975) * sqrt(lhats / t)
  mean(ll < lambda & ul > lambda)
})
ggplot(data.frame(lambdavals, coverage), aes(x = lambdavals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95) + ylim(0, 1.0)
