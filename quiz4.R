#hwk
#Q1
mn <- mean(mtcars$mpg)
s <- sd(mtcars$mpg)
z <- qnorm(.05)
mu0 <- mn - z * s / sqrt(nrow(mtcars))

#Q2
m4 <- mtcars$mpg[mtcars$cyl == 4]
m6 <- mtcars$mpg[mtcars$cyl == 6]
p <- t.test(m4, m6, paired = FALSE, alternative="two.sided", var.equal=FALSE)$p.value

#Q3
n <- 100
m<- 3
s <- 1.1
z <- qnorm(0.975)
up <- m + z*s/sqrt(100)


#Q4
p = 0.5
ans <- round(pbinom(54, prob = .5, size = 100, lower.tail = FALSE),4)


#Q5
pv <- ppois(15800 - 1, lambda = 520 * 30, lower.tail = FALSE)
pnorm(15800 / 30, mean = 520, sd = sqrt(520 / 30), lower.tail = FALSE)
# n technical terms, a P value is the probability of obtaining an effect at least as 
#extreme as the one in your sample data, assuming the truth of the null hypothesis.
# 
# For example, suppose that a vaccine study produced a P value of 0.04. 
#This P value indicates that if the vaccine had no effect, you???d obtain the observed 
#difference or more in 4% of studies due to random sampling error.
# 
# P values address only one question: how likely are your data, 
#assuming a true null hypothesis? It does not measure support for the alternative hypothesis. 
#This limitation leads us into the next section to cover a very common misinterpretation of P values.


#Q6
m1 <- 10; m2 <- 11
n1 <- n2 <- 100
s <- 4
se <- s * sqrt(1 / n1 + 1 / n2)
ts <- (m2 - m1) / se
pv <- 2 * pnorm(-abs(ts))

#Q8
power <- pnorm(10 + qnorm(.95) * .4, mean = 11, sd = .4, lower.tail = FALSE)


mpg8 <- mtcars$mpg[mtcars$cyl == 8]
mpg6 <- mtcars$mpg[mtcars$cyl == 6]
m8 <- mean(mpg8); m6 <- mean(mpg6)
s8 <- sd(mpg8); s6 <- sd(mpg6)
n8 <- length(mpg8); n6 <- length(mpg6)

p <- t.test(mpg8, mpg6, paired = FALSE, alternative="two.sided", var.equal=TRUE)$p.value
mixprob <- (n8 - 1) / (n8 + n6 - 2)
s <- sqrt(mixprob * s8 ^ 2  +  (1 - mixprob) * s6 ^ 2)
z <- (m8 - m6) / (s * sqrt(1 / n8 + 1 / n6))
pz <- 2 * pnorm(-abs(z))


##Quiz

#Q4

pv <- ppois(10, lambda = 1/100*1787, lower.tail = FALSE)
1-pv

#Q5
n1 <- n2 <-9
m1 <- -3
m2 <- 1
s1 <- 1.5
s2 <- 1.8
df <- 16
se <-  sqrt(s1 / n1 + s2 / n2)
ts <- (m2 - m1) / se
pv <- 2 * pnorm(-abs(ts))


#Q7
n <- 100
m <- 0.01
s <- 0.04

power <- pnorm(0+ qnorm(.975) * .04/sqrt(100), mean = 0.01, sd = .04/sqrt(100), lower.tail = FALSE)


#Q8

n <- (qnorm(.95) + qnorm(.9)) ^ 2 * .04 ^ 2 / .01^2
  