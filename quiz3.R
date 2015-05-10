mean<-1100
sd<-30
n=9
error <- qt(0.975,n-1)*sd/sqrt(n)
confint <- mean +c(-1,1)*error


mean2 <- -2
n <- 9
s <- c(2.6,0.3,1.5,2.1)
error2 <- qt(0.975,n-1)*s/sqrt(n)
mean2 + error2


n1 <- 10
n2 <-10
m1 <- 3
var1 <- 0.6
m2<- 5
var2<- 0.68
sp<- sqrt((9*var1+9*var2)/(n1+n2-2))
m1-m2+c(-1,1)*qt(0.975,18)*sp*sqrt(1/10+1/10)



n1<-9
n2<-9
m1<- -3
m2<-1
s1<-1.5
var1<- s1^2
s2<-1.8
var2 <- s2^2
sp<- sqrt(((n1-1)*var1+(n2-1)*var2)/(n1+n2-2))
m1-m2+c(-1,1)*qt(0.95,n1+n2-2)*sp*sqrt(1/n1+1/n2)
