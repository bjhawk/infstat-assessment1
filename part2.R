# simulate lots of rexp(n, lambda)
lambda <- .2
n <- 40
nosim <- 10000
data <- apply(matrix(rexp(n*nosim, lambda), nosim), 1, mean)

hist(data, prob=T, breaks=nosim/250)
lines(density(data), lwd=2)

#show center, compared to theoretical center
abline(v=(mean(data)), lwd=2, lty='dotted', col='red')
abline(v=1/lambda, lwd=2, lty='dotted', col='blue') # thoretical center

#show variance and compare to theoretical variance

# show how normal the data set is
normal.data <- apply(matrix(rnorm(n*nosim, 1/lambda, 1/lambda), nosim), 1, mean)
lines(density(normal.data), lwd=2, col="blue")

# evalute confidence interval for 1/lambda
mean(data) + c(1,-1)*1.96*(sd(data)/sqrt(nosim))
#5.020700 4.989392