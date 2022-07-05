rm(list = ls())
set.seed(1234)
library(glue)


#kernel.gaussian <- function(x, sample, h){
#  n = length(sample)
#  return(dnorm(x, mean = sample, sd = h))
#}

#kde <- function(x, sample, h){
#  n = length(sample)
#  return((1/(n*h))*sum(kernel.gaussian(x, sample, h)))
#}

#curve(z(x, sample,1),
#      from = -40, to = 40,
#      main = glue("h = 1"),
#      xlab = 'sample',
#      ylab = 'estimated density',
#      add = TRUE)

#z <- Vectorize(kde, vectorize.args = 'x')

sample <- rnorm(10000, mean = 0, sd = 10)

par(mfrow=c(3,3), bg = "#FFFFCC")


for(i in 10^seq(-1,1,by=1)){
  plot(density(x = sample,
               kernel = 'triangular',
               bw = i),
       xlim = c(-40,40),
       ylim = c(0,0.055),
       main = glue("h = {i}"),
       xlab = 'sample',
       ylab = 'estimated density using triangular kernel'
  )
  grid()
  legend(
    11,0.055,legend=c("True density", "KDE"),
    col=c("red", "black"), lty=1, cex=0.7
  )
  curve(dnorm(x, mean = 0, sd = 10), col = 2, add = TRUE, lwd = 2)
  rug(sample)
}


for(i in 10^seq(-1,1,by=1)){
  plot(density(x = sample,
               kernel = 'gaussian',
               bw = i),
       xlim = c(-40,40),
       ylim = c(0,0.055),
       main = glue("h = {i}"),
       xlab = 'sample',
       ylab = 'estimated density using gaussian kernel'
  )
  grid()
  legend(
    11,0.055,legend=c("True density", "KDE"),
    col=c("red", "black"), lty=1, cex=0.7
  )
  curve(dnorm(x, mean = 0, sd = 10), col = 2, add = TRUE, lwd = 2)
  rug(sample)
}

###

for(i in 10^seq(-1,1,by=1)){
  plot(density(x = sample,
               kernel = 'epanechnikov',
               bw = i),
       xlim = c(-40,40),
       ylim = c(0,0.055),
       main = glue("h = {i}"),
       xlab = 'sample',
       ylab = 'estimated density using epanechnikovkernel'
  )
  grid()
  legend(
    11,0.055,legend=c("True density", "KDE"),
    col=c("red", "black"), lty=1, cex=0.7
  )
  curve(dnorm(x, mean = 0, sd = 10), col = 2, add = TRUE, lwd = 2)
  rug(sample)
}
