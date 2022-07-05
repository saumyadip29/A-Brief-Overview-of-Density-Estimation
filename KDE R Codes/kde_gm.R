rm(list = ls())
set.seed(1234)
library(glue)
library(EnvStats)


sample <- rnormMix(10000, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5)

par(mfrow=c(3,3), bg = "#FFFFCC")


for(i in 10^seq(-2,0,by=1)){
  plot(density(x = sample,
               kernel = 'triangular',
               bw = i),
       xlim = c(-10,10),
       ylim = c(0,0.4),
       main = glue("h = {i}"),
       xlab = 'sample',
       ylab = 'estimated density using triangular kernel',
  )
  grid()
  legend(
    -3.5,0.4,legend=c("True density", "KDE"),
    col=c("red", "black"), lty=1, cex=0.8
  )
  curve(dnormMix(x, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5), col = 2, add = TRUE, lwd = 2)
  rug(sample)
}


for(i in 10^seq(-2,0,by=1)){
  plot(density(x = sample,
               kernel = 'gaussian',
               bw = i),
       xlim = c(-10,10),
       ylim = c(0,0.4),
       main = glue("h = {i}"),
       xlab = 'sample',
       ylab = 'estimated density using gaussian kernel',
  )
  grid()
  legend(
    -3.5,0.4,legend=c("True density", "KDE"),
    col=c("red", "black"), lty=1, cex=0.8
  )
  curve(dnormMix(x, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5), col = 2, add = TRUE, lwd = 2)
  rug(sample)
}

###

for(i in 10^seq(-2,0,by=1)){
  plot(density(x = sample,
               kernel = 'epanechnikov',
               bw = i),
       xlim = c(-10,10),
       ylim = c(0,0.4),
       main = glue("h = {i}"),
       xlab = 'sample',
       ylab = 'estimated density using epanechnikovkernel',
  )
  grid()
  legend(
    -3.5,0.4,legend=c("True density", "KDE"),
    col=c("red", "black"), lty=1, cex=0.8
  )
  curve(dnormMix(x, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5), col = 2, add = TRUE, lwd = 2)
  rug(sample)
}
