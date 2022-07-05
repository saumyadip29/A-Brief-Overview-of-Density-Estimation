rm(list = ls())
set.seed(1234)
library(glue)



sample <- rexp(10000, rate = 20)

par(mfrow=c(3,3), bg = "#FFFFCC")


for(i in 10^seq(-3,-1,by=1)){
  plot(density(x = sample,
               kernel = 'triangular',
               bw = i),
       xlim = c(0,1.2),
       ylim = c(0,15),
       main = glue("h = {i}"),
       xlab = 'sample',
       ylab = 'estimated density using triangular kernel',
  )
  grid()
  legend(
    0.6,13,legend=c("True density", "KDE"),
    col=c("red", "black"), lty=1, cex=1
  )
  curve(dexp(x, rate = 20), col = 2, add = TRUE, lwd = 2)
  rug(sample)
}


for(i in 10^seq(-3,-1,by=1)){
  plot(density(x = sample,
               kernel = 'gaussian',
               bw = i),
       xlim = c(0,1.2),
       ylim = c(0,15),
       main = glue("h = {i}"),
       xlab = 'sample',
       ylab = 'estimated density using gaussian kernel',
  )
  grid()
  legend(
    0.6,13,legend=c("True density", "KDE"),
    col=c("red", "black"), lty=1, cex=1
  )
  curve(dexp(x, rate = 20), col = 2, add = TRUE, lwd = 2)
  rug(sample)
}

###

for(i in 10^seq(-3,-1,by=1)){
  plot(density(x = sample,
               kernel = 'epanechnikov',
               bw = i),
       xlim = c(0,1.2),
       ylim = c(0,15),
       main = glue("h = {i}"),
       xlab = 'sample',
       ylab = 'estimated density using epanechnikovkernel',
  )
  grid()
  legend(
    0.6,13,legend=c("True density", "KDE"),
    col=c("red", "black"), lty=1, cex=1
  )
  curve(dexp(x, rate = 20), col = 2, add = TRUE, lwd = 2)
  rug(sample)
}
