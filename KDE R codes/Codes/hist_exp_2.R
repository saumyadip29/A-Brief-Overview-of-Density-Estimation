rm(list = ls())
set.seed(1234)
library(glue)


sample <- rexp(1000, rate = 20)

par(mfrow=c(1,3), bg = "#FFFFCC")


for(i in 10^seq(-2,0,by=1)){
  #choosing starting point
  t0 <- min(sample)-i
  # Choosing the breaks
  Bk <- seq(t0, max(sample)+1, by = 0.1)
  hist(
    sample, freq = FALSE, breaks = Bk, 
    main = glue("t0 = {t0}, h = 0.1"),
    xlim = c(0,0.8),
    ylim = c(0,9),
    xlab = 'sample',
    ylab = 'relative frequency ddensity'
  )
  grid()
  hist(
    sample, freq = FALSE, breaks = Bk, 
    main = glue("t0 = {t0}, h = 0.1"),
    xlim = c(0,0.8),
    ylim = c(0,9),
    xlab = 'sample',
    ylab = 'relative frequency density',
    add = TRUE
  )
  legend(
    0.4,7,legend=c("True density"),
    col=c("red"), lty=1, cex=1
  )
  curve(dexp(x, rate = 20), col = 2, add = TRUE, lwd = 2)
  rug(sample) # Plotting the sample
}
