rm(list = ls())
set.seed(1234)
library(glue)


sample <- rexp(10000, rate = 20)
t0 <- min(sample)

par(mfrow=c(1,3), bg = "#FFFFCC")


for(i in 10^seq(-3,-1,by=1)){
  # Choosing the breaks
  Bk <- seq(t0, max(sample)+1, by = i)
  hist(
    sample, freq = FALSE, breaks = Bk, 
    main = glue("t0 = {t0}, h = {i}"),
    xlim = c(0,0.8),
    ylim = c(0,15),
    xlab = 'sample',
    ylab = 'relative frequency density'
  )
  grid()
  hist(
    sample, freq = FALSE, breaks = Bk, 
    main = glue("t0 = {t0}, h = {i}"),
    xlim = c(0,0.8),
    ylim = c(0,15),
    xlab = 'sample',
    ylab = 'relative frequency density',
    add = TRUE
  )
  legend(
    0.4,12,legend=c("True density"),
    col=c("red"), lty=1, cex=1
  )
  curve(dexp(x, rate = 20), col = 2, add = TRUE, lwd = 2)
  rug(sample) # Plotting the sample
}
