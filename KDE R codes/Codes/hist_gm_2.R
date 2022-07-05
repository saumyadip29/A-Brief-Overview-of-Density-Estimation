rm(list = ls())
set.seed(1234)
library(glue)
library(EnvStats)


sample <- rnormMix(10000, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5)

par(mfrow=c(1,3), bg = "#FFFFCC")


for(i in seq(1,5,by=2)){
  #choosing starting point
  t0 <- min(sample)-i
  # Choosing the breaks
  Bk <- seq(t0, max(sample)+15, by = 1.5)
  hist(
    sample, freq = FALSE, breaks = Bk, 
    main = glue("t0 = {t0}, h = 1.5"),
    xlim = c(-10,10),
    ylim = c(0,0.255),
    xlab = 'sample',
    ylab = 'relative frequency'
  )
  grid()
  hist(
    sample, freq = FALSE, breaks = Bk, 
    main = glue("t0 = {t0}, h = 1.5"),
    xlim = c(-10,10),
    ylim = c(0,0.255),
    xlab = 'sample',
    ylab = 'relative frequency',
    add = TRUE
  )
  legend(
    -5,0.25,legend=c("True density"),
    col=c("red"), lty=1, cex=1
  )
  curve(dnormMix(x, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5),
        col = 2, add = TRUE, lwd = 2)
  rug(sample) # Plotting the sample
}
