rm(list = ls())
set.seed(1234)
library(glue)
library(EnvStats)

sample <- rnormMix(10000, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5)
t0 <- min(sample)

par(mfrow=c(1,3), bg = "#FFFFCC")


for(i in 10^seq(-2,0,by=1)){
  # Choosing the breaks
  Bk <- seq(t0, max(sample)+1, by = i)
  hist(
    sample, freq = FALSE, breaks = Bk,
    main = glue("t0 = {t0}, h = {i}"),
    xlim = c(-10,10),
    ylim = c(0,0.4),
    xlab = 'sample',
    ylab = 'relative frequency'
  )
  grid()
  hist(
    sample, freq = FALSE, breaks = Bk,
    main = glue("t0 = {t0}, h = {i}"),
    xlim = c(-10,10),
    ylim = c(0,0.4),
    xlab = 'sample',
    ylab = 'relative frequency',
    add = TRUE
  )
  legend(
    -5,0.35,legend=c("True density"),
    col=c("red"), lty=1, cex=1
  )
  curve(
    dnormMix(x, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5),
    col = 2, add = TRUE, lwd = 2)
rug(sample) # Plotting the sample
}
