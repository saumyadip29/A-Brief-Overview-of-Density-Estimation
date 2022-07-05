rm(list = ls())
set.seed(1234)
library(glue)


sample <- rnorm(10000, mean = 0, sd = 10)
t0 <- min(sample)-1

par(mfrow=c(1,3), bg = "#FFFFCC")

for(i in 10^seq(-1,1,by=1)){
  # Choosing the breaks
  Bk <- seq(t0, max(sample)+10, by = i)
  hist(
    sample, freq = FALSE, breaks = Bk, 
    main = glue("t0 = {t0}, h = {i}"),
    xlim = c(-40,40),
    ylim = c(0,0.06),
    xlab = 'sample',
    ylab = 'relative frequency density'
  )
  grid()
  hist(
    sample, freq = FALSE, breaks = Bk, 
    main = glue("t0 = {t0}, h = {i}"),
    xlim = c(-40,40),
    ylim = c(0,0.06),
    xlab = 'sample',
    ylab = 'relative frequency',
    add = TRUE
  )
  legend(
    -42,0.05,legend=c("True density"),
    col=c("red"), lty=1, cex=1
    )
  curve(dnorm(x, mean = 0, sd = 10), col = 2, add = TRUE, lwd = 2)
  rug(sample) # Plotting the sample
}
