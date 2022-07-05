# Data
set.seed(23456)
x <- rnorm(100, mean = 0, sd = 5)

# UCV gives a warning
bw.ucv(x = x)

library(ks)
# Sample
n <- 100
set.seed(123456)
samp_t <- rnorm(n, mean = 0, sd = 5 )

# Comparison: same output and same parametrization for bandwidth
bw <- bw.ucv(x)
plot(kde <- ks::kde(x = samp_t, h = bw), lwd = 3) # ?ks::plot.kde for options
lines(density(x = samp_t, bw = bw), col = 2)
# Beware: there is no lines() method for ks::kde objects

# The default h is the DPI obtained by ks::hpi
kde <- ks::kde(x = samp_t)

# Manual plot -- recall $eval.points and $estimate
lines(kde$eval.points, kde$estimate, col = 4)
