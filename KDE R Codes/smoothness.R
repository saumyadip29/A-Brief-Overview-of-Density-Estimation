rm(list = ls())
set.seed(1234)
library(glue)
library(EnvStats)


sample <- rnormMix(10000, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5)

par(mfrow=c(1,1), bg = "#FFFFCC")


plot(density(x = sample,
             kernel = 'gaussian',
             bw = 0.01),
     xlim = c(-10,10),
     ylim = c(0,0.4),
     main = 'smoothness of kernels',
     xlab = 'x',
     ylab = 'f(x)',
     col = 2
)
lines(density(x = sample,
             kernel = 'gaussian',
             bw = 0.1),
             col = 3
             )
lines(density(x = sample,
              kernel = 'gaussian',
              bw = 0.5),
      col = 4
)
lines(density(x = sample,
              kernel = 'gaussian',
              bw = 1),
      col = 5
)
lines(density(x = sample,
             kernel = 'gaussian',
             bw = 2),
             col = 6
             )
grid()
legend(
  -1.5,0.4,legend=c("h = 0.01", "h = 0.1", "h = 0.5", "h = 1", "h = 2"),
  col=c(2, 3, 4, 5, 6), lty=1, cex=1
)
rug(sample)

