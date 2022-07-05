rm(list = ls())
set.seed(1234)
library(glue)
library(EnvStats)


sample <- rnormMix(100, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5)

par(mfrow=c(1,1), bg = "#FFFFCC")


plot(density(x = sample,
             kernel = 'triangular',
             bw = 0.15),
     xlim = c(-10,10),
     ylim = c(0,0.4),
     main = 'scale parameter of kernel density',
     xlab = 'x',
     ylab = 'f(x)',
     col = 2
)
#  lines(density(x = sample,
#               kernel = 'gaussian',
#               bw = 0.1),
#               col = 3
#               )
#  lines(density(x = sample,
#                kernel = 'triangular',
#                bw = 0.5),
#        col = 4
#  )
#  lines(density(x = sample,
#                kernel = 'gaussian',
#                bw = 1),
#        col = 5
#  )
#  lines(density(x = sample,
#               kernel = 'gaussian',
#               bw = 2),
#               col = 6
#               )
grid()
legend(
  -4,0.4,legend=c("kernel density estimate", "gaussian densities with s.d. = g(h)", "True density"),
  col=c(2, 1, 3), lty=1, cex=1
)
for(i in 1:100){
  curve(dnorm(x, mean = sample[i], sd = 0.15)/100, col = 1, add = TRUE, lwd = 0.1)
}
curve(dnormMix(x, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5), col = 3, add = TRUE, lwd = 1.5)
rug(sample)

