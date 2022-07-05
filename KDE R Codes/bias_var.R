rm(list = ls())
set.seed(1234)
library(glue)
library(EnvStats)

n = 100
h = 1

par(mfrow=c(1,1), bg = "#FFFFCC")
sample <- rnormMix(n, mean1 = -3, sd1 = 0.3, mean2 = 3, sd2 = 0.6, p.mix = 0.3)
plot(density(x = sample,
             kernel = 'gaussian',
             bw = h),
     xlim = c(-10,10),
     ylim = c(0,1),
     main = glue("n = {n}, h = {h}, nh = {n*h}"),
     xlab = 'x',
     ylab = 'f(x)',
     col = 1
)
for(i in 2:1000){
  sample <- rnormMix(n, mean1 = -3, sd1 = 0.3, mean2 = 3, sd2 = 0.6, p.mix = 0.3)
  lines(density(x = sample,
                kernel = 'triangular',
                bw = h),
        xlim = c(-10,10),
        ylim = c(0,1),
        main = glue("h = {1}"),
        xlab = 'x',
        ylab = 'f(x)',
        col = i
  )
}
grid()
legend(
  3,1,legend=c("True density"),
  col=2, lty=1, cex=1
)
curve(dnormMix(x, mean1 = -3, sd1 = 0.3, mean2 = 3, sd2 = 0.6, p.mix = 0.3), col = 2, add = TRUE, lwd = 2)


