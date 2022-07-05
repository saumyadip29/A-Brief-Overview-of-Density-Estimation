rm(list = ls())
set.seed(1234)
library(glue)
library(EnvStats)


bw.ucv.mod <- function(x, nb = 1000L,
                       h_grid = 10^seq(-3, log10(1.2 * sd(x) *
                                                   length(x)^(-1/5)), l = 200),
                       plot_cv = FALSE) {
  if ((n <- length(x)) < 2L)
    stop("need at least 2 data points")
  n <- as.integer(n)
  if (is.na(n))
    stop("invalid length(x)")
  if (!is.numeric(x))
    stop("invalid 'x'")
  nb <- as.integer(nb)
  if (is.na(nb) || nb <= 0L)
    stop("invalid 'nb'")
  storage.mode(x) <- "double"
  hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
  Z <- .Call(stats:::C_bw_den, nb, x)
  d <- Z[[1L]]
  cnt <- Z[[2L]]
  fucv <- function(h) .Call(stats:::C_bw_ucv, n, d, cnt, h)
  ## Modification
  obj <- sapply(h_grid, function(h) fucv(h))
  h <- h_grid[which.min(obj)]
  if (h %in% range(h_grid)) 
    warning("minimum occurred at one end of h_grid")
  if (plot_cv) {
    plot(h_grid, obj, type = "o", main = 'LSCV for different h', xlab = 'h', ylab = 'LSCV(h)')
    rug(h_grid)
    abline(v = h, col = 2, lwd = 2)
  }
  h
}



sample <- rnormMix(10000, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5)
par(mfrow = c(1,1), bg = "#FFFFCC")
h.lscv = bw.ucv.mod(x = sample, plot_cv = TRUE, h_grid = 10^seq(-1.25, 0.5, l = 200))


par(mfrow=c(1,3), bg = "#FFFFCC")

plot(density(x = sample,
             kernel = 'gaussian',
             bw = h.lscv),
     xlim = c(-10,10),
     ylim = c(0,0.3),
     main = glue("h = {h.lscv}"),
     xlab = 'sample',
     ylab = 'estimated density using gaussian kernel',
)
grid()
legend(
  -5,0.3,legend=c("True density", "KDE"),
  col=c("red", "black"), lty=1, cex=1
)
curve(dnormMix(x, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5), col = 2, add = TRUE, lwd = 2)
rug(sample)


plot(density(x = sample,
             kernel = 'epanechnikov',
             bw = h.lscv),
     xlim = c(-10,10),
     ylim = c(0,0.3),
     main = glue("h = {h.lscv}"),
     xlab = 'sample',
     ylab = 'estimated density using epanechnikov kernel',
)
grid()
legend(
  -5,0.3,legend=c("True density", "KDE"),
  col=c("red", "black"), lty=1, cex=1
)
curve(dnormMix(x, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5), col = 2, add = TRUE, lwd = 2)
rug(sample)


plot(density(x = sample,
             kernel = 'triangular',
             bw = h.lscv),
     xlim = c(-10,10),
     ylim = c(0,0.3),
     main = glue("h = {h.lscv}"),
     xlab = 'sample',
     ylab = 'estimated density using triangular kernel',
)
grid()
legend(
  -5,0.3,legend=c("True density", "KDE"),
  col=c("red", "black"), lty=1, cex=1
)
curve(dnormMix(x, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5), col = 2, add = TRUE, lwd = 2)
rug(sample)
