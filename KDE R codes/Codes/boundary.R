rm(list = ls())
set.seed(1234)
library(glue)


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



sample <- rexp(10000, rate = 1)
par(mfrow = c(1,2), bg = "#FFFFCC")
h.lscv = bw.ucv.mod(x = sample, plot_cv = TRUE, h_grid = 10^seq(-1.25, 0.5, l = 200))






y1 = -1*sample
y2 = sample
y = c(y1,y2)

h.lscv_1 = bw.ucv.mod(x = y, plot_cv = TRUE, h_grid = 10^seq(-1.25, 0.5, l = 200))

par(mfrow=c(1,1), bg = "#FFFFCC")

plot(density(x = sample,
             kernel = 'gaussian',
             bw = h.lscv),
     xlim = c(0,5),
     ylim = c(0,1.1),
     main = glue("Boundary correction"),
     xlab = 'sample',
     ylab = 'estimated density using gaussian kernel'
)

curve(dexp(x, rate = 1), col = 2, add = TRUE, lwd = 2)
rug(sample)

d = density(x = y,
            kernel = 'gaussian',
            bw = h.lscv_1)


x1 = d$x[d$x>0]
y1 = 2*d$y[d$x>0]
lines(
  x1, y1, type = 'l', col = 3, lwd = 2
)
grid()
legend(
  03,1.1,legend=c("True density", "KDE", "KDE with boundary correction"),
  col=c(2, 1, 3), lty=1, cex=1
)