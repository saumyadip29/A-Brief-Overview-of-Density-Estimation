# Sample from a LN(0, 1)
set.seed(123456)
samp <- rlnorm(n = 500)

# kde and density
plot(density(samp), ylim = c(0, 0.8), xlim = c(0,15))
curve(dlnorm(x), from = -2, to = 10, n = 500, col = 2, add = TRUE)
rug(samp)


kernel.xpe <- function(x, sample, h){
  n = length(sample)
  return(dexp(x, mean = sample, sd = h))
}

kde <- function(x, sample, h){
  n = length(sample)
  return((1/(n*h))*sum(kernel.gaussian(x, sample, h)))
}

curve(z(x, sample,1),
      from = -40, to = 40,
      main = glue("h = 1"),
      xlab = 'sample',
      ylab = 'estimated density',
      add = TRUE)

z <- Vectorize(kde, vectorize.args = 'x')