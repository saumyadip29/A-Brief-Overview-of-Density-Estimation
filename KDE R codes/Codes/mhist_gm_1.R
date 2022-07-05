rm(list = ls())
set.seed(1234)
library(glue)
library(EnvStats)

count.range <- function(sample, min, max){
  
  # computing the size of the vector
  size = length(sample)
  
  # declaring sum =0 as the count of elements in range
  sum = 0
  
  # looping over the vector elements
  for(i in 1:size)
  {
    
    # check if elements lies in the range provided
    if(sample[i]>=min && sample[i]<=max)
      
      # incrementing count of sum if condition satisfied
      sum =sum+1
  }
  return(sum)
}

moving.hist <- function(x, sample, h){
  n = length(sample)
  return((1/(2*n*h))*count.range(sample, x-h, x+h))
}

z <- Vectorize(moving.hist, vectorize.args = 'x')

sample <- rnormMix(10000, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5)

par(mfrow=c(1,3), bg = "#FFFFCC")

for(i in 10^seq(-3,-1,by=1)){
  curve(z(x, sample,i),
        from = -10, to = 10,
        main = glue("h = {i}"),
        xlim = c(-10,10),
        ylim = c(0, 0.45),
        xlab = 'sample',
        ylab = 'estimated density')
  grid()
  curve(z(x, sample,i),
        from = -10, to = 10,
        main = glue("h = {i}"),
        xlim = c(-10,10),
        ylim = c(0, 0.45),
        xlab = 'sample',
        ylab = 'estimated density',
        add = TRUE
        )
  legend(
    -0.7,0.45,legend=c("True density", "Moving Histogam"),
    col=c("red", "black"), lty=1, cex=1
  )
  curve(dnormMix(x, mean1 = -5, sd1 = 1, mean2 = 5, sd2 = 1, p.mix = 0.5),
        col = 2, add = TRUE, lwd = 2)
  rug(sample)
}


