#This function uses inverse transform method to sample from a truncated normal distribution


normrnd <- function(mean=0,std=.294,min=-.59,max=.59) {
  #First we find the minimum and maximum bounds
  umin <- pnorm(min,mean = mean,sd=std)
  umax <- pnorm(max,mean = mean,sd=std)
  cdfval <- runif(1,min=umin,max=umax)
  variate  <- qnorm(cdfval,mean=mean,sd=std)
  return(variate)
}
