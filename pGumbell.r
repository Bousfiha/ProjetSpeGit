pGumbel<-function(x,beta,eta) {
  y = exp(-exp((eta-x)/beta))
  return (y)
}