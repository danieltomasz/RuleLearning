computeCRP <-function(c,gammaval){ 
  
  class_sizes=vector()
  for(i in 1:max(c)){
    class_sizes[i]<- sum(c[c==i])
  }
  ns<-class_sizes[class_sizes!=0] # only consider classes that are not empty
  k <- length(ns) # number of classes
  n <- sum(ns) # number of samples
  cat(ns)
  l <- colSums(log(gamma(ns))+k*log(gammaval)+log(gamma(gammaval))-log(gamma(n+gammaval)) )
  return(l)
}

chooseClass <-function( new_scores){
  # choose a class randomly proportional to marginal probability
  c <- 1:length(new_scores)
  new_scores <- new_scores - max(new_scores) # add constant to make calculation of ratios possible
  ps <- exp(new_scores) # calculate relative probabilities
  ps <- ps / sum(ps) # normalize to 1
  cumPs <- cumsum(ps)
  u <- which(runif(1)<cumPs)
  nc <- c[u[1]]
  return(nc)
}

cleanUpClasses <-function(c){ 
  u <- unique(c)
  for (k  in  1:length(u)){
    if (u[1] == 0){ 
       c[c==u[k]] <- (k-1)}
    else{
      c[c==u[k]] <- k
 } 
 }
return(c)
}