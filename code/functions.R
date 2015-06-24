computeCRP <-function(c,gammaval){ 
  
  class_sizes=vector()
  for(i in 1:max(c)){
    class_sizes[i]<- sum(c==i)
  }
  ns<-class_sizes[class_sizes!=0] # only consider classes that are not empty
  k <- length(ns) # number of classes
  n <- sum(ns) # number of samples
  l <- sum(lgamma(ns))+k*log(gammaval)+lgamma(gammaval)-lgamma(n+gammaval)
  return(l)
}

chooseClass <-function(new_scores){
  # choose a class randomly proportional to marginal probability
  c <- 1:length(new_scores)
  new_scores <- new_scores - max(new_scores) # add constant to make calculation of ratios possible
  ps <- exp(new_scores) # calculate relative probabilities
  ps <- ps / sum(ps) # normalize to 1
  cumPs <- cumsum(ps)
  return(c[min(which(runif(1, 0.0, 1.0)<cumPs))])
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

findMLHypotheses <-function( c,train,hs,alpha,index_cache) {
  N_s = length(hs[["all_strings"]])
  N_r = length(hs[["hs"]])
  log_alpha = log(alpha)
  log_notalpha = log(1-alpha)
  ll_rule_string<- vector(mode="list", length=(max(c)))
  ll_rule<- vector(mode="list")
  h<- vector(length=(max(c)))
  noise_vals = log_notalpha + log(1/(N_s -  hs[["card"]]))
  for (k  in  1:max(c)){
    this_train <- train[c==k]
    cache_inds <- which(c==k)    
    # wypelnij wiarogodnosc danych jesli wykreowane dzieki szumowi przez halas    
    #ll_rule_string[[k]] <- matrix(noise_vals,c(1,sum(c==k)))
    if (!length(this_train)){ll_rule_string[[k]] <-matrix(replicate(1,0*noise_vals))}
    else{
      ll_rule_string[[k]] <- replicate(sum(c==k), noise_vals)
      # compute the likelihood of these data
      for (i in 1:length(this_train)){
        poss_rules <-which(hs[["true"]][,index_cache[["train"]][cache_inds[i]]]==TRUE)
        for (r in 1:length(poss_rules)){
          ll_rule_string[[k]][poss_rules[r],i] <- logSumExp(c(log_alpha + hs[["log_probs"]][poss_rules[r]], log_notalpha + log(hs[["card"]][poss_rules[r]]/N_s) + hs[["log_probs"]][poss_rules[r]]))
        }   
      }
    }
    p <- rowSums(ll_rule_string[[k]],1) + log(1/N_r)
    ll = logSumExp(p[!is.infinite(p)])
    h[[k]] <- hs[["hs"]][p==max(p)]
  } 
  out = vector()
  wynik<- vector(mode="list")
  wynik[[1]]<-ll
  wynik[[2]]<-h
  return(wynik)
} #


displayOutput <-function( c,train,hs,alpha,gammaval,index_cache){
  # find the maximum likelihood hypothesis for each cluster (because this
  # helps you interpret what a cluster means)
  u<-findMLHypotheses(c,train,hs,alpha,index_cache)
  ll <-u[[1]]
  mlhs<-u[[2]]
  pr <- computeCRP(c,gammaval)
  
  for (  i  in  1 : length(mlhs) ) {
    cat(sprintf('rule %d: %s %d %s %d %s %d\n',i,mlhs[[i]][[1]][1],mlhs[[i]][[1]][[2]],mlhs[[i]][[1]][[3]],mlhs[[i]][[1]][[4]],mlhs[[i]][[1]][[5]],mlhs[[i]][[1]][[6]]))
  } #
  
  cat('likelihood <- ' ,as.character(round(ll,2)),' / prior <- ', as.character(round(pr,2)),  ' / score <- ' ,as.character(round(ll +pr,2))) 
  cat('\n')
}