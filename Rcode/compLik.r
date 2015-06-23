compLik<-function( hs,c,train,alpha,index_cache){  
    
  #save(hs, train, c2,index_cache,file = "mymodel.rda")
  #load(file = "mymodel.rda")
  
  
  N_s = length(hs[["all_strings"]])
  N_r = length(hs[["hs"]])
  # wiarogodnosc p(d|h)
  #stałe
  log_alpha = log(alpha)
  log_notalpha = log(1-alpha)
  noise_vals = rep(log_notalpha + log(1/(N_s)),N_r )
 
  # wiarogodnosc dla kazdego clustra
  ll_rule_string<- vector(mode="list", length=(max(c)))
  ll_rule<- vector(mode="list", length=(max(c)))
  ll_cluster<- vector(length=(max(c)))
  
  
  for (k  in  1:max(c)){
    this_train <- train[c==k]
    cache_inds <- which(c==k)    
    # wypelnij wiarogodnosc danych jesli wykreowane dzieki szumowi przez halas    
    ll_rule_string[[k]] <- matrix(noise_vals,c(1,sum(c==k)))
    # compute likelihood of the data under each other possible rule
    for (i in 1:length(this_train)){
    poss_rules <-which(hs[["true"]][,index_cache[["train"]][cache_inds[i]]]==TRUE)
    for (r in 1:length(poss_rules)){
    ll_rule_string[[k]][1,poss_rules[r]] <- logSumExp(c(log_alpha + hs[["log_probs"]][poss_rules[r]], log_notalpha + log(hs[["card"]][poss_rules[r]]/N_s) + hs[["log_probs"]][poss_rules[r]]))
    }   
  }
  # now product over rules
  ll_rule[[k]] = colSums(ll_rule_string[[k]],1) + log(1/N_r)
  #now sum for that cluster over all those that aren't -Inf
  ll_cluster[[k]] = logSumExp(c(ll_rule[[k]][!is.infinite(ll_rule[[k]])]))
 }
 return(sum(ll_cluster))
}