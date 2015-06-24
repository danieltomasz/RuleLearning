 findMLHypotheses <-function( c,train,hs,alpha,index_cache) {
    N_s = length(hs[["all_strings"]])
  N_r = length(hs[["hs"]])
  log_alpha = log(alpha)
  log_notalpha = log(1-alpha)
  ll_rule_string<- vector(mode="list", length=(max(c)))
  ll_rule<- vector(mode="list", length=(max(c)))
  h<- vector(length=(max(c)))
  noise_vals = log_notalpha + log(1/(N_s -  hs["card"]))
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
  h[k] <- hs[["hs"]][p==max(p)]
 } 
 return(c(ll,h))
 } #
