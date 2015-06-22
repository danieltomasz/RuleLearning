# find the maximum likelihood hypothesis for each cluster (because this
# helps you interpret what a cluster means)
[ll mlhs] <- findMLHypotheses(c,train,hs,params,index.cache)
pr <- computeCRP(c,params)

for (  i  in  1 : length(mlhs) ) {
  fprintf('rule #d: #s #d #s #d #s #d\n',i,mlhs{i}{1},mlhs{i}{2},mlhs{i}{3},...
    mlhs{i}{4},mlhs{i}{5},mlhs{i}{6})
   } #

disp(['likelihood <- ' num2str(ll,'#2.0f') ' / prior <- ' num2str(pr,'#2.0f') ...
  ' / score <- ' num2str(ll+pr,'#2.0f')]) 

fprintf('\n')


for(i  in  1:length(this_train)) {}
# poss_rules <- find(hs.true.of(:,index.cache.train(cache.inds(i))))
#for (  r  in  1 : length(poss.rules) ) {}
# ll.rule.string[[k]](poss_rules(r),i) <- logsumexp([log_alpha + hs.log_probs(poss_rules(r)) ...
#                                                  log_notalpha + log(cardinalities(poss.rules(r))/N.s) + hs.log.probs(poss.rules(r))])
# }     
#

# now product over rules
#ll.rule{k} <- sum(ll.rule.string{k},2) + log(1/N.r)

# now sum for that cluster over all those that aren't -Inf
#ll.cluster(k) <- logsumexp(ll.rule{k}(~isinf(ll.rule{k})))
#} #


# loglike = sum(ll_cluster)
