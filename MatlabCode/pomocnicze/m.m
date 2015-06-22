% compute the likelihood p(d|h) 
% assuming the noisy likelihood (models 2 and 3)
% this is the fastest version yet
% corrected 2/18/14 via Florent Meyniel

function loglike = computeNoisyLikelihood2(hs,c,train,params,index_cache)

% some constants
N_s = length(hs.all_strings);
N_r = length(hs.hs);
log_alpha = log(params.alpha);
log_notalpha = log(1-params.alpha);
noise_vals = repmat(log_notalpha + log(1 ./ (N_s)),[N_r 1]);

% now compute the likelihood of these data for each cluster
for k = 1:max(c)
this_train = train(c==k);
cache_inds = find(c==k);

% fill in likelihood of data if produced by noise from some rule    
ll_rule_string{k} = repmat(noise_vals,[1 sum(c==k)]);

% compute likelihood of the data under each other possible rule
for i = 1:length(this_train)    
poss_rules = find(hs.true_of(:,index_cache.train(cache_inds(i))));
for r = 1:length(poss_rules);
ll_rule_string{k}(poss_rules(r),i) = logsumexp([log_alpha + hs.log_probs(poss_rules(r)); ...
                                                log_notalpha + log(hs.cardinalities(poss_rules(r))/N_s) + hs.log_probs(poss_rules(r))]);
end    
end

% now product over rules
ll_rule{k} = sum(ll_rule_string{k},2) + log(1/N_r);

% now sum for that cluster over all those that aren't -Inf
    ll_cluster(k) = logsumexp(ll_rule{k}(~isinf(ll_rule{k})));
  end
  
  % now product over clusters
  loglike = sum(ll_cluster);
end
% wrapper function for the crp prior

function prior = computeCRP(c,params)

% get the size of each class
for i = 1:max(c)
  class_sizes(i) = sum(c==i);
end

prior = crp(class_sizes, params.gamma);

% function l = crp(ns, gamma) 
% probability of the partition in ns under a CRP with concentration parameter
% gamma (note that gamma here is NOT the gamma function but just a number)
%
% Provided by Charles Kamp 2006

function l = crp(ns, gamma) 

ns=ns(ns~=0); % only consider classes that are not empty
k = length(ns); % number of classes
n = sum(ns); % number of samples
l = sum(gammaln(ns))+k*log(gamma)+gammaln(gamma)-gammaln(n+gamma); 

% choose a class randomly proportional to marginal probability

function nc = chooseClass(new_scores)

c = 1:length(new_scores);
new_scores = new_scores - max(new_scores); % add constant to make calculation of ratios possible
ps = exp(new_scores); % calculate relative probabilities
ps = ps / sum(ps); % normalize to 1
cumPs = cumsum(ps);
nc = c(find(rand<cumPs,1,'first'));

% make sure that the set of classes is always contiguous (otherwise we will
% orphan some classes

function c = cleanUpClasses(c)

u = unique(c); % if a class became empty then there will be hole in u

for k = 1:length(u)
  if u(1) == 0    
    c(c==u(k)) = k-1;
  else
    c(c==u(k)) = k;
  end
end



