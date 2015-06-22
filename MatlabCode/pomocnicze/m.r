# compute the likelihood p(d|h) 
# assuming the noisy likelihood (models 2 and 3)
# this is the fastest version yet
# corrected 2/18/14 via Florent Meyniel

 computeNoisyLikelihood2 <-function( hs,c,train,params,index.cache) {  loglike 

# some constants
N.s <- length(hs.all.strings)
N.r <- length(hs.hs)
log.alpha <- log(params.alpha)
log.notalpha <- log(1-params.alpha)
noise.vals <- repmat(log.notalpha + log(1 ./ (N.s)),[N.r 1])

# now compute the likelihood of these data for each cluster
for (  k  in  1 : max(c) ) {
this.train <- train(c==k)
cache.inds <- find(c==k)

# fill in likelihood of data if produced by noise from some rule    
ll.rule.string{k} <- repmat(noise.vals,[1 sum(c==k)])

# compute likelihood of the data under each other possible rule
for (  i  in  1 : length(this.train)     ) {
poss.rules <- find(hs.true.of(:,index.cache.train(cache.inds(i))))
for (  r  in  1 : length(poss.rules) ) {
ll.rule.string{k}(poss.rules(r),i) <- logsumexp([log.alpha + hs.log.probs(poss.rules(r)) ...
                                                log.notalpha + log(hs.cardinalities(poss.rules(r))/N.s) + hs.log.probs(poss.rules(r))])
   } #    
   } #

# now product over rules
ll.rule{k} <- sum(ll.rule.string{k},2) + log(1/N.r)

# now sum for that cluster over all those that aren't -Inf
    ll.cluster(k) <- logsumexp(ll.rule{k}(~isinf(ll.rule{k})))
     } #
  
  # now product over clusters
  loglike <- sum(ll.cluster)
   } #
# wrapper function for the crp prior

 computeCRP <-function( c,params) {  prior 

# get the size of each class
for (  i  in  1 : max(c) ) {
  class_sizes(i) <- sum(c==i)
   } #

prior <- crp(class_sizes, params_gamma)

#  crp <-function( ns, gamma)  {  l 
# probability of the partition in ns under a CRP with concentration parameter
# gamma (note that gamma here is NOT the gamma function but just a number)
#
# Provided by Charles Kamp 2006

 crp <-function( ns, gamma)  {  l 

ns<-ns(ns~<-0) # only consider classes that are not empty
k <- length(ns) # number of classes
n <- sum(ns) # number of samples
l <- sum(gammaln(ns))+k*log(gamma)+gammaln(gamma)-gammaln(n+gamma) 

# choose a class randomly proportional to marginal probability

 chooseClass <-function( new.scores) {  nc 


nc <- c[find(rand<cumPs,1,'first')]

# make sure that the set of classes is always contiguous (otherwise we will
# orphan some classes

 cleanUpClasses <-function( c) {  c 

u <- unique(c) # if a class became empty then there will be hole in u

for (  k  in  1 : length(u) ) {
  if u(1) == 0    
    c(c==u(k)) <- k-1
  else
    c(c==u(k)) <- k
     } #
   } #



