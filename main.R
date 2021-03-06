whereAmI <- function() attr(attr(whereAmI, 'srcref'), 'srcfile')$filename
setwd(dirname(whereAmI()))
source("code/functions.R")
source("code/mediateStep.r")
source("code/preparations.R")
source("code/compLik.r")


train = list(c(1, 1 ,4),c(1, 1 ,5),c(2, 2 ,4),c(2, 2, 6),c(3,3,5),c(3, 3, 6),c(1, 4, 1),c(1, 5, 1),c(2, 4, 2),c(2, 6, 2),c(3, 5, 3),c(3 ,6 ,3))


alpha = .9     # memory noise parameter
gammaval = 0.2    # parameter for chinese restaurant process
num_iter = 100 # number of gibbs steps

hs<-createHypothesisSpace(hs)
hs<-cacheCardinalities(hs)
index_cache = cacheItems(hs,train) 
out<-addNoiseToTraining(hs,train,alpha,index_cache)
train<-out[[1]]
index_cache<-out[[2]]
## use gibbs sampler to find cluster assignment

# initialize with every sentence in its own cluster
n = length(train);
c = 1:n; 
c3<- matrix(0,n,num_iter)


# begin gibbs iterations
for(i  in  1:num_iter) {
    cat(sprintf('gibbs step %i',i))
    for (j  in  1:n){
      
      cat(sprintf('.'))
      if(j%%50==0){cat('\n')}  
      ll <- vector()
      prior <- vector()
      new_scores <-vector()
      #try the assignment of this string to every cluster (inc. its own)
      c2 <- vector(mode="list", length=(max(c)+1))
    
      for (l  in  1:(max(c) +1)){
           c2[[l]]<-c
           c2[[l]][j]<-l;
           ll[l] <- compLik(hs,c2[[l]],train,alpha,index_cache)
           prior[l] <- computeCRP(c2[[l]],gammaval)
           new_scores[l] <- (prior[l] + ll[l])
           }
      
      # gibbs step: jump proportional to the relative probability   
      c[j] <- chooseClass(new_scores)
      c <- cleanUpClasses(c)
      #cat(c, cat('\n'))
    }
    
  #wizualizacja i inne  takie  
  cat('\n')
  c3[,i]<-sort(c)
  image(t(c3),col=rainbow(12))
  displayOutput( c,train,hs,alpha,gammaval,index_cache)
  cat('\n') 
}
