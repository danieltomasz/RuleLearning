train = list(c(1, 1 ,4),c(1, 1 ,5),c(2, 2 ,4),c(2, 2, 6),c(3,3,5),c(3, 3, 6),c(1, 4, 1),c(1, 5, 1),c(2, 4, 2),c(2, 6, 2),c(3, 5, 3),c(3 ,6 ,3))

cacheItems <- function(hs, train){
correct =  list(c(1,1,6))
incorrect = list(c(1,6,1))
# consolidate all strings
if(all((length(hs[["test_vocab"]])==length(hs[["train_vocab"]]))&(hs[["train_vocab"]][1]==hs[["test_vocab"]][1])))
{all_strings <- hs[["all_strings"]]
 true_of <- hs[["true"]]}
items <- c(correct, incorrect)
ic <- vector(mode="list") 
ic[["train"]]<-rep(0,length(train))
ic[["train"]]<-rep(0,length(items))

for (i in 1:length(train)){
  ic[["train"]][[i]] <- findString(train[[i]],all_strings)
} 
  
for (i  in  1:length(items)){
    ic[["items"]][[i]] <- findString(items[[i]],all_strings)
} 
return(ic)
}



findString <-function(s,as) {      
for (  i  in  1:length(as)) {
    if(all(s[1] == as[[i]][1] & s[2]==as[[i]][2] & s[3]==as[[i]][3]))
       {
      return(i)}
  }
}

addNoiseToTraining <-function( hs,train,alpha,index_cache) {                                                            
for (i  in 1:length(train)) {
if (runif(1) > alpha){
  ind <- sample(1:length(hs[["all_strings"]]), 1) 
  train[[i]] <- hs[["all_strings"]][[ind]]
  index_cache[["train"]][i] <- ind  
}
                        
                                                                 #find(cellfun(@(x) all(train{i} == x),hs.all.strings))
return(c(train, index_cache))                                                            } #  
} #

