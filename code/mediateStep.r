cacheItems <- function(hs, train){

# consolidate all strings
if(all((length(hs[["test_vocab"]])==length(hs[["train_vocab"]]))&(hs[["train_vocab"]][1]==hs[["test_vocab"]][1])))
{all_strings <- hs[["all_strings"]]
 true_of <- hs[["true"]]}

ic <- vector(mode="list") 
ic[["train"]]<-rep(0,length(train))
# items <- c(correct, incorrect)
# correct =  list(c(1,1,6))
# incorrect = list(c(1,6,1))
#ic[["items"]]<-rep(0,length(items))

for (i in 1:length(train)){
  ic[["train"]][[i]] <- findString(train[[i]],all_strings)
} 
  
# for (i  in  1:length(items)){
#     ic[["items"]][[i]] <- findString(items[[i]],all_strings)
# } 
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
                        
out<- vector(mode="list")
out[[1]]<-train
out[[2]]<-index_cache
return(out)                                                            } #  
} #

