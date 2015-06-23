hs = list(F = list('na','isa','='),train_vocab = 1:6,test_vocab = 1:6)
train = list(c(1, 1 ,4),c(1, 1 ,5),c(2, 2 ,4),c(2, 2, 6),c(3,3,5),c(3, 3, 6),c(1, 4, 1),c(1, 5, 1),c(2, 4, 2),c(2, 6, 2),c(3, 5, 3),c(3 ,6 ,3))



createHypothesisSpace <-function(hs){    

# for each place find out what the primitives are
ps <- vector(mode="list", length=(3))
h3 <- vector(mode="list", length=(3))

for (i in 1:3){
ps[[i]] <- createPrimitiveSet(hs,i)
} 
# now iterating to create

h <- vector(mode="list", length=(length(ps[[2]])*length(ps[[2]])*length(ps[[3]])))

c <- 1
for (  i  in  1:length(ps[[1]])) {
  for (  j  in  1:length(ps[[2]])) {
    for (  k  in  1:length(ps[[3]]) ) {
      h[[c]] <-append(append(ps[[1]][[i]],ps[[2]][[j]]), ps[[3]][[k]])
      c <- c + 1
    } #
  } #
}
hs <-c(hs, list(hs=h))
return(hs)                                        
}

createPrimitiveSet <-function(hs,pos){

p <- vector(mode="list", length=(length(hs[["train_vocab"]])))
p[[1]] <- list('na',0) # always have the null element 
# I add + 1 beacause I want to start from second elemnt
for (i  in  1:(length(hs[["train_vocab"]]+1))){
  p[[i+1]] <- list('isa',hs[["train_vocab"]][i])                              
} 

for (j  in  3:length(hs[["F"]])){
inds <- 1:3
others <- inds[which(inds !=pos)] 
#append additional set elemts, use list(list()) to append new element as list
for (k in seq_along(others)){
  p <- append(p ,list(list(hs[["F"]][[j]],k)))}
}
return(p)
}   
cacheCardinalities <-function( hs) { 
all_strings <- vector(mode="list", length=(length(hs[["train_vocab"]])^3))
#wszystkie stringi w training vocalbulary
  c <- 1
  for (i in seq_along(hs[["train_vocab"]])){
    for (j in seq_along(hs[["train_vocab"]])){
      for (k in seq_along(hs[["train_vocab"]])){
  all_strings[[c]]<- c(i, j, k)
  c <- c + 1
} 
} 
} 
hs <-c(list(all_strings=all_strings),hs )

#dla każdej reguły - aplikuj do kazdego stringa


tic <- Sys.time()
cat('testing each rule against each string \n')
true_of <- matrix(FALSE,length(hs[["hs"]]),length(all_strings))
for (i in 1:length(hs[["hs"]])){
  cat(sprintf('%i',i),cat(' '))
  if(i %% 20==0){cat('\n')}
   for (j in 1:length(all_strings)){
  true_of[i,j] <- applyRuleToString(hs[["hs"]][[i]],hs[["all_strings"]][[j]])
} #
} 
toc<-Sys.time() - tic
cat('\n time elapsed ',toc)
tic<- Sys.time ()
cat('\n removing logically consistent hypotheses \n')
cat('this part can take as long as or longer than testing each rule against each string \n')
i <- 1
while (i < dim(true_of)[1]){
cat(sprintf('%i',i),cat(' '))
if(i %% 20==0){cat('\n')}
j <- i + 1
while (j < dim(true_of)[1]){
if (all(true_of[i,]==true_of[j,])){
true_of<- true_of[-j,]
hs[["hs"]]<-hs[["hs"]][-j]}
else{j <- j + 1}  
}                 
i <- i + 1
} 
toc<-Sys.time() - tic
cat('\n time elapsed ',toc)
cardinalities = rowSums(true_of)
probs_temp<- 1/cardinalities
probs_temp[!is.finite(probs_temp)] = 0
log_prob<-log(probs_temp)
hs <- c(list(true=true_of), hs )
hs <-c(list(log_probs=log_prob),hs)
hs <- c(list(card=cardinalities), hs )
save(hs, file="hs.saved")
#hs[["true"]] <-true_of
#),list(cardinalities=card),list(probs=probs_temp))
save(hs, file = "hs.rda")
return(hs)

}

applyRuleToString <-function( r,s){ 
  tfs <- c(0, 0, 0)
    for (i in 1:3) {
    c <- ((i-1)*2)+1
    type = r[[c]]
    switch(type,
           'na' = {tfs[i]=1},
           'isa' = {tfs[i]<-r[[c+1]]==s[i] },
           '=' = {tfs[i]<-s[r[[c+1]]]==s[i] })
    
    }
  return(sum(tfs)==3)
}


#c(train index_cache] = addNoiseToTraining(hs,train,params,index_cache)
