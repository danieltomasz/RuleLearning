ImportData <-function(){
  z <- matrix(readBin("/home/daniel/Pulpit/RuleLearning/MatFiles/a.bin", "double", 79056), c(366,216))
  hs<- matrix(readBin("/home/daniel/Pulpit/RuleLearning/MatFiles/hs.bin", "double", 366), c(1,366))
  card<- matrix(readBin("/home/daniel/Pulpit/RuleLearning/MatFiles/cardinalities.bin", "double", 366), c(1,366))
  
  true_of <- apply (z, c (1, 2), function (x) {
    (as.logical(x))
  })
  
  library(R.matlab)
  all_strings <- readMat('/home/daniel/Pulpit/RuleLearning/MatFiles/all_strings.mat')
  return(list(true_of,card, hs,all_strings))
}

createHypothesisSpace <-function(hs){    
  
  # for each place find out what the primitives are
  ps <- vector(mode="list", length=(3))
  h3 <- vector(mode="list", length=(3))
  
  for (i in 1:3){
    ps[[i]] <- createPrimitiveSet(hs,i)
  } 
  # now iterating to create
  for (  i  in  1 : length(ps[[3]]) ) {
    h3[[i]] <- ps[[3]][i]
  } 
  h2 <- vector(mode="list", length=(length(ps[[2]])*length(ps[[3]])))
  
  c <- 1
  for (i in 1:length(ps[[2]])) {
    for (j in 1:length(ps[[3]])) {
      h2[[c]] <-append(ps[[2]][[i]],ps[[3]][[j]])
      c <- c + 1
    } 
  }
  
  hu <- vector(mode="list", length=(length(ps[[2]])*length(ps[[2]])*length(ps[[3]])))
  
  c <- 1
  for (  i  in  1:length(ps[[1]])) {
    for (  j  in  1:length(ps[[2]])) {
      for (  k  in  1:length(ps[[3]]) ) {
        hu[[c]] <-append(append(ps[[1]][[i]],ps[[2]][[j]]), ps[[3]][[k]])
        c <- c + 1
      } #
    } #
  }
  return(hu)                                        
}