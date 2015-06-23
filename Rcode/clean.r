#save(c,file = "c.rda")
load(file = "c.rda")

u <- unique(c)
cat(c,cat('\n'))

for (k  in  1:length(u)){
  if (u[1] == 0){ 
    c[c==u[k]] <- (k-1)}
  else{
    c[c==u[k]] <- k
  } 
  
}
cat(c,cat('\n'))