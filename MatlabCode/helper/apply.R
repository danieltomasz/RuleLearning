# tests whether a rule is true of a string. gets used when caching
# cardinalities.

 applyRuleToString <-function( r,s) {  tf 

tf <- [0 0 0]

for (  i  in  1 : 3  ) {
  c <- ((i-1)*2)+1
  
  switch r{c}
    case 'na'
      tfs(i) <- 1
    case 'isa'
      tfs(i) <- r{c+1} == s(i)
    case '<-'
      tfs(i) <- s(r{c+1}) == s(i)
    case '>'
      tfs(i) <- s(r{c+1}) < s(i)
    case '<'
      tfs(i) <- s(r{c+1}) > s(i)
     } #
   } #

tf <- sum(tfs)==3
