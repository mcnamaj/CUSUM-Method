seqsum <- function(x){
  cusum <<- vector("numeric",length(x))
  for(i in seq_along(x)){
    cusum[i]<<-sumfun(x,1,i)
  } 
}


CUSUM <- function(x){
  Zn <<- vector("numeric", length(x))
  seqsum(x)
  for(i in seq_along(x)){
    Zn[i] <<- (1/sqrt(length(x)))*((cusum[i])-((i/length(x))*sum(x)))
  }
  return(Zn)
  plot.ts(Zn)
}

CUSUM(combinedSequence)
 
rm(Mn)


Mn <- (1/var(combinedSequence))*(min(Zn))
Mn
