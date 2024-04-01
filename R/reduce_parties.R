# This function reduces the number of affilated parties to just one.

 reduce_parties <- function(data, var){
  index <- which(sapply(data[[var]], function(x) length(x)>1))
  for(i in 1:length(index)){
    suppressWarnings(data[[var]][index[i]] <- intersect(unlist(data[index[i], var]), rule))
    p <- (i/length(index))*100
    print(paste0(round(p,2), "%"))
  }
  return(data)
}
