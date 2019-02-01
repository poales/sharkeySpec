#'Sort a list of data by time
#'
#'I'm certain there's a better way to do this...
#'@param a_list The data to be sorted
#'@name ss_sorter
#'@export
require(tidyverse)
require(magrittr)
ss_sorter <- function(a_list){
  #there has to be a better way to do this...
  indices <- c()
  times <- c()
  index <- 1
  for(df in a_list){
    indices %<>% rbind(index)
    index <- index+1
    time = unlist(df[150,1])
    times %<>% rbind(time)
  }
  sorter <- data.frame(X1=times,V1=indices)
  sorter <- sorter[order(times),]
  sorted <- a_list
  for (i in 1:length(times)) {
    index <- sorter$V1[i]
    sorted[[i]] <- a_list[[index]]
    names(sorted)[i] <- names(a_list)[index]
  }
  return(sorted)
  
}