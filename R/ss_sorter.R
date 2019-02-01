#'Sort a list of data by time
#'
#'I'm certain there's a better way to do this...
#'@param a_list The data to be sorted
#'@name ss_sorter
#'@export

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
  index<-1
  for(i in 1:length(times)){
    sorted[[i]]<-a_list[[sorter$V1[i]]]
  }
  return(sorted)
  
}