#'ss_listslicer
#'
#'Cuts up a df into two lists, based on two intervals provided. I.e., two sets of data, 
#'one 11 measurements, one 13 measurements, are repeated in a data frame. 
#'Cuts each of the 11 measurement sets and each of the 13 measurement sets out, 
#'groups them in a list, and returns the two lists.
#'@param df The dataframe to be sliced
#'@param r1 The first interval
#'@param r2 The second interval
#'@name ss_listslicer
#'@export

ss_listslicer <- function(df,r1,r2){
list1 <- list()
list2 <- list()
tester<- TRUE
index <- 1
i <- 1
len <- nrow(df)
while(tester){
  if((index+(r1-1))<=len){
    list1[[i]] <- df[index:(index+r1-1),]
    index <- index+r1
    if((index+(r2-1)) <= len){
      list2[[i]] <- df[index:(index+r2-1),]
      index <- index+r2
    }else{
      tester <- FALSE
      if(index<=len){
        list2[[i]] <- df[index:len,]
      }
    }
  }else{
    tester <- FALSE
    if(index<=len){
      list1[[i]] <- df[index:len,]
    }
  }
  i <- i+1
}
return(list(list1,list2))
}