#' A more rigid application of ss_runsetter
#' 
#' Accepts a dataframe and creates a list of dataframes divided into even chunks based on splitint
#' Useful for appended homogenous traces
#' @param df The dataframe to split
#' @param splitint The length of each individual measurement
#' @name ss_splitfun 
#' @export

require(tidyverse)
require(magrittr)
ss_splitfun <- function(df, splitint=250){
  datalist <- c()
  count <- nrow(df)/splitint
  if(count%%1 != 0){
    print("Your splitint is wrong")
    return("")
  }
  for(i in 1:count){
    datalist[[i]] <- df[((i-1)*splitint+1):(i*splitint),]
  }
  return(datalist)
}