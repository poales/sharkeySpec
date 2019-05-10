#'Carves a dataframe into a list based on time indices.
#'
#'Given a data frame, carves a list into chunks based on vectors of times.
#'Functionally vectorizes dplyr::filter
#'@param df The dataframe to be split.
#'@param timer A vector of times to split the chosen column by, start of each interval.
#'@param timer2 A vector of times to split the chosen column by, end of each interval.
#'@name ss_timesplitter
#'@export


ss_timesplitter <- function(df,timer,timer2,column = "elapsed"){
  if(!(column %in% colnames(df))){
    cat("Error setting runs: Chosen column name not in df")
    return("")
  }
  list1 <- list()
  for(i in 1:length(timer)){
    list1[[i]] <- dplyr::filter(df,(!!as.name(column))<=timer[i] & (!!as.name(column)) >= timer2[i])
  }
  return(list1)
}