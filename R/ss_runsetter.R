#' A narrow function to read through an appended dataset and attempt to automatically split it up.
#'
#' Use on appended data files where multiple measurements are made in one file. Will create an "ID" column for each individual measurement, as determined by threshhold.
#' When the difference in the chosen column is greater than the provided threshhold it will decide that it is a new measurement.
#' @param df The data table (singular) to analyze
#' @param threshhold The difference in the chosen column that is to be considered a new measurement.
#' @param aslist When FALSE, returns a dataframe with an ID column that distinguishes separate measurements. When TRUE, returns the same but divided into a list, with one element per measurement.
#' @param column The chosen column to study for differences in threshhold. Defaults to "Time," used by me to differentiate different measurements taken in the same folder but separated by time.
#' @name ss_runsetter
#' @export



ss_runsetter<- function(df,threshhold=2,aslist = FALSE,column = "Time"){
  #scan through the dataframe and look for gaps
  #set gaps to be the break in runs with an ID column
  #first test for a time column - if none exist, send the df thru bookkeeping
  if(!(column %in% colnames(df))){
    cat("Error setting runs: Chosen column name not in df")
    return("")
  }
  if("ID" %in% colnames(df)){
    cat("Error setting runs: ID column already extant in df")
    return("")
  }
  TDIFF <- function(df){
    diff <- df[-1,column]-df[-nrow(df),column]
    changes <- which(diff>threshhold | diff<(-1*threshhold))
    for(i in 1:length(changes)){
      df$ID[(changes[i]+1):length(df$ID)] <- df$ID[(changes[i]+1):length(df$ID)]+1
    }
    return(df)
  }
  df <- tibble::add_column(df,ID=1)
  df <- TDIFF(df)
  if(aslist){
    mylist <- list()
    for(i in unique(df$ID)){
      mylist[[i]] <- filter(df,df$ID==i)
    }
    df <- mylist
    #df <- lapply(df,function(x) select(x,-ID))
  }
  return(df)

}
