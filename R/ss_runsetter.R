#' A narrow function to read through an appended dataset and attempt to automatically split it up.
#' 
#' Use on appended data files where multiple measurements are made in one file. Will create an "ID" column for each individual measurement, as determined by time threshhold.
#' When the difference in time is greater than the provided threshhold it will decide that it is a new measurement.
#' @param df The data table (singular) to analyze
#' @param threshhold The difference in time that is to be considered a new measurement.
#' @name ss_runsetter
#' @export

require(tidyverse)
require(magrittr)

ss_runsetter<- function(df,threshhold=2){
  #scan through the dataframe and look for gaps
  #set gaps to be the break in runs with an ID column
  require(magrittr)
  require(tidyverse)
  #first test for a time column - if none exist, send the df thru bookkeeping
  if(!("Time" %in% colnames(df))){
    df %<>% ss_bookkeeping()
  }
  TDIFF <- function(df){
    diff <- df$Time[-1]-df$Time[-length(df$Time)]
    changes <- which(diff>threshhold | diff<(-1*threshhold))
    for(i in 1:length(changes)){
      df$ID[(changes[i]+1):length(df$ID)] <- df$ID[(changes[i]+1):length(df$ID)]+1
    }
    return(df)
  }
  df %<>% add_column(ID=1) %>% TDIFF() %>% return()
  
}