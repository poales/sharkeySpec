#'Read a single file of data
#'
#'Mostly for internal use
#'@param location The path of data to use (relative or absolute)
#'@name ss_read
#'@export

ss_read <- function(location){
  #require(magrittr)
  #require(tidyverse)
  count <- round(mean(readr::count_fields(location,tokenizer = readr::tokenizer_tsv())))
  labs <- F
  if(count %% 4 == 0){
    count <- count %/% 4
    labs <- c("Time","Raw_Voltage","Ref","DeltaA")
    if(count>1){
      lb <- labs
      for(i in 2:count){
        lb <- c(lb,paste0(labs,"_LED",i))
      }
      labs <- lb
    }
  }
  suppressMessages(dat <- readr::read_delim(location,delim="\t",col_names = labs))
  
  
  
  return(dplyr::mutate_all(dat,as.numeric))
}
