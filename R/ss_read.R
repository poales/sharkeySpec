#'Read a single file of data 
#'
#'Mostly for internal use
#'@param location The path of data to use (relative or absolute)
#'@name ss_read
#'@export

ss_read <- function(location){
  require(magrittr)
  require(tidyverse)
  dat = read_delim(location,delim = "\t",col_names = F) %>% 
    mutate_all(as.numeric) %>% 
    return()
}