#'Read in and aggregate all files in the folder
#'
#'Given a file location, reads in all data files and collect them into lists based on file name patterns
#'The patterns are the numeric appendix on all files based on trace number
#'@param location The FOLDER location
#'@param ecspattern The default trace number for ECS data
#'@param ps2pattern The default trace number for PS2 data
#'@param ps1pattern The default trace number for PS1 data
#'@param foppattern The default trace number for Fo' data
#'@name ss_read_all_folder
#'@export



ss_read_all_folder <- function(location, ecspattern = "0002",ps2pattern="0001",ps1pattern="0003",foppattern="0004"){
  require(tidyverse)
  require(magrittr)
  setwd(location)
  file.names.ecs <- as.list(dir(location,pattern=ecspattern))
  data.ecs <- lapply(file.names.ecs, ss_read)
  file.names.ecs <- sub(pattern="(.*?)\\.dat(.*)000.*$",replacement = "\\1 \\2",x=file.names.ecs)
  names(data.ecs) <- file.names.ecs
  set_tidy_names(data.ecs)
  
  file.names.ps2 <- as.list(dir(location,pattern=ps2pattern))
  data.ps2 <- lapply(file.names.ps2, ss_read)
  file.names.ps2 <- sub(pattern="(.*?)\\.dat(.*)000.*$",replacement = "\\1 \\2",x=file.names.ps2)
  names(data.ps2) <- file.names.ps2
  set_tidy_names(data.ps2)
  
  
  file.names.ps1 <- as.list(dir(location,pattern=ps1pattern))
  data.ps1 <- lapply(file.names.ps1, ss_read)
  file.names.ps1 <- sub(pattern="(.*?)\\.dat(.*)000.*$",replacement = "\\1 \\2",x=file.names.ps1)
  names(data.ps1) <- file.names.ps1
  set_tidy_names(data.ps1)
  
  
  file.names.fop <- as.list(dir(location,pattern=foppattern))
  data.fop <- lapply(file.names.fop, ss_read)
  file.names.fop <- sub(pattern="(.*?)\\.dat(.*)000.*$",replacement = "\\1 \\2",x=file.names.fop)
  names(data.fop) <- file.names.fop
  set_tidy_names(data.fop)
  
  return(list(ps1=data.ps1,ps2=data.ps2,ecs=data.ecs,fop=data.fop))
}