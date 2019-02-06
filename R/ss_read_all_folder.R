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
  setwd(location)
  data.ecs <- ss_readType(location,ecspattern)
  data.ps2 <- ss_readType(location,ps2pattern)
  data.ps1 <- ss_readType(location,ps1pattern)
  data.fop <- ss_readType(location,foppattern)
  return(list(ps1=data.ps1,ps2=data.ps2,ecs=data.ecs,fop=data.fop))
}
