#' General usability function for raw data
#'
#' From a single trace data table, returns a data table with useful column names.
#' Optionally recalculates DeltaA if you feel it is necessary.
#' @param dataframe The data to be treated.
#' @param recalc_delta_a Boolean. Do you wish to recalculate DeltaA
#' @param baselineStart Baseline choices for recalculating DeltaA
#' @param baselineEnd Baseline choices for recalculating DeltaA
#' @name ss_bookkeeping
#' @export


ss_bookkeeping <- function(dataframe, recalc_delta_a = F,baselineStart=80,baselineEnd=99){
  if(ncol(dataframe)==4){
    colnames(dataframe) <- c("Time","Raw_Voltage","Ref","DeltaA")
  } else if(ncol(dataframe)==3){
    colnames(dataframe) <- c("Time","Raw_Voltage","DeltaA")
  } else if(ncol(dataframe==5)){
    colnames(dataframe) <- c("Time","Raw_Voltage","Ref","DeltaA","run")
  } else if(!("DeltaA" %in% colnames(dataframe)) | recalc_delta_a & !("Raw_Voltage" %in% colnames(dataframe))){
    cat("Error: dataframe has an invalid number of columns")
    return()
  }

  if(!("DeltaA" %in% colnames(dataframe))){
    dataframe <- tibble::add_column(dataframe,DeltaA=rep(-10,length(dataframe[,1])))
  }


  #first issue: make sure there is a deltaA column.
  if(dataframe$DeltaA[1]==-10 | recalc_delta_a){
    #DeltaA = -log(I/Io) = -log(sample/reference) where reference is a constant
    #first, calculate Io.  A good approximation is the baseline A520 just before doing shit to it
    Io <- mean(dataframe$Raw_Voltage[baselineStart:baselineEnd])
    dataframe$DeltaA <- -1 * log(dataframe$Raw_Voltage/Io)
  }
  return(dataframe)

}
