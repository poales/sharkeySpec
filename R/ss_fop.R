#' Attempt to work up Fo' data
#'
#' Given a list of PS2 data, generate Fm', fs, phi2, and fo'
#' Assumes a lot about the shape of your curve.
#' @param ps2_data A list of PS2 traces
#' @param graph Boolean. Reconstitutes a graph with chosen points as lines. Default F. Used to check the accuracy of the program.
#' @param fsend Integer. The last data point to be considered "Fs." Default 100
#' @param flashstartpoint Integer. The data point start of the saturation flash. Default 101
#' @param flashendpoint Integer. The terminal Fm' point. Default 190
#' @param terminalpts Integer. The number of points at the end of the trace during which far-red and actinic are both turned off. Default NULL. Only does something if lightterminal is also turned on.
#' @param lightterminal Integer. The number of points at the end of the trace during with actinic is turned on. Default NULL. Only does something if terminalpts is also provided
#' @name ss_fop
#' @export

ss_fop <- function(ps2_data,graph=F, fsend = 100,flashstartpoint=101,flashendpoint=190,terminalpts = NULL,lightterminal=NULL){

  if(is.data.frame(ps2_data)){
    ps2_data <- list(ps2_data)
  }
  #start with boilerplate code from the ps2 calc 
  ps2_data <- lapply(ps2_data, ss_bookkeeping)
  
  getfm <- function(df){
    #take the max value - try to get it sloping up or sloping down
    subset <- df$Raw_Voltage[flashstartpoint:flashendpoint]
    maxindex <- which.max(subset)
    if (maxindex<15){
      maxindex <- 15
    }else if (maxindex >(length(subset)-15)){
      maxindex <- (length(subset)-15)
    }
    return(mean(subset[(maxindex-5):(maxindex+5)]))
  }
  
  getfo <- function(df){
    #take the min value
    if(!is.null(terminalpts) & !is.null(lightterminal)){
      lmt <- nrow(df) - lightterminal -1
      lms <- lmt - terminalpts+1
      return(mean(df$Raw_Voltage[lms:lmt]))
    }
    minindex <- which.min(df$Raw_Voltage)

    return(mean(df$Raw_Voltage[(minindex-4):(minindex+1)]))
    #be really cautious here - I can't do the same +5-5 thing I do with fm, because the
    #length of the dark period is very variable
    #so, average backwards - should be higher, so I cut down on the number of averaged points.
  }
  closure <- function(df){
    fm <- getfm(df)
    fs <- mean(df$Raw_Voltage[1:(fsend-1)])
    fo <- getfo(df)
    phi2 <- 1-(fs/fm)
    time <- df$Time[flashstartpoint]
    graphmaker <- function(graph){
      if(graph){
        mygraph <- ggplot2::ggplot(df,mapping=ggplot2::aes(x=Time,y=Raw_Voltage))+
          ggplot2::geom_point()+
          ggplot2::geom_hline(yintercept = fm)+
          ggplot2::geom_hline(yintercept = fo)+
          ggplot2::geom_hline(yintercept=fs)+
          ggplot2::ggtitle(paste("Fm =",fm,"Fs =",fs,"Fo =",fo))
        return(list(list(Phi2=phi2,Time=time,Fm=fm,Fs=fs,Fo=fo),mygraph))
      } else{
        return(list(Phi2=phi2,Time=time,Fm=fm,Fs=fs,Fo=fo))
      }
    }
    return(graphmaker)
  }
  alldat <- lapply(ps2_data,function(x) closure(x)(graph=graph))
  if(graph){
    return(list(dplyr::bind_rows(lapply(alldat,function(x) x[[1]])),lapply(alldat,function(x) x[[2]])))
  }else{
    return(dplyr::bind_rows(alldat))
  }
}
