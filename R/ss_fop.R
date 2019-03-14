#' Attempt to work up Fo' data
#'
#' Given a list of PS2 data, generate Fm', fs, phi2, and fo'
#' Assumes a lot about the shape of your curve.
#' @param ps2_data A list of PS2 traces
#' @param graph Boolean. Reconstitutes a graph with chosen points as lines. Default F. Used to check the accuracy of the program.
#' @name ss_fop
#' @export

ss_fop <- function(ps2_data,graph=F,flashstartpoint=100,flashendpoint=190){
  #ugh... I don't even remember everything I'm supposed to get from this.
  #so you start with steady state, then flash and turn on farred, then get fo' from the bottom at the end
  #but I keep changing the dark duration... and I take some points with the restoration of light at the edn
  #so I can't just take the last couple points
  #it's necessary to tell the instrument to turn off farred and turn the light back on
  #so it can't be fixed from the script side
  #to get the data proper here, I think I need to do something similar to how I take data for Phi2
  if(is.data.frame(ps2_data)){
    ps2_data <- list(ps2_data)
  }
  #start with boilerplate code from the ps2 calc above
  ps2_data <- lapply(ps2_data, ss_bookkeeping)
  
  getfm <- function(df){
    #take the max value - try to get it sloping up or sloping down
    maxindex <- which.max(df$Raw_Voltage)
    if (maxindex<(flashstartpoint+15)){
      maxindex <- flashstartpoint+15
    }else if (maxindex >(flashendpoint-15)){
      maxindex <- (flashendpoint-15)
    }
    return(mean(df$Raw_Voltage[(maxindex-5):(maxindex+5)]))
  }
  
  getfo <- function(df){
    #take the min value
    
    minindex <- which.min(df$Raw_Voltage)

    return(mean(df$Raw_Voltage[(minindex-4):(minindex+1)]))
    #be really cautious here - I can't do the same +5-5 thing I do with fm, because the
    #length of the dark period is very variable
    #so, average backwards - should be higher, so I cut down on the number of averaged points.
  }
  closure <- function(df){
    fm <- getfm(df)
    fs <- mean(df$Raw_Voltage[1:(flashstartpoint-1)])
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
