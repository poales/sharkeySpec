#' Attempt to work up Fo' data
#'
#' Given a list of PS2 data, generate Fm', fs, phi2, and fo'
#' Assumes a lot about the shape of your curve.
#' @param ps2_data A list of PS2 traces
#' @param graph Boolean. Reconstitutes a graph with chosen points as lines. Default F. Used to check the accuracy of the program.
#' @name ss_fop
#' @export

ss_fop <- function(ps2_data,graph=F){
  #ugh... I don't even remember everything I'm supposed to get from this.
  #so you start with steady state, then flash and turn on farred, then get fo' from the bottom at the end
  #but I keep changing the dark duration... and I take some points with the restoration of light at the edn
  #so I can't just take the last couple points
  #it's necessary to tell the instrument to turn off farred and turn the light back on
  #so it can't be fixed from the script side
  #to get the data proper here, I think I need to do something similar to how I take data for Phi2

  #start with boilerplate code from the ps2 calc above
  ps2_data <- lapply(ps2_data, ss_bookkeeping)
  getfm <- function(df){
    #take the max value - try to get it sloping up or sloping down
    maxindex <- which.max(df$Raw_Voltage)
    if (maxindex<115){
      maxindex <- 115
    }else if (maxindex >185){
      maxindex <- 185
    }
    return(mean(df$Raw_Voltage[(maxindex-5):(maxindex+5)]))
  }
  fs <- unlist(lapply(ps2_data,function(df)mean(df$Raw_Voltage[1:100])))
  fm <- unlist(lapply(ps2_data,getfm))
  phi2 <- 1-(unlist(fs)/unlist(fm))
  time <- unlist(lapply(ps2_data,function(df) df$Time[100]))
  getfo <- function(df){
    #take the min value
    minindex <- which.max(df$Raw_Voltage)

    return(mean(df$Raw_Voltage[(maxindex-4):(maxindex+1)]))
    #be really cautious here - I can't do the same +5-5 thing I do with fm, because the
    #length of the dark period is very variable
    #so, average backwards - should be higher, so I cut down on the number of averaged points.
  }
  fo <- unlist(lapply(ps2_data,getfo))
  phi2 <- dplyr::bind_cols(PhiPS2 = phi2,Time=time,Fs=fs, Fm=fm,Fo=fo)
  if(graph){
    graphs <- lapply(ps2_data, function(x) ggplot2::ggplot(x,mapping=ggplot2::aes(x=Time,y=Raw_Voltage))+ggplot2::geom_point())
    return(list(phi2,graphs))
  }else{
    return(phi2)
  }
}
