#'Backup for phi2 data calculation
#'I don't think I need this any more but I'm keeping it around
#'Will give it namespace but not export it...
#'@name ss_phi2_calculation_bu

ss_phi2_calculation_bu <- function(ps2_data,graph=F){
  ps2_data %<>% lapply(ss_bookkeeping)
  nm <- seq(1:length(ps2_split))
  names(ps2_data) <- nm
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
  
  phi2 <- bind_cols(PhiPS2 = phi2,Time=time,Fs=fs, Fm=fm)
  if(graph){
    graphs <- lapply(ps2_data, function(x) ggplot(x,mapping=aes(x=Time,y=Raw_Voltage))+geom_point())
    return(list(phi2,graphs))
  }
  return(phi2)
}