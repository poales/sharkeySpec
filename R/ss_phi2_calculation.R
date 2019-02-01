#'Calculate Phi2 from PS2 traces
#'
#'Vectorized function. Apply it to a list of traces.
#'TODO: Allow single traces. Until then use ss_phi2_calculation(list(datum))
#'@param ps2_data The list of traces to calculate data from
#'@param graph Boolean. Will create a graph of the traces with lines drawn for Fm' and Fs. Used to double-check that the data is being automatically calculated correctly
#'@param flashstartpoint The data point at which the saturating flash begins. Default: 100
#'@param flashendpoint The data point at which the saturating flash ends. Default: 200
#'@name ss_phi2_calculation
#'@export


ss_phi2_calculation <- function(ps2_data,graph=F,flashstartpoint=100,flashendpoint=200){
  ps2_data %<>% lapply(ss_bookkeeping)
  #goal: lapply a closure over a function, with the closure creating a function that we immediately call
  #what is the purpose of the closure?
  #what will the function do?
  #our issue from above: we can't make it put fm and fs on the graph.
  #the closure will create an environment where we save fm and fs
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
  closure <- function(df){
    fm <- getfm(df)
    fs <- mean(df$Raw_Voltage[1:flashstartpoint])
    phi2 <- 1-(fs/fm)
    time <- df$Time[flashstartpoint]
    graphmaker <- function(graph){
      if(graph){
        mygraph <- ggplot(df,mapping=aes(x=Time,y=Raw_Voltage))+
          geom_point()+
          geom_hline(yintercept = fm)+
          geom_hline(yintercept=fs)+
          ggtitle(paste("Fm =",fm,"Fs =",fs))
        return(list(list(Phi2=phi2,Time=time,Fm=fm,Fs=fs),mygraph))
      } else{
        return(list(Phi2=phi2,Time=time,Fm=fm,Fs=fs))
      }
    }
    return(graphmaker)
  }
  alldat <- lapply(ps2_data,function(x) closure(x)(graph=graph))
  if(graph){
    return(list(bind_rows(lapply(alldat,function(x) x[[1]])),lapply(alldat,function(x) x[[2]])))
  }else{
    return(bind_rows(alldat))
  }
}