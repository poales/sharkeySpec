#' Vectorized fitting of ECS data.
#' 
#' Functionally applies ecs_fit for a vectorized set of data
#' Recommended application: ss_read_all_folder() %>% ss_splitfun() %>% (function(x) x$ecs) %>% ss_ecs_fit_all()
#' @param ecs_list vector of ecs data to analyze
#' @param recalc_delta_a Do you wish to recalculate DeltaA? Calls ss_bookkeeping
#' @param graph Boolean. Recreates the trace with fitting visualized. A list of graphs is returned.
#' @param linFitCount Number of points used for linear fitting for vH+.
#' @param nonlinFitCount Number of points used for nonlinear fitting for pmf and gH+.
#' @param remake Boolean. Improves the detail of recreated graph. Data is untransformed.
#' @param baselineStart Baseline location for recalculating deltaA
#' @param baselineEnd Baseline location for recalculating DeltaA
#' @param abs520 Would you like automatic calculation of baseline 520nm absorbance?
#' @param linadj Boolean. Attempts to correct for signal drift by straightening out non-DIRK part of the trace
#' @name ss_ecs_fit_all
#' @export

require(tidyverse)
require(magrittr)
require(minpack.lm)
ss_ecs_fit_all <- function(ecs_list, recalc_delta_a = F, graph = F,linFitCount=5, nonlinFitCount=35,remake=F,baselineStart=70,baselineEnd=99,abs520=F,linadj=T){
  
  ecs_list <- ss_sorter(ecs_list)
  alldat <- lapply(ecs_list,function(x) ss_ecs_fit(dataframe=x,recalc_delta_a = recalc_delta_a,graph=graph,linFitCount=linFitCount,nonlinFitCount=nonlinFitCount,remake=remake,baselineStart=baselineStart,baselineEnd=baselineEnd,abs520=abs520,linadj=linadj))
  
  
  
  if(!graph){
    times <- lapply(alldat,function(x) x[5]) %>% unlist()
    times <- unlist(times)
    conductivity <- lapply(alldat,function(x) x[4])
    conductivity <- unlist(conductivity)
    velocity <- lapply(alldat,function(x) x[2])
    velocity <- unlist(velocity)
    pmfs <- lapply(alldat,function(x) x[1])
    pmfs <- unlist(pmfs)
    if(abs520){
      a520s <- lapply(alldat,function(x) x[6]) %>% unlist()
      velocity <- data.frame("Time"=times,"Velocity" = velocity, "PMF" = pmfs, "Conductivity" = conductivity, "Baseline" = a520s)
    }else{
      velocity <- data.frame("Time"=times,"Velocity" = velocity, "PMF" = pmfs, "Conductivity" = conductivity)
    }
    velocity<- velocity[order(velocity$Time),]
    return(velocity)
  }else{
    
    ecscoefs <- lapply(alldat,function(list) list[[1]])
    times <- lapply(ecscoefs,function(x) x[5]) %>% unlist()
    times <- unlist(times)
    allgraphs <- lapply(alldat,function(list) list[[2]])
    
    velocity <- lapply(ecscoefs,function(datum) datum[2])
    velocity <- unlist(velocity)
    
    conductivity <- lapply(ecscoefs,function(x) x[4])
    conductivity <- unlist(conductivity)
    
    pmfs <- lapply(ecscoefs,function(datum) datum[1])
    pmfs <- unlist(pmfs)
    if(abs520){
      a520s <- lapply(ecscoefs,function(x) x[6]) %>% unlist()
      velocity <- data.frame("Time"=times,"Velocity" = velocity, "PMF" = pmfs, "Conductivity" = conductivity, "Baseline" = a520s)
    }else{
      velocity <- data.frame("Time"=times,"Velocity" = velocity, "PMF" = pmfs, "Conductivity" = conductivity)
    }
    #velocity <- data.frame("Velocity" = velocity, "PMF" = pmfs, "Time"=times,"Conductivity" = conductivity)
    velocity<- velocity[order(velocity$Time),]
    return(list(velocity,allgraphs))
  }
  
}