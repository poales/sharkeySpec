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
#' @param dirkstart Integer. The last illuminated data point. Default: 100.
#' @name ss_ecs_fit_all
#' @export


ss_ecs_fit_all <- function(ecs_list, recalc_delta_a = F, graph = F,linFitCount=5, nonlinFitCount=35,remake=F,baselineStart=70,baselineEnd=99,abs520=F,linadj=T,dirkstart=100){
  #ecs_list <- ss_sorter(ecs_list) #just in case they're out of order, we need to sort first. Otherwise the graphs will not match up.
  alldat <- lapply(ecs_list,function(x) ss_ecs_fit(dataframe=x,recalc_delta_a = recalc_delta_a,graph=graph,linFitCount=linFitCount,nonlinFitCount=nonlinFitCount,remake=remake,baselineStart=baselineStart,baselineEnd=baselineEnd,abs520=abs520,linadj=linadj,dirkstart=dirkstart))
  if(graph){
    allgraphs <- lapply(alldat,function(list) list[[2]])
    alldat <- lapply(alldat,function(list) list[[1]])
    for(i in 1:length(allgraphs)){
      allgraphs[i][[1]] <- allgraphs[i][[1]] + labs(subtitle = paste("Fit number",i))
    }
  }
  if(abs520){
    velo <- dplyr::bind_rows(lapply(alldat, function(x) c(x[5], x[4], x[2], x[1], x[6])))
    velocity <- dplyr::rename(velo, "Velocity" = `vH+`, "Conductivity" = gH)
  }else{
    velo <- dplyr::bind_rows(lapply(alldat, function(x) c(x[5], x[4], x[2], x[1])))
    velocity <- dplyr::rename(velo, "Velocity" = `vH+`, "Conductivity" = gH)
  }
  #velocity<- velocity[order(velocity$Time),]
  if(graph){
    return(list(velocity,allgraphs))
  } else {
    return(velocity)
  }
}
