#' Narrow function that attempts to correlate gas exchange and SharkeySpec data.
#'
#' Requries minpack.lm, reshape2, and readLicorData packages.
#' @param geLoc location of gas exchange data file.
#' @param ssLoc Folder location of Sharkeyspec data
#' @param dirklen Length of DIRKS in data points
#' @param satflashlen Length of saturating flashes in data points
#' @param timeoffset Difference between elapsed time on the li-cor and the sharkeyspec. Subtracted from li-cor elapsed time
#' @param graph Boolean. Recreate trace graphs for PS2 and ECS data
#' @param remake Boolean. Increase the detail level provided in the graph
#' @param baseline Boolean. Provide baseline A520 measurements.
#' @name ss_full_workup
#' @export

ss_full_workup <- function(geLoc,ssLoc,dirklen,satflashlen,timeoffset,graph=F,remake=F,baseline=F,recalc=T){
  #require(tidyverse)
  #require(magrittr)
  #require(reshape2)
  library(readLicorData)
  gedat <- dplyr::rename(dplyr::select(licorData(geLoc),"A","elapsed"),Time=elapsed)

  gedat$Time <- gedat$Time - timeoffset
  ssdat <- ss_read_all_folder(ssLoc)
  if(((length(ssdat$ecs[[1]]$X1)%%dirklen) != 0) | (length(ssdat$ecs[[1]]$X1) < dirklen)){
    cat("I think your dirklen is incorrect")
    cat("\n",length(ssdat$ecs[[1]]$X1))
    cat("\n",length(ssdat$ecs[[1]]$X1)%%dirklen)
    return()
  }
  ecsdat <- lapply(ssdat$ecs, function(x) ss_splitfun(x,dirklen))
  ecsdat <- lapply(ecsdat, function(x) ss_ecs_fit_all(x,graph=graph,remake=remake,abs520=baseline,recalc_delta_a=recalc))
    #lapply(function(x) ss_bookkeeping(x,recalc)) %>%
    #lapply(function(x)ss_splitfun(x,dirklen)) %>%
    #lapply(function(x) ss_ecs_fit_all(x,graph=graph,remake=remake,abs520=baseline,recalc_delta_a=recalc))
  if(graph){
    ecsgraphs <- lapply(ecsdat,function(x) x[[2]])
    ecsdat <- lapply(ecsdat,function(x) x[[1]])
  }
  fitdat <- rbind_all(ecsdat)
  fitdat <- fitdat[order(fitdat$Time),]

  if(length(ssdat$ps2[[1]]$X1)%%satflashlen != 0 | length(ssdat$ps2[[1]]$X1) < satflashlen){
    cat("I think your satflashlen is incorrect")
    return()
  }
  phi2dat <- lapply(ssdat$ps2,ss_bookkeeping)
  phi2dat <- lapply(phi2dat, function(x) ss_splitfun(x,satflashlen))
  phi2dat <- lapply(phi2dat,function(x) ss_phi2_calculation(x,graph=graph))
  if(graph){
    phi2graphs <- lapply(phi2dat,function(x) x[[2]])
    phi2dat <- lapply(phi2dat, function(x) x[[1]])
  }
  phi2dat <- dplyr::rbind_all(phi2dat)
  #probably not saturating - be advised
  phi2dat <- dplyr::select(phi2dat,-Fm,-Fs)
  alldat <- dplyr::bind_rows(
    lapply(
      X=list(gedat,fitdat,phi2dat),
      FUN= function(x) reshape2::melt(x,id.vars="Time")
    )
  )
  myplot <- ggplot2::ggplot(alldat,mapping=aes(x=Time,y=value,col=variable))+
    ggplot2::geom_point()+
    ggplot2::facet_wrap(~variable,ncol=1,scales="free_y")
  if(graph){
    return(list(alldat,list(ps2=phi2dat,ecs=fitdat,ge= gedat),myplot,list(ps2=phi2graphs,ecs=ecsgraphs)))
  }else{
    return(list(alldat,list(ps2=phi2dat,ecs=fitdat,ge= gedat),myplot))

  }
}
