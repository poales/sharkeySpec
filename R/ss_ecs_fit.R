#' Performs fitting of ECS traces
#'
#' Given a single trace, generates vH+, gH+, ECSt, and optionally A520 baseline.
#' Uses minpack.lm for fitting
#' @param dataframe Single trace ECS data point.
#' @param recalc_delta_a Do you wish to recalculate DeltaA? Calls ss_bookkeeping
#' @param graph Boolean. Recreates the trace with fitting visualized. A list of graphs is returned.
#' @param linFitCount Number of points used for linear fitting for vH+.
#' @param nonlinFitCount Number of points used for nonlinear fitting for pmf and gH+.
#' @param remake Boolean. Improves the detail of recreated graph. Data is untransformed.
#' @param baselineStart Baseline location for recalculating deltaA
#' @param baselineEnd Baseline location for recalculating DeltaA
#' @param abs520 Would you like automatic calculation of baseline 520nm absorbance?
#' @param linadj Boolean. Attempts to correct for signal drift by straightening out non-DIRK part of the trace
#' @name ss_ecs_fit
#' @export



ss_ecs_fit <- function(dataframe, recalc_delta_a = F, graph=F, linFitCount=5, nonlinFitCount=35, remake=F,baselineStart=50,baselineEnd=99,abs520=F,linadj=T,dirkstart = 100){
  #require(tidyverse)
  #require(magrittr)
  require(minpack.lm)
  #bookkeeping: rename the items in the dataframe
  dataframe <- ss_bookkeeping(dataframe,recalc_delta_a,baselineStart=baselineStart,baselineEnd=baselineEnd)
  TheTime <- dataframe$Time[dirkstart]
  a520 <- mean(dataframe$Raw_Voltage[1:(dirkstart-1)])
  #Do a linear regression for the baseline and apply it.
  #First, set time = 0
  dfbu <- dataframe
  dataframe$Time <- dataframe$Time - dataframe$Time[1]
  if(linadj){
    dat.y <- dataframe$DeltaA[baselineStart:baselineEnd]
    dat.x <- dataframe$Time[baselineStart:baselineEnd]
    linfit.temp <- lm(dat.y ~ dat.x)
    #predict(linfit.temp,list(dataframe$Time = dataframe$Time))
    dataframe$DeltaA <- dataframe$DeltaA - predict(linfit.temp,list(dat.x=dataframe$Time))

  }

  #now that the linear regression is applied, move the data up so minimum = 0
  dataframe$DeltaA <- dataframe$DeltaA - min(dataframe$DeltaA)
  #this part would be unnecessary if I would just use a constant at the end instead of counting on the bottom being zero
  #in fact that's what i should do.
  if(remake){
    postmovements <- dataframe
  }
  #clip middle
  dat.mid <- dataframe[dirkstart:(dirkstart+100),]
  #reset time to zero.  This is really important since we have no time offset.
  dat.mid$Time <- dat.mid$Time - dat.mid$Time[1]
  dat.mid$DeltaA[1] <- mean(dataframe$DeltaA[(baselineEnd-20):baselineEnd])
  dataframe$DeltaA[[dirkstart]] <- dat.mid$DeltaA[1]
  x.dat.nl <- dat.mid$Time[1:nonlinFitCount]
  y.dat.nl <- dat.mid$DeltaA[1:nonlinFitCount]

  #fit 1: used to fit PMF + cond
  coefs <- tryCatch({
    coef(nlsLM(y.dat.nl ~ principal * exp(x.dat.nl * -1 * rate) + constant,start=c(principal=.07,rate=50, constant = 0),upper=(c(1,150,.01)),lower=c(0,0,-.01),control=nls.lm.control(maxiter=1000),trace = F),weights=wfct(1-x.dat.nl))
    
    }, error = function(e){
      print("There was an error: The nonlinear fit failed. Make sure you are sending the right points, and that DeltaA has been calculated correctly (try ss_bookkeeping(recalc_delta_a=T)")
      print(e)
      error_fit <- c(0,0,0)
      return(error_fit)
    }
  )
  #do a second fit, a linear fit to get the velocity.
  x.dat.nl <- dat.mid$Time[1:linFitCount]
  y.dat.nl <- dat.mid$DeltaA[1:linFitCount]
  lin <- lm(formula = y.dat.nl ~ x.dat.nl)
  #coef(lin)
  #coef(nonlin)
  #nonlin2 <- nlsLM(y.dat.nl ~ coef(nonlin)[1] * exp(x.dat.nl * rate) + constant,start=c(rate=-20, constant = 0),control=nls.lm.control(maxiter=1000),trace = F)
  if(abs520){
    these_coefs <- dplyr::bind_cols("PMF"=coefs[1],"vH+" = -1*coef(lin)[2],constant=coefs[3], gH=coefs[2],Time=TheTime,Baseline=a520)
  }else{
    these_coefs <- dplyr::bind_cols("PMF"=coefs[1],"vH+" = -1*coef(lin)[2],constant=coefs[3], gH=coefs[2],Time=TheTime)
  }

  if(graph){
    newY= coefs[1] * exp(dat.mid$Time * -1 * coefs[2]) + coefs[3]
    hurt <- data.frame(DeltaA=newY,Time=dat.mid$Time)
    newY2 = predict(lin)
    hurt2 <- data.frame(DeltaA = newY2, Time= x.dat.nl)
    #coef(nonlin)[1]
    annotations <- data.frame(Time=c(.05,.15),DeltaA=c(.5*(max(dat.mid$DeltaA)-min(dat.mid$DeltaA)),.5*(max(dat.mid$DeltaA)-min(dat.mid$DeltaA))),txt=c(paste0("Principal = ",format(coefs[1],digits=4)),paste0("Velocity = ",format(coef(lin)[2],digits=4))))

    myplot <- ggplot2::ggplot()+ggplot2::geom_point(hurt,mapping=ggplot2::aes(x=Time,y=DeltaA,col="Nonlin Fit (PMF)"))+
      ggplot2::geom_line(hurt,mapping=ggplot2::aes(x=Time,y=DeltaA,col="Nonlin Fit (PMF)"))+
      ggplot2::geom_point(dat.mid,mapping=ggplot2::aes(x=Time,y=DeltaA,col="Actual"))+
      ggplot2::geom_point(hurt2,mapping=ggplot2::aes(x=Time,y=DeltaA,col="Linfit (Rate)"))+
      ggplot2::geom_line(hurt2,mapping=ggplot2::aes(x=Time,y=DeltaA,col="Linfit (Rate)"))+
      ggplot2::geom_text(annotations,mapping=ggplot2::aes(x=Time,y=DeltaA,label=txt))+
      ggplot2::scale_color_discrete()+
      ggplot2::ggtitle("Fit curve and original")
    if(remake){
      myplot <- myplot+ ggplot2::geom_point(dfbu,mapping=ggplot2::aes(x=Time-TheTime,y=DeltaA,col="Unfit graph"))+
        ggplot2::geom_point(postmovements,mapping=ggplot2::aes(x=Time-postmovements$Time[dirkstart],y=DeltaA,col="postmove"))
    }
    return(list(these_coefs,myplot))
  } else{
    return(these_coefs)
  }
}
