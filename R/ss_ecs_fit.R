#' Performs fitting of ECS traces
#'
#' Given a single trace, generates vH+, gH+, ECSt, and optionally A520 baseline.
#' Uses minpack.lm for fitting
#'
#' @param dataframe Single trace ECS data point.
#' @param recalc_delta_a Do you wish to recalculate DeltaA? Calls ss_bookkeeping
#' @param graph Boolean. Recreates the trace with fitting visualized. A list of graphs is returned.
#' @param linFitCount Integer. Number of points used for linear fitting for vH+.
#' @param nonlinFitCount Integer. Number of points used for nonlinear fitting for pmf and gH+.
#' @param remake Boolean. Improves the detail of recreated graph. Data is untransformed.
#' @param highWeightVal Integer. The amount to weight high-weight points at the start of the dirk.
#' @param highWeightCount Integer. The number of points to weight highly at the start of the dirk.
#' @param baselineStart Integer. Data point startpoint for recalculating deltaA
#' @param baselineEnd Integer. Data point endpoint for recalculating DeltaA
#' @param abs520 Boolean. Would you like automatic calculation of baseline 520nm absorbance?
#' @param linadj Boolean. Attempts to correct for signal drift by straightening out non-DIRK part of the trace
#' @param dirkstart Integer. The data point count of the last illuminated point.
#'
#' @name ss_ecs_fit
#' @export



ss_ecs_fit <- function(dataframe, recalc_delta_a = F, graph=F, linFitCount=5,nonlinFitCount=35, remake=F,baselineStart=50,baselineEnd=99,abs520=F,linadj=T,dirkstart = 100, highWeightVal = 10000, highWeightCount = 0,dirklen=NA,fixTime=T){
  #require(tidyverse)
  #require(magrittr)
  if(is.na(dirklen)){
    dirklen <- nonlinFitCount + 30
  }
  require(minpack.lm)
  #bookkeeping: rename the items in the dataframe
  dataframe <- ss_bookkeeping(dataframe,recalc_delta_a = recalc_delta_a,baselineStart=baselineStart,baselineEnd=baselineEnd)
  TheTime <- dataframe$Time[dirkstart]
  a520 <- mean(dataframe$Raw_Voltage[1:(dirkstart-1)])
  #Do a linear regression for the baseline and apply it.
  #First, set time = 0
  dfbu <- dataframe
  dataframe$Time <- dataframe$Time - dataframe$Time[1]
  if(fixTime){
    tEa <- (dataframe$Time[nrow(dataframe)] - dataframe$Time[1]) / nrow(dataframe)
    newTime <- 1:(nrow(dataframe)) * tEa
    dataframe$Time <- newTime
  }
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
  dat.mid <- dataframe[dirkstart:(dirkstart+dirklen),]
  #reset time to zero.  This is really important since we have no time offset.
  dat.mid$Time <- dat.mid$Time - dat.mid$Time[1]
  diff <- mean(dataframe$DeltaA[(dirkstart-20):dirkstart])
  dat.mid$DeltaA[1] <- diff
  dataframe$DeltaA[[dirkstart]] <- diff
  dat.mid$DeltaA <- dat.mid$DeltaA - diff
  dataframe$DeltaA <- dataframe$DeltaA - diff

  x.dat.nl <- dat.mid$Time[1:nonlinFitCount]
  y.dat.nl <- dat.mid$DeltaA[1:nonlinFitCount]
  #make a reasonable estimate for pmf
  pmfguess <- diff
  #fit 1: used to fit PMF + cond
  coefs <- tryCatch({
    #do one fit for PMF
    m1 <- nlsLM(data=dat.mid,DeltaA ~ principal * exp(Time * -1 * rate) + constant,start=c(principal=pmfguess,rate=3, constant = diff),upper=(c(1,10000,.3)),lower=c(-1,0,-.3),control=nls.lm.control(maxiter=1000),trace = F,weights=c(100000,rep(1,nrow(dat.mid)-1)))
    PMF <- coef(m1)['principal']
    #PMF <- coef(nlsLM(data=dat.mid,DeltaA ~ principal * exp(Time * -1 * rate) + constant,start=c(principal=pmfguess,rate=3, constant = diff),upper=(c(1,10000,.3)),lower=c(-1,0,-.3),control=nls.lm.control(maxiter=1000),trace = F,weights=c(1000000,rep(1,nrow(dat.mid)-1))))$principal
    #do a second fit for cond
    m2 <- nlsLM(y.dat.nl ~ PMF * exp(x.dat.nl * -1 * rate) + constant,start=c(rate=3, constant = diff),upper=(c(10000,.3)),lower=c(0,-.3),control=nls.lm.control(maxiter=1000),trace = F,weights=c(highWeightVal,rep(highWeightVal, highWeightCount),rep(1,nonlinFitCount-1-highWeightCount)))
    c(PMF,coef(m2))
    
  }, error = function(e){
    print("There was an error: The nonlinear fit failed. Make sure you are sending the right points, and that DeltaA has been calculated correctly (try ss_bookkeeping(recalc_delta_a=T)")
    print(e)
    error_fit <- c(0,0,0)
    error_fit
  }
  )
  #do a second fit, a linear fit to get the velocity.
  x.dat.l <- dat.mid$Time[1:linFitCount]
  y.dat.l <- dat.mid$DeltaA[1:linFitCount]
  lin <- lm(formula = y.dat.l ~ x.dat.l,weights=c(1000000,rep(1,linFitCount-1)))
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
    hurt2 <- data.frame(DeltaA = newY2, Time= x.dat.l)
    #coef(nonlin)[1]
    annotations <- data.frame(Time=c(.05,.15),DeltaA=c(.5*(max(dat.mid$DeltaA)-min(dat.mid$DeltaA)),.75*(max(dat.mid$DeltaA)-min(dat.mid$DeltaA))),txt=c(paste0("Principal = ",format(coefs[1],digits=4)),paste0("Velocity = ",format(coef(lin)[2],digits=4))))

    myplot <- ggplot2::ggplot()+
      ggplot2::geom_point(dat.mid,mapping=ggplot2::aes(x=Time,y=DeltaA,col="Actual"))+
      ggplot2::geom_point(hurt2,mapping=ggplot2::aes(x=Time,y=DeltaA,col="Linfit (Rate)"),alpha=0.3)+
      ggplot2::geom_line(hurt2,mapping=ggplot2::aes(x=Time,y=DeltaA,col="Linfit (Rate)"),alpha=0.3)+
      ggplot2::geom_point(hurt,mapping=ggplot2::aes(x=Time,y=DeltaA,col="Nonlin Fit (PMF)"),alpha=0.2)+
      ggplot2::geom_line(hurt,mapping=ggplot2::aes(x=Time,y=DeltaA,col="Nonlin Fit (PMF)"),alpha=0.2)+
      ggplot2::geom_text(annotations,mapping=ggplot2::aes(x=Time,y=DeltaA,label=txt))+
      ggplot2::scale_color_discrete()+
      ggplot2::ggtitle("Fit curve and original")
    if(remake){
      myplot <- myplot+ ggplot2::geom_point(postmovements,mapping=ggplot2::aes(x=Time-postmovements$Time[dirkstart],y=DeltaA,col="postmove"))
    }
    return(list(these_coefs,myplot))
  } else{
    return(these_coefs)
  }
}
