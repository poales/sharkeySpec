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



ss_ecs_fit <- function(dataframe, recalc_delta_a = F, graph=F, linFitCount=5, nonlinFitCount=35, remake=F,baselineStart=50,baselineEnd=99,abs520=F,linadj=T){
  require(tidyverse)
  require(magrittr)
  require(minpack.lm)
  #bookkeeping: rename the items in the dataframe
  dataframe <- ss_bookkeeping(dataframe,recalc_delta_a,baselineStart=baselineStart,baselineEnd=baselineEnd)
  TheTime <- dataframe$Time[100]
  a520 <- mean(dataframe$Raw_Voltage[1:99])
  #Do a linear regression for the baseline and apply it.
  #First, set time = 0
  if(remake){
    dfbu <- dataframe
  }
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
  dat.mid <- dataframe[100:150,]
  #reset time to zero.  This is really important since we have no time offset.
  dat.mid$Time <- dat.mid$Time - dat.mid$Time[1]
  dat.mid$DeltaA[1] <- mean(dataframe$DeltaA[81:100])
  dataframe$DeltaA[[100]] <- dat.mid$DeltaA[1]
  x.dat.nl <- dat.mid$Time[1:nonlinFitCount]
  y.dat.nl <- dat.mid$DeltaA[1:nonlinFitCount]
  
  #fit 1: used to fit PMF + cond
  nonlin <- nlsLM(y.dat.nl ~ principal * exp(x.dat.nl * -1 * rate) + constant,start=c(principal=.07,rate=50, constant = 0),upper=(c(1,150,.01)),lower=c(0,0,-.01),control=nls.lm.control(maxiter=1000),trace = F)
  
  #do a second fit, a linear fit to get the velocity.  
  x.dat.nl <- dat.mid$Time[1:linFitCount]
  y.dat.nl <- dat.mid$DeltaA[1:linFitCount]
  lin <- lm(formula = y.dat.nl ~ x.dat.nl)
  #coef(lin)
  #coef(nonlin)
  #nonlin2 <- nlsLM(y.dat.nl ~ coef(nonlin)[1] * exp(x.dat.nl * rate) + constant,start=c(rate=-20, constant = 0),control=nls.lm.control(maxiter=1000),trace = F)
  if(abs520){
    these_coefs <- bind_cols("PMF"=coef(nonlin)[1],"vH+" = -1*coef(lin)[2],constant=coef(nonlin)[3], gH=coef(nonlin)[2],Time=TheTime,Baseline=a520)
  }else{
    these_coefs <- bind_cols("PMF"=coef(nonlin)[1],"vH+" = -1*coef(lin)[2],constant=coef(nonlin)[3], gH=coef(nonlin)[2],Time=TheTime)
  }
  
  if(graph){
    newY= predict(nonlin,list(x.dat.nl=dat.mid$Time))
    hurt <- data.frame(DeltaA=newY,Time=dat.mid$Time)
    newY2 = predict(lin)
    hurt2 <- data.frame(DeltaA = newY2, Time= x.dat.nl)
    #coef(nonlin)[1]
    annotations <- data.frame(Time=c(.05,.15),DeltaA=c(.5*(max(dat.mid$DeltaA)-min(dat.mid$DeltaA)),.5*(max(dat.mid$DeltaA)-min(dat.mid$DeltaA))),txt=c(paste0("Principal = ",format(coef(nonlin)[1],digits=4)),paste0("Velocity = ",format(coef(lin)[2],digits=4))))
    
    myplot <- ggplot()+geom_point(hurt,mapping=aes(x=Time,y=DeltaA,col="Nonlin Fit (PMF)"))+
      geom_line(hurt,mapping=aes(x=Time,y=DeltaA,col="Nonlin Fit (PMF)"))+
      geom_point(dat.mid,mapping=aes(x=Time,y=DeltaA,col="Actual"))+
      geom_point(hurt2,mapping=aes(x=Time,y=DeltaA,col="Linfit (Rate)"))+
      geom_line(hurt2,mapping=aes(x=Time,y=DeltaA,col="Linfit (Rate)"))+
      geom_text(annotations,mapping=aes(x=Time,y=DeltaA,label=txt))+
      scale_color_discrete()+
      ggtitle("Fit curve and original")
    if(remake){
      myplot <- myplot+ geom_point(dfbu,mapping=aes(x=Time-dfbu$Time[100],y=DeltaA,col="Unfit graph"))+
        geom_point(postmovements,mapping=aes(x=Time-postmovements$Time[100],y=DeltaA,col="postmove"))
    }
    return(list(these_coefs,myplot))
  } else{
    return(these_coefs)
  }
}