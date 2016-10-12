Pattern.FindPattern<-function(performer,history, xBars,TF,timestamp=TRUE,netto=FALSE,Normalized=FALSE,rm.open=FALSE)
{
  require(xts)
  #Find X bars at a Specific Date BEFORE entrie
  f<-index(history)<=performer
  bars<-tail(history[f],xBars+1)
  bars<-bars[1:xBars]
  bars<-bars[,1:4]
  
  
    
  NullPoint<-0
  ATR<-1
  
  if(Normalized==TRUE)ATR<-mean(coredata(bars[,2])-coredata(bars[,3]))
  if(timestamp==TRUE)
  {
    openCol<-1
    ATR<-c(1,rep(ATR,4))
    bars<-data.frame(date=index(bars), coredata(bars))
    if (netto==TRUE)
    {
      NullPoint<-c(0,rep(bars[1,1],4)) 
    }
  }
  else
  {
    openCol<-1
    bars<-data.frame(coredata(bars))  
    if (netto==TRUE)
    {
      NullPoint<-bars[1,1]
    }
  }

  
  bars.transpo=NULL
  closeCol<-ncol(bars)
  ohlc<-openCol:closeCol
  barnames<-names(bars)
  names(bars)<-paste(TF,barnames,sep=".")
  
  if(rm.open==TRUE)
  {
    name<-paste(TF,"Open",sep=".")
    bars.transpo<- bars[1,1]
    names(bars.transpo)<-name
    ohlc<-2:4
  }
  if(timestamp==TRUE)
  {
    for (n in 1:nrow(bars))
    {
      bars.transpo<-c(bars.transpo,(bars[n,ohlc]-NullPoint))
    }
  }  
  else
  {
    for (n in 1:nrow(bars))
    {
      bars.transpo<-c(bars.transpo,(bars[n,ohlc]-NullPoint)/ATR)
    }
  }
  
  if(Normalized==TRUE)bars.transpo<-c(bars.transpo,ATR=ATR)
  data.frame(bars.transpo)

}


Pattern.KnitWithData<-function(performer, history,xBars,TF,timestamp=TRUE,netto=FALSE,Normalized=FALSE,rm.open=FALSE)
{
  #knit Found Pattern with Performer Data
  require(dplyr)
  Pattern=NULL
  for (open in performer$open)
  {
    Pattern<-rbind(Pattern, Pattern.FindPattern(open, history,xBars,TF,timestamp,netto,Normalized,rm.open))
  }
  #pass new Performer Data
  performer<-cbind(performer,Pattern)
  
}



# # d<-Pattern.FindPattern(per$open,data.EURUSD.M15,5,"M15",2)
# # 
# # 
# per<-data.Performer.clean[100,]
# # #
# result<-Pattern.KnitWithData(per, data.EURUSD.M15,5,"M15",timestamp=TRUE,netto =  FALSE,Normalized =  FALSE,rm.open =  FALSE)
# View(result)
# # 
# # 
# # g<-mutate(data.Performer.clean[100:110,],openm15=Pattern.FindPattern(open,data.EURUSD.M15,5,"M15",2))






