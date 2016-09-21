Pattern.FindPattern<-function(performer,history, xBars,TF,ind)
{
  require(xts)
  #Find X bars at a Specific Date BEFORE entrie
  f<-index(history)<=performer
  bars<-tail(history[f],xBars+1)
  bars<-bars[1:xBars]

  #pass Bars
  #bars
  bars<-data.frame(date=index(bars), coredata(bars))
  bars.transpo=NULL
  
  barnames<-names(bars)
  names(bars)<-paste(TF,barnames,sep=".")
  
  for (n in 1:nrow(bars))
  {
        bars.transpo<-c(bars.transpo,bars[n,])
  }
  data.frame(bars.transpo)
}


Pattern.KnitWithData<-function(performer, history,xBars,TF)
{
  #knit Found Pattern with Performer Data
  require(dplyr)
  Pattern=NULL
  for (open in performer$open)
  {
    Pattern<-rbind(Pattern, Pattern.FindPattern(open, history,xBars,TF))
  }
  #pass new Performer Data
  performer<-cbind(performer,Pattern)
  
}



# d<-Pattern.FindPattern(per$open,data.EURUSD.M15,5,"M15",2)
# 
# 
# per<-data.Performer.clean[100,]
# # 
# # result<-Pattern.KnitWithData(per, data.EURUSD.M15,5,"M15")
# 
# 
# g<-mutate(data.Performer.clean[100:110,],openm15=Pattern.FindPattern(open,data.EURUSD.M15,5,"M15",2))






