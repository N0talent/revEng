Pattern.FindPattern<-function(performer,Action,history, xBars,TF,timestamp=TRUE,netto=FALSE,Normalized=FALSE,rm.open=FALSE)
{
  require(xts)
  #Find X bars at a Specific Date BEFORE entrie
  f<-index(history)<=performer
  bars<-tail(history[f],xBars+1)
  bars<-bars[1:xBars]
  bars<-bars[,1:4]
  
  print(factor(Action))
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
      NullPoint<-c(0,rep(bars[1,2],4)) 
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
    # name<-paste(TF,"Open",sep=".")
    # bars.transpo<- bars[1,1]
    # names(bars.transpo)<-name
    ohlc<-2:4
    if(Normalized==TRUE & Action=="Sell")ohlc<-c(3,2,4)
    
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
      if(Normalized==TRUE & Action=="Sell")
      {
        buff<-(NullPoint-bars[n,ohlc])/ATR
        bars.transpo<-c(bars.transpo,buff)
      }
      else
      {
        buff<-(bars[n,ohlc]-NullPoint)/ATR
        bars.transpo<-c(bars.transpo,buff)
      }
    }
  }
  
  if(Normalized==TRUE)bars.transpo<-c(bars.transpo,ATR=ATR)
  data.frame(bars.transpo)

  
  #testtestest
}


Pattern.KnitWithData<-function(performer, history,xBars,TF,timestamp=TRUE,netto=FALSE,Normalized=FALSE,rm.open=FALSE)
{
  #knit Found Pattern with Performer Data
  require(dplyr)
  Pattern=NULL

  #Function um Absolutprices zu finden
  Findpattern.Absolut<-function(open)
  {
    View(history)
    #FindBars to Timestamp
    f<-index(history)<=open
    bars<-tail(history[f],xBars+1)
    bars<-bars[1:xBars]
    bars<-bars[,1:4]    
    
    #Transpo Bars 
    bars.transpo=NULL
    barnames<-names(bars)
    names(bars)<-paste(TF,barnames,sep=".")
    
    
    for (n in 1:nrow(bars))
    {
      bars.transpo<-cbind(bars.transpo,(bars[n,]))

    }
    #print(bars.transpo)
    data.frame(bars.transpo)
  }
  n<-1
  # for (n in 1:nrow(performer))
  # {
 print(performer$open[n])
     Pattern<-rbind(Pattern,Findpattern.Absolut(performer$open[n]))
  #}
  
  print(Pattern)
  #pass new Performer Data
  performer<-cbind(performer,Pattern)
  
}

Pattern.Entries<-function(pattern,Timeframe, xBars)
{
 
  id<-""
  if(xBars>=2)
  {
    for (x in 1:(xBars-1))
    {
      id<-c(id,x) 
    }
  }
  
  col<-NULL
  for (n in id)
  {
    for(m in Timeframe)
    {
      #if(n==""){lables<-c("Open" ,  "High"  , "Low" ,   "Close")} else {lables<-c("High" , "Low" , "Close")}
      lables<-c("High" , "Low" , "Close")
      d<-paste(m,lables,sep=".")
      if(n!="")d<-paste(d,n,sep=".")
      col<-c(col,d)    
    }
  }  
  
  
  f<-pattern$Action=="Sell" 
  
  for (n in col)
  {
    pattern[f,n]<- pattern[f,n]*(-1)
  }
  
  pattern

}







# # d<-Pattern.FindPattern(per$open,data.EURUSD.M15,5,"M15",2)
# # 
# # 
# g<-c("Symbol","Action")
per<-subset(data.Performer.clean, Symbol=="EURUSD")
per<-per[1,]
 View(per)
# # #
 result<-Pattern.KnitWithData(per, data.History[["EURUSD"]][["M15"]],5,"M15",timestamp=FALSE,netto =  TRUE,Normalized =  TRUE,rm.open =  TRUE)
 #View(result)


# #select(result,Action=="Sell")
# 
# #result <-result[,Action=="Sell"]
# f<-result$Action=="Sell"
# #b<-function(x){result[f,v]<-1}
# 
# 
#View(result)


# #transform(result,result[,v]=-1)#result[result$Action=="Sell",v]*-1
# 
# View(result)
# # 
# # 
# # g<-mutate(data.Performer.clean[100:110,],openm15=Pattern.FindPattern(open,data.EURUSD.M15,5,"M15",2))
# 
# d<-data.EURUSD.M15[1000]
# v<-coredata(d)
# v-v[1]
# c<-1/v
# c-c[1]
