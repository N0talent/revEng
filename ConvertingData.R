Pattern.KnitWithData<-function(performer, history,xBars,TF,timestamp=TRUE,netto=FALSE,Normalized=FALSE,rm.open=FALSE)
{
  #knit Found Pattern with Performer Data
  require(dplyr)
  require(xts)
  Pattern=NULL

  #Function um Absolutprices zu finden
  Findpattern.Absolut<-function(open,action)
  {
    #FindBars to Timestamp
    f<-index(history)<=open
    bars<-tail(history[f],xBars+1)
    bars<-bars[1:xBars]
    bars<-bars[,1:4]    
    
    
    #Normalized
    ATR<-1
    StartPrice<-0
    if(Normalized==TRUE)
    {
      ATR<-mean(coredata(bars[,2])-coredata(bars[,3]))
      bars<-data.frame(coredata(bars))
      barnames<-names(bars[2:4])
      StartPrice<-bars[1,1]
      
      if(action=="Sell")
      {
        #flip Highs with Lows
        f<-c(3,2,4)
        bars<-bars[,f]
        names(bars)<-paste(TF,barnames,sep=".")
        
        #Create Netto Prices & Scale 
        bars<-StartPrice-bars
        bars<-bars/ATR
      }
      else
      {
        #flip Highs with Lows
        f<-c(2:4)
        bars<-bars[,f]  
        names(bars)<-paste(TF,barnames,sep=".")
        
        #Create Netto Prices & Scale 
        bars<-bars-StartPrice
        bars<-bars/ATR
      }
    }
    else
    {
      bars<-data.frame(date=index(bars), coredata(bars))
      barnames<-names(bars)
      names(bars)<-paste(TF,barnames,sep=".")
    }
    
    #Transpo Bars 
    bars.transpo=NULL

    for (n in 1:nrow(bars))
    {
      bars.transpo<-c(bars.transpo,bars[n,])
    }
    
    #Pass Values
    if(Normalized==FALSE)
    {
      data.frame(bars.transpo)
    }
    else
    {
      data.frame(bars.transpo,ATR=ATR)
    }
    
  }
  
  n<-1
  for (n in 1:nrow(performer))
  {
    Pattern<-rbind(Pattern,Findpattern.Absolut(performer$open[n],performer$Action[n]))
  }

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
per<-per[1:10,]
# View(per)
# # #
 result<-Pattern.KnitWithData(per, data.History[["EURUSD"]][["M15"]],5,"M15",Normalized =  FALSE)
 View(result)


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
