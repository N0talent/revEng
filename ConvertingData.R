Pattern.KnitWithData<-function(performer, history,xBars,TF,Normalized=FALSE)
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



##---------------------------------------------------------------------------------------------------------------
Pattern.Entries<-function(col,data)#pattern,Timeframe, xBars)
{
  #Filter Performer Data
  date<-as.POSIXct(data[,"open"])
    
  Hour<-format(date,"%H")
  
  Min<-format(date,"%M")
  f<-c("open","Symbol")
  
  #Columns for Pattern Data
  start<-col+2
  end<-ncol(data)
  pattern<-cbind(data[,f],Hour,Min, data[,start:end])
  
  #Pass Values
  pattern

}

##---------------------------------------------------------------------------------------------------------------
Pattern.CreateData<-function(performer,history,Symbol,Timeframe,Norm)
{
  pattern.AbsolutPrices<-NULL
  Performer.pattern<-NULL
  for (Sym in Symbol)  
  {
    Performer.pattern<- subset(performer,Symbol==Sym)
    for(t in Timeframe)
    {
      Performer.pattern<-Pattern.KnitWithData(Performer.pattern,
                                              history[[Sym]][[t]],
                                              xBars= Bars.Lookback,
                                              TF= t,
                                              Normalized =  Norm)
    }
    pattern.AbsolutPrices<-rbind(pattern.AbsolutPrices, Performer.pattern)
  }
  pattern.AbsolutPrices
}

##---------------------------------------------------------------------------------------------------------------


# # d<-Pattern.FindPattern(per$open,data.EURUSD.M15,5,"M15",2)
# # 
# # 
# g<-c("Symbol","Action")
# per<-subset(data.Performer.clean, Symbol=="EURUSD")
# per<-per[1:10,]
# # View(per)
# # # #
#  result<-Pattern.KnitWithData(per, data.History[["EURUSD"]][["M15"]],xBars=5,TF="M15",Normalized =  TRUE)
#  View(result)


# #select(result,Action=="Sell")
# 
# #result <-result[,Action=="Sell"]
# f<-result$Action=="Sell"
# #b<-function(x){result[f,v]<-1}
# 
# 
#View(result)

# d<-as.POSIXct(data.Performer.pattern.ScaledPrice[1:10,"open"])
# #d<-factor(d)
# d<-as.Date.POSIXct(d)
# format(d,"%H")
# print(d)

#View(data.Performer.pattern.ScaledPrice[1:10,"open"])

# ColumnsCleanData<-ncol(data.Performer.clean)
# d<-Pattern.Entries(ColumnsCleanData,data.Performer.pattern.ScaledPrice[1:10,])
# View(d)

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
