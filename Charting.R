Chart.Find<-function(Bartime, history, xBars=0,includeBartime=FALSE,cutend=FALSE)
{
  require(xts)
  #Find Entry point
  f<-index(history)<=Bartime
  
  #Cut out last X bars
  if (xBars==0)
  {
    bars<-history[f]
    xBars<-nrow(bars)
  }
  else
  {
    bars<-tail(history[f],xBars+1)
    
  }
  
  #include Lastbar
  if (includeBartime==FALSE)
  {
    
    bars<-bars[1:xBars]
  }    
  
  #pass bars
  if (cutend==TRUE)
  {
    addBars<-history[!f]
    bars<-rbind(bars,addBars)
    bars
  }
  else
  {
    bars
  }
  
}

Chart.FindSegmentBar<-function(Bartime,history)
{
  #find Bar
  f<-index(history)<=Bartime  
  
  #pass Total Bars Found
  nrow(history[f])
}

d<-subset(data.Performer.clean,Symbol=="EURUSD")
End<- Chart.Find(d[200,"close"],data.EURUSD.M15,0,FALSE)
Start<-Chart.Find(d[200,"open"],End,5,TRUE,TRUE)




open<-d[200,"open"]
Open.Price<-d[200,"Open.Price"]
close<-d[200,"close"]
Close.Price<-d[200,"Close.Price"]
if (d[200,"Action"]=="Buy"){col="green"}else{col="red"}
chart_Series(Start)
X0<-Chart.FindSegmentBar(open,Start)
X1<-Chart.FindSegmentBar(close,Start)

segments(X0,Open.Price,X1,Close.Price,col=col)



