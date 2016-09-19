PerformerHistoryMerge<-function()
{
  # Open Datum mit in Histroy finden in TF X
  
  
  # X bars zurückschauen und dann Kopieren
  
  
  # Kopierte Bars Transponieren und in File
  
  
  
}


TimeSyncVis<-function(history, performer)
{
  #Generate a Random Entry
  performer<-performer[performer$Symbol=="EURUSD",]
  Randomentry<-floor(runif(1,min=200,max=nrow(performer)))  
  Open<-performer[Randomentry,1]
  OpenPrice<-as.double(performer[Randomentry,"Open.Price"])
  Close<-performer[Randomentry,2]
  ClosePrice<-as.double(performer[Randomentry,"Close.Price"])
  Action<-as.character(performer[Randomentry,"Action"])
  
  #Look for random Entry in History
  Entrypoint<-head(which(index(history)>=Open),1)-100
  Exitpoint<-head(which(index(history)>=Close),1)  
  x<-data.frame(Entrypoint,Exitpoint,OpenPrice,ClosePrice,Open,Close,Action)
  
  
  #Draw Chart
  if(x[7]=="Sell"){colL="red" }else colL="green"
  print(chart_Series(history[Entrypoint:Exitpoint,]))
  segments(100,as.double(x[3]),as.numeric(x[2])-as.numeric(x[1]),as.double(x[4]),col = colL,lwd=3)

  
  print(x)
  
}


RandomePAHistory<- function(history, performer)
{
  #filter History and Performer
  performer<-performer[performer$Symbol=="EURUSD",]
  earliestEntry<-min(performer$open)
  subset<-which(earliestEntry<index(history))-1
  history<-history[subset,]
  
  
  #Gernerate a Random Chart
  set.seed(1)
  RandomChartEnd<-floor(runif(1,500, max=nrow(performer)))
  RandomChartStart<-RandomChartEnd-300
  
  
  #Find all Entries in Chart
  f<-as.numeric(performer$open)>=index(history[RandomChartStart]) & as.numeric(performer$open)<=index(history[RandomChartEnd]) 
  Entries<-performer[f,]
  #View(Entries)  
  RowCount<-1:nrow(history[RandomChartStart:RandomChartEnd,])
  buff<-cbind(history[RandomChartStart:RandomChartEnd,],RowCount)
  
  names(buff)<- c("Open" ,  "High"  , "Low" ,   "Close" , "Volume", "Rowcount")
  View(buff)
  
  
  
  fun<-function(entryData,plotData)
  {
    f<-as.numeric(entryData)>=index(plotData) & as.numeric(entryData)<=index(plotData)-1
    plotData[f,"Rowcount"]
  }
  
  test<-sapply(Entries$open,fun,plotData=buff)
  View(test)
  #f<- as.numeric(Entries$open)<=index(buff) & 
  View(buff[f,])
  # x0<-c(1,100)
  # y0<-c(1.354,1.355)
  # x1<-c(100,200)
  # y1<-c(1.354,1.355)
  # segments(x0,y0,x1,y1,lwd=3)
  
  
  #Print Random chart
  # chart_Series(history[RandomChartStart:RandomChartEnd,])
  # segments(1,1.354,200,1.355,lwd=3)
  
  #print(c(index(history[RandomChartStart]), index(history[RandomChartEnd]),earliestEntry))

}


RandomePAHistory(data.EURUSD.M15,data.Performer.clean)



















