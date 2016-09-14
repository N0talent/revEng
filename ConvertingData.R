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

#Test
#x<-TimeSyncVis(data.EURUSD.M15,data.Performer.clean)
# 
# buff<-data.EURUSD.M15[index(data.EURUSD.M15)<= data.Performer.clean[100,1],]
# 
# 
# Entrypoint<-tail(which(index(data.EURUSD.M15)<= data.Performer.clean[500,1]),1)
# Exitpoint<-tail(which(index(data.EURUSD.M15)<= data.Performer.clean[500,2]),1)
# 
# Exitpoint
# data.EURUSD.M15[Exitpoint]
# 
# 
# data.Performer.clean[100,1]
