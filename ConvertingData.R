PerformerHistoryMerge<-function()
{
  # Open Datum mit in Histroy finden in TF X
  
  
  # X bars zurückschauen und dann Kopieren
  
  
  # Kopierte Bars Transponieren und in File
  
  #testtttt
}


TimeSyncVis<-function(history, performer,Sym)
{
  #Generate a Random Entry
  performer<-performer[performer$Symbol==Sym,]
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


RandomePAHistory<- function(history, performer, Sym,Bars)
{
  #filter History and Performer
  performer<-performer[performer$Symbol==Sym,]
  earliestEntry<-min(performer$open)
  subset<-which(earliestEntry<index(history))-1
  history<-history[subset,]

  #Gernerate a Random Chart
  RandomChartEnd<-floor(runif(1,500, max=nrow(performer)))
  RandomChartStart<-RandomChartEnd-Bars
  
  #Find all Entries in Chart
  f<-as.numeric(performer$open)>=index(history[RandomChartStart]) & as.numeric(performer$open)<=index(history[RandomChartEnd]) 
  Entries<-as.data.frame(performer[f,])
 
  RowCount<-1:nrow(history[RandomChartStart:RandomChartEnd,])
  buff<-cbind(history[RandomChartStart:RandomChartEnd,],RowCount)
  
  names(buff)<- c("Open" ,  "High"  , "Low" ,   "Close" , "Volume", "Rowcount")

  getBarPosition<- function(x)
      {  bar<-NULL
        for (open in x)  
        {
            f<-index(buff)<=open
            #print(paste(open ," in ", tail(index(buff[f]),1),", in reihe ",tail(buff[f,"Rowcount"],1) ))
            bar<- c(bar,tail(buff[f,"Rowcount"],1))
        }
      bar
      }
  
  x0<-getBarPosition(Entries$open)
  y0<-as.numeric(Entries$Open.Price)
 
  x1<-getBarPosition(Entries$close) 
  y1<-as.numeric(Entries$Close.Price)
  
  
  Entries<-mutate(Entries,Col=ifelse(as.character(Action)=="Sell","red","green"))
  col<-Entries$Col

  #Print Random chart
  print(chart_Series(history[RandomChartStart:RandomChartEnd,]))
  if(length(x0)>0)
      {
      segments(x0,y0,x1,y1,lwd=3,col = as.character(col))
      }

}






















