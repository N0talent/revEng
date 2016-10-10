ImportHistory<-function(Symbol,TF)
{
  require(dplyr)
  #read CSV files
  dir<-paste("History/",Symbol,"/",TF,"/",sep="")
  file<- list.files(dir)
  data<- read.csv(paste(dir,file,sep=""))

  #convert into readable Timestamps
  time<- as.POSIXct(strptime(data[,1],format="%d.%m.%Y %H:%M:%OS"))
  data<-as.xts(data[,2:6],time)
  
  #pass Datadrame
  data
}

ImportPerformer<-function(FileName)
{
  dir<-"Files/"
  filedir<- paste(dir,FileName,sep="")
  data<- read.csv(file=filedir)
  #data<-as.data.frame(data)
  data
}  
  
cleanPerformer<-function(data)
{
  #check Duplicates and remove
  buff<-duplicated(data$Open.Date)
  data<-data[!buff,]
  
  #convert into readable Timestamps
  open<-as.POSIXct(strptime(data[,1],format="%m/%d/%Y %H:%M"))
  close<-as.POSIXct(strptime(data[,2],format="%m/%d/%Y %H:%M"))
  data<- cbind(open,close,data[,3:ncol(data)])

 
  #remove all actions except Buy and Sell
  data<-data[(data$Action=="Buy" | data$Action=="Sell"),  ]

  data$Symbol<-factor(data$Symbol)
  
  data  
}

removeSymbols<-function(Sym,perfomer)
{
  perfomer<-perfomer[perfomer$Symbol %in% Sym,]
  perfomer$Symbol<-factor(perfomer$Symbol)
  perfomer
}


checkSync<-function(performer,history)
{
  require(xts)
  f<-index(history)<=performer[,1]
  bars<-tail(history[f],1)

  c(index(bars),bars[,2],bars[,3],performer)

  returnVek=NULL
  cVek=NULL
  for (n in 1:nrow(performer))
  {
    f<-index(history)<=performer[n,1]
    bars<-tail(history[f],1)

    if(performer[n,"Open.Price"]<=bars[,2] & performer[n,"Open.Price"]>=bars[,3])result=1 else result=0
    if(result==0)
    {
      if(performer[n,"Open.Price"]<=bars[,2]) dev<- -(performer[n,"Open.Price"]-bars[,2])
      if(performer[n,"Open.Price"]>=bars[,3])dev<- -(bars[,3]-performer[n,"Open.Price"])
    }
    else
    {
      dev<-NA
    }
    cVek<-c(as.character(performer[n,1]),as.character(index(bars)),performer[n,"Open.Price"],bars[,2],bars[,3],as.numeric(result),dev)

    returnVek<-rbind(returnVek,cVek)
  }
  
  returnVek
}

#d<-checkSync(data.Performer.clean.EURUSD[100:nrow(data.Performer.clean.EURUSD),],data.EURUSD.M15)






