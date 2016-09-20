ImportHistory<-function(Symbol,TF)
{
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






