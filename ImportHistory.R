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

ImportPerformer<-function()
{
  
  
}  
  