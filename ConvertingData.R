Pattern.FindPattern<-function(performer,history, xBars)
{
  #Find X bars at a Specific Date BEFORE entrie
  View(performer)
  print(index(history[1:5]))
  f<-index(history)<=performer$open
  #View(history)
  bars<-tail(history[f],xBars+1)
  #bars<-bars[1:5]
  View(bars)
  # print(bars)
   print(performer$open)

  #Pass Pattern
  #bars
}


Pattern.KnitWithData<-function(performer, pattern)
{
  #knit Found Pattern with Performer Data
  
  
  
  #pass new Performer Data
  
}

per<-data.Performer.clean[data.Performer.clean$Symbol=="EURUSD",]
per<-per[101,]


Pattern.FindPattern(per,data.EURUSD.M15,5)















