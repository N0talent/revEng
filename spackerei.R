require(xts)

#very slow. faster version requires work
approxMatchTimes <- function (t1, t2, minutesTol = 5) {
  t1 <- as.numeric(t1)
  t2 <- as.numeric(t2)
  tol <- minutesTol * 60
  inds <- unlist(lapply(t1, function (x) which.min(abs(x-t2))))
  diffs <- unlist(lapply(1:length(inds), function (i) abs(t1[i] - t2[inds[i]])))
  inds[diffs > tol] <- 0
  inds
}

#requires historyData to be of class xts
#data has to be sorted by time
getCandles <- function (historyData = data.EURUSD.M15,
                        performerData = data.Performer.clean,
                        indices = NULL,
                        candleNumber = 5,
                        maxEntries = 100) {
  if (!length(indices)) indices <- approxMatchTimes(index(historyData), data.Performer.clean$open)
  indices <- indices[(indices - candleNumber + 1) > 0]
  starts <- indices - candleNumber
  
}



checkvalid <- function (i, diffallowed = 0) {
  op <- min(abs(difftime(p1$open[i], d$time, units = "secs")))
  cl <- min(abs(difftime(p1$close[i], d$time, units = "secs")))
  if (op <= diffallowed * 60 & cl <= diffallowed * 60) { 
    op <- which.min(abs(difftime(p1$open[i], d$time, units = "secs")))
    cl <- which.min(abs(difftime(p1$close[i], d$time, units = "secs")))
  dd <- d[c(op, cl),]
  #print (dd)
  #print (p1[i,])
  rownames(dd) <- NULL
  st <- dd$Open[1]
  en <- dd$Close[2]
  hist1 <- (en - st)*10000 
  bla <- data.frame(Index = i, Perf = p1$Pips[i], Hist = hist1)
  return (bla)
  } else {
    return (NULL)
  }
}