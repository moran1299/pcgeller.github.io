for (group in 1:length(WG)){
  print(WG[group])
  idx <- meta(abstracts, "WG") == WG[group]
  subabstracts <- abstracts[idx]
  invisible(dtm <- DocumentTermMatrix(subabstracts))
  invisible(freq <- colSums(as.matrix(dtm)))
  invisible(wf <- data.frame(word=names(freq), freq=freq))
  invisible(wf <- arrange(wf,freq))
  invisible(dtmss <- removeSparseTerms(dtm, 0.9))
          
#   inspect(dtm)
#   print(freq)
#   print(wf)
#   inspect(dtmss)
  #path <- file.path("home", "rstudio","MORS","plots", paste(WG[group],".png", sep=""))
 # freqpath <- file.path("./plots","freqplot", paste(WG[group],"freq",".png", sep=""))
#   cloudpath <- file.path("./plots","wordcloud", paste(WG[group],"cloud",".png", sep=""))
#   dendopath <- file.path("./plots","dendo", paste(WG[group],"dendo",".png", sep=""))
#   clusterpath <- file.path("./plots","cluster", paste(WG[group],"cluster",".png", sep=""))
  path <- file.path("/home","rstudio","MORS","plots",WG[group])
  print(path)
  dir.create(path)
  setwd(path)
  getwd()
  makeplot(100, path)
  makewordcloud(250,path)
  makedendo(.8, path)
  makecluster(path)
}