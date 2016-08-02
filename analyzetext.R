library(tm)
library(dplyr)
library(SnowballC)
library(wordcloud)
library(cluster)
library(fpc)
library(ggplot2)

setwd("/home/rstudio/MORS")
##load data and format into corpus
  excel <- read.csv("mors8.csv", stringsAsFactors = FALSE)
  text <- excel[c("EventID","Abstract.Text","ED_Track")]
  unq <- text[!duplicated(text$Abstract.Text),]
  m <- list(content = "Abstract.Text", ID = "EventID", WG = "ED_Track")
  reader <- readTabular(mapping = m)
  (abstracts <- Corpus(DataframeSource(unq), readerControl = list(reader = reader)))

##pre process data
  abstracts <- tm_map(abstracts, removePunctuation)
  abstracts <- tm_map(abstracts, removeNumbers)
  abstracts <- tm_map(abstracts, content_transformer(tolower))
  abstracts <- tm_map(abstracts, removeWords, stopwords("english"))
  ##Stemming the abstracts takes off too many important endings - a lot of ses (s's?) get dropped from the ends of words
  #abstracts <- tm_map(abstracts, stemDocument)
  abstracts <- tm_map(abstracts, stripWhitespace)

#make freq plot
makeplot <- function(freqthreshold,path){
  #png(filename = paste(path, "/freq.png", sep = ""))
  p <- ggplot(subset(wf,freq>freqthreshold), aes(x = reorder(word,freq),y = freq))
  p <- p + geom_bar(stat="identity")
  p <- p + theme(axis.text.x=element_text(angle=45, hjust = 1))
  ggsave(filename = paste(path, "/freq.png", sep = ""), plot = p, width = 6, height = 6)
 # dev.off()
}

#make word cloud
makewordcloud <- function(freqthreshold,path){
  png(filename = paste(path, "/cloud.png", sep = ""))
  cloud <- wordcloud(names(freq), freq, min.freq=freqthreshold, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   
  #ggsave(filename = paste(path, "/cloud.png", sep = ""), plot = cloud)
  dev.off()
}

#make dendogram
makedendo <- function(sparsity,path){
  png(filename = paste(path, "/dendo.png", sep = ""))
  dtmss <- removeSparseTerms(dtm, sparsity)
  d <- dist(t(dtmss), method="euclidian")
  fit <- hclust(d=d, method="ward.D2")
  dendo <- plot(fit, hang = -1)
  #ggsave(filename = paste(path, "/dendo.png", sep = ""), plot = dendo)
  dev.off()
}

#make kmeans cluster
makecluster <- function(path){
  png(filename = paste(path, "/cluster.png", sep=""))
  d <- dist(t(dtmss), method = "euclidian")
  kfit <- kmeans(d,2)
  clus <- clusplot(as.matrix(d), kfit$cluster, color = T, shade = T, labels = 2, lines = 0)
  #ggsave(filename = paste(path, "/cluster.png", sep=""), plot = clus)
  dev.off()
}

#calc variables for plots

#   dtm <- DocumentTermMatrix(abstracts)
#   tdm <- TermDocumentMatrix(abstracts)
#   freq <- colSums(as.matrix(dtm))
#   wf <- data.frame(word=names(freq), freq=freq)
#   wf <- arrange(wf,freq)
#   dtmss <- removeSparseTerms(dtm, 0.8)

#loop through all working groups and create DTM, and plot for each WG.  save under unique file name
WG <- distinct(text["ED_Track"])
WG <- as.vector(WG[,1])
WG <- WG[-23]

