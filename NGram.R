library(ngram)
library(dplyr)
setwd("c:/Users/ankkn/Desktop/R/kursus/capstone")
set.seed(1234)

twitter<- readLines("en_US.twitter.txt")     
news<- readLines("en_US.news.txt")     
blogs<- readLines("en_US.blogs.txt")     

dat<-c(twitter, news, blogs)
dat<-sample(dat,500000)

i<-0
for(i in 1:length(dat)){
     dat[i]<-preprocess(dat[i], case = "lower", remove.punct = TRUE, remove.numbers = TRUE, fix.spacing = TRUE)
     dat[i]<-gsub("’","",dat[i])
}

wc<-vapply(dat, wordcount, integer(1))

i<-0
for (i in 1:5){
  ny_dat<-dat[wc>=i]
  ng<-ngram(ny_dat, n=i)
  pt<-get.phrasetable(ng)
  fil<-data.frame(ngram=pt$ngrams,freq=pt$freq)
  fil<-fil[fil$freq>1,]
  write.csv(fil, file=paste0("ng", i, ".txt"), row.names = FALSE)
}