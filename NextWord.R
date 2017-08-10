library(ngram)
library(stringr)
library(dplyr)

setwd("c:/Users/ankkn/Desktop/R/kursus/capstone")
#ng1<- read.table("ng1.txt", sep=",",header = TRUE, nrows = 5, stringsAsFactors=FALSE)
#ng2<- read.table("ng2.txt", sep=",",header = TRUE, stringsAsFactors=FALSE)
#ng3<- read.table("ng3.txt", sep=",",header = TRUE, stringsAsFactors=FALSE)
#ng4<- read.table("ng4.txt", sep=",",header = TRUE, stringsAsFactors=FALSE)
#ng5<- read.table("ng5.txt", sep=",",header = TRUE, stringsAsFactors=FALSE)

nextWord<-function(tekst){
  org<-tekst
  word<-""
  match<-TRUE
  tekst<-preprocess(tekst, case = "lower", remove.punct = TRUE, remove.numbers = TRUE, fix.spacing = TRUE)
  tekst<-gsub("’","",tekst)
  wc<-wordcount(tekst)
  if(wc>=4){
       string<-paste(word(tekst, wc-3,-1),"\\b")
       string<-head(filter(ng5,grepl(paste("^\\b", string,sep=""),ngram)),5)
       if(nrow(string)>0){
            word<-word(string[,1],5)
            match<-FALSE
       }       
  }

  if(wc>=3 & match){
       string<-paste(word(tekst, wc-2,-1),"\\b")
       string<-head(filter(ng4,grepl(paste("^\\b", string,sep=""),ngram)),5)
       if(nrow(string)>0){
            word<-word(string[,1],4)
            match<-FALSE
       }       
  }
  
  if(wc>=2 & match){
       string<-paste(word(tekst, wc-1,-1),"\\b")
       string<-head(filter(ng3,grepl(paste("^\\b", string,sep=""),ngram)),5)
       if(nrow(string)>0){
            word<-word(string[,1],3) 
            match<-FALSE
       }       
  }
  
  if(wc>=1 & match){
       string<-paste(word(tekst, wc,-1),"\\b")
       string<-head(filter(ng2,grepl(paste("^\\b", string,sep=""),ngram)),5)
       if(nrow(string)>0){
            word<-word(string[,1],2)
            match<-FALSE
       }       
  }
  
  if(match){
       word<-word(ng1[,1],1)
  }
  
  return<-word 
}

print(nextWord(""))
