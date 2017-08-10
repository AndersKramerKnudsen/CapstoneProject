library(shiny)
library(ngram)
library(stringr)
library(dplyr)

server <- function(session, input, output) {
     
     #######################################################################################
     #setwd("c:/Users/ankkn/Desktop/R/kursus/capstone")
     ng1<- read.table("ng1.txt", sep=",",header = TRUE, nrows = 5, stringsAsFactors=FALSE)
     ng2<- read.table("ng2.txt", sep=",",header = TRUE, stringsAsFactors=FALSE)
     ng3<- read.table("ng3.txt", sep=",",header = TRUE, stringsAsFactors=FALSE)
     ng4<- read.table("ng4.txt", sep=",",header = TRUE, stringsAsFactors=FALSE)
     ng5<- read.table("ng5.txt", sep=",",header = TRUE, stringsAsFactors=FALSE)
     
     trim <- function (x) gsub("^\\s+|\\s+$", "", x)
     
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
     #######################################################################################
     
     observeEvent(input$cap,{
          isolate({
               x<-nextWord(input$cap)
               if(is.na(x[1])){
                    disable("b1")
               } else{
                    enable("b1")
               }
               if(is.na(x[2])){
                    disable("b2")
               } else{
                    enable("b2")
               }
               if(is.na(x[3])){
                    disable("b3")
               } else{
                    enable("b3")
               }
               if(is.na(x[4])){
                    disable("b4")
               } else{
                    enable("b4")
               }
               if(is.na(x[5])){
                    disable("b5")
               } else{
                    enable("b5")
               }
               updateActionButton(session,"b1",x[1])
               updateActionButton(session,"b2",x[2])
               updateActionButton(session,"b3",x[3])
               updateActionButton(session,"b4",x[4])
               updateActionButton(session,"b5",x[5])          
          })
     })
     observeEvent(input$b1,{
          isolate({
               w<-trim(nextWord(input$cap)[1])
               if(!is.na(w)){
                    w<-paste(input$cap, w)
                    updateTextInput(session, "cap", value = w)
               }
          })
     })
     observeEvent(input$b2,{
          isolate({
               w<-trim(nextWord(input$cap)[2])
               if(!is.na(w)){
                    w<-paste(input$cap, w)
                    updateTextInput(session, "cap", value = w)
               }
          })
     })
     observeEvent(input$b3,{
          isolate({
               w<-trim(nextWord(input$cap)[3])
               if(!is.na(w)){
                    w<-paste(input$cap, w)
                    updateTextInput(session, "cap", value = w)
               }
          })
     })
     observeEvent(input$b4,{
          isolate({
               w<-trim(nextWord(input$cap)[4])
               if(!is.na(w)){
                    w<-paste(input$cap, w)
                    updateTextInput(session, "cap", value = w)
               }
          })
     })
     observeEvent(input$b5,{
          isolate({
               w<-trim(nextWord(input$cap)[5])
               if(!is.na(w)){
                    w<-paste(input$cap, w)
                    updateTextInput(session, "cap", value = w)
               }
          })
     })
}