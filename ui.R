library(shiny)
library(shinyjs)

ui <- fluidPage(
     useShinyjs(),
     textInput("cap", "Enter your sentence below:", "just write the words of your",width = "100%"),
     
     "Are you looking for one of these words? In your context they are the most common ones",
     h1(""),
     actionButton("b1", ""),
     actionButton("b2", ""),
     actionButton("b3", ""),
     actionButton("b4", ""),
     actionButton("b5", ""),
     h1(""),
     "powered by NeW-P"
     
     )
