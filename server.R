options(shiny.maxRequestSize =  30*1024^2)

library(shiny)
library(SnowballC)
library(quanteda)

## Load Training Corpus and Document Frequency Matrix
load("app1gram.Rda")
load("app2gram.Rda")
load("app3gram.Rda")
load("app4gram.Rda")

feat4g <- colnames(tdfm4gram[1, ])
feat3g <- colnames(tdfm3gram[1, ])
feat2g <- colnames(tdfm2gram[1, ])
feat1g <- colnames(tdfm1gram[1, ])

## Define N-gram algothrism
pred <- function(input){
  ## Tokenize input
  t3g <- tokenize(toLower(input), removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, removeTwitter = TRUE, verbose = TRUE, ngrams = 3)
  t2g <- tokenize(toLower(input), removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, removeTwitter = TRUE, verbose = TRUE, ngrams = 2)
  t1g <- tokenize(toLower(input), removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, removeTwitter = TRUE, verbose = TRUE, ngrams = 1)
  
  if(length(tail(t1g[[1]],1))<1){
    predf <- NA
    return(predf)
  } else {
    ## Initiatize key words and predictions
    search4g <- NA
    search3g <- NA
    search2g <- NA
    
    c4 <- NA
    c3 <- NA
    c2 <- NA
    c1 <- NA
    
    pred4 <- NA
    pred3 <- NA
    pred2 <- NA
    pred1 <- NA
    
    ## Identify search key words and search for mathces
    if (!is.null(t3g[[1]])){
      base <- tail(t3g[[1]],1)
      search4g <- paste("^", base, "_", sep = "")
      search3g <- gsub("\\^[a-zA-Z']+_", "\\^", search4g)
      search2g <- gsub("\\^[a-zA-Z']+_", "\\^", search3g)
      
      c4 <- topfeatures(tdfm4gram[1, grep(search4g, feat4g)], 3)
      pred4 <- gsub("^[a-zA-Z']+_[a-zA-Z']+_[a-zA-Z']+_", "", names(c4))
      if(!is.na(pred4[1])){
        predf <- pred4
        return(predf)
      } else {
        c3 <- topfeatures(tdfm3gram[1, grep(search3g, feat3g)], 3)
        pred3 <- gsub("^[a-zA-Z']+_[a-zA-Z']+_", "", names(c3))
        if(!is.na(pred3[1])){
          predf <- pred3
          return(predf)
        } else {
          c2 <- topfeatures(tdfm2gram[1, grep(search2g, feat2g)], 3)
          pred2 <- gsub("^[a-zA-Z']+_", "", names(c2))
          if(!is.na(pred2[1])){
            predf <- pred2
            return(predf)
          } else {
            c1 <- topfeatures(tdfm1gram[1, ], 3)
            pred1 <- names(c1)
            predf <- pred1
            return(predf)
          }
        }
      }
    } else if(!is.null(t2g[[1]])){
      base <- tail(t2g[[1]],1)
      search3g <- paste("^", base, "_", sep = "")
      search2g <- gsub("\\^[a-zA-Z']+_", "\\^", search3g)
      
      c3 <- topfeatures(tdfm3gram[1, grep(search3g, feat3g)], 3)
      pred3 <- gsub("^[a-zA-Z']+_[a-zA-Z']+_", "", names(c3))
      if(!is.na(pred3[1])){
        predf <- pred3
        return(predf)
      } else {
        c2 <- topfeatures(tdfm2gram[1, grep(search2g, feat2g)], 3)
        pred2 <- gsub("^[a-zA-Z']+_", "", names(c2))
        if(!is.na(pred2[1])){
          predf <- pred2
          return(predf)
        } else {
          c1 <- topfeatures(tdfm1gram[1, ], 3)
          pred1 <- names(c1)
          predf <- pred1
          return(predf)
        }
      }
    } else {
      base <- tail(t1g[[1]],1)
      search2g <- paste("^", base, "_", sep = "")
      
      c2 <- topfeatures(tdfm2gram[1, grep(search2g, feat2g)], 3)
      pred2 <- gsub("^[a-zA-Z']+_", "", names(c2))
      if(!is.na(pred2[1])){
        predf <- pred2
        return(predf)
      } else {
        c1 <- topfeatures(tdfm1gram[1, ], 3)
        pred1 <- names(c1)
        predf <- pred1
        return(predf)
      }
    }
  }
}

# Server codes
shinyServer({
  function(input, output) {
    output$pred1 <- renderText(if(!is.na(pred(input$inphrase)[1])){paste("Next word you want to type is very likely to be: ", toupper(pred(input$inphrase)[1]))} else{""})
    output$pred2 <- renderText(if(!is.na(pred(input$inphrase)[1])){paste("Next word you want to type is likely to be: ", toupper(pred(input$inphrase)[2]))} else{""})
    output$pred3 <- renderText(if(!is.na(pred(input$inphrase)[1])){paste("Next word you want to type might be: ", toupper(pred(input$inphrase)[3]))} else{""})
    output$message <- renderText(if(!is.na(pred(input$inphrase)[1])){""} else{print("Please type something!")})
  }
})