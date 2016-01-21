
  
  # You can access the value of the widget with input$text, e.g.
#  output$value <- renderPrint({ input$text })


  US_T_frq <- readRDS("US_T_frq")
  US_T_bi_frq <- readRDS("US_T_bi_frq")
  US_T_tri_frq <- readRDS("US_T_tri_frq")
  US_T_quad_frq <- readRDS("US_T_quad_frq")

#   t <- as.character(input$text)
#  lookup <- function(input){
    #cleaning up

shinyServer(function(input, output) {
  
    clean <- reactive({
      t <- tolower(input$text) #convert to lowercase
      t <- removePunctuation(t) #remove punctuation
      t <- removeNumbers(t) #remove numbers
      t <- gsub("^\\s+|\\s+$", "", t) #remove leading and trailing white spaces
      #if ( sapply(gregexpr("\\S+", t),length)==5)
      #t <-paste(unlist(strsplit(x, split = "\\s+"))[1:4],collapse=" ")
      #{t <- paste(unlist(head(strsplit(t,split=" ")[[1]],4)),collapse=" ")}
      #{t <- "o"}
    })
    
    pred <- reactive({
      
      t <- clean()
      
      #if more than 4 words are input
      if ( sapply(gregexpr("\\S+", t),length)>5){
        t <-paste(unlist(strsplit(t, split = "\\s+"))[1:4],collapse=" ")
      }
      
      #prepare list of swear words
      sw <- c("shit","piss","fuck","cunt","cocksucker","motherfucker","tits")
      msg <- c("Are you sure to use this word?","D;","DX","Maybe reconsider to use this word")
      
      tt <- tail(strsplit(t,split=" ")[[1]],1)
      if (is.element(tt,sw)) {
        t <- (sample(msg,1))  
      }
     
      #quadrigram
      else if (sapply(gregexpr("\\S+", t), length)==3){
        result <- match(t,US_T_quad_frq$word2)
        if (is.na(result)) {t <- sample(US_T_frq$word[1:30],1)}
        else {t <- tail(strsplit(US_T_quad_frq$word[result],split=" ")[[1]],1)}
      }
      
      #bigram
      else if (sapply(gregexpr("\\S+", t), length)==1){
        result <- match(t,US_T_bi_frq$word2)
        if (is.na(result)) {t <- sample(US_T_frq$word[1:30],1)}
        else {t <- tail(strsplit(US_T_bi_frq$word[result],split=" ")[[1]],1)}
      }
      #trigram
      else if (sapply(gregexpr("\\S+", t), length)==2){
        result <- match(t,US_T_tri_frq$word2)
        if (is.na(result)) {t <- sample(US_T_frq$word[1:30],1)}
        else {t <- tail(strsplit(US_T_tri_frq$word[result],split=" ")[[1]],1)}
      }

      else {t <- sample(US_T_frq$word[1:30],1)}
    })
    
    output$value <- renderText({pred()})
#    t <- removePunctuation(t) #remove punctuation
#    t <- removeNumbers(t) #remove numbers
#    t <- gsub("^\\s+|\\s+$", "", t) #remove leading and trailing white spaces
    
    #give a notification if swear words are used
#    sw <- c("shit","piss","fuck","cunt","cocksucker","motherfucker","tits")
#    msg <- c("Are you sure to use this word?","D;","DX","Maybe reconsider to use this word")
#    if (is.element(input,sw)) {
#      output$value <- sample(msg,1)
      #return (sample(msg,1))  
#    }
    #bigram
#    else if (sapply(gregexpr("\\S+", input), length)==1){
#      result <- match(input,US_T_bi_frq$word2)
#      print(result)
#      if (is.na(result)) {output <- sample(US_T_frq$word[1:30],1)}
#      else {output <- tail(strsplit(US_T_bi_frq$word[result],split=" ")[[1]],1)}
#      return (output)
#    }
    #trigram
#    else if (sapply(gregexpr("\\S+", input), length)==2){
#      result <- match(input,US_T_tri_frq$word2)
#      if (is.na(result)) {output <- sample(US_T_frq$word[1:30],1)}
#      else {output <- tail(strsplit(US_T_tri_frq$word[result],split=" ")[[1]],1)}
#      return (output)
#    }
    #quadrigram
#    else if (sapply(gregexpr("\\S+", input), length)==3){
#      result <- match(input,US_T_quad_frq$word2)
#      if (is.na(result)) {output <- sample(US_T_frq$word[1:30],1)}
#      else {output <- tail(strsplit(US_T_quad_frq$word[result],split=" ")[[1]],1)}
#      return (output)
#    }
#    else {output <- sample(US_T_frq$word[1:30],1)}
#    return (output)
#  }
#    output$value <- t

    })
#    })


