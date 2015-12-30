library("tm")
library("SnowballC")
library("wordcloud")

function(input, output, session) {
  # Define a reactive expression for the document term matrix
  cat("foo1\n")  
  terms <- reactive({
    # Reactive after the user clicks on UPDATE button
    input$update
    #Execute the Global Function to get the new set of tweets.
    isolate({
      withProgress({
        setProgress(message = "Updating the search of tweets")
        getTermMatrix(input$hashtagIn)
      })
    })
  })
  
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  #Generate the Word Cloud Plot
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, 
                  scale=c(5,.7),
                  min.freq = input$freq,
                  max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  

  #Generate the table or Word Counting
  output$mytable2 <- renderTable({
    data.frame(x=terms())
  })
  
  #Generate the table of Tweets used to word counting
  output$mytable1 <- renderTable({
    get("tweets", envir=.GlobalEnv)
    tweetsClean <- iconv(tweets$text, "latin1", "UTF-8" )
    data.frame(x=tweetsClean)
  })
  
}

