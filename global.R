library("ROAuth")
library("twitteR")
library("memoise")
library("devtools")
library("rjson")
library("bit64")
library("base64enc")
library("httr")


#Twitter Application Credentials
api_key <- "VyPw4ylpYlH2CarmGS63GJsE5"
api_secret <- "6jBDHZ1vmmi8pBh1wWcKPNkDZxStKg1fXrEarJbHpJ5WOfs89s"
accesstoken <- "84731779-IzVEbFfXbhKhEe6rDePZks9W7zdTABC1vMBN0n9ti"
access_token_secret <- "SvaofwMuobfaXrxHIk9ARqqnuPiKbsgh3hxVwpbNkpMeZ"

# 1. Find OAuth settings for twitter:
#    https://dev.twitter.com/docs/auth/oauth
oauth_endpoints("twitter")

# 2. Register an application at https://apps.twitter.com/
#    Make sure to set callback url to "http://127.0.0.1:1410"
#
#    Replace key and secret below
myapp <- oauth_app("twitter",
                   key = api_key,
                   secret = api_secret
)

# 3. Get OAuth credentials
twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)

# 4. Use API
req <- GET("https://api.twitter.com/1.1/statuses/home_timeline.json",
           config(token = twitter_token))

stop_for_status(req)

#content(req)


#download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

#create an object "cred" that will save the authenticated object that we can use for later sessions
#cred <- OAuthFactory$new(consumerKey='VyPw4ylpYlH2CarmGS63GJsE5',
#                         consumerSecret='6jBDHZ1vmmi8pBh1wWcKPNkDZxStKg1fXrEarJbHpJ5WOfs89s',
#                         requestURL='https://api.twitter.com/oauth/request_token',
#                         accessURL='https://api.twitter.com/oauth/access_token',
#                         authURL='https://api.twitter.com/oauth/authorize')

####Executing the next step generates an output --> To enable the connection, please direct your web browser to: <hyperlink> . Note:  You only need to do this part once
#cred$handshake(cainfo="cacert.pem")

####save for later use for Windows
#save(cred, file="twitter authentication.Rdata")
#load("twitter authentication.Rdata")

setup_twitter_oauth(api_key, api_secret, accesstoken, access_token_secret)

tweets <-""

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(hashtag) {
  
  tweets <- searchTwitter(hashtag, n=100, lang="en")
  #Data Loading

  tweets.df <- twListToDF(tweets)
  #print.simple.list(tweets.df$text)
  #tweets <<- tweets.df$text
  assign("tweets",tweets.df, envir = .GlobalEnv)

   #Load into the Text mining package
  tweetCorpus <- Corpus(VectorSource(tweets.df$text))
  ##################
  #Data Cleansing
  ##################
  #Remove URLS
  removeURL <- function(x) gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", x)
  tweetCorpus <- tm_map(tweetCorpus, content_transformer(removeURL))

  
  #Repace Any Non Character Except Space, #, or @ with Empty Space
  removeNonCharacters  <- function(x) gsub("[^a-zA-Z #@]", "", x)
  tweetCorpus <- tm_map(tweetCorpus, content_transformer(removeNonCharacters))
  
  #Convert to lowercase, remove punctuations, numbers, URLs
  tweetCorpus <- tm_map(tweetCorpus,content_transformer(tolower))

  #Remove Stop words
  tweetCorpus <- tm_map(tweetCorpus, function(x) removeWords(x, stopwords("english")))

  #Remove Stopwords
  tweetCorpus <- tm_map(tweetCorpus, removeWords, c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  tweetCorpus <- tm_map(tweetCorpus, function(x) removeWords(x, "https"))
  tweetCorpus <- tm_map(tweetCorpus, function(x) removeWords(x, unlist(strsplit(hashtag," "))))
  tweetCorpus <- tm_map(tweetCorpus, stemDocument, lazy=TRUE)
  #Create a copy of the Corpus
  myCorpusCopy <- tweetCorpus
  #Complete stems to original form
  completeStems  <- function(y, dictionary){
    y <- unlist(strsplit(as.character(y), " "))
    y <- y[y != ""]
    y <- stemCompletion(y, dictionary=dictionary)
    y <- paste(y, sep="", collapse=" ")
    PlainTextDocument(stripWhitespace(y))
  }
  tweetCorpus <- lapply(tweetCorpus, completeStems, dictionary=myCorpusCopy)
  tweetCorpus <- Corpus(VectorSource(tweetCorpus)) 
  
  
  #Print Tweets Vector after cleansing
  #print.simple.list(tweetCorpus$content)

  levels(tweetCorpus)
  #Word Counting
  myDTM = TermDocumentMatrix(tweetCorpus, control = list(minWordLength = 1))
  m = as.matrix(myDTM)
  #Sort the results of the matrix
  sort(rowSums(m), decreasing = TRUE)
 
})
