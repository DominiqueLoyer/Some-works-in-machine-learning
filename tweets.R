##**********Steps to Set up authorization to connect and extract tweets********
### Setting Working Directory
setwd("/Users/SherbrookeInformatique/Bureau")


library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(sentiment)
library(RCurl)
library(syuzhet)
library(sentimentr)
# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)

install.packages("httpuv")
library(httpuv)



### Fill out my tokens
consumer_key=""
consumer_secret=""
access_token=""
access_secret=""



setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
ytw = twitteR::searchTwitter('#realDonaldTrump + #HillaryClinton', n = 6, since = '2016-11-08', retryOnRateLimit = 1e3)
d = twitteR::twListToDF(tw)

oauth_endpoint(authorize = "https://api.twitter.com/oauth",
               access = "https://api.twitter.com/oauth/access_token")

#connect to API
download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'

### put my credientials here
consumer_key=""
consumer_secret=""
access_token=""
access_token_secret=""

Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=reqURL,
                         accessURL=accessURL,
                         authURL=authURL)
Cred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')) #There is URL in Console. You need to go to it, get code and enter it on Console

##### Authorization PIN -DYNAMIC


Elec = search_tweets("#Election2020", n=100000, lang='en', include_rts=FALSE)
Elec





save(Cred, file='twitter authentication.Rdata')

load('twitter authentication.Rdata') 
#Once you launch the code first time, you can start from this line in the future (libraries should be connected)

setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret, access_token =accesstoken, access_secret = accesssecret )



##****************Step 3: Perform tweets extraction and data cleaning****************

# Harvest some tweets

some_tweets = searchTwitter("Election2020", n=1000, since = "2020-11-03", lang= "en")

# Explore Tweets

length.some_tweets <- length(some_tweets)
length.some_tweets

some_tweets.df <- ldply(some_tweets, function(t) t$toDataFrame())
write.csv(some_tweets.df, "tweets.csv")

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())

# Cleaning 1-  remove people name, RT text etc. 

some_txt1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",some_txt)

# Cleaning 2- remove html links
some_txt2 = gsub("http[^[:blank:]]+", "", some_txt1)

# Cleaning 3- remove people names

some_txt3 = gsub("@\\w+", "", some_txt2)

# Cleaning 4- remove Punctuations 

some_txt4 = gsub("[[:punct:]]", " ", some_txt3)

# Cleaning 5- remove Punctuations 

some_txt5 = gsub("[^[:alnum:]]", " ", some_txt4)

# Exporting to Excel

write.csv(some_txt5, "tweets1.csv")

# Creating wordcorpus and cleaning

some_txt6 <- Corpus(VectorSource(some_txt5))
some_txt6 <- tm_map(some_txt6, removePunctuation)
some_txt6 <- tm_map(some_txt6, content_transformer(tolower))
some_txt6 <- tm_map(some_txt6, removeWords, stopwords("english"))
some_txt6 <- tm_map(some_txt6, stripWhitespace)

# Building wordcloud

pal <- brewer.pal(12,"Dark2")

wordcloud(some_txt6, min.freq = 5,  max.words = Inf, width=1000, height =1000,  random.order = FALSE, color=pal)

y# Sentiment Analysis

# how the function works

get_nrc_sentiment("I bought an iPhone a few days ago. It is such a nice phone, although a little large. The touch screen is cool.The voice quality is clear too. I simply love it!")

# Running on our data

mysentiment <- get_nrc_sentiment(some_txt5)
SentimentScores <- data.frame(colSums(some_txt5[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")

