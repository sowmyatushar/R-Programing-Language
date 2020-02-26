#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
library("twitteR")
#install.packages("ROAuth")
library("ROAuth")

cred <- OAuthFactory$new(consumerKey='2u1YoldtZAt9mJt4rvYLBA6rr', # Consumer Key (API Key)
                         consumerSecret='rPQZwMP07uc2sE8I7cwFP8Xi83rzg1Ll9XaU5UBx2VVyYQOSMy', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

#install.packages("base64enc")
library(base64enc)

#install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("2u1YoldtZAt9mJt4rvYLBA6rr", # Consumer Key (API Key)
                    "rPQZwMP07uc2sE8I7cwFP8Xi83rzg1Ll9XaU5UBx2VVyYQOSMy", #Consumer Secret (API Secret)
                    "1220986997134610432-eG1xPOQkpkQX9jUYpwtVfrvPByJgeG",  # Access Token
                    "4pn2H4UclHRRNfhUCAUWEBoUArzJqfbcChqTe0CFioMRf")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('imVkohli', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)
write.csv(TweetsDF, "Tweets.csv",row.names = F)

getwd()
# 
#handleTweets <- searchTwitter('DataScience', n = 10000)
# handleTweetsDF <- twListToDF(handleTweets)
# dim(handleTweetsDF)
# View(handleTweetsDF)
# #handleTweetsMessages <- unique(handleTweetsDF$text)
# #handleTweetsMessages <- as.data.frame(handleTweetsMessages)
# #write.csv(handleTweetsDF, "TefalHandleTweets.csv")
# library(rtweet)

######### Emotion Mining ###############
library(syuzhet)
library(plotly)
library(tm)
Tweeter <- TweetsDF %>% select(text)
View(Tweeter)


Tweet <- readLines("C:/Users/tussh/Documents/Text Mining/tweets.txt")
s_v <- get_sentences(Tweet)

syuzhet <- get_sentiment(s_v, method="syuzhet")
bing <- get_sentiment(s_v, method="bing")
afinn <- get_sentiment(s_v, method="afinn")
nrc <- get_sentiment(s_v, method="nrc")
sentiments <- data.frame(syuzhet, bing, afinn, nrc)

#anger", "anticipation", "disgust", "fear", "joy", "sadness", 
#"surprise", "trust", "negative", "positive."
emotions <- get_nrc_sentiment(Tweet)
head(emotions)
emo_bar = colSums(emotions)
barplot(emo_bar)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))

plot(syuzhet, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")

# To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(syuzhet)]

# and to extract the most positive sentence
positive <- s_v[which.max(syuzhet)]
