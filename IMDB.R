

# ############# IMDB reviews Extraction ################
a<-10
Venom1 <-NULL
url1<-"https://www.imdb.com/title/tt1270797/reviews?ref_=tt_sa_3"
for(i in 0:22){
  url<-read_html(as.character(paste(url1,i*a,sep="")))
   Venom<-url %>%
     html_nodes("text show-more__control") %>%
     html_text() 
   Venom1<-c(Venom1,Venom)
 }
write.table(Venom1,file="Venom.txt")
getwd()


###### Emotion mining ########
library(syuzhet)
library(plotly)
library(tm)
Venom  = readLines("C:/Users/tussh/Documents/Text Mining/venom.txt")
s_v <- get_sentences(Venom)

syuzhet <- get_sentiment(s_v, method="syuzhet")
bing <- get_sentiment(s_v, method="bing")
afinn <- get_sentiment(s_v, method="afinn")
nrc <- get_sentiment(s_v, method="nrc")
sentiments <- data.frame(syuzhet, bing, afinn, nrc)

#anger", "anticipation", "disgust", "fear", "joy", "sadness", 
#"surprise", "trust", "negative", "positive."
emotions <- get_nrc_sentiment(Venom)
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


