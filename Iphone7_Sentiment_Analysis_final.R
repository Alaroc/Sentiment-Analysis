
#Author: Ryan Moore
#9/8/2016:
#Inspired by https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/comparison-cloud

library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)
library(data.table)


setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_token_secret)

#Search twitter for tweets about the Iphone 7
#loading past tweets(9/7/2016, 1000) and combining them with the mined tweets of (9/8/2016, 3000 and 9/9/2016, 3000)
tweets2 <- searchTwitter("Iphone 7", n=3000, lang="en")

save(tweets, file="tweets972016.rda")
save(tweets, file='tweets982016.rda')
save(tweets3, file="tweets992016.rda")

load("tweets972016.rda")
load("tweets982016.rda")
load("tweets992016.rda")

tweets<-c(tweets,tweets2,tweets3)


#get text from the list of elements and remove troubling characters
tweets_text <- sapply(tweets, function(x) x$getText())
tweets_text<-sapply(tweets_text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#Set up a cleaning function that will remove unwanted aspects of the tweets(taken from the inspired line above ^)
clean.text = function(x)
{
        # tolower
        x = tolower(x)
        # remove rt
        x = gsub("rt", "", x)
        # remove at
        x = gsub("@\\w+", "", x)
        # remove punctuation
        x = gsub("[[:punct:]]", "", x)
        # remove numbers
        x = gsub("[[:digit:]]", "", x)
        # remove links http
        x = gsub("http\\w+", "", x)
        # remove tabs
        x = gsub("[ |\t]{2,}", "", x)
        # remove blank spaces at the beginning
        x = gsub("^ ", "", x)
        # remove blank spaces at the end
        x = gsub(" $", "", x)
        return(x)
}


#call cleaning function
tweets_clean<-clean.text(tweets_text)

#turn tweets_clean into a corpus
tweets_clean <- Corpus(VectorSource(tweets_clean))

#if any manual adjusting needs to be made
tweets_clean<-tm_map(tweets_clean, removeWords, c(stopwords("en"),"iphone","apple","iphoneis"))

#Turn it into a TermDocumentMatrix
tweets_c_tdm<-TermDocumentMatrix(tweets_clean)

#Turn the TDM into a Matrix
tweets_c_m<-as.matrix(tweets_c_tdm)

#Sum Rows
tweets_words<-rowSums(tweets_c_m)

#order the words decreasingly
tweets_words<-sort(tweets_words, decreasing=TRUE)

#Create Data.Frame
tweets_df<-data.frame(term=names(tweets_words), num=tweets_words)

#Removing bottom two colors from Green Blue Palette
blues<-brewer.pal(9, "GnBu")
blues<-blues[-(1:3)]

#Testing first wordcloud of terms
wordcloud(tweets_df$term, tweets_df$num, col=blues, max.words = 150, random.order=FALSE )

#---------------------------------------------------------------

#Sentiment Analysis using syuzhet's get_nrc_sentiment for a barchart and large wordcloud
Sent_matrix<-get_nrc_sentiment(tweets_text)

sentiment<-c("disgust","sadness", "trust","joy","surprise","anger","fear","anticipation","negative","positive")

#Optional if you want to set tweet values to a max of 1
Sent_matrix[Sent_matrix>0]<-1

#barplot of the results
plotdata<-data.frame(emotion=colnames(Sent_matrix), value=colSums(Sent_matrix))

#plot the bar chart.
ggplot(plotdata, aes(x=reorder(emotion, value),y=value,fill=emotion))+
        geom_bar(stat="identity", aes())+
        labs(y="Frequency\n",x="\nSentiment")+
        ggtitle("9/7/2016 - 9/9/2016")

#The next steps will be to seperate the tweets by sentiment(they aren't mutually exclusive)
#and create a comparison cloud that allows words that pertain to that sentiment arise

#changing the matrix into a data.table
Sent_matrix_dt<-as.data.table(Sent_matrix)

#adding a column to maintain the tweet
Sent_matrix_dt[,tweet:=rownames(Sent_matrix)]

#looping through each sentiment to filter for it's specific text then cleaning and collapsing the text 
#Then combining it in the variable all
all<-character()
for (i in sentiment){
        assign(paste0(i,"_text"), Sent_matrix_dt[eval(as.name(paste(i)))>0,tweet])
        assign(paste0(i,"_clean"), clean.text(eval(as.name(paste0(i,"_text")))))
        assign(paste0(i,"_collapsed"), paste(eval(as.name(paste0(i,"_clean"))), collapse=" "))
        all<-c(all, eval(as.name(paste0(i,"_collapsed"))))
}

#brazenly taken from https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/comparison-cloud
#Manually removal of stopwords
all <- removeWords(all,
                  c(stopwords("english"),"iphone", "apple","iphoneno","eduaubdedubueduaubdedubueduaubdedubu","eduaubdedubua","eduaubdedubud","eduaubdedubueduaubdedubueduaubdedubueduaubdedubu","eduaubdedubuadeduaubdedubuad","eduaubdedubuaeduaubdedubuaeduaubdedubua"))

# create corpus
corpus <- Corpus(VectorSource(all))

# create term-document matrix
tdm <- TermDocumentMatrix(corpus)

# convert as matrix
tdm <- as.matrix(tdm)

# add column names
sentiment2<-c("Disgust","Sadness", "Trust","Joy","Surprise","Anger","Fear","Anticipation","Negative","Positive")
colnames(tdm) <- sentiment2

#10 Colors
colorchoice<-c("gold","goldenrod3","darkorange2",
           "orangered","red1","mediumpurple4",
               "navy","dodgerblue","green3","greenyellow")

save(tdm, file="tdm.rda")
load("tdm.rda")

# comparison cloud
comparison.cloud(tdm, random.order=FALSE, 
                 colors = colorchoice,
                 title.size=1.75, max.words=900)


