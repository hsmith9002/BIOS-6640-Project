#BIOS 6640 Script for doing sentiment analysis on Twitter Data
#Load Required packages
setwd('/Users/Owner/Desktop')
install.packages('stringr')
library(twitteR)
library(plyr)
library(stringr)
#Authorize Twitter API access

consumer_key <- "wMM7duA6RikaMQwa1IDD9OxhC"
consumer_secret <- "zoI0ZviSH7dxCL0fSS33lO7Ni8hf46SOJWLyzpbAbhfDSjZmHw"
access_token <- "722889587522035712-WKCAOHWgQ9LSjnO1xjj1YK6y56n5ias"
access_secret <- "xCEcDiBqb6dFr9M98yB81aEmvH2LXBrp5Uawnae4SE8nK"
setup_twitter_oauth(consumer_key, consumer_secret,
                    access_token, access_secret)

#Collect Zika related tweets

zika_tweets <- searchTwitter("#zika", n = 700
                        )

#Do sentiment analysis on tweets

zika_text <- laply(zika_tweets, function(t) t$getText())
head(zika_text)
pos <- scan('positive-words.txt', what = 'character', comment.char = ';')
neg <- scan('negative-words.txt', what = 'character', comment.char = ';')
source('sentiment_new.r')

analysis <- score.sentiment(zika_text,
                            pos.words = pos,
                            neg.words = neg)
table(analysis$score)
mean(analysis$score)
median(analysis$score)
hist(analysis$score)




