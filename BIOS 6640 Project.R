#BIOS 6640 Data Mining Twitter for Zika Virus data Project.
#Harry Smith

#Import necessary packages
install.packages('twitteR')
install.packages('tm')
install.packages('wordcloud')
install.packages('XML')
install.packages('topicmodels')
install.packages('syuzhet')
install.packages('lubridate')
install.packages('scales')
install.packages('reshape2')
install.packages('dplyr')
library(twitteR)
library(tm)
library(wordcloud)
library(XML)
library(ggplot2)
library(topicmodels)
library(syuzhet)
library(lubridate)
library(scales)
library(reshape2)
library(dplyr)

#Authorize Twitter API access

consumer_key <- "wMM7duA6RikaMQwa1IDD9OxhC"
consumer_secret <- "zoI0ZviSH7dxCL0fSS33lO7Ni8hf46SOJWLyzpbAbhfDSjZmHw"
access_token <- "722889587522035712-WKCAOHWgQ9LSjnO1xjj1YK6y56n5ias"
access_secret <- "xCEcDiBqb6dFr9M98yB81aEmvH2LXBrp5Uawnae4SE8nK"
setup_twitter_oauth(consumer_key, consumer_secret,
                    access_token, access_secret)

#Get 1200 tweets filtering for "#Zika", dating back 1 week
set_1 <- searchTwitter("#zika", n = 1500, lang ="en"
                       , since = '2016-04-21', until = '2016-04-22')
set_2 <- searchTwitter("#zika", n = 1500, lang ="en"
                       , since = '2016-04-23', until = '2016-04-24')
set_3 <- searchTwitter("#zika", n = 1500, lang ="en"
                       , since = '2016-04-24', until = '2016-04-26')
set_4 <- searchTwitter("#zika", n = 1500, lang ="en"
                       , since = '2016-04-27', until = '2016-04-28')
ztweets <- rbind(set_1, set_2, set_3, set_4)


ztweets <- searchTwitter("#zika", n = 1500, lang ="en"
                         , since = '2016-04-21', until = '2016-04-22')

#Convert ztweets into a data frame
ztweets_df <- twListToDF(ztweets)

#Build a Word Cloud
ztweet_text <- sapply(ztweets, function(x) x$getText())
ztweet_corpus <- Corpus(VectorSource(ztweet_text))
ztweet_corpus <- tm_map(ztweet_corpus,
                              content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                              mc.cores=1
)
ztweet_corpus <- tm_map(ztweet_corpus, content_transformer(tolower), mc.cores=1)
ztweet_corpus <- tm_map(ztweet_corpus, removePunctuation, mc.cores=1)
ztweet_corpus <- tm_map(ztweet_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
wordcloud(ztweet_corpus)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(ztweet_corpus,min.freq=2,max.words=100, random.order=T, colors=pal2)

#build a term-document matrix
ztweet_dtm <- TermDocumentMatrix(ztweet_corpus)
findAssocs(ztweet_dtm, 'zika', 0.20)
ztweet_dtm2 <- removeSparseTerms(ztweet_dtm, sparse=.90)
# convert the sparse term-document matrix to a standard data frame
ztweet_df2 <- as.data.frame(inspect(ztweet_dtm2))

#Build word association tree
ztweet.df.scale <- scale(ztweet_df2)
d <- dist(ztweet.df.scale, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram?

groups <- cutree(fit, k=4) # cut tree into 4 clusters
# draw dendogram with red borders around the 4 clusters
rect.hclust(fit, k=4, border="red")

#Build Word Frequency Bar Graph
tdm <- TermDocumentMatrix(ztweet_corpus, control = list(wordLenghts=c(1, Inf)))
freq_terms <- findFreqTerms(tdm, lowfreq = 350)
term_freq <- rowSums(as.matrix(tdm))
term_freq <- subset(term_freq, term_freq>= 350)
df <- data.frame(term = names(term_freq), freq =term_freq)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = 'identity')+ xlab('Terms') + ylab('Count') +coord_flip()


