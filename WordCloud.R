# install
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("wordcloud2")
install.packages("tm")

# library
library(dplyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(wordcloud)
library(tm)

# import data
search_google_ads <- read.csv("UCI Google Ads FTMBA and FEMBA Search terms Report Sept 2018 - Jan 2020.csv", header = TRUE)

# keyword
text <- search_google_ads$Keyword 
docs <- Corpus(VectorSource(text))

# clean
docs <- docs %>% tm_map(removeNumbers) %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# create a document-term-matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# wordcloud
set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq =3,max.words=100, random.order=FALSE, 
          rot.per=0.35,colors=brewer.pal(9, "Dark2"),scale=c(4,1))

# Frequency chart
df %>% arrange(desc(freq)) %>% slice(1:8) %>% ggplot(., aes(x=word, y=freq))+
  geom_bar(stat='identity', fill="#E69F00", colour="black")+
  labs(title="Keyword Frequency",x=NULL,y="Frequency")

################

# keyword
text <- search_google_ads$Keyword 

# clean data
text <- gsub("https\\S*", "", text) 
text <- gsub("@\\S*", "", text) 
text <- gsub("amp", "", text) 
text <- gsub("[\r\n]", "", text)
text <- gsub("[[:punct:]]", "", text)
text

keyword <- data.frame(table(text))
sort(keyword$text)
keyword_order <- keyword %>% arrange(desc(Freq)) %>% slice(1:8) 

#vertical
ggplot(keyword_order, aes(x=text, y=Freq)) + 
  geom_bar(stat="identity", fill="#E69F00", colour="black")+
  theme(plot.title = element_text(hjust = 0, size=20), axis.text.x=element_text(angle=90, hjust=1, size=13), panel.spacing.x=unit(0.5, "lines")) +
    labs(title="Keyword Frequency",x=NULL,y="Frequency")


# horizontal
ggplot(keyword_order, aes(x=text, y=Freq)) + 
  geom_bar(stat="identity", fill="#E69F00", colour="black")+coord_flip()+
  theme(plot.title = element_text(hjust = 0, size=20), axis.text.x=element_text(angle=90, hjust=1, size=10), panel.spacing.x=unit(0.5, "lines"))+
  labs(title="Keyword Frequency",x=NULL,y="Frequency")+theme(axis.text = element_text(size=12))


# wordcloud
set.seed(1234)
wordcloud(words = keyword$text, freq = keyword$Freq, min.freq =2, max.words=50, 
          random.order=FALSE, rot.per=0.35,colors=brewer.pal(9, "Dark2"))

################

# Search term
text <- search_google_ads$Search.term
docs <- Corpus(VectorSource(text))

# clean
docs <- docs %>% tm_map(removeNumbers) %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# create a document-term-matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# wordcloud
set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=100, 
          random.order=FALSE, rot.per=0.35,colors=brewer.pal(6, "Dark2"))

# Frequency chart
df %>% arrange(desc(freq)) %>% slice(1:8) %>% ggplot(., aes(x=word, y=freq))+
  geom_bar(stat='identity', fill="#E69F00", colour="black")+
  labs(title="Search Term Frequency",x=NULL,y="Frequency")+
  theme(axis.text = element_text(size = 8),axis.text.x=element_text(angle=90, hjust=1, size=6),plot.title = element_text(size=11))
# +coord_flip()

# searching term is suitable for word, and keyword is suitable for whole sentence