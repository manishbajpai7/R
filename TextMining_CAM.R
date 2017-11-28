library(tm)
options(header=TRUE, stringsAsFactors = FALSE)
setwd("C:/Users/manis/Desktop/Text Mining")
text<-readLines("CAM.csv")
corpus<-Corpus(VectorSource(text))
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeNumbers)
cleanset<-tm_map(corpus,removeWords,stopwords())
cleanset<-tm_map(cleanset,stripWhitespace)
dtm<-TermDocumentMatrix(cleanset,control = list(minWordLength=c(1,Inf)))
DocTrmMtrx <- as.matrix(dtm)
DocTrmMtrx.TotalFreq <- colSums(dtm)
#findFreqTerms(dtm,lowfreq = 2)
#termFrequency<-rowSums(as.matrix(dtm))
#termFrequency<-subset(termFrequency,termFrequency>=10)
#library(ggplot2)
#barplot(termFrequency,las=2,col = rainbow(20))
library(wordcloud)
wordcloud(words=colnames(DocTrmMtrx),freq= DocTrmMtrx.TotalFreq,min.freq= 2,scale=c(4,0.1), colors= brewer.pal(n= 10,"Paired"),random.order=F)

# reading the positive and negative words
pos <- readLines(con="C:\\Users\\manis\\Desktop\\Text Mining\\opinion-lexicon-English\\positive-words.txt")
neg <- readLines(con="C:\\Users\\manis\\Desktop\\Text Mining\\opinion-lexicon-English\\negative-words.txt")
# matching the terms with the dictionary of positive and negative words
pos.matches <- match(colnames(DocTrmMtrx),pos)
neg.matches <- match(colnames(DocTrmMtrx),neg)
# all the non-matches will be indicated by NA (that means the word is not in the docu-ment)
# converting the presence to true
pos.matches <- !is.na(pos.matches) # checking the value is not NA
neg.matches <- !is.na(neg.matches) # checking the value is not NA
# creating respective matrices
pos.matrix <- DocTrmMtrx[,pos.matches]
neg.matrix <- DocTrmMtrx[,neg.matches]
# calculating the sentiment score
sentiment.score <- rowSums(pos.matrix) - rowSums(neg.matrix)
summary(sentiment.score)
