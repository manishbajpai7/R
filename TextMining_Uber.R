Text<-read.csv("C:/Users/manis/Desktop/Uber_ORM/Comments.csv",header=T)
Text
library(tm)
TextCorpus<- Corpus(DataframeSource(Text))

TextCorpus.processed<- tm_map(TextCorpus, content_transformer(tolower))

# Converting the words to lower case so that there is no difference between the same 
# word written in different cases, for example, "Panasonic" and "panasonic"
# Removing punctuation (for example, comma, period, etc.) 
TextCorpus.processed <- tm_map(TextCorpus.processed, content_transformer(removePunctuation))
# Removing numbers
TextCorpus.processed <- tm_map(TextCorpus.processed,content_transformer(removeNumbers))
# Removing stop words which are general words like "if", "between", "about", etc. To 
#see the entire list
TextCorpus.processed <- tm_map(TextCorpus.processed, content_transformer(removeWords), stopwords())
# shows the stop words 
stopwords()
# copying the corpus for future reference
TextCorpus.Copy <- TextCorpus.processed
TextCorpus.processed
# Creatining Document term Matrix
DocTrmMtrx <- DocumentTermMatrix(TextCorpus.processed)
DocTrmMtrx
DocTrmMtrx <- as.matrix(DocTrmMtrx)
# converting the document term matrix to a double matrix
DocTrmMtrx.TotalFreq <- colSums(DocTrmMtrx)
DocTrmMtrx.TotalFreq

# calculating the total term frequencies

# package for creating wordcloud
library(wordcloud)

wordcloud(words=colnames(DocTrmMtrx),freq= DocTrmMtrx.TotalFreq,min.freq= 2,scale=c(4,0.1), colors= brewer.pal(n= 10,"Paired"),random.order=F)

# reading the positive and negative words
pos <- readLines(con="C:\\Users\\manis\\Desktop\\Text Mining\\opinion-lexicon-English\\positive-words.txt")
neg <- readLines(con="C:\\Users\\manis\\Desktop\\Text Mining\\opinion-lexicon-English\\negative-words.txt")

# matching the terms with the dictionary of positive and negative words
pos.matches <- match(colnames(DocTrmMtrx),pos)
neg.matches <- match(colnames(DocTrmMtrx),neg)

# all the non-matches will be indicated by NA (that means the word is not in the document)
# converting the presence to true

pos.matches <- !is.na(pos.matches) # checking the value is not NA
neg.matches <- !is.na(neg.matches) # checking the value is not NA 

# creating respective matrices
pos.matrix <- DocTrmMtrx[,pos.matches]
inspect(pos.matrix)
neg.matrix <- DocTrmMtrx[,neg.matches]
neg.matrix
# calculating the sentiment score
sentiment.score <- rowSums(pos.matrix) - rowSums(neg.matrix)
sentiment.score
summary(sentiment.score)
hist(sentiment.score)
library(sentimentr)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr);
  require(stringr);
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[^A-z ]','', sentence)
    sentence = tolower(sentence);
    word.list = str_split(sentence, '\\s+');
    words = unlist(word.list);
    pos.matches = match(words, pos.words);
    neg.matches = match(words, neg.words);
    pos.matches = !is.na(pos.matches);
    neg.matches = !is.na(neg.matches);
    score = sum(pos.matches) - sum(neg.matches);
    return(score);
  }, pos.words, neg.words, .progress=.progress );
  scores.df = data.frame(score=scores, text=sentences);
  return(scores.df);
}
sample=c("You're awesome and I love you","I hate and hate and hate. So angry. Die!","Impressed and amazed: you are peerless in your achievement of unparalleled mediocrity.")
result=score.sentiment(sample,pos,neg)
result
