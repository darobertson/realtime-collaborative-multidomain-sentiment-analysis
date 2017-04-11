
Conversation opened. 1 unread message.

Skip to content
Using Gmail with screen readers
Oops! Your selected image failed to load.
Something's not right.
We're having trouble connecting to Google. We'll keep trying...
1 of 1,074

proj
Inbox
x
tom cool

Attachments9:55 PM (11 hours ago)

to me
Attachments area

Click here to Reply or Forward
14.61 GB (97%) of 15 GB used
Manage
Terms - Privacy
Last account activity: 1 day ago
Details



a=proc.time()
require(twitteR)
require(stringr)
require(RCurl)
require(qdap)
require(wordcloud)
require(tm)
require(ngram)
require(XML)
require(class)
require(tm)
require(RTextTools)
require(shiny)
setwd("D:/CMSA/Main")
ck<-'yRudGaS3f1ySiyrIlkYoBhtPM'
cs<-'Ne694V5pv5q5j6x0e8SlSLnJVfVZ6NNTYWBFpnCASujSGiOUDa'
at<-'761938864697122817-xWZSEcLJUAX70PMrpfnkbiSTW2IL4MH'
as<-'XizvBqirw02MoFCzdiWAXEa8s3V2dbHEj3OXT797FTOye'
setup_twitter_oauth(ck,cs,at,as)
elec_neg <- xmlToDataFrame("electronics_neg.xml")
elec_pos <- xmlToDataFrame("electronics_pos.xml")
books_pos <- xmlToDataFrame("books_pos.xml")
books_neg <- xmlToDataFrame("books_neg.xml")
books_neg=cbind(books_neg,"category")
books_neg$"category"=1
books_pos=cbind(books_pos,"category")
books_pos$"category"=1
elec_neg=cbind(elec_neg,"category")
elec_neg$"category"=2
elec_pos=cbind(elec_pos,"category")
elec_pos$"category"=2
books=rbind(books_pos,elec_pos)
elec=rbind(books_neg,elec_neg)
alldata=rbind(books,elec)
#read data 2


dvd_neg <- xmlToDataFrame("dvd_neg.xml")
dvd_pos <- xmlToDataFrame("dvd_pos.xml")
kitchen_pos <- xmlToDataFrame("kitchen_pos.xml")
kitchen_neg <- xmlToDataFrame("kitchen_neg.xml")

kitchen_neg=cbind(kitchen_neg,"category")
kitchen_neg$"category"=1
kitchen_pos=cbind(kitchen_pos,"category")
kitchen_pos$"category"=1
dvd_neg=cbind(dvd_neg,"category")
dvd_neg$"category"=2
dvd_pos=cbind(dvd_pos,"category")
dvd_pos$"category"=2
kitchen=rbind(kitchen_pos,dvd_pos)
dvd=rbind(kitchen_neg,dvd_neg)
alldata1=rbind(kitchen,dvd)

#FOR DATA1
dtMatrix <- create_matrix(alldata$review_text)
container <- create_container(dtMatrix, alldata$category, trainSize=1:4000, virgin=FALSE)
model <- train_model(container, "SVM", kernel="linear", cost=1)
#detect in part2
dtMatrix1 <- create_matrix(alldata1$review_text)
container1 <- create_container(dtMatrix1, alldata1$category, trainSize=1:4000, virgin=FALSE)
model1 <- train_model(container1, "SVM", kernel="linear", cost=1)


##TWITTER

tweets<-searchTwitter("lenovo",n=1000,lang="en")
tweets<-strip_retweets(tweets)
tweets.df <- twListToDF(tweets)
tweets.df$text=gsub('http.*\\s*', '', tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text <- str_replace_all(tweets.df$text,"\n","")
tweets.df$text=str_replace_all(tweets.df$text,"&amp;"," and ")
tweets.df$text=gsub("[^\x20-\x7E]", "", tweets.df$text)	
tweets=unique(tweets.df$text)

predictionData <- tweets

predMatrix <- create_matrix(predictionData, originalMatrix=dtMatrix)
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
results <- classify_model(predictionContainer, model)
ab=results[which(results$SVM_PROB>0.80),]
pro_book=length(which(ab$SVM_LABEL==1))
pro_elec=length(which(ab$SVM_LABEL==2))



predMatrix1 <- create_matrix(predictionData, originalMatrix=dtMatrix1)
predSize1 = length(predictionData);
predictionContainer1 <- create_container(predMatrix1, labels=rep(0,predSize1), testSize=1:predSize1, virgin=FALSE)
results <- classify_model(predictionContainer1, model1)
ab=results[which(results$SVM_PROB>0.80),]
pro_kitchen=length(which(ab$SVM_LABEL==1))
pro_dvd=length(which(ab$SVM_LABEL==2))

score=c(pro_book,pro_dvd,pro_elec,pro_kitchen)
which.max(score)
proc.time()-a












------------
tweets.df <- twListToDF(tweets)
a=corpus(tweets.df$text)
b=dfm(a,ngrams=1,removePunct=TRUE,removeNumbers=TRUE)
b[,1:15]	




------------------------------------------------------------------------------

http://www.svm-tutorial.com/2014/11/svm-classify-text-r/

a=proc.time()
require(twitteR)
require(stringr)
require(RCurl)
require(qdap)
require(wordcloud)
require(tm)
require(ngram)
require(XML)
require(class)
require(tm)
require(RTextTools)
setwd("D:/CMSA/Main")
ck<-'yRudGaS3f1ySiyrIlkYoBhtPM'
cs<-'Ne694V5pv5q5j6x0e8SlSLnJVfVZ6NNTYWBFpnCASujSGiOUDa'
at<-'761938864697122817-xWZSEcLJUAX70PMrpfnkbiSTW2IL4MH'
as<-'XizvBqirw02MoFCzdiWAXEa8s3V2dbHEj3OXT797FTOye'
setup_twitter_oauth(ck,cs,at,as)
tweets<-searchTwitter("furniture",n=1000,lang="en")
tweets<-strip_retweets(tweets)
tweets.df <- twListToDF(tweets)
tweets.df$text=gsub('http.*\\s*', '', tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text <- str_replace_all(tweets.df$text,"\n","")
tweets.df$text=str_replace_all(tweets.df$text,"&amp;"," and ")
tweets.df$text=gsub("[^\x20-\x7E]", "", tweets.df$text)	
tweets=unique(tweets.df$text)
elec_neg <- xmlToDataFrame("electronics_neg.xml")
elec_pos <- xmlToDataFrame("electronics_pos.xml")
books_pos <- xmlToDataFrame("books_pos.xml")
books_neg <- xmlToDataFrame("books_neg.xml")
books_neg=cbind(books_neg,"category")
books_neg$"category"=1
books_pos=cbind(books_pos,"category")
books_pos$"category"=1
elec_neg=cbind(elec_neg,"category")
elec_neg$"category"=2
elec_pos=cbind(elec_pos,"category")
elec_pos$"category"=2
books=rbind(books_pos,elec_pos)
elec=rbind(books_neg,elec_neg)
alldata=rbind(books,elec)
trace("create_matrix",edit=T)

dtMatrix <- create_matrix(alldata$review_text)
container <- create_container(dtMatrix, alldata$category, trainSize=1:4000, virgin=FALSE)
model <- train_model(container, "SVM", kernel="linear", cost=1)
predictionData <- tweets

predMatrix <- create_matrix(predictionData, originalMatrix=dtMatrix)
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
results <- classify_model(predictionContainer, model)
ab=results[which(results$SVM_PROB>0.80),]
pro_book=length(which(ab$SVM_LABEL==1))
pro_elec=length(which(ab$SVM_LABEL==2))


dvd_neg <- xmlToDataFrame("dvd_neg.xml")
dvd_pos <- xmlToDataFrame("dvd_pos.xml")
kitchen_pos <- xmlToDataFrame("kitchen_pos.xml")
kitchen_neg <- xmlToDataFrame("kitchen_neg.xml")

kitchen_neg=cbind(kitchen_neg,"category")
kitchen_neg$"category"=1
kitchen_pos=cbind(kitchen_pos,"category")
kitchen_pos$"category"=1
dvd_neg=cbind(dvd_neg,"category")
dvd_neg$"category"=2
dvd_pos=cbind(dvd_pos,"category")
dvd_pos$"category"=2
kitchen=rbind(kitchen_pos,dvd_pos)
dvd=rbind(kitchen_neg,dvd_neg)
alldata1=rbind(kitchen,dvd)

dtMatrix <- create_matrix(alldata1$review_text)
container <- create_container(dtMatrix, alldata1$category, trainSize=1:4000, virgin=FALSE)
model <- train_model(container, "SVM", kernel="linear", cost=1)
predictionData <- tweets

predMatrix <- create_matrix(predictionData, originalMatrix=dtMatrix)
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
results <- classify_model(predictionContainer, model)
ab=results[which(results$SVM_PROB>0.80),]
pro_kitchen=length(which(ab$SVM_LABEL==1))
pro_dvd=length(which(ab$SVM_LABEL==2))

score=c(pro_book,pro_dvd,pro_elec,pro_kitchen)
which.max(score)
proc.time()-a

pro.txt
Displaying pro.txt.