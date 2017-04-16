require(twitteR)
require(stringr)
require(RCurl)
require(qdap)
require(wordcloud)
require(tm)
ck<-'yRudGaS3f1ySiyrIlkYoBhtPM'
cs<-'Ne694V5pv5q5j6x0e8SlSLnJVfVZ6NNTYWBFpnCASujSGiOUDa'
at<-'761938864697122817-xWZSEcLJUAX70PMrpfnkbiSTW2IL4MH'
as<-'XizvBqirw02MoFCzdiWAXEa8s3V2dbHEj3OXT797FTOye'
#source(file = "train.r", local = FALSE)
#source(file = "searchtrain.r", local = FALSE)
#source(file = "svmmodel.r", local = FALSE)
#source(file = "classify.r", local = FALSE)
#source(file = "get_tweets.r", local = FALSE)
#source(file = "train.r", local = FALSE)

setup_twitter_oauth(ck,cs,at,as)
getvote<-function(b){
#  tweets<-searchTwitter(b,n=200,lang="en")
#  tweets<-strip_retweets(tweets)
#  tweets.df <- twListToDF(tweets)
#  tweets.df$text=gsub('http.*\\s*', '', tweets.df$text)
#  tweets.df$text = gsub("@\\w+", "", tweets.df$text)
#  tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
#  tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
#  tweets.df$text <- str_replace_all(tweets.df$text,"\n","")
#  tweets.df$text=str_replace_all(tweets.df$text,"&amp;"," and ")
#  tweets.df$text=gsub("[^\x20-\x7E]", "", tweets.df$text)
#  tweets=unique(tweets.df$text)
  
  tweets<-get_tweets(b)
  
  domain<-get_domain(tweets)
  class<-classify(tweets,domain)
  if(class==1)
  {
    fileimg <- normalizePath(file.path('./www',
                                          paste('handup','.png', sep='')))
  }
  else
  {
    fileimg <- normalizePath(file.path('./www',
                                       paste('handdown','.png', sep='')))
  }
  test<-train(tweets,class,domain)
  new_w<-unlist(test[[1]])
  new_W<-unlist(test[[2]])
  w<<-rbind(w,new_w)
  print(length(w))
  W<<-rbind(W,new_W)
  return(list(src = fileimg))
  
}
getword<-function(b){
  tweets<-searchTwitter(b,n=200,lang="en")
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
  
  tweets_corpus<-Corpus(VectorSource((tweets)))
  return(wordcloud(tweets_corpus))
  
}