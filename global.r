
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
setup_twitter_oauth(ck,cs,at,as)
setup_twitter_oauth(ck,cs,at,as)
getcloud<-function(b){
  tweets<-searchTwitter(b,n=1000,lang="en")
  tweets<-strip_retweets(tweets)
  tweets.df <- twListToDF(tweets)
  tweets.df$text=gsub('http.*\\s*', '', tweets.df$text)
  tweets.df$text = gsub("@\\w+", "", tweets.df$text)  
  tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
  tweets.df$text <- str_replace_all(tweets.df$text,"\n","")
  tweets.df$text=str_replace_all(tweets.df$text,"&amp;"," and ")
  tweets.df$text=gsub("[^\x20-\x7E]", "", tweets.df$text)
  tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
  tweets=unique(tweets.df$text)
  rw<-c("a","an","the","you","me","i","then","do","with","just","it","while","he","She","us","is","to","for","of","this","up","at","in","on","over","you","I","The","THE","be","has","by","tweets","about","from","after","my","that","your","we")
  tweets=removeWords(tweets,rw)
  
  tweets_corpus<-Corpus(VectorSource((tweets)))
  
  return(wordcloud(tweets_corpus))
}

