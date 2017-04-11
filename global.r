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
getvote<-function(b){
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
  
  tokens<-tokenize(paste(tweets,collapse = " " ))
  tokens<-unlist(tokens)
  match_tokens<-match(tokens,fet)
  match_tokens<-match_tokens[!is.na(match_tokens)]
  a<-sum(w[match_tokens])
  if(a>0.5)
  {
    fileimg <- normalizePath(file.path('./www',
                                          paste('handup','.png', sep='')))
    
  }
  else
  {
    
    fileimg <- normalizePath(file.path('./www',
                                       paste('handdown','.png', sep='')))
    
  }
#  source(file = "train.r", local = FALSE)
  shell(cmd = 'Rscript.exe computeCost.R', wait=FALSE)
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