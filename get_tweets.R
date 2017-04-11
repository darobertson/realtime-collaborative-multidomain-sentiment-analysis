require(twitteR)
require(stringr)
require(RCurl)
ck<-'yRudGaS3f1ySiyrIlkYoBhtPM'
cs<-'Ne694V5pv5q5j6x0e8SlSLnJVfVZ6NNTYWBFpnCASujSGiOUDa'
at<-'761938864697122817-xWZSEcLJUAX70PMrpfnkbiSTW2IL4MH'
as<-'XizvBqirw02MoFCzdiWAXEa8s3V2dbHEj3OXT797FTOye'
setup_twitter_oauth(ck,cs,at,as)
tweets<-searchTwitter("motog5",n=1000,lang="en")
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