fet<-rownames(w)
tokens<-tokenize(paste(tweets.df$text,collapse = " " ))
tokens<-unlist(tokens)
match_tokens<-match(tokens,fet)
match_tokens<-match_tokens[!is.na(match_tokens)]
class=sum(w[match_tokens],na.rm = TRUE)
if (class<0)
  class=-1
if (class>0)
  class=1
