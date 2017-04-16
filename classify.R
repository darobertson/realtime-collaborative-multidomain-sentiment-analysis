classify<-function(check_class_text,domain)
{
  fet<-rownames(w)
  tokens<-tokenize(paste(check_class_text,collapse = " " ))
  tokens<-unlist(tokens)
  match_tokens<-match(tokens,fet)
  match_tokens<-match_tokens[!is.na(match_tokens)]
  class=sum(w[match_tokens]+W[match_tokens,domain],na.rm = TRUE)
  if (class<0)
    class=-1
  if (class>0)
    class=1
  return(class)
}
#test_y<-c(0)
#j=1
#for( i in c(1401:2000,3401:4000,5401:6000,7401:8000))
#{
# print(i) 
#  test_y[j]<-classify(X[i])
#    j=j+1
#}

