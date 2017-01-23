sentiscore <- function(){}
  domains=c("books","dvd","kitchen","electronics");i_ndomain=1;j_ndomain=2; 
  #Get domain vector variables
  domain1<-paste(domains[i_ndomain],"lex_df",sep="_")
  domain2<-paste(domains[j_ndomain],"lex_df",sep="_")
  
  #Clean domain lexicons
  data1<-unique(removeNA(get(domain1)))
  data1<-data1[data1[,1]!="",]
  data2<-unique(removeNA(get(domain2)))
  data2<-data2[data2[,1]!="",]
 # is.numeric(get(domain2)[get(domain2)[,3]==1,2])
  ##Theorotical Explanation of the code
  
  #Given a set of labeled samples in each domain, in this
  #paper, we propose to extract a sentiment word vector for each
  #domain. Denote si {R D×1} as the sentiment word vector of
  # domain i, where si
  #t is the sentiment score of the term t. The
  #sentiment score of each term in a domain is computed using
  #their associations with positive label in that domain minus their
  #associations with negative label. In this paper we use pointwise
  #mutual information (PMI) to measure the associations
  #between terms and labels, and the sentiment score of term t
  #in domain i can be formulated as follows:
  #  s(i,t) =PMI(t, posLabeli) ??? PMI(t, negLabeli)
  #=log { [n(t, posLabeli)Ni]/[n(t)n(posLabeli)] ??? log [n(t, negLabeli)Ni]/[n(t)n(negLabeli)]}
  #=log { [n(t, posLabeli)n(negLabeli)] / n(t, negLabeli)n(posLabeli)    .......(3)
  #where n(posLabeli) and n(negLabeli) are the numbers of
  #positive samples and negative samples in domain i respectively,
  #and n(t, posLabeli) and n(t, negLabeli) are the frequencies
  #of term t occurring in positive and negative samples in domain
  #i respectively. Ni is the number of samples in domain i.
  #After extracting the sentiment word vector for each domain,
  #we can compute the sentiment similarity of two domains using
  #the cosine similarity of their sentiment word vectors, which is
  #defined as follows:
  #  SentiSim(i, j) = ( si · sj) /(||si||2 · ||sj||2)     ...... (4)
  #Note that SentiSim(i, j) defined in Eq. (4) can be negative,
  #although the probability is very small. In this paper, we
  #constrain that the similarities between domains should be nonnegative.
  #If the SentiSim score of a pair of domains is
  #negative, we set it to 0.
 
  
  ##Code Begins Here ##
  #Get the positive and negative label count for both the domains
  
  assign(paste("posLabel1"),sum(as.numeric(data1[data1[,3]==1,2]),na.rm = TRUE))
  assign(paste("negLabel1"),sum(as.numeric(data1[data1[,3]==-1,2]),na.rm = TRUE))
  assign(paste("posLabel2"),sum(as.numeric(data2[data2[,3]==1,2]),na.rm = TRUE))
  assign(paste("negLabel2"),sum(as.numeric(data2[data2[,3]==-1,2]),na.rm = TRUE))
  
  #Get the total label count for both the domains
  N1<-sum(as.numeric(data1[,2]),na.rm = TRUE)
  N2<-sum(as.numeric(data2[,2]),na.rm = TRUE)
  
  #Calculate count of every term in its domain
  term_freq<-aggregate(data1$count, by=list(lexicon_name=data1$lexicon_name), FUN=sum)
  all_terms<-term_freq[,1]
  #Preparing data
  pos_data1<-data1[data1[,3]==1,]
  pos_term_freq<-aggregate(pos_data1$count, by=list(lexicon_name=pos_data1$lexicon_name), FUN=sum)
  neg_data1<-data1[data1[,3]==-1,]
  neg_term_freq<-aggregate(neg_data1$count, by=list(lexicon_name=neg_data1$lexicon_name), FUN=sum)
  #
  pos_missing_terms<-setdiff(term_freq[,1], pos_term_freq[,1])
  #
  neg_missing_terms<-setdiff(term_freq[,1], neg_term_freq[,1])
  s=c()
  for (i in 1:length(all_terms))
  {
    print(i)
    current_term<-all_terms[i]
    if (length(pos_missing_terms[current_term])==0) {
      s1=0
    }
    else
    {
      s1<-log10((pos_term_freq[pos_term_freq[,1]==current_term,2][1]*N1)/(termfreq[termfreq[1,]==current_term,2][1]* posLabel1 ))
    }
    if (length(neg_missing_terms[current_term])==0) {
      s2=0
    }
    else
    {
      s2<-log10((neg_term_freq[neg_term_freq[,1]==current_term,2][1]*N1)/(termfreq[termfreq[1,]==current_term,2][1]* negLabel1 ))
    }
    s<-c(s,(s1+s2))
  }
  #s1=
  #s2=
  #s=s1+s2