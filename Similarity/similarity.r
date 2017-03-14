xmldatatable <- setDT(xmlToDataFrame("positive.xml"))





#sentiscore <- function(){}
domains=c("books","dvd","kitchen","electronics");i_ndomain=1;j_ndomain=2; 
#Get domain vector variables
domain1<-paste(domains[i_ndomain],"lex_dt",sep="_")
domain2<-paste(domains[j_ndomain],"lex_dt",sep="_")

#Clean domain lexicons
data1<-unique(get(domain1))[lexicon_name!=""]
data2<-unique(get(domain2))[lexicon_name!=""]
# is.numeric(get(domain2)[get(domain2)[,3]==1,2])

##Code Begins Here ##
#Get the positive and negative label count for both the domains

#assign(paste("posLabel1"),sum(as.numeric(data1[data1[,3]==1,2]),na.rm = TRUE))
posLabel1<-data1[value==1,sum(count)]
# assign(paste("negLabel1"),sum(as.numeric(data1[data1[,3]==-1,2]),na.rm = TRUE))
negLabel1<-data1[value==-1,sum(count)]

#assign(paste("posLabel2"),sum(as.numeric(data2[data2[,3]==1,2]),na.rm = TRUE))
#assign(paste("negLabel2"),sum(as.numeric(data2[data2[,3]==-1,2]),na.rm = TRUE))

#Get the total label count for both the domains
N1<-data1[,sum(count)]
#N2<-sum(as.numeric(data2[,2]),na.rm = TRUE)

#Calculate count of every term in its domain
#term_freq<-aggregate(data1$count, by=list(lexicon_name=data1$lexicon_name), FUN=sum)
term_freq<-data1[,.(count=sum(count)),by=lexicon_name]
all_terms<-term_freq[,lexicon_name]
#Preparing data
#pos_data1<-data1[data1[,3]==1,]
pos_data1<-data1[value==1]
#pos_term_freq<-aggregate(pos_data1$count, by=list(lexicon_name=pos_data1$lexicon_name), FUN=sum)
pos_term_freq<- pos_data1[,.(count=sum(count)),by=lexicon_name]
#neg_data1<-data1[data1[,3]==-1,]
neg_data1<-data1[value==-1]
#neg_term_freq<-aggregate(neg_data1$count, by=list(lexicon_name=neg_data1$lexicon_name), FUN=sum)
neg_term_freq<- neg_data1[,.(count=sum(count)),by=lexicon_name]

#
# pos_missing_terms<-setdiff(term_freq[,1], pos_term_freq[,1])
pos_missing_terms<-setdiff(term_freq[,lexicon_name], pos_term_freq[,lexicon_name])
#
pos_missing_freq<-data.table(lexicon_name=pos_missing_terms,count=0)
# neg_missing_terms<-setdiff(term_freq[,1], neg_term_freq[,1])
neg_missing_terms<-setdiff(term_freq[,lexicon_name], neg_term_freq[,lexicon_name])
neg_missing_freq<-data.table(lexicon_name=neg_missing_terms,count=0)

#s=c()
initial_time<- proc.time()
##Old code can be found at similaity.r
#s1=term_freq
pos_term_freq_n1<- pos_term_freq[,.(lexicon_name,count=count*N1)]
term_freq_posLabel1<-term_freq[,.(lexicon_name,count=count*posLabel1)]
neg_term_freq_n1<- neg_term_freq[,.(lexicon_name,count=count*N1)]
term_freq_negLabel1<-term_freq[,.(lexicon_name,count=count*negLabel1)]
pos_term_freq_n1<-rbind(pos_term_freq_n1,pos_missing_freq)
neg_term_freq_n1<-rbind(neg_term_freq_n1,neg_missing_freq)

#Sort the data tables accoding to lexicon names
setkey(pos_term_freq_n1,lexicon_name)
setkey(neg_term_freq_n1,lexicon_name)
setkey(term_freq_posLabel1,lexicon_name)
setkey(term_freq_negLabel1,lexicon_name)

#   s1<-as.integer(as.integer(pos_term_freq_n1[,count])/term_freq_posLabel1[,count])
s1<-pos_term_freq_n1[,count]/term_freq_posLabel1[,count]
#s1<-pos_term_freq_n1/term_freq_posLabel1
# s1<-s1[,]
# s1<-
s2<-neg_term_freq_n1[,count]/term_freq_negLabel1[,count]

s1<-log10(s1)
s1[s1==-Inf]<-0
s2<-log10(s2)
s2[s2==-Inf]<-0
s<-s1-s2
s3<-pos_term_freq_n1[,count:=s]
finaltime<-proc.time()-initial_time 


















################
similarity <- function(i,j) {
  
}
for (i in 1:length(all_terms))
{
  print(i)
  current_term<-all_terms[i]
  if (length(pos_missing_terms[pos_missing_terms[]==current_term])!=0) {
    # print("pos")
    s1=0
  }
  else
  {
    # s1<-log10((pos_term_freq[pos_term_freq[,1]==current_term,2][1]*N1)/(term_freq[term_freq[,1]==current_term,2][1]* posLabel1 ))
    s1<-log10((pos_term_freq[lexicon_name==current_term,count]*N1)/(term_freq[lexicon_name==current_term,count]* posLabel1 ))
    
  }
  if (length(neg_missing_terms[neg_missing_terms[]==current_term])!=0) {
    # print("neg")
    s2=0
  }
  else
  {
    s2<-log10((neg_term_freq[lexicon_name==current_term,count]*N1)/(term_freq[lexicon_name==current_term,count]* posLabel1 ))
  }
  #if()
  s<-c(s,(s1+s2))
}