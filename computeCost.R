 computeCost<- function()
{
  

require(XML)
require(data.table)
M<-length(domains)
terms_all_domains<-unique(c(books_score[,lexicon_name],kitchen_score[,lexicon_name],dvd_score[,lexicon_name],electronics_score[,lexicon_name]))
#Dictionary Size
D <- length(terms_all_domains)
pos_xml_files<-paste(pos_domains,"xml",sep = ".")
         neg_xml_files<-paste(neg_domains,"xml",sep = ".")

for( i in 1:length(domains))
{
  posxmldatatable<-setDT(xmlToDataFrame(pos_xml_files[i],stringsAsFactors = FALSE))
  pos_review_text<-setDT(list(posxmldatatable[,review_text]))
  negxmldatatable<-setDT(xmlToDataFrame(neg_xml_files[i],stringsAsFactors = FALSE))
  
  neg_review_text<-setDT(list(negxmldatatable[,review_text]))
  assign(paste("X",i,sep="_"),rbind.data.frame(  pos_review_text,  neg_review_text)) 
  
  #assign(paste("X",i,sep="_"),get(paste("X",i,sep="_"))[,review_text]) 
  assign(paste("y",i,sep="_"),c(rep.int(x = 1,times = nrow(posxmldatatable)),rep.int(x = -1,times = nrow(negxmldatatable))))
  assign(paste("d",i,sep="_"),c(rep.int(x =i,times = nrow(posxmldatatable)+nrow(negxmldatatable))))
  
}
# count_matrix<-matrix(data = 0,nrow = 2,ncol = length(terms_all_domains))
# library(stringr)
#for(k in 1:length(terms_all_domains))
{
#  count_matrix[1][k]=str_count(X_1[1], terms_all_domains[k])
}

#xmldatatable <- setDT(xmlToDataFrame("positive.xml"))
#w<-matrix(data = 0,ncol = 1  ,nrow = D)
#W<-matrix(data=0,ncol = M,nrow =D )
#domain_dt<-paste(domains,"lex_dt",sep = "_")

#N<-c();
#for( i in 1:length(domains))
##{
# N<-c(N,nrow(get(domain_dt[i])))
#}
#N<-matrix(N,nrow = length(N))
#maxlen<-max(N)
##X<-matrix("",nrow = maxlen,ncol = M)
#X<-matrix()
#for( i in 1:length(domains))
#{
# cbind(X,c(get(domain_dt[i])[,lexicon_name],rep(NA,maxlen-N[i])))
#}
#X1<-matrix(data = 0,nrow = )
 }