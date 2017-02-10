rm(list=ls())
options(stringsAsFactors = FALSE)
setwd("E:/CMSA/Main")
require(data.table)
lex_dt<-function(pos_file="pos.csv",neg_file="neg.csv"){
  #get positive lexicons
  #pos_lex <- unique(read.csv(pos_file,header = FALSE))
  pos_lex <- setDT(unique(read.table(pos_file,header = FALSE,sep = ":",stringsAsFactors = FALSE)))
  #pos_lex<-pos_lex[,.(lexicon_name=V1,count=V2)]
  pos_lex<-pos_lex[,.(lexicon_name=V1,count=V2)]
  pos_val<-rep.int(1, nrow(pos_lex))
  pos_lex[,value:=pos_val]
  
#  pos_lex["value"]<-pos_val
 # names(pos_lex)<-c("lexicon_name","count","value")
  #get negative lexicons
  #neg_lex <- unique(read.csv(neg_file,header = FALSE))
  neg_lex <- setDT(unique(read.table(neg_file,header = FALSE,sep = ":",stringsAsFactors = FALSE)))
  neg_lex<-neg_lex[,.(lexicon_name=V1,count=V2)]
  neg_val<-rep.int(-1, nrow(neg_lex))
  neg_lex[,value:=neg_val]
 # names(neg_lex)<-c("lexicon_name","count","value")
  pos_lex<-pos_lex[!lexicon_name%in%neg_lex[,lexicon_name]]
  neg_lex<-neg_lex[!lexicon_name%in%pos_lex[,lexicon_name]]
    #Combine positive and negative lexicons to a single dataframe
  rbind(pos_lex,neg_lex)
 # setdiff(union(unique(lex_dt[value==1,lexicon_name]),unique(lex_dt[value==-1,lexicon_name])),intersect(unique(lex_dt[value==1,lexicon_name]),unique(lex_dt[value==-1,lexicon_name])))
}
removeNA<- function(df){
  df[complete.cases(df),]
}
  #define the domains
  domains<-c("books","dvd","kitchen","electronics")
  #create list of domain file names
  pos_domains<-paste(domains,"pos",sep = "_")
  neg_domains<-paste(domains,"neg",sep = "_")
  pos_domains_files<-paste(pos_domains,"review",sep = ".")
  neg_domains_files<-paste(neg_domains,"review",sep = ".")
  #get all domain lexicons and store into dynamic variables Format: <domain_name>_lex_df
  #combined_lexicons<-data.frame(matrix(ncol=3,nrow=1))
  for (i in 1:length(domains))
  {
    combined_lexicons<-lex_dt(pos_domains_files[i],neg_domains_files[i])
    combined_lexicons<-combined_lexicons[lexicon_name!=""]
    #assign(paste(domains[i],"lex_df",sep="_"),lex_df(pos_domains_files[i],neg_domains_files[i]))
    #Clean/Remove NA values
    assign(paste(domains[i],"lex_dt",sep="_"),combined_lexicons[complete.cases(combined_lexicons),]) ##
  }
  