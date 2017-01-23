rm(list=ls())
setwd("E:/CMSA/Main")
lex_df<-function(pos_file="pos.csv",neg_file="neg.csv"){
  #get positive lexicons
  #pos_lex <- unique(read.csv(pos_file,header = FALSE))
  pos_lex <- unique(read.table(pos_file,header = FALSE,sep = ":"))
  pos_lex<-pos_lex[,1:2]
  pos_val<-rep.int(1, nrow(pos_lex))
  pos_lex["value"]<-pos_val
  names(pos_lex)<-c("lexicon_name","count","value")
  #get negative lexicons
  #neg_lex <- unique(read.csv(neg_file,header = FALSE))
  neg_lex <- unique(read.table(neg_file,header = FALSE,sep = ":"))
  neg_lex<-neg_lex[,1:2]
  neg_val<-rep.int(-1, nrow(neg_lex))
  neg_lex["value"]<-neg_val
  names(neg_lex)<-c("lexicon_name","count","value")

    #Combine positive and negative lexicons to a single dataframe
  rbind(pos_lex,neg_lex)
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
    combined_lexicons<-lex_df(pos_domains_files[i],neg_domains_files[i])
    #assign(paste(domains[i],"lex_df",sep="_"),lex_df(pos_domains_files[i],neg_domains_files[i]))
    #Clean/Remove NA values
    assign(paste(domains[i],"lex_df",sep="_"),combined_lexicons[complete.cases(combined_lexicons),])
   
  }