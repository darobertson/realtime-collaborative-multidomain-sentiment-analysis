command=paste("java SentiWordNetDemoCode",paste(features[1:4046],collapse = " "))
test<-system(command,intern = TRUE)
#Multidomain<-function()
lapply(features,comm(features))
comm<-function(feature)
{
  command<- paste("java SentiWordNetDemoCode",feature)
  
  system(command,intern = TRUE)
  
}