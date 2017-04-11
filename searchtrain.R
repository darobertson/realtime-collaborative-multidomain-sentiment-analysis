time<-proc.time()
require(Matrix)



predictionData <- tweets

predMatrix <- create_matrix(predictionData, originalMatrix=dtMatrix)
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
results <- classify_model(predictionContainer, model)
ab=results[which(results$SVM_PROB>0.80),]
pro_book=length(which(ab$SVM_LABEL==1))
pro_elec=length(which(ab$SVM_LABEL==2))



predMatrix1 <- create_matrix(predictionData, originalMatrix=dtMatrix1)
predSize1 = length(predictionData);
predictionContainer1 <- create_container(predMatrix1, labels=rep(0,predSize1), testSize=1:predSize1, virgin=FALSE)
results <- classify_model(predictionContainer1, model1)
ab=results[which(results$SVM_PROB>0.80),]
pro_kitchen=length(which(ab$SVM_LABEL==1))
pro_dvd=length(which(ab$SVM_LABEL==2))

score=c(pro_book,pro_dvd,pro_kitchen,pro_elec)
detect_domain<-which.max(score)
#detect_domain=4
print(detect_domain)
library(rJava)
.jinit()
senti <- J("senti")


new_data <- dfm(as.matrix(tweets), n = 1, removePunct = TRUE, removeNumbers = TRUE)
new_features<-setdiff(colnames(new_data),fet)
test<-match(colnames(new_data),new_features)
test<-test[!is.na(test)]
stripped_new_data<-new_data[,test]
stripped_new_data[apply(stripped_new_data[,-1], 1, function(x) !all(x==0)),]
extra_rows<-matrix(0,nrow = 4,ncol = ncol(stripped_new_data))
extra_rows<-as.dfm(extra_rows)
colnames(extra_rows)<-colnames(stripped_new_data)
train_data<-rbind(stripped_new_data,extra_rows)
train_d<-c(rep.int(detect_domain,times = nrow(stripped_new_data)),c(1,2,3,4))
train_p <- senti$getsenti(.jarray(new_features))
train_y<-rep.int(class,nrow(train_data))
test <- MultiDomain(train_data, train_y, train_d, train_p, alpha, beta, S, lambda1, lambda2, type)
new_w<-unlist(test[[1]])
new_W<-unlist(test[[2]])
w<-(rbind(w,new_w))

W<-(rbind(W,new_W))
print(proc.time()-time)