require(RTextTools)
require(XML)
elec_neg <- xmlToDataFrame("electronics_neg.xml")
elec_pos <- xmlToDataFrame("electronics_pos.xml")
books_pos <- xmlToDataFrame("books_pos.xml")
books_neg <- xmlToDataFrame("books_neg.xml")
books_neg=cbind(books_neg,"category")
books_neg$"category"=1
books_pos=cbind(books_pos,"category")
books_pos$"category"=1
elec_neg=cbind(elec_neg,"category")
elec_neg$"category"=2
elec_pos=cbind(elec_pos,"category")
elec_pos$"category"=2
books=rbind(books_pos,elec_pos)
elec=rbind(books_neg,elec_neg)
alldata=rbind(books,elec)
#read data 2


dvd_neg <- xmlToDataFrame("dvd_neg.xml")
dvd_pos <- xmlToDataFrame("dvd_pos.xml")
kitchen_pos <- xmlToDataFrame("kitchen_pos.xml")
kitchen_neg <- xmlToDataFrame("kitchen_neg.xml")

kitchen_neg=cbind(kitchen_neg,"category")
kitchen_neg$"category"=1
kitchen_pos=cbind(kitchen_pos,"category")
kitchen_pos$"category"=1
dvd_neg=cbind(dvd_neg,"category")
dvd_neg$"category"=2
dvd_pos=cbind(dvd_pos,"category")
dvd_pos$"category"=2
kitchen=rbind(kitchen_pos,dvd_pos)
dvd=rbind(kitchen_neg,dvd_neg)
alldata1=rbind(kitchen,dvd)

#FOR DATA1
dtMatrix <- create_matrix(alldata$review_text)
container <- create_container(dtMatrix, alldata$category, trainSize=1:4000, virgin=FALSE)
model <- train_model(container, "SVM", kernel="linear", cost=1)
#detect in part2
dtMatrix1 <- create_matrix(alldata1$review_text)
container1 <- create_container(dtMatrix1, alldata1$category, trainSize=1:4000, virgin=FALSE)
model1 <- train_model(container1, "SVM", kernel="linear", cost=1)
