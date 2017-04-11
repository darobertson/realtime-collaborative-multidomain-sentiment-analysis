setwd("E:/CMSA/Main")
source(file = "get_lex.r", local = FALSE)
source(file = "pmi.r", local = FALSE)
source(file = "computeCost.r", local = FALSE)
source(file = "train.r", local = FALSE)
require(Rcpp)
sourceCpp("test1.cpp")
sourceCpp("test.cpp")
require(quanteda)
X_combined <- rbind(X_1, X_2, X_3, X_4)
d <- c(d_1, d_2, d_3, d_4)
X <- dfm(as.matrix(X_combined), n = 1, removePunct = TRUE, removeNumbers = TRUE)

y <- c(y_1, y_2, y_3, y_4)
features <- colnames(X)
library(rJava)
.jinit()
senti <- J("senti")
p <- senti$getsenti(.jarray(features))
alpha = 0.3;
beta = 0.5;
lambda1 = 6;
lambda2 = 0.55;
type = "ls";
S <- SentiSim
multi_res <- MultiDomain(X, y, d, p, alpha, beta, S, lambda1, lambda2, type)
w<-unlist(multi_res[[1]])
W<-unlist(multi_res[[2]])
require(microbenchmark)
print(microbenchmark(times = 10, MultiDomain(X, y, d, p, alpha, beta, S, lambda1, lambda2, type)))
