computeLoss<- function(X,y,d,p,alpha,beta,S,lambda1,lambda2,w,W, type){
N = nrow(X);
D = ncol(X);
M = length(unique(d));
f = 0;

if (type=="ls")
{
for (m in 1:M)
f = f+sum((X[d==m,]%*%(w+W[,m])-y[d=m])^2);
}
f = f-alpha*t(p)%*%w;
f = f+lambda1*(sum(w^2)+sum(sum(W^2)));
for (i in 1:M){
    for (j in 1:M)
        f = f+beta*S[i,j]*sum((W[,i]-W[,j])^2);
    }
}
norm_vec <- function(x) sqrt(sum(x^2))