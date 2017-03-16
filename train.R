MultiDomain  = function(X,y,d,p,alpha,beta,S,lambda1,lambda2, type)
{
  D = ncol(X);
  M = length(unique(d));
  w = matrix(data = 0,nrow = D,ncol = 1)
  W = matrix(data = 0,nrow = D,ncol = M)
  
 # X <- Matrix(X, sparse = TRUE)
  #y <- Matrix(y, sparse = TRUE)
# d <- Matrix(d, sparse = TRUE)
#  p <- Matrix(p, sparse = TRUE)
#  S <- Matrix(S, sparse = TRUE)
#  W <- Matrix(W, sparse = TRUE)
#  w <- Matrix(w, sparse = TRUE)
  
  w_2 = w;
  w_1 = w;
  W_2 = W;
  W_1 = W;
  f = computeLoss(X, y, d, p, alpha, beta, S, lambda1, lambda2, w, W, type,M)+lambda2*(sum(abs(w))+sum(sum(abs(W))));
  loss=f;
  k = 0;
  gamma = 1;
  while  (k<100 && gamma>10^(-40))
  {
    print(k)
    k=k+1;
    a = k/(k+3);
    
    y_w = (1+a)*w_1 - a*w_2;
    #print(y_w)
    y_W = (1+a)*W_1 - a*W_2;
    
    output=computeGradient(X, y, d, p, alpha, beta, S, lambda1, lambda2, y_w, y_W, type,M);
    g_w=output[[1]]
    g_W=output[[2]]
    w = y_w - gamma*g_w;
    W = y_W - gamma*g_W;
    w[abs(w)<=lambda2*gamma]=0;
    W[abs(W)<=lambda2*gamma]=0;
    y1 = sign(w);
    y2 = abs(w)-lambda2*gamma;
    w = y1*y2;
    y1 = sign(W);
    y2 = abs(W)-lambda2*gamma;
    W = y1*y2;
    ##I was here
    f = computeLoss(X,y,d,p,alpha,beta,S,lambda1,lambda2,w,W, type,M);
    f_y = computeLoss(X,y,d,p,alpha,beta,S,lambda1,lambda2,y_w,y_W, type,M);
    
    f_lip = f_y + t(g_w)%*%(w-y_w) + 1/2/gamma*norm_vec((w-y_w))^2;
    
    for (m in 1:M)
    {
      
      f_lip = f_lip+t(g_W[,m])%*%(W[,m]-y_W[,m])+1/2/gamma*norm_vec((W[,m]-y_W[,m]))^2;
      #####
    }
    
    iter = 0;
    
    while ((f>f_lip)[1] && gamma>10^(-40))
    {
      iter = iter+1;
      gamma = gamma/2;
      w = y_w - gamma*g_w;
      W = y_W - gamma*g_W;
      w[abs(w)<=lambda2*gamma]=0;
      W[abs(W)<=lambda2*gamma]=0;
      y1 = sign(w);
      y2 = abs(w)-lambda2*gamma;
      w = y1*y2;
      y1 = sign(W);
      y2 = abs(W)-lambda2*gamma;
      W = y1*y2;
      f = computeLoss(X,y,d,p,alpha,beta,S,lambda1,lambda2,w,W, type,M);
      f_lip = f_y + t(g_w)%*%(w-y_w) + 1/2/gamma*norm_vec((w-y_w))^2;
      for (m in 1:M){
        f_lip = f_lip+t(g_W[,m])%*%(W[,m]-y_W[,m])+1/2/gamma*norm_vec((W[,m]-y_W[,m]))^2;
      }
    }
    
    loss = cbind(loss, f+lambda2*(sum(abs(w))+sum(sum(abs(W)))));
    loss[ncol(loss)];
    w_2 = w_1;
    w_1 = w;
    W_2 = W_1;
    W_1 = W;
    
    if (k>1 && abs(loss[ncol(loss)]-loss[ncol(loss)-1])/abs(loss[ncol(loss)])<0.001)# || (loss(end)<0))
    break;
  }
  
  return(list(w,W))
  
}

#figure;
#plot(loss);


##################
computeGradient =function(X,y,d,p,alpha,beta,S,lambda1,lambda2,w,W, type,M){
N = nrow(X);
D = ncol(X);
#M = length(unique(d));
g_w = matrix(data = 0,nrow = nrow((w)),ncol = ncol(w))
g_W = matrix(data = 0,nrow = nrow(W),ncol = ncol(W))
g_w <- Matrix(g_w, sparse = TRUE)
g_W <- Matrix(g_W, sparse = TRUE)

if (type=="ls"){
for (m in 1:M){
temp = (2*t(X[as.vector(d==m),]))%*% ((X[as.vector(d==m),]%*%(w+W[,m])-y[d==m]));
g_w = g_w + temp;
g_W[,m] = g_W[,m] + temp;
}
}

for (m in 1:M){
  #----------------#####################
g_W[,m] = g_W[,m] + 4*beta*(sum(S[m,])*W[,m]-W%*%S[m,]);
}
g_w = g_w - alpha*p+(2*lambda1)*w;
g_W = g_W + (2*lambda1)*W;

return(list(g_w,g_W))
}

##################################
computeLoss<- function(X,y,d,p,alpha,beta,S,lambda1,lambda2,w,W, type,M){
N = nrow(X);
D = ncol(X);
#M = length(unique(d));
f = 0;

if (type=="ls")
{
for (m in 1:M)
  f = f+norm_vec((X[as.vector(d==m),]%*%(w+W[,m])-y[d==m]))^2  # sum((X[d==m,]%*%(w+W[,m])-y[d=m])^2);
}
f = f-alpha*t(p)%*%w;
f = f+lambda1*(sum(w^2)+sum(sum(W^2)));


for (i in 1:M){
    for (j in 1:M)
        f = f+beta*S[i,j]*sum((W[,i]-W[,j])^2);
}
f
}
norm_vec <- function(x) sqrt(sum(x^2))