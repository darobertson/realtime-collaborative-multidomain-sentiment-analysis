  Calculate similarity using corpus and sparse matrix
  Check whether the answer is correct when regression is calculated using sparse matrix
  -Prepare data for regression
  setwd("E:/CMSA/Main")
  source(file = "get_lex.r",local = FALSE)
  source(file = "pmi.r",local = FALSE)
  source(file = "computeCost",local = FALSE)
  #Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111')
  #require(rJava)
  #.jinit()
  require(quanteda)
  X_combined<-rbind(X_1,X_2,X_3,X_4)
  d<-c(d_1,d_2,d_3,d_4)
  X<-dfm(as.matrix(X_combined),n=1,removePunct = TRUE, removeNumbers = TRUE)
  
  y<-c(y_1,y_2,y_3,y_4)
  features<-colnames(X)
  library(rJava)
   .jinit()
   senti<-J("senti")
   p<-senti$getsenti(.jarray(features))
   
   
   obj=.jnew("SentiWordNetDemoCode")
  command=paste("java SentiWordNetDemoCode ",paste(features,collapse = " "))
  convert(y,to=data.frame()
  ))
  
  
  senti<-J("senti")
  senti$getsenti(.jarray(c("good","bad")))
  
  T <- J( "Test" )
  T$sample("1")
  #X=[1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0;0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0;0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0;0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1];
  
  #myMatrix<-matrix(data = 1:100000000)
  #system.time(apply(myMatrix, 2, function(x) x^2))
  
  library(Rcpp)
  
  A <- matrix(rnorm(10000), 100, 100)
  B <- matrix(rnorm(10000), 100, 100)
  
  library(microbenchmark)
  sourceCpp("test.cpp")
  microbenchmark(A%*%B, armaMatMult(A, B), eigenMatMult(A, B), eigenMapMatMult(A, B))
  
  
  
  yy=matrix(c(1,-1,-1,1),nrow=4,ncol=1);
  dd=matrix(c(1,1,2,2),nrow=4,ncol=1);
  pp=matrix(c(1,0,-1,0,-1,-1,0,0,1,1,1,0,0,0,1,0,-1,1,1,-1,-1),nrow = 21,ncol = 1);
  XX=t(matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1),nrow = 21,ncol=4));
  
  
  
  alpha=0.3;
  beta=0.5;
  lambda1=6;
  lambda2=0.55;
  type="ls";
  S<-SentiSim
  
  
  
  SS=matrix(c(0,0,0,0),nrow = 2,ncol = 2);
  
  
  
  
  
  
  
  MultiDomain  = function(X,y,d,p,alpha,beta,S,lambda1,lambda2, type)
  {
    D = ncol(X);
    M = length(unique(d));
    w = matrix(data = 0,nrow = D,ncol = 1)
    W = matrix(data = 0,nrow = D,ncol = M)
    w_2 = w;
    w_1 = w;
    W_2 = W;
    W_1 = W;
    f = computeLoss(X, y, d, p, alpha, beta, S, lambda1, lambda2, w, W, type)+lambda2*(sum(abs(w))+sum(sum(abs(W))));
    loss=f;
    k = 0;
    gamma = 1;
    while  (k<1000 && gamma>10^(-40))
    {
      k=k+1;
      a = k/(k+3);
      
      y_w = (1+a)*w_1 - a*w_2;
      #print(y_w)
      y_W = (1+a)*W_1 - a*W_2;
      
      output=computeGradient(X, y, d, p, alpha, beta, S, lambda1, lambda2, y_w, y_W, type);
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
      f = computeLoss(X,y,d,p,alpha,beta,S,lambda1,lambda2,w,W, type);
      f_y = computeLoss(X,y,d,p,alpha,beta,S,lambda1,lambda2,y_w,y_W, type);
      
      f_lip = f_y + t(g_w)%*%(w-y_w) + 1/2/gamma*norm_vec((w-y_w))^2;
      
      for (m in 1:M)
      {
        
        f_lip = f_lip+t(g_W[,m])%*%(W[,m]-y_W[,m])+1/2/gamma*norm_vec((W[,m]-y_W[,m]))^2;
        #####
      }
      
      iter = 0;
      
      while (f>f_lip && gamma>10^(-40))
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
        f = computeLoss(X,y,d,p,alpha,beta,S,lambda1,lambda2,w,W, type);
        # f_lip = f_y + t(g_w)%*%(w-y_w) + 1/2/gamma*norm_vec((w-y_w))^2;
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
  computeGradient =function(X,y,d,p,alpha,beta,S,lambda1,lambda2,w,W, type){
    N = nrow(X);
    D = ncol(X);
    M = length(unique(d));
    g_w = matrix(data = 0,nrow = nrow((w)),ncol = ncol(w))
    g_W = matrix(data = 0,nrow = nrow(W),ncol = ncol(W))
    
    if (type=="ls"){
      for (m in 1:M){
        temp = (2*t(X[d==m,]))%*% ((X[d==m,]%*%(w+W[,m])-y[d==m]));
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
  computeLoss<- function(X,y,d,p,alpha,beta,S,lambda1,lambda2,w,W, type){
    N = nrow(X);
    D = ncol(X);
    M = length(unique(d));
    f = 0;
    
    if (type=="ls")
    {
      for (m in 1:M)
        f = f+norm_vec((X[d==m,]%*%(w+W[,m])-y[d==m]))^2  # sum((X[d==m,]%*%(w+W[,m])-y[d=m])^2);
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