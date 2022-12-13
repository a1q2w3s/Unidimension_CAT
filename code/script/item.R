
item_generation <- function(n,model_,dimension){
  if(!n>0){
    stop("n must be positive number!")
  }
  if(!model_ %in% c('M1PL','M2PL','M3PL')){
    stop("the model must be 'M1PL', 'M2PL' or 'M3PL'!")
  }
  
  para = matrix(data=0,nrow=n,ncol=dimension+1)
  
  para[,dimension+1] = rnorm(n)
  para[,1:dimension] = exp(rnorm(n*dimension))
  
  return(para)
  
}

answer <- function(theta,item_index){
  ##
  #get item parameter
  A = para[,1:2]
  d = para[3]
  ##
  
  p = M2PL(A,d,theta)
  temp = runif(1)
  flag = as.integer(p>temp)
  
  return(flag)  
}



M1PL <- function(d,theta){
  p = exp(sum(theta)+d)/(1+exp(sum(theta)+d))
  return(p)
}

M2PL <- function(A,d,theta){
  p = exp(sum(A*theta)+d)/(1+exp(sum(A*theta)+d))
  return(p)
}

M3PL <- function(A,d,c,theta){
  p = c+(1-c)*exp(sum(A*theta)+d)/(1+exp(sum(A*theta)+d))
  return(p)
}

MLE = function(answer,theta,d,A,episilon=1e-3){
  theta_est = theta
  dimension = dim(A)[2]
  while(abs(delta)>episilon){
    p = exp(sum(A*theta)+d)/(1+exp(sum(A*theta)+d))
    
    derivative = apply(A,2,function(x){sum(x*(answer-p))})
    
    H = matrix(0,dimension,dimension)
    for (i in c(1:dimension)) {
      for (j in c(i:dimension)) {
        H[i,j] = sum(A[i]*A[j]*p*(1-p))
      }
    }
    
    delta = solve(H)*derivative
    theta_est = theta_est-delta
  }
  return(theta_est)
}

