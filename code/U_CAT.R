#install.packages('ggplot2')
library(ggplot2)

n_library = 200
theta_t = 1.3
fix_length = 100
first_n = 5

item_library = data.frame(a=exp(rnorm(n_library)),b=rnorm(n_library))
item_library = item_library[order(item_library$b),]

MAP <- function(answer,theta,b,a,episilon=1e-3){
  delta = 1
  theta_est = theta
  while(abs(delta)>episilon){
    p_est = 1/
      (1+exp(a*(theta_est-b)))
    
    g = sum(a*(answer-p_est))-theta_est
    g_ = sum(a^2*p_est*(1-p_est))-1
    
    if(g_==0){
      break
    }
    
    delta = g/g_
    theta_est = theta_est-delta
  }
  return(theta_est)
}

answer <- function(theta,a,b){
  p = 1/
    (1+exp(a*(theta_est-b)))
  temp = runif(1)
  flag = p>temp
  
  return(flag)  
}



flag = rep(TRUE,n_library)

theta_est = 0
result = c()

first = sample(n_library,first_n)
flag[first] = FALSE

for (i in c(1:first_n)) {
  result[i]=answer(theta_t,item_library$a[first[i]],item_library$b[first[i]])
  #Sys.sleep(0.5)
}

for (i in c((first_n+1):fix_length)) {
  repeat{
    item = round(runif(1)*n_library)#select_strategy()
    if(flag[item]){
      break
    }
  }
  flag[item] = FALSE
  result[i]=answer(theta_t,item_library$a[item],item_library$b[item])
  theta_est = MAP(result,theta_est,item_library$a[flag],item_library$b[flag])
  
  #Sys.sleep(0.5)
}

