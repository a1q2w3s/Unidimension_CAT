#install.packages('ggplot2')
library(ggplot2)

n_library = 200
theta_t = -0.333
fix_length = 30
first_n = 5
D=1.702

a=exp(rnorm(n_library))
repeat{
  index=c(a>=4)
  a[index]=exp(rnorm(sum(index)))
  if(sum(index)==0){
    break
  }
}

item_library = data.frame(a,b=rnorm(n_library))
item_library = item_library[order(item_library$b),]

MAP <- function(result,theta,b,a,episilon=1e-3,D=1.702){
  delta = 1
  theta_est = theta
  while(abs(delta)>episilon){
    p_est = 1/(1+exp(-D*a*(theta_est-b)))
    
    g = sum(D*a*(result-p_est))-theta_est
    g_ = sum(-D^2*a^2*p_est*(1-p_est))-1
    
    if(g_==0){
      break
    }
    
    delta = g/g_
    
    theta_est = theta_est-delta
  }
  return(theta_est)
}

answer <- function(theta,b,a){
  p = 1/(1+exp(-D*a*(theta-b)))

  temp = runif(1)
  flag = p>temp
  
  return(flag)
}

flag = rep(TRUE,n_library)

theta_est=c()
result = c()

items = sample(n_library,first_n)
flag[items] = FALSE

for (i in c(1:first_n)) {
  result[i]=answer(theta_t,item_library$b[items[i]],item_library$a[items[i]])
  #Sys.sleep(0.5)
}

theta_est[1] =log(max(sum(as.numeric(result)),1)/max((first_n-sum(as.numeric(result))),1))
theta_est[5] = MAP(as.numeric(result),theta_est[1],item_library$b[items],item_library$a[items])

for (i in c((first_n+1):fix_length)) {
  repeat{
    item = which.min(abs(theta_est[i-1]-item_library$b[flag]))
    item = which(item_library$b==item_library$b[flag][item])
    message('#',theta_est[i-1],'  ',item_library$b[item],'  ')
    #item = round(runif(1)*(n_library-1)+1)#select_strategy()
    # if(flag[item]){
      flag[item] = FALSE
      items[i] = item
      break
    # }
    
  }
  result[i]=answer(theta_t,item_library$b[item],item_library$a[item])
  theta_est[i] = MAP(as.numeric(result),theta_est[i-1],item_library$b[items],item_library$a[items])
  plot(theta_est[6:fix_length],ylim=c(-3,3))
  #message(theta_est)
  Sys.sleep(0.5)
}
theta_est[fix_length]
