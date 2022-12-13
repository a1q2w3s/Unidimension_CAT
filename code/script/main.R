#install.packages('ggplot2')
library(ggplot2)

library_n = 200
dimension = 2
theta_t = c(1,-1)

# Import functions --------------------------------------------------------

setwd("./code/function")
lapply(dir(),function(x){
  source(x)
})

# Initiation library--------------------------------------------------------------

# model_type = 'M2PL'
# item_library = one_dimension_cat(library_n,model_type)
# item_library = item_library[order(item_library$b),] #sort convenient for selection

para = matrix(data=0,nrow=n,ncol=dimension+1)

para[,dimension+1] = rnorm(n)
para[,1:dimension] = exp(rnorm(n*dimension))



# Adaptation ---------------------------------------------------------------------

flag = rep(TRUE,library_n)
first_n = 5
theta_est = c(0,0)
fix_length = 30
result = c()

first = sample(library_n,first_n)
flag[first] = FALSE

for (i in c(1:first_n)) {
  result[i]=answer(theta_t,para[first[i]])
  Sys.sleep(0.5)
}

theta_est = MAP(result,theta_est,para[!flag,3],para[!flag,1:2])

for (i in c((first_n+1):fix_length)) {
  repeat{
    item = round(runif(1)*n)#select_strategy()
    if(flag[item]){
      break
    }
  }
  flag[item] = FALSE
  result[i]=answer(theta_t,para[item])
  theta_est = MAP(result,theta_est,para[!flag,3],para[!flag,1:2])
  
  #Sys.sleep(0.5)
}




