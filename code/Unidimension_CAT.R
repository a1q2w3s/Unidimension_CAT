
# initialize environment --------------------------------------------------

##install.packages('ggplot2')
library(ggplot2)

setwd("./code/function")
lapply(dir(),function(x){
  source(x)
})


# set index ---------------------------------------------------------------

n_library = 2000 # size of item library
theta_t = 0.333 # true ability value of our subject 
n_first = 3 # number of items before adaptation
fix_length = 30 # length of fix-length exam
ability_est_method = MLE # MLE or MAP
selection_strategy = random_select #random_select or MFI

# generate library --------------------------------------------------------

a = exp(rnorm(n_library)) # discrimination parameter(log normal)
repeat{ # no too large a
  index = a>4
  a[index] = exp(rnorm(sum(index)))
  if(sum(index)==0) break
}
rm(index)
b = rnorm(n_library) # difficulty parameter(normal)

item_library = data.frame(a,b)
item_library = item_library[order(item_library$b),]
rm(a,b)

# main --------------------------------------------------------------------

flag = rep(TRUE, n_library) # sign items which not used(FALSE->used)
theta_est = c() # save estimated theta
result = c() # save subject's response 
items = c() # save chosen items in order

## begin with random items without adaptation
items[1:n_first] = sample(
  x = n_library,
  size = n_first)
flag[items] = FALSE
result[1:n_first] = answer(
  theta = theta_t,
  item_parameters = item_library[items,])

theta_est[1] = log(max(sum(as.numeric(result)),1)/max((n_first-sum(as.numeric(result))),1))
theta_est[2] = ability_est_method(
  result = as.numeric(result),
  theta = theta_est[1],
  item_parameters = item_library[items,])

## adaptation procedure
for (i in c(n_first+1):fix_length) {
  # select item
  item_now = selection_strategy(flag,item_library,tail(theta_est,1))
  items[i] = item_now
  flag[item_now] = FALSE
  
  result[i] = answer(
    theta = theta_t,
    item_parameters = item_library[item_now,])
  
  theta_est[i-n_first+2] = ability_est_method(
    result = as.numeric(result),
    theta = theta_est[i-n_first+1],
    item_parameters = item_library[items,])
  
  plot(theta_est,ylim=c(-3,3),xlim=c(0,fix_length))
  Sys.sleep(0.5)
}



