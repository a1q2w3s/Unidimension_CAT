
# initialize environment --------------------------------------------------

##install.packages('ggplot2')
library(ggplot2)

setwd("./code/function")
lapply(dir(),function(x){
  source(x)
})


# set index ---------------------------------------------------------------

n_library = 200 # size of item library
theta_t = 1.333 # true ability value of our subject 
n_first = 5 # number of items before adaptation
fix_length = 40 # length of fix-length exam
IRT_model = '2PL' # 1PL, 2PL or 3PL
ability_est_method = 'MAP' # MLE or MAP
selection_strategy = 'MFI' # random_select or MFI

# generate library --------------------------------------------------------

a = exp(rnorm(n_library)) # discrimination parameter(log normal)
repeat{ # no too large a
  index = a>4
  a[index] = exp(rnorm(sum(index)))
  if(sum(index)==0) break
}
rm(index)
b = rnorm(n_library) # difficulty parameter(normal)
id = c(1:n_library)

item_library = data.frame(id,a,b)
item_library = item_library[order(item_library$b),]
rm(a,b,id)

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
theta_est[1] = eval(as.symbol(ability_est_method))(
  result = as.numeric(result),
  theta = theta_est[1],
  item_parameters = item_library[items,])

## adaptation procedure
for (i in c(n_first+1):fix_length) {
  # select item
  item_now = eval(as.symbol(selection_strategy))(flag,item_library,tail(theta_est,1))
  items[i] = item_now
  flag[item_now] = FALSE
  
  result[i] = answer(
    theta = theta_t,
    item_parameters = item_library[item_now,])
  
  theta_est[i-n_first+1] = eval(as.symbol(ability_est_method))(
    result = as.numeric(result),
    theta = theta_est[i-n_first],
    item_parameters = item_library[items,])
  
  # draw pic
  plot(theta_est,
       ylim = c(-3,3),
       xlim = c(0,fix_length),
       col = 'red',
       type = 'o',
       cex = 1.2,
       lwd = 1.1,
       pch = 16,
       bty = 'l',
       ylab = 'ability estimation or item difficulty',
       xlab = 'item(adaptation)',
       cex.lab = 1.2,
       cex.axis = 1.1,
       tck = 0.01)
  points(c(NA,item_library$b[items[c((n_first+1):length(items))]]),
       type = 'o',
       cex = 1.2,
       lwd = 1.1,
       pch = 17)
  abline(h=theta_t,lty=2)
  legend("bottomright",
         title = paste('model = ',IRT_model,sep = ''),
         legend = c('ability estimate','item diffculty'),
         col = c('red','black'),
         lwd = 1.1,
         pch = c(16,17),
         xpd=TRUE)
  title(main = paste(
          "Scatter Plot for CAT with ",
          ability_est_method,
          ' and ',
          selection_strategy,
          sep = ''),
        sub = paste(
          'number of items = ', n_library,', ',
          'length = ', fix_length,', ',
          'random items = ', n_first,
          sep = ''))
  
    
  Sys.sleep(0.2)
}

tail(theta_est,1)

