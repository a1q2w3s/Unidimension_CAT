#install.packages('ggplot2')
library(ggplot2)

library_n = 200
theta_t = 1

# Import functions --------------------------------------------------------

setwd("./code/function")
lapply(dir(),function(x){
  source(x)
})

# Initiation library--------------------------------------------------------------

model_type = '2PL'
item_library = one_dimension_cat(library_n,model_type)
item_library = item_library[order(item_library$b),] #sort convenient for selection


# Adaptation ---------------------------------------------------------------------

flag = rep(TRUE,library_n)
first_n = 5
theta_est = 0
fix_length = 30


first = sample(library_n,first_n)
flag[first] = FALSE

for (i in c(1:fix_length)) {
  if (i<=5) {
    
  }
}




