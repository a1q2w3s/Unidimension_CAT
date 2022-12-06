#Define different item type

one_dimension_cat <-function(n,model_){
  if(n<=0){
    stop("n must be positive!")
  }
  if(!model_ %in% c('1PL','2PL','3PL')){
    stop("the model must be '1PL', '2PL' or '3PL'!")
  }
  b = rnorm(n)
  a = exp(rnorm(n))
  c = rbeta(n,2,10)
  switch(model_,
         '1PL' = return(data.frame(b=b)),
         '2PL' = return(data.frame(a=a,b=b)),
         '3PL' = return(data.frame(a=a,b=b,c=c)))
}

