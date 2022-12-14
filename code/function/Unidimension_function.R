# Define functions for uni-dimensional CAT
D = 1.702

# IRT model ---------------------------------------------------------------

uni_model <- function(theta,item_parameters){
  #
  # b: difficulty parameter
  # theta: ability parameter
  # a: discrimination parameter, if default, then 1PLM
  # c: guessing parameter, if default, then 2PLM
  #
  b = item_parameters$b
  a = if(is.null(item_parameters$a)) 1 else item_parameters$a
  c = if(is.null(item_parameters$c)) 0 else item_parameters$c
  p = c+(1-c)/(1+exp(-D*a*(theta-b)))
  return(p)
} 


# item generation ---------------------------------------------------------

item_generate <- function(n,IRT_model){
  
  a = exp(rnorm(n_library)) # discrimination parameter(log normal)
  repeat{ # no too large a
    index = a>4
    a[index] = exp(rnorm(sum(index)))
    if(sum(index)==0) break
  }
  
  b = rnorm(n_library) # difficulty parameter(normal)
  id = c(1:n_library)
  
  item_library = data.frame(id,a,b)
  item_library = item_library[order(item_library$b),]
  
  return(item_library)
}

# ability estimation ------------------------------------------------------

##ability_est_method(result,theta,b,a=1,c=0)

MLE <- function(result,theta,item_parameters,episilon=1e-3){
  b = item_parameters$b
  a = if(is.null(item_parameters$a)) 1 else item_parameters$a
  c = if(is.null(item_parameters$c)) 0 else item_parameters$c
  lambda = 1 # down-hill index
  temp_d1 = Inf
  temp_d2 = 0
  temp_theta = theta
  #message('#############')
  ## N-R iteration
  repeat{
    #message('========')
    #message('theta_old:',theta)
    p_est = uni_model(theta,item_parameters)
    
    #first derivation of log-likelihood
    d1 = sum(D*a*(p_est-c)/(1-c)*(result-p_est)/p_est)
    #second derivation of log-likelihood
    d2 = -D^2*sum(a^2*(1-p_est)*(p_est-c)*(c*result-p_est^2)/p_est^2/(1-c)^2)
    d2 = sum(-D^2*a^2*p_est*(1-p_est))
    # down-hill index check
    if(abs(temp_d1)<abs(d1)){
      d1 = temp_d1
      d2 = temp_d2
      theta = temp_theta
      lambda = lambda*0.5
    }else{
      lambda = 1
      temp_d1 = d1
      temp_d2 = d2
      temp_theta = theta
    }
    
    delta = lambda*d1/d2
    theta = theta-delta
    #message('delta:',delta)
    #message('theta_now:',theta)
    if(d2==0) break
    if(abs(delta)<episilon) break # iteration exit
  }
  
  theta = if(theta > 4) 4 else theta
  theta = if(theta < -4) -4 else theta
  
  return(theta)
}

MAP <- function(result,theta,item_parameters,episilon=1e-3){
  b = item_parameters$b
  a = if(is.null(item_parameters$a)) 1 else item_parameters$a
  c = if(is.null(item_parameters$c)) 0 else item_parameters$c
  lambda = 1 # down-hill index
  temp_d1 = Inf
  temp_d2 = 0
  temp_theta = theta
  #message('#############')
  ## N-R iteration
  repeat{
    #message('========')
    #message('theta_old:',theta)
    p_est = uni_model(theta,item_parameters)
    
    #first derivation of log-likelihood
    d1 = sum(D*a*(p_est-c)/(1-c)*(result-p_est)/p_est)-theta
    #second derivation of log-likelihood
    d2 = -D^2*sum(a^2*(1-p_est)*(p_est-c)*(c*result-p_est^2)/p_est^2/(1-c)^2)-1
    d2 = sum(-D^2*a^2*p_est*(1-p_est))
    # down-hill index check
    if(abs(temp_d1)<abs(d1)){
      d1 = temp_d1
      d2 = temp_d2
      theta = temp_theta
      lambda = lambda*0.5
    }else{
      lambda = 1
      temp_d1 = d1
      temp_d2 = d2
      temp_theta = theta
    }
    
    delta = lambda*d1/d2
    theta = theta-delta
    #message('delta:',delta)
    #message('theta_now:',theta)
    if(d2==0) break
    if(abs(delta)<episilon) break # iteration exit
  }
  
  theta = if(theta > 4) 4 else theta
  theta = if(theta < -4) -4 else theta
  
  return(theta)
}


# selection strategy ------------------------------------------------------

##selection_strategy(flag,item_library=NULL,theta=NULL)

random_select <- function(flag,item_library=NULL,theta=NULL){
  repeat{
    item = round(runif(1)*(n_library-1)+1)#select_strategy()
    if(flag[item]) return(item)
  }
}

MFI <- function(flag,item_library,theta){
  # select the closest b to theta
  item_id = item_library$id[flag][which.min(abs(theta-item_library$b[flag]))]
  # select the biggest a
  if(length(item_id)>1)
    item_id = item_library$id[item_library$id %in% item_id][which.max(item_library$a[item_library$id %in% item_id])]
  # select the smallest c
  if(length(item_id)>1)
    item_id = item_library$id[item_library$id %in% item_id][which.min(item_library$c[item_library$id %in% item_id])]
  item = which(item_library$id==item_id)
}


# other -------------------------------------------------------------------

answer <- function(theta,item_parameters){
  p = uni_model(theta,item_parameters)
  
  temp = runif(length(p))
  flag = p>temp
  
  return(flag)
}

