# 定义用于单维CAT的函数
D = 1.702

# IRT 模型 ---------------------------------------------------------------

uni_model <- function(theta,item_parameters){
  #
  # b: 难度参数
  # theta: 能力参数
  # a: 区分度参数
  # c: 猜测参数
  #
  b = item_parameters$b
  a = if(is.null(item_parameters$a)) 1 else item_parameters$a
  c = if(is.null(item_parameters$c)) 0 else item_parameters$c
  p = c+(1-c)/(1+exp(-D*a*(theta-b))) # 基于三参数模型的算式，可以涵盖一参数和两参数的情况
  return(p)
} 


# 题库生成 ---------------------------------------------------------

item_generate <- function(n,IRT_model){
  
  b = rnorm(n) # 标准正态分布中抽取难度参数
  repeat{ # 控制b的绝对值不过大
    index = abs(b)>4
    b[index] = rnorm(sum(index))
    if(sum(index)==0) break
  }
  
  a = exp(rnorm(n)) # 对数正态分布中抽取区分度参数
  repeat{ # 控制a的值不过大
    index = a>5
    a[index] = exp(rnorm(sum(index)))
    if(sum(index)==0) break
  }
  
  c = rbeta(n,2,20) # beta分布中抽取猜测参数
  
  # 根据题目模型调整参数
  if(IRT_model=='2PL'){
    c = 0
  }else if(IRT_model=='1PL'){
    a = 1
    c = 0
  }
  
  id = c(1:n)
  item_library = data.frame(id,a,b,c)
  item_library = item_library[order(item_library$b),]
  return(item_library)
}

# 能力估计 ------------------------------------------------------

## theta_est = ability_est_method(result,theta,item_parameters)
#能力估计函数统一的接口：输入为作答、当前能力估计值、已完成题目参数；输出为新的能力估计值

EAP_MLE <- function(result,theta,item_parameters){
  # 当题目数量小于10，或作答为全对全错时，使用EAP
  if(length(result)<10 || sum(result)==length(result) || sum(result)==0) return(EAP(result,theta,item_parameters))
  else return(MLE(result,theta,item_parameters))
}

EAP <- function(result,theta,item_parameters,steps=0.2){
  b = item_parameters$b
  a = if(is.null(item_parameters$a)) 1 else item_parameters$a
  c = if(is.null(item_parameters$c)) 0 else item_parameters$c
  x = seq(-4,4,steps)
  
  p = apply(matrix(x),1,uni_model,item_parameters=item_parameters)
  l = apply(p,2,function(p){
    return(prod((p^result)*(1-p)^(1-result)))
  })
  theta =
    (sum(x*l*pnorm(x)))/(sum(l*pnorm(x)))
  return(theta)
}

MLE <- function(result,theta,item_parameters,episilon=1e-3){
  b = item_parameters$b
  a = if(is.null(item_parameters$a)) 1 else item_parameters$a
  c = if(is.null(item_parameters$c)) 0 else item_parameters$c
  theta0 = theta
  lambda = 1 # 下山因子
  temp_d1 = Inf
  temp_d2 = 0
  temp_theta = theta

  ## 牛顿-拉夫逊迭代
  repeat{
    p_est = uni_model(theta,item_parameters)
    #对数似然函数的一阶导
    d1 = sum(D*a*(p_est-c)/(1-c)*(result-p_est)/p_est)
    #对数似然函数的二阶导
    d2 = -D^2*sum(a^2*(1-p_est)*(p_est-c)*(c*result-p_est^2)/p_est^2/(1-c)^2)
    d2 = sum(-D^2*a^2*p_est*(1-p_est))

    # 下山因子机制检查
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
    # 更新能力估计值
    delta = lambda*d1/d2
    theta = theta-delta
    
    
    if(d2==0) break
    
    if(abs(delta)<episilon) break # 常规迭代出口
    
    if(theta > 4) return(theta0)
    if(theta < -4) return(theta0)
  }
  
  return(theta)
}

MAP <- function(result,theta,item_parameters,episilon=1e-3){
  b = item_parameters$b
  a = if(is.null(item_parameters$a)) 1 else item_parameters$a
  c = if(is.null(item_parameters$c)) 0 else item_parameters$c
  theta0 = theta
  lambda = 1 # 下山因子
  temp_d1 = Inf
  temp_d2 = 0
  temp_theta = theta
  
  ## N-R iteration
  repeat{
    p_est = uni_model(theta,item_parameters)
    # 对数似然函数的一阶导
    d1 = sum(D*a*(p_est-c)/(1-c)*(result-p_est)/p_est)-theta
    # 对数似然函数的二阶导
    d2 = -D^2*sum(a^2*(1-p_est)*(p_est-c)*(c*result-p_est^2)/p_est^2/(1-c)^2)-1
    d2 = sum(-D^2*a^2*p_est*(1-p_est))
    
   
    # 下山因子机制检查
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
   
    # 更新能力估计值
    delta = lambda*d1/d2
    theta = theta-delta
    
    if(d2==0) break
    if(abs(delta)<episilon) break # 常规迭代出口
    
    if(theta > 4) return(theta0)
    if(theta < -4) return(theta0)
  }

  return(theta)
}


# 选题策略 ------------------------------------------------------

##next_item = selection_strategy(flag,item_library=NULL,theta=NULL)
#选题策略函数统一的接口：输入为当前已选题目标记、题库、能力估计值；输出为选出的题目题号
random_select <- function(flag,item_library=NULL,theta=NULL){
  repeat{
    item = round(runif(1)*(n_library-1)+1)
    if(flag[item]) return(item)
  }
}

MFI <- function(flag,item_library,theta){
  # 选择拥有与当前能力估计值最接近的难度参数的题目
  item_id = item_library$id[flag][which.min(abs(theta-item_library$b[flag]))]
  # 选择最大的区分度参数
  if(length(item_id)>1)
    item_id = 
      item_library$id[item_library$id %in% item_id][which.max(item_library$a[item_library$id %in% item_id])]
  # 选择最小的猜测参数
  if(length(item_id)>1)
    item_id = 
      item_library$id[item_library$id %in% item_id][which.min(item_library$c[item_library$id %in% item_id])]
  item = which(item_library$id==item_id)
}


# 其他 -------------------------------------------------------------------

answer <- function(theta,item_parameters){
  p = uni_model(theta,item_parameters)
  
  temp = runif(length(p))
  flag = p>temp
  
  return(flag)
}

