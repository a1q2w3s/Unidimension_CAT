
setwd("./code/function")
lapply(dir(),function(x){
  source(x)
})

CAT <- function(theta_t,
                item_library,
                n_first,# number of items before adaptation
                fix_length,# length of fix-length exam
                ability_est_method,# EAP_MLE or MAP
                selection_strategy, # random_select or MFI
                draw = FALSE
){
  n_library = dim(item_library)[1]
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
    if(draw){
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
  }
  return(tail(theta_est,1))
}  

n_library = 200
IRT_model = '2PL'
item_library = item_generate(n_library,IRT_model)
CAT(theta_t = rnorm(1),
    item_library,
    n_first = 5,
    fix_length = 40,
    ability_est_method = 'MLE',
    selection_strategy = 'MFI',
    draw = TRUE
)

bias = c()
rmse = c()
runtime = c()
epoch = 1000
for (n_library in c(200,2000)) {
  for(IRT_model in c('1PL','2PL','3PL')){
    item_library = item_generate(n_library,IRT_model)
    for(ability_est_method in c('EAP_MLE','EAP','MAP')){
      for (selection_strategy in c('random_select','MFI')) {
        est = c() # store estimated theta
        tru = c() # store true theta
        tempt = c() # store run time per epoch
        for (k in c(1:epoch)) {
          if(k%%100==0){
            message(
              'n_library: ',n_library,', ',
              'IRT_model: ',IRT_model,', ',
              'est_method: ',ability_est_method,', ',
              'selection: ',selection_strategy,', ',
              '##',k,'/',epoch)
          }
          tru[k] <- theta_t <- rnorm(1)
          t0 = Sys.time()
          est[k] <- theta_est <- CAT(theta_t,item_library,n_first = 5,fix_length = 40,ability_est_method,selection_strategy)
          tempt[k] = Sys.time()-t0
        }
  
        png(filename = paste('../../pic/',
                           n_library,'_',
                           IRT_model,'_',
                           ability_est_method,'_',
                           selection_strategy,
                           '.png',sep=''))
        message('est: ',max(est),', ',min(est))
        message('tru: ',max(tru),', ',min(tru))
        plot(x = tru,
             y = est,
             xlim = c(-4,4),
             ylim = c(-4,4),
             col = rgb(0.2,0.2,0.2),
             cex = 0.9,
             pch = 16,
             bty = 'l',
             ylab = 'estimated theta',
             xlab = 'true theta',
             cex.lab = 1.2,
             cex.axis = 1.1,
             tck = 0.01
        )
        title(main = paste(
          ability_est_method,
          ' and ',
          selection_strategy,
          sep = ''),
          sub = paste(
            '(number of items = ', n_library,', ',
            'model = ', IRT_model,')',
            sep = ''))
        
        dev.off()
        
        bias = append(bias,sum(abs(est-tru))/epoch)
        rmse = append(rmse,sqrt(sum((est-tru)^2)/epoch))
        runtime = append(runtime,mean(tempt))
      }
    }
  }
}


