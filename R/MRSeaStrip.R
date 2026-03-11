#function to reduce model size by stripping out environments
stripMRSea = function(cm) {
  cm$y = c()
  #cm$model = c()#this cannot be taken out without breaking "do.bootstrap.cress.robust"
  cm$effects = c()
  cm$data = c()
  cm$splineParams[[1]]$dist = c()
  
  #cm$family$aic = c()
  #cm$family$validmu = c()
  #cm$family$simulate = c()
  #cm$family$variance = c()#this cannot be taken out without breaking "do.bootstrap.cress.robust"
  #cm$family$dev.resids = c()#this cannot be taken out without breaking "do.bootstrap.cress.robust"
  
  attr(cm$residuals,"names") = c()
  attr(cm$linear.predictors,"names") = c()
  attr(cm$weights,"names") = c()
  attr(cm$prior.weights,"names") = c()
  
  attr(cm$terms,".Environment") = c()
  attr(cm$family,".Environment") = c()
  attr(cm$model,".Environment") = c()
  attr(cm$call,".Environment") = c()
  attr(cm$call$formula,".Environment") = c()
  attr(cm$formula,".Environment") = c()
  attr(cm$model$`offset(log(area))`,".Environment") = c()
  attr(attr(cm$model,"terms"),".Environment")= c()
  cm
}

breakdown = function(mod) {
  sapply(mod, FUN=function(x){length(serialize(x, NULL))}, simplify=T)
}

# stripmod<-stripMRSea(salsa2dbest)
# breakdown(stripmod)#some very large terms, due to stored environments