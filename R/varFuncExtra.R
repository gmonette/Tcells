#' Experimenting with varFunc 
#' 
#' This is contain variance functions
varPois <- function() NULL

if (FALSE) {
  library(nlme)
  library(nlmeYextra)
  library(spida)
  library(magrittr)
  
  hs$sex <- 1*(hs$Sex == 'Female')
  ls()
  fit4 <- lme(mathach ~ ses + sex, hs,
              random = list( school = pdInd( ~ 1 + ses + sex)),
              weights = varPower(.2, form= ~fitted(.,level=1)), 
              control = list(msVerbose=T,returnObject=T,msMaxIter=1000))
  fit <- lme(mathach ~ ses + sex, hs,
              random = list( school =  ~ 1 ),
              control = list(msVerbose=T,returnObject=T,msMaxIter=1000))
  
  
  summary(fit4)
  intervals(fit4)
  AIC(fit4)
  anova(fit,fit4)
  wald(fit4,-1)
  vv <- fit4$modelStruct$varStruct
  str(vv)
  
  ww <- attr(vv,"weights")
  cov <- attr(vv,"covariate")
  
  plot(cov,ww)
  
  # Experiment with weights
  
  dd <- data.frame( x = 1:1000, y1 = 1:1000 + (1:1000)*rnorm(1000), y0 = 1:1000 +rnorm(1000))
  plot(y ~ x, dd)
  fit0 <- gls( y0 ~ x , dd, weights = varPower(.2, form = ~fitted(.)),
               control = list(msVerbose=T, returnObject = T, msMaxIter = 1000,
                              maxIter = 1000))
  fit1 <- gls( y1 ~ x , dd, weights = varPower(.2, form = ~fitted(.)),
               control = list(msVerbose=T, returnObject = T, msMaxIter = 1000,
                              maxIter = 1000))
  summary(fit0)
  summary(fit1)
  fit0$modelStruct$varStruct %>% str
  pwc <- function(fit) {
    library(magrittr)
    covariate <- fit$modelStruct$varStruct %>% attr("covariate")
    weights <- fit$modelStruct$varStruct %>% attr("weights")
    plot(covariate,log(weights))
  }
  pwc(fit0)
  pwc(fit1)
  
  fitted
  fitted1 <- function(object,...) {
    cat(".\n")
    cat(class(object))
    print(str(object))
    cat("\n")
    assign("fit.obj", object, 1)
    UseMethod("fitted")
  }
  
  fit0 <- gls( y0 ~ x , dd, weights = varPower(form = ~fitted1(.), fixed = .2),
               control = list(msVerbose=T, returnObject = T, msMaxIter = 1000,
                              maxIter = 1000))
  

  library(nlmeYextra)

  fitted1 <- function(object,...) {
    cat(".\n")
    cat(class(object))
    print(str(object))
    cat("\n")
    assign("fit.obj", object, 1)
    ret <- fitted(object,...)
    assign("ret.obj", ret, 1)
    ret
  }
  
  
  
  
  
  fit <- lme( mathach ~ (ses + sex)*Sector , hs, 
              weights = varPower(.02, form = ~fitted1(.)),
              random = list( school = pdInd( ~ 1 + ses + sex)),
               control = list(msVerbose=T, returnObject = T, msMaxIter = 1000,maxIter = 1000))
  
  ret.obj  %>% str
  
  fit.obj$varStruct %>% str 
  fit.obj$varStruct %>% attributes %>% formula
  fit.obj  %>% attributes  %>% names
  fit.obj$varStruct  %>% str
  fit.obj %>% str

  sel <- `[`
  sel2 <- `[[`
  s <- function(x,...) UseMethod("s")
  s.matrix <- function(x,...) {
    list( dim = dim(x), head = head(x))
  }
  s.list <- function(x,...) lapply(x,s)
  s.default <- function(x,...) {
    list( class = class(x),
          str = str(x))
  }

  fit.obj %>% attr('conLin') %>% str
  fit.obj %>% attr('conLin') %>% str
  fit.obj %>% attr('conLin') %>% sel2("Xy")  %>%  s
  fit.obj %>% attr('conLin') %>% sel("Xy")  %>%  s
  s
  # beta
  sel <- `[`  
  fit.obj %>% attr('lmeFit') %>% sel('beta')   # beta
  fit.obj %>% attr('lmeFit') %>% class
  fit.obj %>% attr('lmeFit') %>% names
  fit.obj %>% attr('lmeFit') %>% sel('b')   # random effects
  
  
  
  hs$sex <- 1*(hs$Sex == "Male") 
  
  
  fit <- lme( mathach ~ (ses + sex)*Sector , hs, 
              random = list( school = pdInd( ~ 1 + ses + sex)),
              control = list(msVerbose=T, returnObject = T, msMaxIter = 1000,
                             maxIter = 1000))
  summary(fit)
  ff <- fit$modelStruct$reStruct$school
  class(ff)
  pdMatrix(ff) %>% eigen 
  pdMatrix(solve(ff))
  pdMatrix(ff) %>% cond
ff %>% unclass
-pdMatrix(ff)[1,3]/pdMatrix(ff)[3,3]
  
  fit  %>% str
  
  fit.obj  %>% attr("glsFit") -> x
  ret.obj  %>% str
  
  ?varFixed





  
} # end of FALSE

