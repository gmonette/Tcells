## ----include=FALSE-------------------------------------------------------
library(magrittr)


## ------------------------------------------------------------------------
(V <- diag(1:3) + 1)
(f <- chol(V))
t(f) %*% f
f[2,3] <- 0
f
(v2 <- t(f) %*% f)
# The conditional covariance is zero
v2[2:3,2:3] - 
  v2[2:3,1,drop=FALSE] %*% 
    solve(v2[1,1],drop=FALSE) %*%
    v2[1,2:3,drop=FALSE]
# How to do a left-factorization
cholR <- function(x, ...) {
  # this can return a non-triangular factor, thus can't  be used for our purposes
  ret <- chol(x, pivot = TRUE)
  pivot <- attr(ret, "pivot")
  ret[, order(pivot)]
  }

cholL <- function(x, ...) {
  x[,] <- rev(x)
  ret <- chol(x) # not cholR (see above)
  ret[,] <- rev(ret)
  ret
  }
# test rank deficient V
V0 <- crossprod( cbind(1, 1:10, (1:10)^2, 11:20))
tryCatch(chol(V0), error = function(e) e)
tryCatch(cholR(V0), error = function(e) e)


lfac <- cholL(V)  
t(lfac) %*% lfac
lfac[3,2] <- 0
t(lfac) %*% lfac

## ------------------------------------------------------------------------
(lfac <- cholL(V))
rfac <- qr.R(qr(lfac))
chol(V)
t(rfac) %*% rfac - (t(lfac) %*% lfac)
X <- cbind(1,1:10,(1:10)^2,11:20)
qr.R(qr(X))
qr.R(qr(X, pivot = T))

L2R <- function(x, ...) {
    R <- qr.R(qr(x))
    sign(diag(R)) * R
  }
V

cholL(V) %>% round(2)
V - crossprod( cholL(V))
V - crossprod( chol(V))

V - crossprod( L2R(cholL(V)))


R2L <- function(x, ...) {
    x[] <- rev(x)
    ret <- L2R(x, ...)
    ret[] <- rev(ret)
    ret
  }
x <- matrix(rnorm(10000), 100)
system.time(
    for(i in 1:1000) R2L(x)
  )
system.time(
    for(i in 1:1000) L2R(x)
  )
# Check that it works

V
Vr <- crossprod( matrix(rnorm(9),3))
svd(Vr)$d
lc <- cholL(Vr)
Vr - crossprod(lc)
rc <- chol(Vr)
Vr - crossprod(rc)
lc2r <- L2R(lc)
Vr - crossprod(lc2r)
rc2l <- R2L(rc)
Vr - crossprod(rc2l)

## ------------------------------------------------------------------------
library(nlme)
methods(class="pdLogChol")
methods(class="pdSymm")
nlme:::coef.pdSymm

methods(class="pdMat")

nlme:::pdLogChol
nlme:::pdSymm
nlme:::pdMat

pdFactorfull <- function (object) 
{
    Ncol <- round((-1 + sqrt(1 + 8 * length(object)))/2)
    .C(logChol_pd, Factor = double(Ncol * Ncol), as.integer(Ncol), 
        as.double(object))
}
environment(pdFactorfull) <- environment(pdFactor)




## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

