procrustesR <- function(X, V){
svdXV <- svd(X %*% V)
U <- tcrossprod(svdXV$u, svdXV$v)
return(U)
}

sparsePCAR <- function(X, Vstart, lambda, tol){
  U <- procrustesR(X, Vstart)

  V <- Vstart

  old_obj <- sum((X - tcrossprod(U, V))^2) / 2 + lambda + sum(abs(V))

  error <- 100

  while(error > tol){
    XtU <- crossprod(X,U)
    V <- sign(XtU) * pmax(abs(XtU) - lambda, 0)
    U <- procrustesR(X, V)
    new_obj <- sum((X - tcrossprod(U, V))^2) / 2 + lambda + sum(abs(V))
    error <- abs(new_obj - old_obj)
    old_obj <- new_obj
    }
  return(list(U = U, V = V, error = error))
  }
