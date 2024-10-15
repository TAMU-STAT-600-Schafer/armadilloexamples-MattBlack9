library(rcpp)
library(amadillo)

sourceCpp("ArmadilloExamples.cpp)

X <- matrix(rnorm(300), 30, 10)
Y <- matrix(rnorm(200), 10, 20)

prodCpp <- matrix_mult(X, Y)
prodR <- X %*% Y

all.equal(prodCpp, prodR)



X <- matrix(rnorm(30000), 300, 100)
Y <- matrix(rnorm(20000), 100, 200)

library(microbenchmark)
microbenchmark(matrix_mult(X, Y), X %*% Y)
