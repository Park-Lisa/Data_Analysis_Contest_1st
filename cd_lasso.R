# set soft-thresholding function
S <- function(z, lambda) {
  (z - lambda) * (z > lambda) + 
    (z + lambda) * (z < -lambda) + 
    0 * (abs(z) <= lambda)
}

# Coordinate decent algorithm for LASSO
cd.lasso <- function(x, y, lambda)
{
# CD algorithm for lasso
# marginal standardization of x
z <- scale(x)
m <- attr(z, "scaled:center")
s <- attr(z, "scaled:scale")

# centering of y
u <- (y - mean(y))

# initialization
beta <- coef(lm(u ~ z - 1))
r <- u - z %*% beta

for (iter in 1:100) {
   new.beta <- beta
   for (j in 1:p) {
      temp <- beta[j] + crossprod(z[,j], r)/n
      new.beta[j] <- S(temp, lambda/s[j]) 
      r <- r - (new.beta[j] - beta[j]) * z[,j]
   }
delta <- max(abs(new.beta - beta))
if (delta < 1.0e-3) break
beta <- new.beta
}
beta <- new.beta/s 
beta0 <- mean(y) - crossprod(beta, m)

c(beta0, beta)
}



set.seed(1)

n <- 100
p <- 5   # try 5, 20, 50

x <- matrix(rnorm(n*p, 1, 1), n, p) # predictor
e <- rnorm(n, 0, 0.5) # noise

true.beta <- rep(0, p + 1)
true.beta[1] <- 1 # intercept
true.beta[2:(p+1)] <- c(rep(1, 3), rep(0, p-3)) # coefficients

y <- true.beta[1] + x %*% true.beta[-1] + e # response

# 1. ols
est0 <- coef(lm(y ~ x))

# 2. lasso
lambda <- 0.1
est1 <- cd.lasso(x, y, lambda)

## 3. glmnet
library(glmnet)
est2 <-coef(glmnet(x, y, lambda = lambda, standardize = F))

result <- cbind(true.beta, est1, est2)
rownames(result) <- 0:p
colnames(result) <- c("true", "ours", "glmnet")
print(round(result, 4))

############################### 
# test prediction performance #
###############################
set.seed(2)
n.test <- 1000
x.test <- matrix(rnorm(n*p, 1, 1), n.test, p) # predictor
e <- rnorm(n.test, 0, 0.5) # noise
y.test <- true.beta[1] + x.test %*% true.beta[-1] + e # response


y0 <- est0[1] + x.test %*% est0[-1] # ols prediction
y1 <- est1[1] + x.test %*% est1[-1] # lasso prediction

mse0 <- sum((y.test - y0)^2)
mse1 <- sum((y.test - y1)^2)

cat("OLS  :", mse0, "\n")
cat("Lasso:", mse1, "\n")
