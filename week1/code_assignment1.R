n <- 1e3
library(dplyr)

NX <- rnorm(n)
NY <- rnorm(n)
NZ <- rnorm(n)


X <- NX
Y <- -X + NY
Z <- X + 2*Y + NZ
cov(data.frame(X,Y,Z)) %>% round(0)

#part 2
n <- 1e3

Y <- rnorm(n = n, sd = sqrt(2))
X <- -1/2*Y + rnorm(n = n, sd = sqrt(1/2))
Z <- 1*X + 2*Y + rnorm(n = n, sd = sqrt(1))

df <- data.frame(X,Y,Z)
round(cov(df),0)

var(X)
var(Y)
var(Z)

cov(X,Y)
cov(X,Z)
cov(Y,Z)

mean(Y)

#interven! 
Y <- rnorm(n, mean = 0, sd = sqrt(2))



5 %% 1.3

# 4) ----------------------------------------------------------------------
n <- 1e4
library(mvtnorm)

sigma <- matrix(c(1,-1, -1,
                  -1, 2, 3,
                  -1, 3, 6), nrow = 3, byrow = T)
sigma

xyz <- rmvnorm(n = n, sigma = sigma)
y <-xyz[,2]

#our best beta from assignment
b <- c(-3, 2)/5

#Expected squared error - function
mse <- function(b){
  mean((y - b%*%t(xyz[ ,c(1,3)]) )^2 )
}
#test 
(min <- mse(b = b))


#grid of beta's
b1 <- seq(from = -3, to = 3, length.out = 1e2)
b2 <- seq(from = -5, to = 5,length.out = 1e2)

b_grid <- expand.grid(b1 = b1, b2 = b2)

#add mse for each beta vector
mse_vec <- apply(X = b_grid, MARGIN = 1, FUN = function(vec) mse(b = vec))
b_grid$mse <- mse_vec

mse_mat <- matrix(data = mse_vec, nrow = length(b1), ncol = length(b2), byrow = T)
length(b1)*length(b2)

plot_ly(x = b1, y = b2, z = mse_mat) %>% add_surface() %>% add_markers(x = b[1], y = b[2], z = min)
