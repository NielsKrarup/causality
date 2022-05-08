n <- 3000
library(dplyr)

A <- rbinom(n = n, size = 1, prob = 1/2) 
H <- (A + rbinom(n = n, size = 1, prob = 1/3)) %% 2
B <- (H + rbinom(n = n, size = 1, prob = 1/20)) %% 2

head(cbind(A,B,H))

tb <- tibble(A,H,B)
tb %>% group_by(A,H,B) %>% tally() %>% group_by(A) %>% mutate(A_prop = sum(n))

cat("A == H in", sum(A==H)/n, "of the cases.\n")
cat("B == H in", sum(B==H)/n, "of the cases.\n")

fitAB <- glm(H~A+B,family=binomial())
summary(fitAB)

# Q1: Fit the models H~A and H~B, too, and convince yourself that 
# the model H~A+B yields the best fit. 


