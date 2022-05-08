#10min 10min

# We draw a sample from an SCM
set.seed(1)
n <- 200
C <- rnorm(n)
A <- 0.8*rnorm(n)
K <- A + 0.1*rnorm(n)
X <- C - 2*A + 0.2*rnorm(n)
F <- 3*X + 0.8*rnorm(n)
D <- -2*X + 0.5*rnorm(n)
G <- D + 0.5*rnorm(n)
Y <- 2*K - D + 0.2*rnorm(n)
H <- 0.5*Y + 0.1*rnorm(n)
data.obs <- cbind(C, A, K, X, F, D, G, Y, H)

# Q1a: What is the graph corresponding to the above SCM? (Draw on a paper.)

# Q1b: Take a pair of variables and think about whether you expect this pair to be dependent 
#      (at this stage, you can only guess, later you will have tools to know). Check empirically. 

# Q2: Generate a sample of size 300 from the interventional distribution P_{do(X := N(2,1)}
#     and store the data matrix in data.int
#
# fill in code HERE
# ...
# data.int <- 

# Q3: Do you expect the marginal distr. of Y to be different in both samples?

# Q4: Do you expect the joint distr. of (A,Y) to be different in both samples?

# Q5: Check your answers in Q3 and Q4 empirically.
