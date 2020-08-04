###################################################
#### This file computes several segregation indices
###################################################

#### Dissimilarity index
D.compute <- function(Y, N)
{
#### Compute the dissimilarity index
N.all <- sum(N)
p.all <- sum(Y) / sum(N)
p <- Y / N
D <- sum(N * abs(p - p.all)) / (2 * N.all * p.all * (1-p.all))

#### Return the result
return(D)
}   
    


#### Gini index
G.compute <- function(Y, N)
{
#### Compute the quantities needed in the index
N.all <- sum(N)
p.all <- sum(Y) / sum(N)
p <- Y / N
dist.p <- as.matrix(dist(p))
N.prod <- N %*% t(N)

#### Compute the Gini index
G <- sum(N.prod * dist.p) / (2 * N.all^2 * p.all * (1-p.all))

#### Return the result
return(G)
} 



#### Moran's I index
I.compute <- function(Y, N, W)
{
#### Compute the quantities needed in the index
K <- length(Y)
p <- Y / N
p.bar <- mean(p)
p.centre <- p - p.bar
p.centre.sq <-  p.centre %*% t(p.centre)
    
#### Compute the Gini index
I <- K * sum(W * p.centre.sq) / (sum(W) * sum(p.centre^2))

#### Return the result
return(I)
} 




RCI.compute <- function(Y, N, order)
{
#### Storage vectors for X = ordered vector of people "in poverty"; Y = "not in pov"
X <- 1:length(Y)
Z <- X
    
#### Compute Z and X
    for(i in 1:length(X))
    {
    X[i] <- sum(Y[order[1:i]])/sum(Y)
    Z[i] <- sum(N[order[1:i]] - Y[order[1:i]])/sum(N - Y)
    }
    
#### Compute the RCI
RCI <- 0
    for(i in 2:length(X))
    {
    RCI <- RCI + X[i-1] * Z[i] - X[i] * Z[i-1]
    }
    
#### Return the result
return(RCI)
}
