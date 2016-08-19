## Author:  Patrick F
## Version: 0.1
## Date:    16 Aug 2016

## In this Programming Assignment it is required to write an R function that is 
## able to cache potentially time-consuming computations. We can take advantage of the 
## (lexical) scoping rules of the R language and how they can be manipulated to preserve 
## state inside of an R object.

## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly. Here we will 
## write a pair of functions that cache the inverse of a matrix.

## We use the <<- operator which can be used to assign a value to an object in an 
## environment that is different from the current environment. Below are two functions 
## that are used to create a special object that stores a matrix and caches its inverse.

## The makeCacheMatrix creates a special "matrix", which is really a list containing 
## 4 functions to;
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse
## 4 get the value of the inverse

## makeCacheMatrix takes a matrix as input and stores it in the parent environment via 
## use of the <<- operator. makeCachMatrix returns a list of 4 functions that are then available
## in the parent environment so access in the parent environment to the functions is available 
## using the x$... notation

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes a 'cacheMakeMatrix' as input (it will raise an error if given an ordinary
## matrix) and can access the variables (e.g. m) and methods (get..., set...) directly that are defined
## in that environment. This means that if the inverse of a 'makeCacheMatrix' has already been calculated
## it will be available through the variable m (cached) and not have to be recalculated.

## See further notes below the definitions of makeCacheMatrix & cacheSolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## See the following useful references:

## Directly related to this Programming Assignment...
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprogAssignment2Prototype.md
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

## More generally related to R...
## http://adv-r.had.co.nz/S3.html

## See the example below for generating a large Invertible Matrix:
## http://stats.stackexchange.com/questions/14951/efficient-calculation-of-matrix-inverse-in-r

#> library(MASS)
#> source('~/Dropbox/Coursera/R-Programming/work/wk3/ProgAss2/cachematrix.R')
#> k <- 2000
#> rho <- 0.3
#> S <- matrix(rep(rho, k*k), nrow = k)
#> diag(S) <- 1
#> dat <- mvrnorm(10000, mu=rep(0,k), Sigma=S)
#> R <- cor(dat)

## R is now a large (2000 x 2000) invertible matrix
## We can create a makeCacheMatrix 'object' and then, accessing the get() method, see that
## the matrix R exists in its environment:

#> source('~/Dropbox/Coursera/R-Programming/work/wk3/ProgAss2/cachematrix.R')
#> CR <-makeCacheMatrix(R)
#> identical(R, CR$get())
#[1] TRUE

## No Inverse Matrix is defined yet in the 'makeCacheMatrix' object environment

#> CR$getinverse()
#NULL

## Ordinary solve method does not work with the makeCacheMatrix object
## cacheSolve function does not work with an ordinary matrix

#> system.time(CRI <- solve(CR))
#Error in solve.default(CR) : 'a' must be a numeric matrix
#Timing stopped at: 0.001 0 0.001 
#> system.time(CRI <- cacheSolve(R))
#Error in x$getinverse : $ operator is invalid for atomic vectors
#Timing stopped at: 0 0 0 

## Because the Inverse matrix in the makeCacheMatrix object is NULL, the first cacheSolve
## will take as long as an ordinary solve 

#> system.time(RI <- solve(R))
#user  system elapsed 
#15.368   0.040  15.434 
#> system.time(CRI <- cacheSolve(CR))
#user  system elapsed 
#14.412   0.017  14.429 


## The Inverse matrices produced with both methods are the same

#> identical(RI, CRI)
#[1] TRUE

## Subsequent calls to calculate the Inverse matrix take just as long for solve with the 
## ordinary matrix case

#> system.time(RI <- solve(R))
#user  system elapsed 
#14.771   0.019  14.790 

## but now cacheSolve can access the cached Inverse in the makeCacheMatrix object
## environment via its getinverse() method, so is much faster

#> system.time(CRI <- cacheSolve(CR))
#getting cached data
#user  system elapsed 
#0       0       0 

## And now we can see that accessing the Inverse matrix in the makeCacheMatrix 
## object environment is possible with the getinverse() method

#> identical(RI, CRI)
#$[1] TRUE
#> CR$getinverse()[1:10]
#[1]  1.830038749  0.008867010 -0.030248875  0.003359585  0.003034054  0.043901901
#[7]  0.022851798 -0.041431035  0.028358966 -0.046478323
#> RI[1:10]
#[1]  1.830038749  0.008867010 -0.030248875  0.003359585  0.003034054  0.043901901
#[7]  0.022851798 -0.041431035  0.028358966 -0.046478323
