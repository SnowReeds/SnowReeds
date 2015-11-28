Saravannan
============
## makeCacheMatrix create list of fuction to set matrix and get matrix and
## set solve for Matrix and get the solve matrix

## Create square Matix 
## minv<-matrix(c(3,2,5,6),nrow=2,ncol=2) 

## cacheMat<-makeCacheMatrix(minv)

##cacheMat$setMat()------> Set Matrix value
##cacheMat$getMat()------> get Matrix value
##cacheMat$setSolve()----> set Solve Matrix
##cacheMat$getSolve()----> get Solve matrix

makeCacheMatrix <- function(x = matrix())
{
        ## Set the value of the matrix which we pass through y, it will store 
        ## in x (lexical scoping)
        
	sol <- NULL
        setMat <- function(y)
        {
                x <<- y
                sol <<- NULL
        }
        
	## Get the value of the matrix x that store in setMat
        getMat <- function() x
        
	## Set the solve function 
        setSolve <- function(solve) sol <<- solve
        
	## store solve matrix value
        getSolve<- function() sol
        list(setMat = setMat, getMat = getMat, setSolve = setSolve,
             getSolve= getSolve)
        
}
## CacheSolve returns the value of solve matrix
## cacheSolve(cacheMat) contains all the list function created in cacheMat
## cacheSolve(cacheMat) -------> returns inverse of matrix

cacheSolve <- function(x, ...)
{
        sol <- x$getSolve()
        
        ## if the solve function empty it returns
        ## function (a, b, ...) 
        ## UseMethod("solve")
        ## <bytecode: 0x0000000007de45d0>
        ## <environment: namespace:base>
        
	if(!is.null(sol)) {
                message("getting cached data")
                return(sol)
        }
        data <- x$getMat()
        
	## solve(a, b, ...) is default. But we are going to compute for square matrix
        ## solve(a=data) is used for inverse of square matrix
        
	sol <- solve(data, ...)
        ## Return a matrix that is the inverse of 'x'
        x$setSolve(sol)
        sol
        
}
