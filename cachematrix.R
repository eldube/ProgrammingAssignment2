####################################INTRODUCTION##############################
## Computing the inverse of a matrix may be time consuming. However, if the 
## value of the matrix does not change, and the inverse is computed repeatedly,
## it may make sense to cache the value of the inverse of the matrix so that 
## when we need it again, it can be looked up in the cache rather than being
## recomputed. 

## This script defines two functions: makeCacheMatrix and cacheSolve , and 
## illustrates how the special assignment operator <<- can be used within a 
## function to cache the value of a variable in the global environment. In 
## this script the inverse of the matrix is cached in the global environment
## by assigning its value using the <<- operator instead of <- operator. The
## operator <<- looks up the value of the inverse in the global environment,
## and if its already computed (not NULL), it is not re-computed.


################################makeCacheMatrix function ##################### 
## The makeCacheMatrix(x) function takes a single invertible numeric square 
## matrix x and creates a special "Matrix" object, which is really a list 
## containing following public functions on the object:  set(matrixvalue),
## get(), setinverse(inversevalue), and getinvers().

## For example, the expression MO <- makeCacheMatrix()creates special "Matrix"
## object whose value is an empty matrix. However, the expression 
## MO <- makeCacheMatrix(A) creates special  "Matrix" object whose value is 
## matrix A.
 
## set(matrixvalue) -- takes a single matrix argument(matrixvalue) and assigns 
## it to be the matrix value of the special "Matrix" object. For example if A 
## is a numeric matrix, and MO is a special matrix object (created using the 
## makeCacheMatrix function), then  MO$set(A), sets the matrix value of MO to 
## be matrix A.

## get()-- returns the matrix value of the special "Matrix" object.For example,
## if MO is a special matrix object, and MO$set(A) is used to set its value to 
## be matrix A, then MO$get returns matrix A

## setinverse(inverseValue)-- takes a single matrix argument(inverseValue)and
## sets the inverse matrix  value of the special "Matrix" object. For example
## if Ainv is a numeric matrix, and MO is a special "Matrix" object, then 
## MO$setinverse(Ainv), set the inverse matrix value of MO to be matrix Ainv.
 
## getinverse()-- returns the inverse matrix value of the special "Matrix" 
## object.For example,if MO is a special matrix object,and MO$setinverse(Ainv)
## is used to set its inverse matrix value to be matrix Ainv,then 
## MO$getinverse()returns matrix Ainv


makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )

}

################################cacheSolve function ##########################
## The cacheSolve(x,...) function takes a single special matrix object(created 
## using the makeCacheMatrix function) and uses the solve function in R to 
## compute and set the inverse value of the special "Matrix". 

## For example, if MO is a special matrix object created using makeCacheMatrix,
## then cacheSolve(MO), uses the get() function(MO$get) to retrieve the matrix 
## value of MO (say data), and then calls the solve function to compute the 
## inverse of the matrix (say solve(data, ...)).If the inverse value of the 
## matrix has already been and cached before, the cached value is simple 
## retrieved and not-recomputed.

## For example if MO is created using the expression MO <- makeCacheMatrix (A) 
## where A is a invertible numeric matrix, then cacheSolve (MO) computes the 
## inverse of matrix A,and then calls function setinverse()to set the inverse 
## matrix value of MO. After this sequence of statements, MO$getinverse()will
## return the inverse of matrix A.


cacheSolve <- function(x, ...) {                 
  ## argument 'x' must be special "Matrix" object created using makeCacheMatrix 
  ## Return a matrix that is the inverse of matrix value of 'x'

	inv <- x$getinverse ()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## retrieve matrix value of special "Matrix" 'x'
        data <- x$get()

        ## compute inverse matrix
        inv <- solve(data, ...)

        ##set inverse matrix value of 'x' to be the computed inverse  
        x$setinverse(inv)

        ## return inverse of 'x'
        inv             

}
######################################END SCRIPT #############################
