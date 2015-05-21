# Matrix inversion can be a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it over and over. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y){
               x <<- y
               inv <<- NULL
        } #set
        get <- function() x     #get
        setinverse <- function(inverse) inv <<- inverse  #setmatrix #create it's inverse using solve(X)
        getinverse <- function() inv  #getinverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #store the inverse to cache
   }


# The cacheSolve function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If it has, it takes the result and skips the
# computation. If not, it then computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() #check if inverse has been created
        if(!is.null(inv)){
                message("getting cached data.")
                return(inv)   #if yes return inverse
        }  
        data <- x$get()  #if no compute inverse
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
    }
