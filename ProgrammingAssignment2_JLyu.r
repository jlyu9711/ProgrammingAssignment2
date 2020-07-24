makeCacheMatrix <- function(x=numeric()) {
        #initially nothing to cache so using NULL
        m <- NULL
        
        #set up a matrix
        set <- function(y) {
                x <<- y
                m <<-NULL
        }
        get <- function () {x}
        
        #cache and get the cached value
        setinverse <- function(inverse) { 
                m <<-inverse
        }
        getinverse <- function() {m} 
        
        #returning a list that each element is a function
        list(set=set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        #this function is to return an inverse of matrix "x"
        inverse <- x$getinverse()
        
        #if a cached value already exist return it
        if (!is.null(inverse)) {
                print("getting cached data")
                return(inverse)
        }
        
        #if not, calculate the inverse 
        #store it in the cache for future returns
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        
        #return the inverse of x
        inverse
}

