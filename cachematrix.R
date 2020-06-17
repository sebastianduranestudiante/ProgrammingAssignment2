## We are going to write a couple of functions that cache the inverse of a matrix.
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<-function(y){
                x<<-y
                inv <<-NULL
        }
        get<-function(){x}
        setinverse<-function(inverse){inv<<-inverse}
        getinverse<-function(){inv}
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#f the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setinverse(inv)
        inv
}
