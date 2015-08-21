#makeCacheMatrix`: This function creates a special "x" object
#    that can cache its inverse.
#  `cacheSolve`: This function computes the inverse of the special
#    "x" returned by `makeCacheMatrix` above. If the inverse has
#    already been calculated (and the matrix has not changed), then the
#    `cachesolve` should retrieve the inverse from the cache.

#Storing the matrix, "x" in cache memory. Creating the functions for 
# retrieving its inverse, if existing in cache

makeCacheMatrix <- function(x = matrix()) {
invX<-NULL
    print(invX)
    #getters and setters for retrieving the value of x
    set<-function(y){
    #To commit x in cache
        x<<-y
    }
    get<-function() x
    #getters and setters for the value of inverse
    setMatrix<-function(inv) invX<<- inv
    getMatrix<-function() invX
    list(set=set, get=get,
         setMatrix=setMatrix,
         getMatrix=getMatrix)
}


## This function returns the inverse of the matrix "x", the inverse is picked from memory if existing

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-matrix()
    #print(x)
    #to fetch the value of inverse of x(from memory,if present) and store in m
    m<-x$getMatrix()
    if(!is.null(m)){
    #if not null, return the value from cache
        print("getting cached data")
        return(m)
    }
    
    print(x$get())
    matrix<-x$get()
    #else, calculate inverse
    inv<-solve(matrix)
    #store in cache
    invX<-x$setMatrix(inv)
    return(invX)
}

