## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

