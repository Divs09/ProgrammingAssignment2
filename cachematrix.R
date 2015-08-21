## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
invX<-NULL
    print(invX)
    set<-function(y){
        print("1")
        x<<-y
        invX<<-solve(x)
        print(invX)
        return(invX)
    }
    get<-function() x
    setMatrix<-function(inv) invX<<- inv
    getMatrix<-function() invX
    list(set=set, get=get,
         setMatrix=setMatrix,
         getMatrix=getMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            #as.matrix(x,2,2)
    m<-matrix()
    #x<-as.matrix(x,2,2)
    print(x)
    m<-x$getMatrix()
    if(!is.null(m)){
        print("getting cached data")
        return(m)
    }
    
    print(x$get())
    matrix<-x$get()
    inv<-solve(matrix)
    invX<-x$setMatrix(inv)
    return(invX)
}

