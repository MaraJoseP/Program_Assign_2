## A PAIR OF FUNCTIONS THAT CACHE THE INVERSE OF A MATRIX


## The first one (makeCacheMatrix) creates some kind of special matrix object
## that caches it inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Set as NULL the matrix where I'm going to save the inverse.
    # It is done in order to check later if the Inverse is on cache or not.
    Inv<-NULL
    
    #This function sets the value of the matrix which I want to compute the inverse
    set<-function(A){
        x<<-A
        Inv<<-NULL
    }
    
    get<-function()x
    
    #Function to compute the inverse using the solve R function
    setinverse<-function(solve)
    Inv<<-solve
    getinverse<-function()Inv
    
    #Constructs the list (formed by four functions) that makeCacheMatrix returns to the user (internally)
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}




## The second one, CacheSolve, computes the inverse of the special "matrix" returned
## by makeCacheMatrix.

## This function looks if the inverse is cached and if not, computes it using the
## list retrieved by the function makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## First of all, it checks if the inverse has already been calculated and has it cached.
        
        ## If it is the case, the program writes a message to the user reporting that the inverse
        ## is get from cache and also prints on screen the explicit inverse matrix.
        ## Then the function finishes.
        Inv<-x$getinverse()
        if(!is.null(Inv)){
            message("getting cached data")
            return(Inv)
        }
        
        ## If not, the program gets the matrix which I want to compute the inverse by using the second
        ## function (get) of the list returned by makeCacheMatrix and computes its inverse with the
        ## corresponding functions (also obtained from the list retrieved by makeCacheMatrix)
        data<-x$get()
        Inv<-solve(data,...)
        x$setinverse(Inv)
        
        ## Print the result to the user on screen
        Inv
}
