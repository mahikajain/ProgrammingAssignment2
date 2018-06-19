##Two functions are involved in this assignment. The idea here is to find the inverse of a matrix and then cache these values for 
#future use. The concept of Lexical Scoping is used for this purpose.

## This function makeCacheMatrix as the name suggests is used to cache inverse matrix solution and original matrix pair for future use when
#the same value is called again.

makeCacheMatrix <- function(x = matrix()) {
        I<-NULL
        set<-function(y){
                x<<-y
                I<<-NULL
        }
        get<-function() x 
        #value of x is retrieved
        setinv<-function(inv) I<<-inv
        #an inverse matrix solution is set to I
        getinv<-function() I
        #this function is used to call the inverse if cached
        list(set=set, get=get, 
             setinv=setinv, 
             getinv=getinv)
        #all the functions are stored as a list so they can be called using '$'
}


## cacheSolve function is mainly used to obtain the inverse of a matrix using the 'solve' function. It takes the makeCacheMatrix
#solution of the matrix as input and then checks if the value was cached or not to use it accordingly.

cacheSolve <- function(x, ...) {
        I<-x$getinv()
        #Inverse value is cached
        if(!is.null(I)){
                message("getting cached data")
                return(I)
        }
        #the above steps are used to check if I is null to accordingly decide whether to find an inverse or use cached value
        data<-x$get()
        I<-solve(data,...) #inverse computed
        x$setinv(I)
        I
        
}
