## This function is an improved version of solve() function, which is highly used to find
## the inverse of matrix. Its advantage is calculation time is less than solve

## This function creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix<- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setS <- function(solve) I <<- solve
        getS <- function() I
        list(set = set, get = get,
             setS = setS,
             getS = getS)
}


## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        I <- x$getS()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setS(I)
        I
}
a<-c(1,4,5)
b<-c(3,4,7)
c<-c(5,2,5)
k<-rbind(a,b,c)
k
class(k)
n<-makeCacheMatrix(k)
n
s<-cacheSolve(n)
s


