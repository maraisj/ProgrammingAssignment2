## ProgrammingAssignment 2:
##Calculate the inverse of a matrix

## This function creates a list of functions that will:
## 1: Set the value of a matrix (set);
## 2: Get the value of a matrix (get);
## 3: Set the new inverse matrix (setinv);
## 4: Get the new inverse matrix (getinv);

makeCacheMatrix <- function(x = matrix()) {
        inv<<-NULL
        set<- function(y){
                x<<-y
                inv<<-NULL
        }
        getinv<-function() inv
        get <-function() x
        setinv <- function(inve) inv <<- inve
        list(set = set, get = get, setinv = setinv,getinv = getinv)
}


##This function will return the inverse of a matrix. Firstly it will check 
##if the Inverse of a matrix has been calculated. If it has, then it will return 
##the result. If not, it will calculate it and store the result


cacheSolve <- function(x, ...) {
        
        inv<-x$getinv()
        if(!is.null(inv)) {
                message("getting cached data...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        ##Stores the result of inv in the cache
        x$setinv(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}

## test data:
##x <- matrix(1:4,2,2)
##a <- makeCacheMatrix(x)
##b <- cacheSolve(a)
##_________
##result:
## b
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
