## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL      # assigns m - nothing
        set <- function(y) { # 1 set-function- first element of the list that will be created in which x and m are defined as variables for the global environment
                x <<- y
                m <<- NULL
        }
        get <- function() x  # 2 get-function- second element of the list that will be created from which x can be read
        setinverse <- function(solve) m <<- solve # 3 solve-function- third element of the list that will calculate the inverse of the matrix
        getinverse <- function() m # 4 get the solve-function- 4-th element of the list that will be created from which the solve/solution can be read
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) # the list is defined - and it will be the result

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
                
                m <- x$getinverse() # assigns to the local "m" the result stored in the list in point 4 (in case it was calculated it will be the inverse, in case it is not it will be NULL)
                
                if(!is.null(m)) { # cheks if it is empty or not the result (inverse of matrix)
                        message("getting cached data") # if it is not empty, then it prints "getting cached data" and then the result from the previous function ()
                        return(m)
                }
                data <- x$get() # assigns to "data" the matrix from privious function from within the list, point number 2
                m <- solve(data, ...) # calculates the inverse in case if it was not priviously calculated
                x$setinverse(m) # stores the inverse in the list at point number 3
                return(m)
        }

