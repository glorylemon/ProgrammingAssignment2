## These two functions cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions.

makeCacheMatrix <- function(x = matrix()) {

ivs<-NULL
set<-function(y){
x<<-y
ivs<<-NULL
}
get<-function()x
setivs<-function(ivs) ivs<<-solve(x)
getivs<-function()ivs
list(set=set,get=get,setivs=setivs,getivs=getivs)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
ivs<-x$getivs()
if(!is.null(ivs)){
message("getting cached data")
return(ivs)
}
data<-x$get()
ivs<-solve(data)
x$setivs(ivs)
ivs
}

}
