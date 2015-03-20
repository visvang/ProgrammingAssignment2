## These are two functions that can compute matrix inverse once
## and store it in cache for later use


## The first function creates a special "matrix" object
## that stores the matrix itself, its cached inverse if computed
## and has four "methods" to deal with it

## returns "matrix" object 

## x -- matrix for computing inverse
makeCacheMatrix <- function(x = matrix()) {
	## xinv -- variable to store the inverse of matrix x
	## in the beginning the inverse is not computed, 
	## minv is assigned NULL
	xinv <- NULL
	
	## function to store given matrix into the "matrix" object
	## m is the matrix to be stored
	set <- function (m) {
		## whatever matrix is given to set function,
		## it's going to be the matrix of this "matrix" object
		x <<- m
		
		## if the set function is used to store the matrix into the object
		## then this matrix could be changed 
		## erase the stored inverse because it may be not corresponding anymore
		xinv <<- NULL
	}
	
	## function to get the stored matrix from "matrix" object
	get <- function() {
		## returns x
		x
	}
	
	## function to store computed inverse into the "matrix" object
	## inverse -- computed inverse matrix
	setinv <- function(inverse) {
		## stores it into xinv variable
		xinv <<- inverse
	}
	
	## function to get cached inverse from "matrix" object
	getinv <- function() {
		## returns xinv
		xinv
	}
	
	## create the "matrix" object itself
	## it is a list of four elements (functions) 
	## used to deal with "hidden" variable for cached inverse
	list (set = set,
			get = get,
			setinv = setinv,
			getinv = getinv
			)	
}


## The second function computes inverse for matrix
## or gets it directly from cache if it was computed before

## returns matrix inverse

## x -- matrix to return inverse to
## ... -- possible parameters for Solve() function
cacheSolve <- function(x, ...) {
		
		## check with special function if corresponding "matrix" object exists
		## the function returns corresponding object
        ## assign corresponding object to a variable outside of current function
		x_obj <<- checkExistence(x)		
		
		## try to get inverse from cache with getinv element of "matrix" object
		xinv <- x_obj$getinv()
		
		## NULL if inverse was not computed
		## not NULL if inverse was computed and cached
		if (!is.null(xinv)) {
			message ("getting cached inverse")
			## return inverse from cache
			return (xinv)
		}
		
		## if inverse wasn't cached and its value wasn't returned
		## compute inverse now, parameters will be passed via ... if needed
		xinv <- solve(x, ...)
		## store computed inverse in cache now with setinv function
		## into the "matrix" object
		x_obj$setinv(xinv)
		## Return a matrix that is the inverse of 'x'
		xinv
}


## This function checks if corresponding "matrix" object exists in global environment

## returns such object if it exists
## otherwise creates it and then returns

## x -- matrix
checkExistence <- function (x) {	
		## here I assume that I'm going to use my functions in my own code
		## so I assume I have a rule every time I call makeCacheMatrix
		## on a matrix <name>
		## I name the resulting "matrix" object <name>_obj
		
		## here I create variable with the possible object name in it
		## take name of the matrix, add "_obj" to it
		objname <- paste(deparse(substitute(x)), "obj", sep="_")
          		
		## check if such object exists in global environment
		## if it doesn't exist, function creates new "matrix" object and returns it     
        if (!exists(objname)) {
			message ("object not found")
            return (makeCacheMatrix(x))
		}
		else {
		    ## check if it really is the corresponding "matrix" object
			
			## assign the value to xobj variable
			xobj <- get(objname)
			## get stored matrix into matr variable
			matr <- xobj$get()
			## compare stored matrix and matrix in question
			## if they are different, create new "matrix" object for the matrix in question
			## and return it
			if (!identical(matr, x)) {
				message ("wrong object")	
                return (makeCacheMatrix(x))
			}
		}
	
		## otherwise return the found "matrix" object
        message ("object found")
		get(objname)
}