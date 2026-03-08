
colMatchesHelp = function() {
    cat("# --------------------------------------------------\n")
    cat("Syntax: colMatches(A, n)\n")
    cat("A is an array\n")
    cat("n = the number of matches in each column to check for \n")
    cat("For each column in array A, colMatches() checks if at least\n")
    cat("one entry is repeated n times\n")
    cat("\n")
    cat("Returns a vector of 0's and 1's: 1 means at least\n")
    cat("one entry was repeated at least n times.\n")
    cat("# --------------------------------------------------\n")

}
colMatches = function(A, size_match=2) {
    # See colMatchesHelp()

    #RED_FLAG: We assume A is a vector or 2 dimensional array
    #RED_FLAG: We don't check that size_match > 0
    A.dim = dim(A)
    if (is.null(A.dim)) {
        #assume A is a column vector
        nrows = length(A)
        ncols = 1
        Asrt= matrix(sort(A), nrow=nrows, ncol=ncols)
    }
    else {
        nrows = A.dim[1]
        ncols = A.dim[2]
        #apply() is an r-magic function. In this case it applies sort to each column. To apply to each row use apply(A, 1, sort)
        Asrt = apply(A, 2, sort)
    }
    if (size_match > nrows) {
        #Can't possibly have more matches than rows, return a vector of 0's
        b = rep(0, times=ncols)
    }
    else {
        #Sneaky way to look for runs of size_match in sorted columns
        x = Asrt[size_match:nrows,] == Asrt[1:(nrows-size_match+1),]
        if (ncols == 1) {
            b = 1.0*(sum(x) > 0)
        }
        else if (size_match == nrows) {
            b = as.vector(1.0*x)
        }
        else {
            b = as.vector(1.0*(apply(x, 2, sum) >0))
        }
    }
    return(b)
}
#---------------------------------------------------------
# OpenCourseWare: https://ocw.mit.edu
#---------------------------------------------------------

