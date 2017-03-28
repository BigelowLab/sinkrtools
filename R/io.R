#' Read a matlab file and return the first element or the specified element
#'
#' @export
#' @param filename the fully qualified file path
#' @param elem character or numeric index of the element to return
#' @return the item at index 'elem' unless there is an error in which case NULL
read_matlab <- function(filename, elem = 1){
    
    if (!file.exists(filename[1])) stop("file not found:", filename[1])
    
    x <- try(R.matlab::readMat(filename[1]))
    if (inherits(x, 'try-error')){
        return(NULL)
    } else {
        return(x[[elem[1]]])
    }
}

#' Write a matlab file
#'
#' @export
#' @param x the object to write
#' @param filename the fully qualified path to the file
#' @return value returned by \code{R.matlab::writeMat()}
write_matlab <- function(x, filename){

    if (missing(filename)) stop("filename is required")
    
    R.matlab::writeMat(x, filename)
}
   