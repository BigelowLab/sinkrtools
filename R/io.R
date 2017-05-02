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
        cat("error reading matlab file:", filename[1], "\n")
        return(NULL)
    } else {
        if (is.character(elem[1])){
            if ( !(elem[1] %in% names(x)) ){
                cat("element not found in matlab file:", elem[1], "\n")
                return(NULL)
            }
        }
        x <- x[[elem[1]]]
    }
    x[is.nan(x)] <- NA
    invisible(x)
}

#' Write a matlab file
#'
#' @export
#' @param x the object to write
#' @param filename the fully qualified path to the file
#' @return value returned by \code{R.matlab::writeMat()}
write_matlab <- function(x, filename){

    if (missing(filename)) stop("filename is required")
    
    R.matlab::writeMat(filename, x = x)
}
   
   
#' Write a Raster to a mat file
#'
#' The output file captures basic elements of the Raster
#'  \itemize{
#'      \item{data the variable data}
#'      \item{projection charcate string}
#'      \item{locations list of cols, rows and z}
#'  }
#'
#' @export
#' @param x the Raster* object
#' @param filename the fully qualified filename to write to
#' @param overwrite logical, if FALSE then stop if filename already exists
#' @param compress numeric if non-zero then gzip the result at the compression level specified
#' @return the original Raster* invisibly
raster2mat <- function(x, 
    filename = "raster2mat.mat.gz", 
    overwrite = TRUE,
    compress = 6){
    if (file.exists(filename[1]) && !overwrite){
        cat("file already exists:", filename[1], "\n")
        return(invisible(x))
    }
    
    compress <- as.numeric(compress[1])
    dat <- raster::as.array(x)
    cols <- raster::xFromCol(x,1:ncol(x))
    rows <- raster::yFromRow(x,1:nrow(x))
    z <- raster::getZ(x)
    if (is.null(z)){
        z <- 1:raster::nlayers(x)
    } else if (inherits(z, 'Date')){
        z <- format(z, "%Y-%m-%d")
    } else {
        z <- format(z, "%Y-%m-%dT%H:%M:%S %Z")
    }
    
    if (compress[1] > 0){
        ff <- gzfile(filename[1], compress = compress[1])
        
    } else {
        ff <- filename[1]
    }
    
    numbytes <- R.matlab::writeMat(ff,
        data = dat,
        projection = raster::projection(x),
        locations = list(cols = cols, rows = rows, z = z))

    if (numbytes <= 0) cat("no data written\n")
    
    
    invisible(x)
}