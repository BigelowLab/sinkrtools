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
#' @param compress numeric if non-zero then gzip the result at the compression
#'    level specified
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


#' Given a path to rasters and a factor for aggregating, create a brick and 
#' save as mat.  Not to be confused with raster2mat which handles the save.
#'
#' @export
#' @param srcpath character, the path to the raster files (.grd)
#' @param fact numeric the positive aggregating factor
#' @param dstfile the destination filename (.mat or .mat.gz)
#' @param verbose logical
#' @return value returned by \code{sinkrtools::raster2mat()} or NULL
rasters_to_mat <- function(
    srcpath = '.',
    fact = 5,
    dstfile = './output.mat.gz',
    verbose = FALSE){
 
    if (!file.exists(srcpath)) stop("srcpath not found\n")
    if (!file.exists(dirname(dstfile))) stop("dstfile path not found\n")
 
    if(verbose) cat("listing files\n")
    ff <- list.files(srcpath, pattern = glob2rx("*.grd"), full.names = TRUE)
    if (length(ff) == 0) stop("no .grd files found in:", srcpath)
    
    if (verbose) cat("reading", length(ff), "files into a stack\n")
    RR <- raster::stack(ff)    
    
    if (fact > 1) {
        if (verbose) cat("aggregating by factor of", fact, "\n")
        RR <- raster::aggregate(RR, fact = c(fact[1], fact[1], 1))
    }
    if (verbose) cat("writing mat file:", dstfile)
    RR <- try(raster2mat(RR, filename = dstfile))
    if (inherits(RR, 'try-error')){
        print(RR)
    }
    invisible(RR)
}
