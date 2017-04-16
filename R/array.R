#' Split a vector into groups of MAX (or possibly fewer)
#'
#' @export
#' @param v vector or list to split
#' @param MAX numeric the maximum size per group
#' @return a list of the vector split into groups
split_vector <- function(v, MAX = 200){
    nv <- length(v)
    if (nv <= MAX) return(list('1' = v))
    split(v, findInterval(1:nv, seq(from = 1, to = nv, by = MAX)))
}


#' Run dineof on a [x,y,t] array of real values
#'
#' @export
#' @param x numeric 3-d array in the form [nrow, ncol, ntime]
#' @param mask a 2-d array of form [nrow, ncol] with NA indicating mask (like land)
#' @param step numeric, input is divided into this many time steps
#' @param n_cores numeric the number of cores to use for parallel processing.
#'  If n_cores <= 1 then parallel execistion is not used.
#' @return numeric interpolated array of same shape as input
dineof_array <- function(x, 
    mask = NULL, 
    step = 90,
    n_cores = pmin(parallel::detectCores()/2, 5) ){
    
    d <- dim(x)
    if (length(d) != 3) stop("input must be [nrow, ncol, ntime]")
    
    # for x into [nrow*ncol,ntime]
    dim(x) <- c(d[1]*d[2], d[3])
    
    # cull mask points if provided
    if (!is.null(mask)){
        m <- dim(mask)
        if (!identical(m, d[1:2])) 
            stop("mask must has same [nrow, ncol] dims as input")
        imask <- if(is.logical(mask)) as.vector(mask) else as.vector(is.na(mask))
        x <- x[!imask,]
    } else {
        imask <- NULL
    }
    
    
    if (n_cores > 1){
        # split into time slabs
        ix <- split_vector(seq_len(d[3]), step)
        xx <- lapply(ix, function(i) x[,i])
        # create the parallel cluster
        cl <- parallel::makeCluster(n_cores)
        # 'export' required packages to each core
        parallel::clusterEvalQ(cl, library(irlba))
        parallel::clusterEvalQ(cl, library(sinkr))
        # parallel loop
        xx <- parallel::parLapply(cl, xx,
            function(x){
                sinkr::dineof(x)[['Xa']]
            }) 
        # clean up
        parallel::stopCluster(cl)
        
        
        if (!is.null(imask)){
            # recreate input
            x <- matrix(NA_real_, ncol = d[3], nrow = d[1]*d[2])
            # insert interpolated rows
            x[!imask,] <- do.call(cbind,xx)
        } else {
            x <- do.call(cbind, xx)
        }
    } else {
        xa <- sinkr::dineof(x)[['Xa']]
        if (!is.null(imask)){
            x <- matrix(NA_real_, ncol = d[3], nrow = d[1]*d[2])
            x[!imask,] <- xa
        } else {
            x <- xa
        }
    }
    
    # reshape to original dims
    dim(x) <- d
    
    invisible(x)

}
