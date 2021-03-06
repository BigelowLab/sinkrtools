% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/array.R
\name{dineof_array}
\alias{dineof_array}
\title{Run dineof on a [x,y,t] array of real values}
\usage{
dineof_array(x, mask = NULL, step = 90,
  n_cores = pmin(parallel::detectCores()/2, 5))
}
\arguments{
\item{x}{numeric 3-d array in the form [nrow, ncol, ntime]}

\item{mask}{a 2-d array of form [nrow, ncol] with NA indicating mask (like land)}

\item{step}{numeric, input is divided into this many time steps}

\item{n_cores}{numeric the number of cores to use for parallel processing.
If n_cores <= 1 then parallel execistion is not used.}
}
\value{
numeric interpolated array of same shape as input
}
\description{
Run dineof on a [x,y,t] array of real values
}

