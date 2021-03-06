% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/activate.R
\name{activate}
\alias{activate}
\title{Activates a neural network}
\usage{
activate(input, nn, act_fun = sigmoid)
}
\arguments{
\item{input}{Numerical vector of the same length of the input neurons of the net}

\item{nn}{Neural Network}

\item{act_fun}{activation function used}
}
\value{
Numerical Vector of the output for the Neural Network
}
\description{
Activates a neural network
}
\examples{
build.nn(2, 1, enable_rate = 1) \%>\% activate(c(1,1), .)
build.nn(2, 1, 2, enable_rate = 1) \%>\% activate(c(1,1), .)
build.nn(2, 1, c(2,3), enable_rate = 1) \%>\% activate(c(1,1), .)
build.nn(2, 2, c(5,5,5), enable_rate = 1) \%>\% activate(c(1,1), .)
}
