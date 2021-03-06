% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dummymodel.R
\name{shift}
\alias{shift}
\title{shift function}
\usage{
shift(points, shift = c(0, 0, 0), shiftto = NULL)
}
\arguments{
\item{points}{the P * 3 matrix that is generated by other base functions}

\item{shift}{an len 3 vector indicating how much we want to shift, default is (0,0,0)}

\item{shiftto}{an len 3 vector indicate where do we want to shift model to. default is NULL}
}
\value{
the points that is shifted by shift amount. or one of the closest point shift to shiftto location.
}
\description{
shift function
}
\examples{
shift(matrix(c(0,0,1),nrow = 1),c(1,1,1)) # return matrix(1,1,2) that every point in matrix shift (1,1,1) direction
}
