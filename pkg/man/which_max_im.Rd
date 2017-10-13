\name{which_max_im}
\alias{which_max_im}
\title{
Return (Row, Column) Index of (First) Maximum of an Integer Matrix
}
\description{
Written in C++, this function tends to run much faster than the equivalent (if 
maximum is unique) base R solution \code{which(x == max(x), arr.ind = TRUE)}.
}
\usage{
which_max_im(x)
}
\arguments{
  \item{x}{
Integer matrix.
}
}
\details{
NA
}
\value{
Integer vector.
}
\note{
For optimal speed, choose the version of this function that matches the class of 
your \code{x}:

\code{\link{which_max_nv}} for numeric vector. \cr
\code{\link{which_max_iv}} for integer vector. \cr
\code{\link{which_max_nm}} for numeric matrix. \cr
\code{\link{which_max_im}} for integer matrix.
}
\references{
Acknowledgment: This material is based upon work supported by the National
Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
NA
}
\examples{
# which_max_im is typically much faster than which(x == max(x), arr.ind = TRUE)
x <- matrix(rpois(100, lambda = 15), ncol = 10)
all(which(x == max(x), arr.ind = TRUE) == which_max_im(x))
microbenchmark(which(x == max(x), arr.ind = TRUE),
               which_max_im(x),
               times = 100)
}
