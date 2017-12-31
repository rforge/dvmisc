\name{headtail}
\alias{headtail}
\title{
Return the First and Last Part of an Object
}
\description{
Simply \code{\link{head}} and \code{\link{tail}} combined into one function.
}
\usage{
headtail(x, ...)
}
\arguments{
  \item{x}{
Input object.
}
  \item{...}{
Additional arguments to pass to \code{\link{head}} and \code{\link{tail}}
functions.
}
}
\details{
NA
}
\value{
Same class as \code{x}.
}
\note{
NA
}
\references{
Acknowledgment: This material is based upon work supported by the National
Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
\code{\link{inside}}
}
\examples{
# Generate data from N(0, 1), sort, and look at smallest and largest 3 values
x <- rnorm(1000)
x.sorted <- sort(x)
headtail(x.sorted, 3)
}
