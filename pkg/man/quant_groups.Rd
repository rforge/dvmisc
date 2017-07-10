\name{quant.groups}
\alias{quant.groups}
\title{
Split Continuous Variable into Quantile Groups
}
\description{
Splits a continuous variable into quantiles groups. Basically combines
\code{\link{quantile}} and \code{\link{cut}} into a single function.
}
\usage{
quant.groups(x, groups = 5, ...)
}
\arguments{
  \item{x}{
Numeric vector.
}
  \item{groups}{
Numeric value indicating how many quantile groups should be created.
}
  \item{...}{
Further arguments to pass to \code{\link{quantile}} or \code{\link{cut}}.
}
}
\details{
NA
}
\value{
Factor variable.
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
\code{\link{quantile}}, \code{\link{cut}}, \code{\link{interval.groups}}
}
\examples{
# Convert values from N(0, 1) into quintiles (i.e. 5 groups)
x <- rnorm(1000)
groups <- quant.groups(x, 5)
table(groups)
}
