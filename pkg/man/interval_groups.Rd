\name{interval.groups}
\alias{interval.groups}
\title{
Split Continuous Variable into Equal-Width Groups
}
\description{
Splits a continuous variable into equal-width groups. Useful for assessing
linearity in regression models.
}
\usage{
interval.groups(x, groups = 5, ...)
}
\arguments{
  \item{x}{
Numeric vector.
}
  \item{groups}{
Numeric value indicating how many groups should be created.
}
  \item{...}{
Further arguments to pass to \code{\link{cut}}.
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
\code{\link{cut}}, \code{\link{quant.groups}}
}
\examples{
# Convert values from N(0, 1) into 6 equal-width groups
x <- rnorm(1000)
groups <- interval.groups(x, 6)
table(groups)

# Use interval.groups to detect non-linearity
set.seed(123)
x <- rnorm(1000)
y <- 1.5 + 1.25 * x + 0.25 * x^2 + rnorm(1000)
plot(tapply(y, interval.groups(x), mean))
}
