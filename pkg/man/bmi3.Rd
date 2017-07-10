\name{bmi3}
\alias{bmi3}
\title{
Convert Continuous BMI Values into 3-Level Factor
}
\description{
Converts a continuous BMI variable into a 3-level factor variable: Normal weight
if [-Inf, 25), Overweight if [25, 30), and Obese if [30, Inf).
}
\usage{
bmi3(x, labels = TRUE)
}
\arguments{
  \item{x}{
Numeric vector of BMI values.
}
  \item{labels}{
If \code{TRUE}, factor levels are labeled "Normal weight", "Overweight", and
"Obese"; if \code{FALSE}, factor levels are [-Inf, 25), [25, 30), and [30, Inf).
}
}
\details{
NA
}
\value{
Factor variable with 3 levels.
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
\code{\link{bmi4}}
}
\examples{
NA
}
