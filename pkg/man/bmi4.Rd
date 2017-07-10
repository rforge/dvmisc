\name{bmi4}
\alias{bmi4}
\title{
Convert Continuous BMI Values into 4-Level Factor
}
\description{
Converts a continuous BMI variable into a 4-level factor variable: Underweight
if [-Inf, 18.5), Normal weight if [18.5, 25), Overweight if [25, 30), and Obese
if [30, Inf).
}
\usage{
bmi4(x, labels = TRUE)
}
\arguments{
  \item{x}{
Numeric vector of BMI values.
}
  \item{labels}{
If \code{TRUE}, factor levels are labeled "Underweight", "Normal weight",
"Overweight", and "Obese"; if \code{FALSE}, factor levels are [-Inf, 18.5),
[18.5, 25), [25, 30), and [30, Inf).
}
}
\details{
NA
}
\value{
Factor variable with 4 levels.
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
\code{\link{bmi3}}
}
\examples{
NA
}
