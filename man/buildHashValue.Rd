% Please edit documentation in R/morphr.R
\name{buildHashValue}
\alias{buildHashValue}
\title{Build a single hash value from two parameter/value combinations}
\usage{
buildHashValue(param1, value1, param2, value2)
}
\arguments{
\item{param1}{One of the two morphological field parameters to look up the
consistency for. It corresponds to one column in the morphological field.}

\item{value1}{The value within \code{param1} to look up consistency for. It
belongs to one cell in the morphological field.}

\item{param2}{Like \code{param1}, but for the other side of the comparison.}

\item{value2}{Like \code{value1}, but for the other side of the comparison.}
}
\value{
A single character string, which is the sorted hash of the value
  combination for lookup in the CCM.
}
\description{
This function simply combines four strings (with two strings forming a pair
consisting of a parameter and a value) into a single one. It does so in a
way that the order of the parameter-value combinations does not matter.
Switching param1/value1 pair and the param2/value2 pair will result in the
same hash character string.
}
\details{
The purpose of this function is to provide hash keys that can be used to
index a named list representing the CCM (cross-consistency matrix) of a
morphological field. See \code{\link{morphfield}()} for information on the
CCM and in what format it is expected by \code{\link{morphfield}()}.
}
