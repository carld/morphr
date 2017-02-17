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
morphological field. The CCM contains a logical for each pair-wise comparison
of the parameter values of a morphological field, being TRUE if the
combination of the two values is consistent or valid, otherwise FALSE. It is
a symmetric matrix with empty diagonal. This means that the result of
assessing the consistency of value1 in param1 (column1 of the morphological
field) with value2 in param2 is the same as the consistency of value2 in
param2 with value1 in param1 (hence the symmetry). Parameter values are not
cross-checked with other values of the same parameter (hence the empty
diagonal).

The CCM is stored as a list whose names are hash strings of the two compared
parameter/value pairs. One could also store the CCM as a simple matrix, but
this has two drawbacks: (1) Even though it's declare to make a matrix as
symmetric with \code{\link{Matrix::forceSymmetric}()}, this does not appear
to make the matrix \emph{stay} symmetric when it is modified, i.e. updating
the upper/lower triangle does not change the lower/upper triangle
accordingly. One must therefore decide for one half of the matrix and
remember to operate only on that. (2) It is slightly less straight forward to
look up the consistency for a given pair of values. One must first find the
indices corresponding to them. (However, this would be more trivial when the
matrix columns/rows are named.) I find it a bit easier to use
\code{\link{buildHashValue}()} on the value pair and use the hash for the
lookup in a named list, where one does not need to remember which value is
column, which is row.
}
