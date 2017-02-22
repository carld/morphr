% Please edit documentation in R/morphr.R
\name{paramValuesToDataFrame}
\alias{paramValuesToDataFrame}
\title{Create a fixed dimension data.frame from variable length lists of morphological field parameter values}
\usage{
paramValuesToDataFrame(param_values)
}
\arguments{
\item{param_values}{A named list of vectors/lists. The names of the list are
the names of the parameters (columns) in the morphological field. The
vectors/lists contain the possible values that the parameter can have.}
}
\description{
A morphological field can consist of parameters with variable number of
values. However, a DataTable accepts only a data.frame, which must
have the same number of rows in each column. Therefore, this function takes
a list of lists (each list representing one list of parameter configurations,
i.e. one column) and fills the empty cells with empty character strings to
create a fixed dimension data.frame.
}
