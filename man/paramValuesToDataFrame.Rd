% Please edit documentation in R/morphr.R
\name{paramValuesToDataFrame}
\alias{paramValuesToDataFrame}
\title{Create a Fixed Dimension data.frame from Variable Length Lists of Morphological Field Parameter Values}
\usage{
paramValuesToDataFrame(param_values)
}
\description{
A morphological field can consist of parameters with variable number of
configurations. However, a DataTable accepts only a data.frame, which must
have the same number of rows in each column. Therefore, this function takes
a list of lists (each list representing one list of parameter configurations,
i.e. one column) and fills the empty cells with empty character strings to
create a fixed dimension data.frame.
}
