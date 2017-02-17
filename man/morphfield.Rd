% Please edit documentation in R/morphr.R
\name{morphfield}
\alias{morphfield}
\title{Create an HTML morphological field widget using the DataTables library}
\usage{
morphfield(param_values, specific_configurations = NULL)
}
\arguments{
\item{param_values}{A named list of vectors/lists. The names of the list are
the names of the parameters (columns) in the morphological field. The
vectors/lists contain the possible values that the parameter can have.}

\item{specific_configurations}{Optional. One can define one or more parameter
columns to \emph{specify} the field. This means that choosing a value in
the specifying column sets the values of some or all other columns. It
constrains the field. The \code{specific_configurations} are expected to be
a named list of named lists of named lists. The top-level hierarchy names
represent the parameter names that are specifying. The next level names
represent the parameter's values that are specifying. The deepest level
names represent the parameter names that are specified and the values
(list elements) are the parameter value(s) that are specified (i.e.
considered possible) in this configuration. Defaults to NULL, i.e. no
specification and the field is \emph{open}.}
}
\description{
This function creates an HTML widget to display a morphological field (a kind
of table where each column represents the possible configurations of a
certain parameter) using the JavaScript library DataTables.
}
