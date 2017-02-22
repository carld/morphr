% Please edit documentation in R/morphr.R
\name{initializeCCM}
\alias{initializeCCM}
\title{Create a CCM from parameter values with a single default value}
\usage{
initializeCCM(param_values, def_val = TRUE)
}
\arguments{
\item{param_values}{A named list of vectors/lists. The names of the list are
the names of the parameters (columns) in the morphological field. The
vectors/lists contain the possible values that the parameter can have.}

\item{def_val}{Logical for the initialization of all CCM entries.}
}
\description{
The CCM (cross-consistency matrix) from this function will have the same
logical value \code{def_val} for all value combinations. Use this to create
either a completely unconstrained CCM (all \code{TRUE}), or a completely
impossible CCM (all \code{FALSE}).
}
