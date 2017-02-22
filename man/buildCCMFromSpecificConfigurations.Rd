% Please edit documentation in R/morphr.R
\name{buildCCMFromSpecificConfigurations}
\alias{buildCCMFromSpecificConfigurations}
\title{Convert specific configurations to CCM}
\usage{
buildCCMFromSpecificConfigurations(param_values, specific_configurations)
}
\arguments{
\item{param_values}{A named list of vectors/lists. The names of the list are
the names of the parameters (columns) in the morphological field. The
vectors/lists contain the possible values that the parameter can have.}

\item{specific_configurations}{Optional. The specific configurations are a
list of only the valid parameter configrations as alternative to the CCM.
If the \code{ccm} is also given, \code{specific_configurations} are
ignored. See details.}
}
\description{
If you specify your morphological field with \code{specific_configurations},
then they can be converted to a CCM (cross-consistency matrix) using this
function.
}
