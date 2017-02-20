% Please edit documentation in R/morphr-shiny.R
\name{installMorphField}
\alias{installMorphField}
\alias{morphFieldOutput}
\alias{renderMorphField}
\title{Helper functions for using morphr in Shiny}
\usage{
installMorphField(input, output, session, id, param_values, ccm = NULL, 
    specific_configurations = NULL, styleFunc = NULL)

morphFieldOutput(outputId, width = "100\%", height = "auto")

renderMorphField(expr, server = TRUE, env = parent.frame(), quoted = FALSE, 
    ...)
}
\arguments{
\item{id}{The unique ID string that the morphological field will have in the
HTML document. This ID can be used with functions like
\code{\link{dataTableProxy}}() to programmatically alter the field/table.}

\item{param_values}{A named list of vectors/lists. The names of the list are
the names of the parameters (columns) in the morphological field. The
vectors/lists contain the possible values that the parameter can have.}

\item{ccm}{Optional. The cross-consistency matrix (CCM) for the morphological
field can be given to constrain the possible configurations of the field.
If provided, the \code{specific_configurations} are ignored. See details.}

\item{specific_configurations}{Optional. The specific configurations are a
list of only the valid parameter configrations as alternative to the CCM.
If the \code{ccm} is also given, \code{specific_configurations} are
ignored. See details.}

\item{outputId}{output variable to read the value from}

\item{width}{the width of the table container}

\item{height}{the height of the table container}

\item{expr}{An expression that returns an R object that can be used as an
argument to \code{cat}.}

\item{server}{whether to use server-side processing. If \code{TRUE}, then the
data is kept on the server and the browser requests a page at a time; if
\code{FALSE}, then the entire data frame is sent to the browser at once.
Highly recommended for medium to large data frames, which can cause
browsers to slow down or crash.}

\item{env}{The environment in which to evaluate \code{expr}.}

\item{quoted}{Is \code{expr} a quoted expression (with \code{quote()})? This
is useful if you want to save an expression in a variable.}

\item{...}{ignored when \code{expr} returns a table widget, and passed as
additional arguments to \code{datatable()} when \code{expr} returns a data
object}
}
\description{
If you want to use the built-in functionality, you must use the function
\code{installMorphField} instead of \code{output$mf <- renderMorphField(...)}.
This is because it makes use of shiny's reactivity, so it needs to install
the morph field widget in the output object and observe the input object.
Besides that, there are the two usual functions \code{fooOutput()} and
\code{renderFoo()} typical for the \pkg{shiny} package. The former is used to
create a container for the morphological field, and the latter is used in the
server logic to render the field.
}
