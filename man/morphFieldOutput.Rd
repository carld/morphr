% Please edit documentation in R/morphr-shiny.R
\name{morphFieldOutput}
\alias{morphFieldOutput}
\alias{renderMorphField}
\title{Helper functions for using morphr in Shiny}
\usage{
morphFieldOutput(outputId, width = "100\%", height = "auto")

renderMorphField(expr, server = TRUE, env = parent.frame(), quoted = FALSE, ...)
}
\arguments{
\item{width}{the width of the table container}

\item{height}{the height of the table container}

\item{expr}{an expression to create a table widget (normally via
\code{\link{datatable}()}), or a data object to be passed to
\code{datatable()} to create a table widget}

\item{server}{whether to use server-side processing. If \code{TRUE}, then the
data is kept on the server and the browser requests a page at a time; if
\code{FALSE}, then the entire data frame is sent to the browser at once.
Highly recommended for medium to large data frames, which can cause
browsers to slow down or crash.}

\item{...}{ignored when \code{expr} returns a table widget, and passed as
additional arguments to \code{datatable()} when \code{expr} returns a data
object}
}
\description{
These two functions are like most \code{fooOutput()} and \code{renderFoo()}
functions in the \pkg{shiny} package. The former is used to create a
container for the morphological field, and the latter is used in the server
logic to render the field.
}
