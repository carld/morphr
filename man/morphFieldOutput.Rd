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
\code{\link{installMorphField}()} instead of \code{output$mf <-
renderMorphField(...)}. This is because it makes use of shiny's reactivity,
so it needs to install the morph field widget in the output object and
observe the input object. Besides that, there are the two usual functions
\code{fooOutput()} and \code{renderFoo()} typical for the \pkg{shiny}
package. The former is used to create a container for the morphological
field, and the latter is used in the server logic to render the field.
}
