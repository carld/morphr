% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/morphr-shiny.R
\name{reactivateMorphField}
\alias{reactivateMorphField}
\title{Make a morphological field reactive}
\usage{
reactivateMorphField(input, output, id, param_values,
  value_descriptions = function() {     NULL }, placement = function()
  {     NULL }, ccm = function() {     NULL },
  configurations = function() {     NULL }, pre_selections = function()
  {     NULL }, spec_columns = function() {     NULL },
  field_df = function() {     NULL }, styleFunc = NULL,
  editable = FALSE)
}
\arguments{
\item{input}{The Shiny session's input object (the same as is passed into the
Shiny server function as an argument).}

\item{output}{The Shiny session's output object (the same as is passed into
the Shiny server function as an argument).}

\item{id}{The unique ID string that the morphological field will have in the
HTML document. This ID can be used with functions like
\code{\link{dataTableProxy}}() to programmatically alter the field/table.}

\item{param_values}{A function returning the \code{param_values}, see
\code{\link{installMorphField}}.}

\item{value_descriptions}{A function returning the \code{value_descriptions}, see
\code{\link{installMorphField}}.}

\item{placement}{A function returning the \code{placement}, see
\code{\link{installMorphField}}.}

\item{ccm}{A function returning the \code{ccm}, see
\code{\link{installMorphField}}.}

\item{configurations}{A function returning the
\code{configurations}, see \code{\link{installMorphField}}.}

\item{pre_selections}{A function returning the
\code{pre_selections}, see \code{\link{installMorphField}}.}

\item{spec_columns}{Optional. A list of columns that shall be marked with dark
gray color. These can be "specifying" columns, e.g. a summarizing column like
for the selection of a scenario. These columns can then be seen as "input"
columns: By clicking them, one gets a certain "output" response from
the field, e.g. the parameter values that belong to a specific scenario.
The \code{spec_columns} can also be any other "special" columns.}

\item{field_df}{A function returning the \code{field_df}, as returned by
\code{\link{paramValuesToDataFrame}}. Can be provided if it already exists
anyway to save some time. If not provided, \code{field_df} is calculated
by applying \code{\link{paramValuesToDataFrame}} on \code{param_values}.}

\item{styleFunc}{Optional function to style the field. It should accept the field
returned by \code{\link{morphfield}()} (actually a datatable as returned by
\code{\link{datatable}()}), modify it with \code{
field <- field \%>\% \link{formatStyle}(...)
}
or similar, and then return it.}

\item{editable}{Logical, if TRUE, a button is shown to enter the field edit
mode.}
}
\value{
The proxy object to the morphological field as returned by
  \code{\link{dataTableProxy}()}.
}
\description{
If you want interactive features, i.e. that you can click cells to specify
a certain configuration and see the values compatible with that configuration,
then you need to run this function. It installs shiny observers that provide
the interactivity. This function is called internally by
\code{\link{installMorphField}}.
}
\details{
If you plan to update/replace the same field several times (while keeping the
ID the same), then you should not use \code{\link{installMorphField}}, but
instead use \code{\link{placeMorphFieldUI}} to update the field. This function
should be run only once, while \code{\link{placeMorphFieldUI}} can be run many
times to update the field. If \code{reactivateMorphField} runs many times (or
\code{\link{installMorphField}} runs many times) together with the field
update, then there will be bugs introduced because the old observers will
remain in place using outdated data. Multiple observes will run, one for each
representation of the field (from the first one to the current one), while
only one should run for the current field.

The single execution of \code{reactivateMorphField} must have access to the
data attributed with the current field. Because this data may only become
available while the updated field is installed with
\code{\link{placeMorphFieldUI}}, one method to achieve this is to use
\code{\link{reactive}} expressions for each of the data returning functions
in the arguments \code{param_values}, \code{ccm},
\code{configurations}, and \code{field_df}. These reactive
expressions are then used both in the field update code with
\code{\link{placeMorphFieldUI}} and given to \code{reactivateMorphField}.
}
