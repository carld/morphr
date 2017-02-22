#' Install a morphological field widget in Shiny
#'
#' If you want to use the built-in functionality, you must use the function
#' \code{installMorphField} instead of \code{output$mf <-
#' \link{renderMorphField}(...)}. This is because it makes use of shiny's
#' reactivity, so it needs to install the morph field widget in the output
#' object and observe the input object.
#'
#' @section Details:
#'
#' The \code{param_values} contain the column-wise elements of the morphological
#' field in form of a named list. The names are the column headers (names of the
#' parameters of the field). The values are vectors/lists with the character
#' strings that represent the possible values that the parameter can take on.
#'
#' There are two ways to constrain the possible configurations, i.e. parameter
#' value combinations, of the morphological field. Number one is to provide a
#' \emph{cross-consistency matrix (CCM)}, which lists the validity status of all
#' pair-wise parameter combinations (see below for details). Number two is to
#' provide a list of \emph{specific configurations}, which instead of listing
#' all value combinations lists only the valid configurations. Internally, the
#' specific configurations are converted to a CCM as well.
#'
#' @section Cross-consistency matrix (CCM):
#'
#' The \emph{CCM} contains a logical for each pair-wise comparison of the
#' parameter values of a morphological field, being TRUE if the combination of
#' the two values is consistent or valid, otherwise FALSE. It is a symmetric
#' matrix with empty diagonal. This means that the result of assessing the
#' consistency of value1 in param1 (column1 of the morphological field) with
#' value2 in param2 is the same as the consistency of value2 in param2 with
#' value1 in param1 (hence the symmetry). Parameter values are not cross-checked
#' with other values of the same parameter (hence the empty diagonal).
#'
#' The CCM is stored as a list whose names are hash strings of the two compared
#' parameter/value pairs. Those hash strings can be created with the function
#' \code{\link{buildHashValue}()}. One could also store the CCM as a simple
#' matrix, but this has two drawbacks: (1) Even when a matrix is devlared to be
#' symmetric with \code{\link{Matrix::forceSymmetric}()}, this does not appear
#' to make the matrix \emph{stay} symmetric when it is modified, i.e. updating
#' the upper/lower triangle does not change the lower/upper triangle
#' accordingly. One must therefore decide for one half of the matrix and
#' remember to operate only on that. (2) It is slightly less straight forward to
#' look up the consistency for a given pair of values. One must first find the
#' indices corresponding to them. (However, this would be more trivial when the
#' matrix columns/rows are named.) I find it a bit easier to use
#' \code{\link{buildHashValue}()} on the value pair and use the hash for the
#' lookup in a named list, where one does not need to remember which value is
#' column, which is row.
#'
#' @section Specific configurations:
#'
#' With \emph{specific configurations}, one can define one or more parameter
#' columns to \emph{specify} the field. This means that choosing a value in the
#' specifying column sets the values of some or all other columns. It constrains
#' the field. Specifying columns are marked with a darker grey than normal
#' columns. The \code{specific_configurations} are expected to be a named list
#' of named lists of named lists. The top-level hierarchy names represent the
#' parameter names that are specifying. The next level names represent the
#' parameter's values that are specifying. The deepest level names represent the
#' parameter names that are specified and the values (list elements) are the
#' parameter value(s) that are specified (i.e. considered possible) in this
#' configuration. Defaults to NULL, i.e. no specification and the field is
#' \emph{open}.
#'
#' @param input The Shiny session's input object (the same as is passed into the
#'   Shiny server function as an argument).
#' @param output The Shiny session's output object (the same as is passed into
#'   the Shiny server function as an argument).
#' @param id The unique ID string that the morphological field will have in the
#'   HTML document. This ID can be used with functions like
#'   \code{\link{dataTableProxy}}() to programmatically alter the field/table.
#' @param param_values A named list of vectors/lists. The names of the list are
#'   the names of the parameters (columns) in the morphological field. The
#'   vectors/lists contain the possible values that the parameter can have.
#' @param ccm Optional. The cross-consistency matrix (CCM) for the morphological
#'   field can be given to constrain the possible configurations of the field.
#'   If provided, the \code{specific_configurations} are ignored. See details.
#' @param specific_configurations Optional. The specific configurations are a
#'   list of only the valid parameter configrations as alternative to the CCM.
#'   If the \code{ccm} is also given, \code{specific_configurations} are
#'   ignored. See details.
#' @param styleFunc Optional function to style the field. It should accept the field
#'   returned by \code{\link{morphfield}()} (actually a datatable as returned by
#'   \code{\link{datatable}()}), modify it with \code{
#'   field <- field \%>\% \link{formatStyle}(...)
#'   }
#'   or similar, and then return it.
#' @return The proxy object to the morphological field as returned by
#'   \code{\link{dataTableProxy}()}.
#' @export
installMorphField <- function(input, output, id,
                              param_values, ccm = NULL,
                              specific_configurations = NULL,
                              styleFunc = NULL) {
  if (is.null(ccm)) {
    if (!is.null(specific_configurations)) {
      ccm <- buildCCMFromSpecificConfigurations(param_values, specific_configurations)
    } else {
      ccm <- buildUnconstrainedCCM(param_values)
    }
  }

  field <- morphfield(param_values, ccm, specific_configurations)
  if (!is.null(styleFunc)) {
    field <- styleFunc(field)
  }
  output[[id]] <- renderMorphField(
    field
  )

  proxy <- dataTableProxy(id)
  field_df <- paramValuesToDataFrame(param_values)

  # Immediately deselect empty cells, they shall not be selectable
  observeEvent(input[[paste0(id, "_cells_selected")]], {
    sel_cells <- input[[paste0(id, "_cells_selected")]]
    # print("selected cells:")
    # print(sel_cells)
    if (isLastSelectedCellEmpty(sel_cells, field_df)) {
      # print("Deselecting empty cell:")
      # print(getLastSelectedCell(sel_cells))
      proxy %>% selectCells(removeLastSelectedCell(sel_cells))
    }
  })

  # Update the field with the new consistent cells after selection
  observeEvent(input[[paste0(id, "_cells_selected")]], {
    sel_cells <- input[[paste0(id, "_cells_selected")]]
    ## clicked_cell <- input[[paste0(id, "_cell_clicked")]]
    ## # if last clicked cell is last selected cell (if a cell was selected)
    ## if (clicked_cell$row == row && clicked_cell$col == col) {
    # print("selected cells:")
    # print(sel_cells)
    # print("last_selected_cell:")
    # print(getLastSelectedCell(sel_cells))
    if (isLastSelectedCellEmpty(sel_cells, field_df)) {
      # print("Last selected cell is empty, ignoring it...")
      sel_cells <- removeLastSelectedCell(sel_cells)
      # print("selected cells:")
      # print(sel_cells)
    }
    consistent_cells <- findConsistentCells(param_values, ccm, sel_cells)
    # print("consistent_cells:")
    # print(consistent_cells)
    proxy %>% setCellsConsistent(consistent_cells)
  })

  return(proxy)
}


getLastSelectedCell <- function(sel_cells) {
  matrix(sel_cells[nrow(sel_cells), ], ncol = 2)
}


isLastSelectedCellEmpty <- function(sel_cells, field_df) {
  if (nrow(sel_cells) == 0) return(FALSE)
  last_selected_cell <- getLastSelectedCell(sel_cells)
  row <- last_selected_cell[1]
  col <- last_selected_cell[2]
  cell_content <- field_df[row, col + 1]
  return(is.null(cell_content) || is.na(cell_content) || cell_content == "")
}


removeLastSelectedCell <- function(sel_cells) {
  matrix(sel_cells[-nrow(sel_cells), ], ncol = 2)
}

#' Helper functions for using morphr in Shiny
#'
#' If you want to use the built-in functionality, you must use the function
#' \code{\link{installMorphField}()} instead of \code{output$mf <-
#' renderMorphField(...)}. This is because it makes use of shiny's reactivity,
#' so it needs to install the morph field widget in the output object and
#' observe the input object. Besides that, there are the two usual functions
#' \code{fooOutput()} and \code{renderFoo()} typical for the \pkg{shiny}
#' package. The former is used to create a container for the morphological
#' field, and the latter is used in the server logic to render the field.
#'
#' @inheritParams shiny::textOutput
#' @export
morphFieldOutput <- function(outputId, width = '100%', height = 'auto') {
  htmltools::attachDependencies(
    htmlwidgets::shinyWidgetOutput(
      outputId, 'datatables', width, height, package = 'morphr'
    ),
    c(
      crosstalk::crosstalkLibs(),
      list(htmltools::htmlDependency(
        "morphr-css", "0.0.1",
        c(file = system.file("htmlwidgets", package = "morphr")),
        stylesheet = "css/morphr.css"
      ))
    ),
    append = TRUE
  )
}


#' @rdname morphFieldOutput
#' @inheritParams shiny::renderText
#' @inheritParams renderDataTable
#' @export
renderMorphField <- function(expr, server = TRUE, env = parent.frame(),
                             quoted = FALSE, ...) {
  # if (!quoted) expr = substitute(expr)
  # shinyRenderWidget(expr, sigmaOutput, env, quoted = TRUE)
  # shiny::markRenderFunction(morphFieldOutput, function(shinysession, name, ...) {
  #   print(names(shinysession))
  #   print(names(shinysession$output))
  #   print("Hallo Welt!")
  # })
  renderDataTable(expr = expr, server = server, quoted = quoted, ...) # must not give the default env (parent.frame()) to renderDataTable, or the expr will not be found in scope
}


#' @rdname proxy
#' @param consistent a matrix of two columns (row and column indices, respectively) for
#'   cell indices that identify the cells that should be marked consistent, i.e.
#'   appearing in a possible configuration; you may use NULL to clear existing
#'   consistency markings
#' @export
setCellsConsistent = function(proxy, consistent) {
  invokeRemote(proxy, 'setCellsConsistent', list(consistent))
}
