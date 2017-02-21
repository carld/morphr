#' Helper functions for using morphr in Shiny
#'
#' If you want to use the built-in functionality, you must use the function
#' \code{installMorphField} instead of \code{output$mf <- renderMorphField(...)}.
#' This is because it makes use of shiny's reactivity, so it needs to install
#' the morph field widget in the output object and observe the input object.
#' Besides that, there are the two usual functions \code{fooOutput()} and
#' \code{renderFoo()} typical for the \pkg{shiny} package. The former is used to
#' create a container for the morphological field, and the latter is used in the
#' server logic to render the field.
#' @inheritParams shiny::shinyServer
#' @param id The unique ID string that the morphological field will have in the
#'   HTML document. This ID can be used with functions like
#'   \code{\link{dataTableProxy}}() to programmatically alter the field/table.
#' @inheritParams morphfield
#' @export
installMorphField <- function(input, output, session, id,
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


#' @rdname installMorphField
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


#' @rdname installMorphField
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
#' @export
setCellsConsistent = function(proxy, consistent) {
  invokeRemote(proxy, 'setCellsConsistent', list(consistent))
}
