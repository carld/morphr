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
#' @export
installMorphField <- function(input, output, session, id,
                              param_values, specific_configurations = NULL,
                              styleFunc = NULL) {
  param_values <- paramValuesToDataFrame(param_values)

  field <- morphfield(param_values, specific_configurations)
  if (!is.null(styleFunc)) {
    field <- styleFunc(field)
  }
  output[[id]] <- renderMorphField(
    field
  )

  proxy <- dataTableProxy(id)

  # immediately deselect empty cells, they shall not be selectable
  observeEvent(input[[paste0(id, "_cells_selected")]], {
    sel_cells <- input[[paste0(id, "_cells_selected")]]
    last_selected_cell <- sel_cells[nrow(sel_cells),]
    row <- last_selected_cell[1]
    col <- last_selected_cell[2]
    cell_content <- param_values[row, col + 1]
    if (is.null(cell_content) || cell_content == "") {
      proxy %>% selectCells(sel_cells[-nrow(sel_cells),])
    }
  })

  if (!is.null(specific_configurations)) {
    # enforce the specific configurations for automatic cell selection
    observeEvent(input[[paste0(id, "_cells_selected")]], {
      sel_cells <- input[[paste0(id, "_cells_selected")]]
      last_selected_cell <- sel_cells[nrow(sel_cells),]
      row <- last_selected_cell[1]
      col <- last_selected_cell[2]
      if (is.null(row) || is.null(col) ||
          is.na(row) || is.na(col)) return()
      clicked_cell <- input[[paste0(id, "_cell_clicked")]]
      if (is.null(clicked_cell$row) || is.null(clicked_cell$col) ||
          is.na(clicked_cell$row) || is.na(clicked_cell$col)) return()
      # if last clicked cell is last selected cell (if a cell was selected)
      if (clicked_cell$row == row && clicked_cell$col == col) {
        determined_cells <- determineCells(param_values, specific_configurations,
                                           row, col)
        if (!is.null(determined_cells)) {
          proxy %>% selectCells(determined_cells)
        }
      }
    })
  }

  return(proxy)
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
