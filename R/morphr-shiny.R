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
#' Note: If you plan to replace/update an existing field with another field that
#' has the same ID as the old one, then you should not use
#' \code{installMorphField}. Instead, use \code{\link{placeMorphFieldUI}} every
#' time you update the field, and run \code{\link{reactivateMorphField}} only
#' once on the ID of the field.
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
#' @param value_descriptions Optional. Each parameter value in param_values can
#'   have an accompanying (long) description that will be shown as tooltip/popover.
#'   The structure of \code{value_descriptions} is like that of \code{param_values},
#'   but instead of containing parameter values as list elements, the parameter
#'   values must be names whose list elements are the description texts.
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
#' @param editable Logical, if TRUE, a button is shown to enter the field edit
#'   mode.
#' @return The proxy object to the morphological field as returned by
#'   \code{\link{dataTableProxy}()}.
#' @export
installMorphField <- function(input, output, id,
                              param_values, value_descriptions = NULL,
                              ccm = NULL, specific_configurations = NULL,
                              styleFunc = NULL,
                              editable = FALSE, edit_mode = FALSE) {
  field_df <- placeMorphFieldUI(output, id, param_values, value_descriptions,
                                specific_configurations, styleFunc,
                                editable, edit_mode)
  proxy <- reactivateMorphField(input, output, id,
                                param_values = function() {param_values},
                                value_descriptions = function() {value_descriptions},
                                ccm = function() {ccm},
                                specific_configurations = function() {specific_configurations},
                                field_df = function() {field_df},
                                styleFunc = styleFunc, editable = editable)
  return(proxy)
}


#' Place a morphological field widget into Shiny UI
#'
#' This function only puts the widget into the Shiny UI, i.e. the \code{output}
#' object, but does not reactivate it. \code{\link{reactivateMorphField}} can be
#' called afterwards for the reactivation. Alternatively, use
#' \code{\link{installMorphfield}} to do both at the same time. Use this function
#' with \code{\link{reactivateMorphField}} to avoid multiple reactivation.
#' See \code{\link{reactivateMorphField}} for details.
#'
#' @inheritParams installMorphField
#' @export
placeMorphFieldUI <- function(output, id, param_values,
                              value_descriptions = NULL,
                              specific_configurations = NULL,
                              styleFunc = NULL, editable = FALSE,
                              edit_mode = FALSE) {
  field_df <- placeMorphFieldUIWithoutToolbar(
    output, id, param_values, value_descriptions, specific_configurations,
    styleFunc, edit_mode
  )
  if (editable) {
    placeMorphFieldUIToolbar(id, edit_mode)
    placeEditButtonRow(id, edit_mode)
  }
  field_df
}

placeMorphFieldUIWithoutToolbar <- function(output, id, param_values,
                                            value_descriptions = NULL,
                                            specific_configurations = NULL,
                                            styleFunc = NULL, edit_mode = FALSE) {
  l <- morphfield(param_values, value_descriptions, specific_configurations,
                  edit_mode, id)
  field <- l$field
  field_df <- l$field_df
  if (!is.null(styleFunc)) {
    field <- styleFunc(field)
  }
  output[[id]] <- renderMorphField(field)
  field_df
}

placeEditButtonRow <- function(id, edit_mode = FALSE) {
  if (edit_mode) {
    insertUI(
      selector = paste0("#", id),
      where = "beforeBegin",
      ui = fluidRow(
        id = paste0(id, "_edit_btn_row"),
        column(
          12,
          actionButton(paste0(id, "_rem_item_btn"), "Remove Selected Item",
                       class = "disabled", disabled = ""),
          actionButton(paste0(id, "_mod_item_btn"), "Modify Selected Item",
                       class = "disabled", disabled = ""),
          div(
            class = "pull-right",
            actionButton(paste0(id, "_add_col_btn"), "Add Column"),
            actionButton(paste0(id, "_rem_col_btn"), "Remove Column"),
            actionButton(paste0(id, "_mod_col_btn"), "Modify Column")
          )
        )
      )
    )
  } else {
    removeUI(selector = paste0("#", id, "_edit_btn_row"))
  }
}

placeMorphFieldUIToolbar <- function(id, edit_mode) {
  insertUI(
    selector = paste0("#", id),
    where = "beforeBegin",
    ui = tagList(
      shinyjs::useShinyjs(),
      actionButton(paste0(id, "_edit_btn"), "", icon = icon("edit"),
                   style = "font-size: 10px;"),
      tags$head(shinyBS::bsTooltip(paste0(id, "_edit_btn"), "Edit",
                                   placement = "right")),
      # Insert JS event handlers for buttons in table:
      tags$head(tags$script(HTML(sprintf(
        "
        $(document).on('click', '.add-item-btn-%s', function () {
          Shiny.onInputChange('%s_add_item_btn', this.id + ' ' + Math.random());
        });
        ", id, id
      )))),

      div(
        style = "display: none;",
        checkboxInput(paste0(id, "_edit_mode"), "Edit Mode", value = edit_mode)
      )
    )
  )
}


#' Make a morphological field reactive
#'
#' If you want interactive features, i.e. that you can click cells to specify
#' a certain configuration and see the values compatible with that configuration,
#' then you need to run this function. It installs shiny observers that provide
#' the interactivity. This function is called internally by
#' \code{\link{installMorphField}}.
#'
#' If you plan to update/replace the same field several times (while keeping the
#' ID the same), then you should not use \code{\link{installMorphField}}, but
#' instead use \code{\link{placeMorphFieldUI}} to update the field. This function
#' should be run only once, while \code{\link{placeMorphFieldUI}} can be run many
#' times to update the field. If \code{reactivateMorphField} runs many times (or
#' \code{\link{installMorphField}} runs many times) together with the field
#' update, then there will be bugs introduced because the old observers will
#' remain in place using outdated data. Multiple observes will run, one for each
#' representation of the field (from the first one to the current one), while
#' only one should run for the current field.
#'
#' The single execution of \code{reactivateMorphField} must have access to the
#' data attributed with the current field. Because this data may only become
#' available while the updated field is installed with
#' \code{\link{placeMorphFieldUI}}, one method to achieve this is to use
#' \code{\link{reactive}} expressions for each of the data returning functions
#' in the arguments \code{param_values}, \code{ccm},
#' \code{specific_configurations}, and \code{field_df}. These reactive
#' expressions are then used both in the field update code with
#' \code{\link{placeMorphFieldUI}} and given to \code{reactivateMorphField}.
#'
#' @inheritParams installMorphField
#' @param param_values A function returning the \code{param_values}, see
#'   \code{\link{installMorphField}}.
#' @param param_values A function returning the \code{value_descriptions}, see
#'   \code{\link{installMorphField}}.
#' @param ccm A function returning the \code{ccm}, see
#'   \code{\link{installMorphField}}.
#' @param specific_configurations A function returning the
#'   \code{specific_configurations}, see \code{\link{installMorphField}}.
#' @param field_df A function returning the \code{field_df}, as returned by
#'   \code{\link{paramValuesToDataFrame}}. Can be provided if it already exists
#'   anyway to save some time. If not provided, \code{field_df} is calculated
#'   by applying \code{\link{paramValuesToDataFrame}} on \code{param_values}.
#' @return The proxy object to the morphological field as returned by
#'   \code{\link{dataTableProxy}()}.
#' @export
reactivateMorphField <- function(input, output, id, param_values,
                                 value_descriptions = function() {NULL},
                                 ccm = function() {NULL},
                                 specific_configurations = function() {NULL},
                                 field_df = function() {NULL},
                                 styleFunc = NULL, editable = FALSE) {
  proxy <- reactivateMorphFieldWithoutToolbar(
    input, id, param_values, ccm, specific_configurations, field_df
  )

  if (editable) {
    reactivateMorphFieldToolbar(input, output, id, param_values,
                                value_descriptions, ccm, specific_configurations,
                                field_df, styleFunc)
  }

  return(proxy)
}

reactivateMorphFieldWithoutToolbar <- function(input, id, param_values,
                                              ccm = function() {NULL},
                                              specific_configurations = function() {NULL},
                                              field_df = function() {NULL}) {
  proxy <- dataTableProxy(id)

  # Immediately deselect empty cells, they shall not be selectable
  observeEvent(input[[paste0(id, "_cells_selected")]], {
    sel_cells <- input[[paste0(id, "_cells_selected")]]
    # print("selected cells:")
    # print(sel_cells)
    field_df <- field_df()
    if (is.null(field_df)) {
      field_df <- paramValuesToDataFrame(param_values())
    }
    if (isLastSelectedCellEmpty(sel_cells, field_df)) {
      # print("Deselecting empty cell:")
      # print(getLastSelectedCell(sel_cells))
      proxy %>% selectCells(removeLastSelectedCell(sel_cells))
    }
  })

  # Update the field with the new consistent cells after selection
  observeEvent(input[[paste0(id, "_cells_selected")]], {
    sel_cells <- input[[paste0(id, "_cells_selected")]]
    if (input[[paste0(id, "_edit_mode")]]) {
      if (nrow(sel_cells) == 1) {
        shinyjs::enable(selector = paste0("#", id, "_rem_item_btn"))
        shinyjs::enable(selector = paste0("#", id, "_mod_item_btn"))
      } else {
        shinyjs::disable(selector = paste0("#", id, "_rem_item_btn"))
        shinyjs::disable(selector = paste0("#", id, "_mod_item_btn"))
      }
      return() # do nothing else
    }
    field_df <- field_df()
    if (is.null(field_df)) {
      field_df <- paramValuesToDataFrame(param_values())
    }
    if (isLastSelectedCellEmpty(sel_cells, field_df)) {
      # print("Last selected cell is empty, ignoring it...")
      sel_cells <- removeLastSelectedCell(sel_cells)
      # print("selected cells:")
      # print(sel_cells)
    }
    ccm <- ccm()
    if (is.null(ccm)) {
      if (!is.null(specific_configurations())) {
        ccm <- buildCCMFromSpecificConfigurations(param_values(),
                                                  specific_configurations())
      } else {
        ccm <- buildUnconstrainedCCM(param_values())
      }
    }
    consistent_cells <- findConsistentCells(param_values(), ccm, sel_cells)
    # print("consistent_cells:")
    # print(consistent_cells)
    proxy %>% setCellsConsistent(consistent_cells)
  })

  return(proxy)
}

installModMorphField <- function(input, output, id, param_values, value_descriptions,
                                 ccm, specific_configurations, styleFunc) {
  orig_id <- sub(pattern = "__modified__[0-9]+$", replacement = "", x = id)
  new_id <- sprintf("%s__modified__%d", orig_id, as.integer(runif(1)*1e9))
  removeUI(selector = sprintf("#%s_edit_btn", id))
  removeUI(selector = sprintf("#%s_edit_btn_row", id))
  removeUI(selector = sprintf("#%s", id))
  insertUI(paste0("#", orig_id, "_container"), ui = morphFieldOutputWithoutContainer(new_id))
  installMorphField(input, output, new_id, param_values, value_descriptions,
                    ccm, specific_configurations, styleFunc,
                    editable = TRUE, edit_mode = TRUE)
}

reactivateMorphFieldToolbar <- function(input, output, id, param_values,
                                        value_descriptions = function() {NULL},
                                        ccm = function() {NULL},
                                        specific_configurations = function() {NULL},
                                        field_df = function() {NULL},
                                        styleFunc = NULL) {
  observeEvent(input[[paste0(id, "_edit_btn")]], {
    prev_mode <- input[[paste0(id, "_edit_mode")]]
    if (!prev_mode) { # toggle (edit_mode was previously off, turn it on)
      updateCheckboxInput(getDefaultReactiveDomain(), paste0(id, "_edit_mode"), value = TRUE)
      placeMorphFieldUIWithoutToolbar(output, id, param_values(), value_descriptions(),
                                      specific_configurations(), styleFunc,
                                      edit_mode = TRUE)
      placeEditButtonRow(id, edit_mode = TRUE)
    } else {
      updateCheckboxInput(getDefaultReactiveDomain(), paste0(id, "_edit_mode"), value = FALSE)
      placeMorphFieldUIWithoutToolbar(output, id, param_values(), value_descriptions(),
                                      specific_configurations(), styleFunc)
      placeEditButtonRow(id)
    }
  })

  addItemModal <- function(col, failed = FALSE) {
    modalDialog(
      textInput(paste0(id, "_new_item"), "New item",
                placeholder = "Enter item text here..."),
      div(
        textInput(paste0(id, "_new_item_col"), "Column", value = col),
        style = "display: none;"
      ),
      if (failed)
        div(tags$b("Invalid item text.", style = "color: red;")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(paste0(id, "_add_item_ok"), "OK")
      )
    )
  }

  observeEvent(input[[paste0(id, "_add_item_btn")]], {
    col <- as.integer(sub(
      pattern = sprintf("%s_add_item_btn_([0-9]+).*", id),
      replacement = "\\1",
      x = input[[paste0(id, "_add_item_btn")]]
    ))
    showModal(addItemModal(col))
  })

  observeEvent(input[[paste0(id, "_add_item_ok")]], {
    col <- as.integer(input[[paste0(id, "_new_item_col")]])
    new_item <- input[[paste0(id, "_new_item")]]
    if (!is.null(new_item) && nzchar(trimws(new_item))) {
      removeModal()
      param_values <- param_values()
      param_values[[col]] <- c(param_values[[col]], new_item)
      installModMorphField(input, output, id, param_values, value_descriptions(),
                           ccm(), specific_configurations(), styleFunc)
    } else {
      showModal(addItemModal(col, failed = TRUE))
    }
  })

  remItemModal <- function(item, row, col) {
    modalDialog(
      sprintf("Really remove item '%s'?", item),
      div(
        textInput(paste0(id, "_rem_item_row"), "Row", value = row),
        textInput(paste0(id, "_rem_item_col"), "Column", value = col),
        style = "display: none;"
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(paste0(id, "_rem_item_ok"), "OK")
      )
    )
  }

  observeEvent(input[[paste0(id, "_rem_item_btn")]], {
    sel_cells <- input[[paste0(id, "_cells_selected")]]
    row <- sel_cells[1, 1]
    col <- sel_cells[1, 2] + 1
    showModal(remItemModal(param_values()[[col]][row], row, col))
  })

  observeEvent(input[[paste0(id, "_rem_item_ok")]], {
    removeModal()
    row <- as.integer(input[[paste0(id, "_rem_item_row")]])
    col <- as.integer(input[[paste0(id, "_rem_item_col")]])
    param_values <- param_values()
    param_values[[col]] <- param_values[[col]][-row]
    if (length(param_values[[col]]) == 0) {
      # Remove empty column
      param_values <- param_values[-col]
    }
    installModMorphField(input, output, id, param_values, value_descriptions(),
                         ccm(), specific_configurations(), styleFunc)
  })

  modItemModal <- function(row, col, failed = FALSE) {
    modalDialog(
      textInput(paste0(id, "_mod_item"), "Rename item to:",
                placeholder = "Enter new item text here..."),
      div(
        textInput(paste0(id, "_mod_item_row"), "Column", value = row),
        textInput(paste0(id, "_mod_item_col"), "Column", value = col),
        style = "display: none;"
      ),
      if (failed)
        div(tags$b("Invalid item text.", style = "color: red;")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(paste0(id, "_mod_item_ok"), "OK")
      )
    )
  }

  observeEvent(input[[paste0(id, "_mod_item_btn")]], {
    sel_cells <- input[[paste0(id, "_cells_selected")]]
    row <- sel_cells[1, 1]
    col <- sel_cells[1, 2] + 1
    showModal(modItemModal(row, col))
  })

  observeEvent(input[[paste0(id, "_mod_item_ok")]], {
    row <- as.integer(input[[paste0(id, "_mod_item_row")]])
    col <- as.integer(input[[paste0(id, "_mod_item_col")]])
    mod_item <- input[[paste0(id, "_mod_item")]]
    if (!is.null(mod_item) && nzchar(trimws(mod_item))) {
      removeModal()
      param_values <- param_values()
      param_values[[col]][row] <- mod_item
      installModMorphField(input, output, id, param_values, value_descriptions(),
                           ccm(), specific_configurations(), styleFunc)
    } else {
      showModal(modItemModal(row, col, failed = TRUE))
    }
  })

  addColModal <- function(title_failed = FALSE, item_failed = FALSE) {
    modalDialog(
      textInput(paste0(id, "_new_col"), "New column title",
                placeholder = "Enter column title here..."),
      textInput(paste0(id, "_new_first_item"), "First item",
                placeholder = "Enter item text here..."),
      if (title_failed)
        div(tags$b("Invalid column title.", style = "color: red;")),
      if (item_failed)
        div(tags$b("Invalid item text.", style = "color: red;")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(paste0(id, "_add_col_ok"), "OK")
      )
    )
  }

  observeEvent(input[[paste0(id, "_add_col_btn")]], {
    showModal(addColModal())
  })

  observeEvent(input[[paste0(id, "_add_col_ok")]], {
    new_col <- input[[paste0(id, "_new_col")]]
    new_first_item <- input[[paste0(id, "_new_first_item")]]
    if (!is.null(new_col) && nzchar(trimws(new_col))) {
      if (!is.null(new_first_item) && nzchar(trimws(new_first_item))) {
        removeModal()
        new_pv <- list(c(new_first_item))
        names(new_pv) <- new_col
        param_values <- param_values()
        param_values <- c(param_values, new_pv)
        installModMorphField(input, output, id, param_values, value_descriptions(),
                             ccm(), specific_configurations(), styleFunc)
      } else {
        showModal(addColModal(item_failed = TRUE))
      }
    } else {
      showModal(addColModal(title_failed = TRUE))
    }
  })

  remColModal <- function() {
    modalDialog(
      selectizeInput(paste0(id, "_rem_col"), "Select column to remove",
                     choices = names(param_values())),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(paste0(id, "_rem_col_ok"), "Remove it!")
      )
    )
  }

  observeEvent(input[[paste0(id, "_rem_col_btn")]], {
    showModal(remColModal())
  })

  observeEvent(input[[paste0(id, "_rem_col_ok")]], {
    removeModal()
    rem_col <- input[[paste0(id, "_rem_col")]]
    param_values <- param_values()
    col <- which(names(param_values) == rem_col)
    param_values <- param_values[-col]
    installModMorphField(input, output, id, param_values, value_descriptions(),
                         ccm(), specific_configurations(), styleFunc)
  })
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
  ret_val <- is.null(cell_content) || is.na(cell_content) || cell_content == "" ||
    grepl(pattern = "^<button ", x = cell_content)
  return(ret_val)
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
  div(
    id = paste0(outputId, "_container"),
    morphFieldOutputWithoutContainer(outputId, width = '100%', height = 'auto')
  )
}

morphFieldOutputWithoutContainer <- function(outputId, width = '100%', height = 'auto') {
  htmltools::attachDependencies(
    htmlwidgets::shinyWidgetOutput(
      outputId, 'datatables', width, height, package = 'morphr'
    ),
    c(
      crosstalk::crosstalkLibs(),
      list(
        htmltools::htmlDependency(
          "morphr", "0.0.1",
          c(file = system.file("htmlwidgets", package = "morphr")),
          stylesheet = "css/morphr.css", script = "morphr.js"
        ),
        htmltools::htmlDependency(
          "shinyBS", packageVersion("shinyBS"),
          c(file = system.file("www", package = "shinyBS")),
          stylesheet = "shinyBS.css", script = "shinyBS.js"
        )
      )
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
