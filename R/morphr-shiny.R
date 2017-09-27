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
#' Right now, it is not possible to view or edit the CCM, but it can be
#' specified in code or, alternatively, be computed from configurations.
#'
#' @section Configurations:
#'
#' \emph{Configurations} are collections of parameter values that shall be
#' consistent with each other. With configurations, one can define consistencies,
#' from which the CCM is built. If configurations are specified, then all
#' parameter values are inconsistent with all other parameter values to begin
#' with. Then, the configurations are looped over and all combinations of all
#' values with all other values within each configuration are set to be consistent.
#'
#' The \code{configurations} can be defined in one of two formats:
#'
#' In the old compact format, \code{configurations} are expected to be
#' a named list of named lists of named lists. The top-level hierarchy names
#' represent the parameter names that are specifying. The next level names
#' represent the parameter's values that are specifying. The deepest level names
#' represent the parameter names that are specified and the values (list elements)
#' are the parameter value(s) that are specified (i.e. considered possible) in this
#' configuration.
#'
#' In the new extended format, \code{configurations} are expected to be
#' a list of configurations, where each configuration is a list of items, and
#' each item is a list with names \code{param} and \code{value}, identifying the
#' parameter value.
#'
#' \code{configurations} defaults to NULL: No constraints are applied and
#' the field is \emph{open}, i.e. every value is consistent with all other
#' values.
#'
#' If \code{edit_mode} is \code{TRUE}, the configurations can also be set
#' interactively.
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
#'   If \code{param_values} is NULL (default), then an empty field is created.
#'   Use \code{editable = TRUE} in \code{\link{installMorphField}} to fill the
#'   empty field in the browser.
#' @param value_descriptions Optional. Each parameter value in param_values can
#'   have an accompanying (long) description that will be shown as tooltip/popover.
#'   The structure of \code{value_descriptions} is like that of \code{param_values},
#'   but instead of containing parameter values as list elements, the parameter
#'   values must be names whose list elements are the description texts.
#' @param ccm Optional. The cross-consistency matrix (CCM) for the morphological
#'   field can be given to constrain the possible configurations of the field.
#'   If provided, the \code{configurations} are ignored. See details.
#' @param configurations Optional. The configurations are a list of the valid
#'   parameter configurations as alternative to the CCM. If the \code{ccm}
#'   is also given, \code{configurations} are ignored. See details.
#' @param spec_columns Optional. A list of columns that shall be marked with dark
#'   gray color. These can be "specifying" columns, e.g. a summarizing column like
#'   for the selection of a scenario. These columns can then be seen as "input"
#'   columns: By clicking them, one gets a certain "output" response from
#'   the field, e.g. the parameter values that belong to a specific scenario.
#'   The \code{spec_columns} can also be any other "special" columns.
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
                              param_values = NULL, value_descriptions = NULL,
                              ccm = NULL, configurations = NULL,
                              spec_columns = NULL, styleFunc = NULL,
                              editable = FALSE, edit_mode = FALSE, edit_config_mode = FALSE) {
  proxy <- NULL
  # Server code must be wrapped into any renderXXX() function in order
  # to be executed not directly, but later when the UI is ready.
  # Otherwise, you can get the error: 'ID not found in the DOM' when insertUI
  # is used.
  f <- renderMorphField({
    l <- returnMorphFieldUI(output, id, param_values, value_descriptions,
                            configurations, spec_columns, styleFunc,
                            editable, edit_mode, edit_config_mode)
    field <- l$field
    field_df <- l$field_df
    proxy <<- reactivateMorphField(input, output, id,
                                   param_values = function() {param_values},
                                   value_descriptions = function() {value_descriptions},
                                   ccm = function() {ccm},
                                   configurations = function() {configurations},
                                   spec_columns = function() {spec_columns},
                                   field_df = function() {field_df},
                                   styleFunc = styleFunc, editable = editable)
    field
  })
  output[[id]] <- f
  return(proxy)
  # return(f)
}


returnMorphFieldUI <- function(output, id, param_values = NULL,
                               value_descriptions = NULL,
                               configurations = NULL, spec_columns = NULL,
                               styleFunc = NULL, editable = FALSE,
                               edit_mode = FALSE, edit_config_mode = FALSE) {
  l <- returnMorphFieldUIWithoutToolbar(
    output, id, param_values, value_descriptions, spec_columns,
    styleFunc, edit_mode, edit_config_mode
  )
  if (editable) {
    placeMorphFieldUIToolbar(id, edit_mode)
    placeEditButtonRow(id, edit_mode, edit_config_mode, configurations)
  }
  l
}

returnMorphFieldUIWithoutToolbar <- function(output, id, param_values = NULL,
                                             value_descriptions = NULL,
                                             spec_columns = NULL,
                                             styleFunc = NULL, edit_mode = FALSE,
                                             edit_config_mode = FALSE) {
  l <- morphfield(param_values, value_descriptions, spec_columns,
                  edit_mode, id, edit_config_mode)
  field <- l$field
  field_df <- l$field_df
  if (!is.null(styleFunc)) {
    field <- styleFunc(field)
  }
  list(field = field, field_df = field_df)
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
placeMorphFieldUI <- function(output, id, param_values = NULL,
                              value_descriptions = NULL,
                              configurations = NULL, spec_columns = NULL,
                              styleFunc = NULL, editable = FALSE,
                              edit_mode = FALSE, edit_config_mode = FALSE) {
  output[[id]] <- renderMorphField({
    l <- returnMorphFieldUI(output, id, param_values,
                            value_descriptions,
                            configurations, spec_columns,
                            styleFunc, edit_mode, edit_config_mode)
    l$field
  })
}


placeMorphFieldUIWithoutToolbar <- function(output, id, param_values = NULL,
                                            value_descriptions = NULL,
                                            configurations = NULL,
                                            spec_columns = NULL,
                                            styleFunc = NULL, edit_mode = FALSE,
                                            edit_config_mode = FALSE) {
  output[[id]] <- renderMorphField({
    l <- returnMorphFieldUIWithoutToolbar(output, id, param_values,
                                          value_descriptions,
                                          spec_columns,
                                          styleFunc, edit_mode, edit_config_mode)
    l$field
  })
}


placeEditButtonRow <- function(id, edit_mode = FALSE, edit_config_mode = FALSE,
                               configurations = NULL) {
  if (edit_mode) {
    insertUI(
      selector = paste0("#", id),
      where = "beforeBegin",
      ui = fluidRow(
        id = paste0(id, "_edit_btn_row"),
        column(
          4,
          actionButton(paste0(id, "_rem_item_btn"), "Remove Selected Item",
                       class = "disabled", disabled = ""),
          actionButton(paste0(id, "_mod_item_btn"), "Modify Selected Item",
                       class = "disabled", disabled = "")
        ),
        column(
          4,
          {
            v <- if (is.null(edit_config_mode)) {FALSE} else {edit_config_mode}
            d <- checkboxInput(paste0(id, "_edit_config_mode"), "Edit Configurations",
                               value = v)
            d$attribs$class <- paste(d$attribs$class, "pull-left")
            d$attribs$style <- "width: auto; margin-left: 10px; margin-right: 10px;"
            d
          },
          actionButton(paste0(id, "_save_config_btn"), "Save Configuration",
                       class = "pull-left disabled", disabled = ""),
          {
            d <- actionButton(paste0(id, "_rem_config_btn"), "Remove Configuration(s)",
                              class = "pull-left")
            if (length(configurations) == 0) {
              d$attribs$class <- paste(d$attribs$class, "disabled")
              d$attribs$disabled = ""
            }
            d
          },
          actionButton(paste0(id, "_show_ccm_btn"), "Show CCM",
                       class = "pull-left")
        ),
        column(
          4,
          div(
            class = "pull-right",
            actionButton(paste0(id, "_add_col_btn"), "Add Column"),
            actionButton(paste0(id, "_rem_col_btn"), "Remove Column"),
            actionButton(paste0(id, "_mod_col_btn"), "Modify Column"),
            actionButton(paste0(id, "_set_spec_columns_btn"), "Mark Column(s)")
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
      # Edit mode toggle button:
      actionButton(paste0(id, "_edit_btn"), "", icon = icon("edit"),
                   style = "font-size: 10px;"),
      tags$head(shinyBS::bsTooltip(paste0(id, "_edit_btn"), "Toggle Edit Mode",
                                   placement = "right")),

      # Download button:
      downloadButton(paste0(id, "_download_btn"), "", icon = icon("download"),
                     style = "font-size: 10px;"),
      tags$head(shinyBS::bsTooltip(paste0(id, "_download_btn"), "Download Field",
                                   placement = "right")),

      # Upload button:
      actionButton(paste0(id, "_upload_btn"), "", icon = icon("upload"),
                   style = "font-size: 10px;"),
      tags$head(shinyBS::bsTooltip(paste0(id, "_upload_btn"), "Upload Field",
                                   placement = "right")),

      # Hidden edit mode check box:
      div(
        id = paste0(id, "_edit_mode_div"),
        style = "display: none;",
        checkboxInput(paste0(id, "_edit_mode"), "Edit Mode", value = edit_mode)
      ),

      # Insert JS event handlers for buttons in table:
      # (Helpful resources:
      # https://stackoverflow.com/questions/40631788/shiny-observe-triggered-by-dynamicaly-generated-inputs/40643541#40643541
      # https://stackoverflow.com/questions/40168801/r-shiny-last-clicked-button-id)
      singleton(
        tags$head(tags$script(HTML(sprintf(
          "
        $(document).on('click', '.add-item-btn-%s', function () {
          Shiny.onInputChange('%s_add_item_btn', this.id + ' ' + Math.random());
        });
        ", id, id
        ))))
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
#' \code{configurations}, and \code{field_df}. These reactive
#' expressions are then used both in the field update code with
#' \code{\link{placeMorphFieldUI}} and given to \code{reactivateMorphField}.
#'
#' @inheritParams installMorphField
#' @param param_values A function returning the \code{param_values}, see
#'   \code{\link{installMorphField}}.
#' @param value_descriptions A function returning the \code{value_descriptions}, see
#'   \code{\link{installMorphField}}.
#' @param ccm A function returning the \code{ccm}, see
#'   \code{\link{installMorphField}}.
#' @param configurations A function returning the
#'   \code{configurations}, see \code{\link{installMorphField}}.
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
                                 configurations = function() {NULL},
                                 spec_columns = function() {NULL},
                                 field_df = function() {NULL},
                                 styleFunc = NULL, editable = FALSE) {
  proxy <- reactivateMorphFieldWithoutToolbar(
    input, id, param_values, ccm, configurations, field_df, editable
  )

  if (editable) {
    reactivateMorphFieldToolbar(input, output, id, param_values,
                                value_descriptions, ccm, configurations,
                                spec_columns, field_df, styleFunc)
  }

  return(proxy)
}

getFieldDF <- function(field_df, param_values) {
  field_df <- field_df()
  if (is.null(field_df)) {
    field_df <- paramValuesToDataFrame(param_values())
  }
  field_df
}

reactivateMorphFieldWithoutToolbar <- function(input, id, param_values,
                                               ccm = function() {NULL},
                                               configurations = function() {NULL},
                                               field_df = function() {NULL},
                                               editable = FALSE) {
  proxy <- dataTableProxy(id)

  # Immediately deselect empty cells, they shall not be selectable
  observeEvent(input[[paste0(id, "_cells_selected")]], {
    sel_cells <- input[[paste0(id, "_cells_selected")]]
    field_df <- getFieldDF(field_df, param_values)
    if (isLastSelectedCellEmpty(sel_cells, field_df)) {
      proxy %>% selectCells(removeLastSelectedCell(sel_cells))
    }
  })

  # Update the field with the new consistent cells after selection
  observeEvent(input[[paste0(id, "_cells_selected")]], {
    configs <- convertConfigsToExtendedAndSort(configurations())
    sel_cells <- input[[paste0(id, "_cells_selected")]]
    field_df <- getFieldDF(field_df, param_values)
    if (editable) {
      em <- input[[paste0(id, "_edit_mode")]]
      if (!is.null(em) && em) {
        updateEditButtons(input, id, sel_cells, field_df, configs)
        return() # do nothing else
      }
    }
    if (isLastSelectedCellEmpty(sel_cells, field_df)) {
      sel_cells <- removeLastSelectedCell(sel_cells)
    }
    ccm <- ccm()
    if (is.null(ccm)) {
      if (!is.null(configs)) {
        ccm <- buildCCMFromSpecificConfigurations(param_values(),
                                                  configs)
      } else {
        ccm <- buildUnconstrainedCCM(param_values())
      }
    }
    consistent_cells <- findConsistentCells(param_values(), ccm, sel_cells)
    proxy %>% setCellsConsistent(consistent_cells)
  })

  return(proxy)
}

installModMorphField <- function(input, output, id, param_values, value_descriptions,
                                 ccm, configurations, spec_columns, styleFunc,
                                 edit_mode = TRUE, edit_config_mode = FALSE) {
  orig_id <- sub(pattern = "__modified__[0-9]+$", replacement = "", x = id)
  new_id <- sprintf("%s__modified__%d", orig_id, as.integer(runif(1)*1e9))
  removeUI(selector = sprintf("#%s_edit_btn", id))
  removeUI(selector = sprintf("#%s_download_btn", id))
  removeUI(selector = sprintf("#%s_upload_btn", id))
  removeUI(selector = sprintf("#%s_edit_mode_div", id))
  removeUI(selector = sprintf("#%s_edit_btn_row", id))
  removeUI(selector = sprintf("#%s", id))
  insertUI(paste0("#", orig_id, "_container"), ui = morphFieldOutputWithoutContainer(new_id))
  installMorphField(input, output, new_id, param_values, value_descriptions,
                    ccm, configurations, spec_columns, styleFunc,
                    editable = TRUE, edit_mode = edit_mode, edit_config_mode = edit_config_mode)
}

reactivateMorphFieldToolbar <- function(input, output, id, param_values,
                                        value_descriptions = function() {NULL},
                                        ccm = function() {NULL},
                                        configurations = function() {NULL},
                                        spec_columns = function() {NULL},
                                        field_df = function() {NULL},
                                        styleFunc = NULL) {
  observeEvent(input[[paste0(id, "_edit_btn")]], {
    configs <- convertConfigsToExtendedAndSort(configurations())
    if (!input[[paste0(id, "_edit_mode")]]) { # toggle (edit_mode was previously off, turn it on)
      placeMorphFieldUIWithoutToolbar(output, id, param_values(), value_descriptions(),
                                      configs, spec_columns(), styleFunc,
                                      edit_mode = TRUE)
      updateCheckboxInput(getDefaultReactiveDomain(), paste0(id, "_edit_mode"), value = TRUE)
      placeEditButtonRow(id, edit_mode = TRUE, configurations = configs)
    } else {
      placeMorphFieldUIWithoutToolbar(output, id, param_values(), value_descriptions(),
                                      configs, spec_columns(), styleFunc)
      updateCheckboxInput(getDefaultReactiveDomain(), paste0(id, "_edit_mode"), value = FALSE)
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
    configs <- convertConfigsToExtendedAndSort(configurations())
    col <- as.integer(input[[paste0(id, "_new_item_col")]])
    new_item <- input[[paste0(id, "_new_item")]]
    if (!is.null(new_item) && nzchar(trimws(new_item))) {
      removeModal()
      param_values <- param_values()
      param_values[[col]] <- c(param_values[[col]], new_item)
      installModMorphField(input, output, id, param_values, value_descriptions(),
                           ccm(), configs, spec_columns(), styleFunc)
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
    configs <- convertConfigsToExtendedAndSort(configurations())
    row <- as.integer(input[[paste0(id, "_rem_item_row")]])
    col <- as.integer(input[[paste0(id, "_rem_item_col")]])
    param_values <- param_values()
    param_values[[col]] <- param_values[[col]][-row]
    installModMorphField(input, output, id, param_values, value_descriptions(),
                         ccm(), configs, spec_columns(), styleFunc)
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
    configs <- convertConfigsToExtendedAndSort(configurations())
    row <- as.integer(input[[paste0(id, "_mod_item_row")]])
    col <- as.integer(input[[paste0(id, "_mod_item_col")]])
    mod_item <- input[[paste0(id, "_mod_item")]]
    if (!is.null(mod_item) && nzchar(trimws(mod_item))) {
      removeModal()
      param_values <- param_values()
      param_values[[col]][row] <- mod_item
      installModMorphField(input, output, id, param_values, value_descriptions(),
                           ccm(), configs, spec_columns(), styleFunc)
    } else {
      showModal(modItemModal(row, col, failed = TRUE))
    }
  })

  addColModal <- function(failed = FALSE) {
    modalDialog(
      textInput(paste0(id, "_new_col"), "New column title",
                placeholder = "Enter column title here..."),
      if (failed)
        div(tags$b("Invalid column title.", style = "color: red;")),
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
    if (!is.null(new_col) && nzchar(trimws(new_col))) {
      removeModal()
      new_pv <- list(c())
      names(new_pv) <- new_col
      param_values <- param_values()
      param_values <- c(param_values, new_pv)
      configs <- convertConfigsToExtendedAndSort(configurations())
      installModMorphField(input, output, id, param_values, value_descriptions(),
                           ccm(), configs, spec_columns(), styleFunc)
    } else {
      showModal(addColModal(failed = TRUE))
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
    configs <- convertConfigsToExtendedAndSort(configurations())
    installModMorphField(input, output, id, param_values, value_descriptions(),
                         ccm(), configs, spec_columns(), styleFunc)
  })

  modColModal <- function(failed = FALSE) {
    modalDialog(
      selectizeInput(paste0(id, "_mod_col"), "Select column to rename",
                     choices = names(param_values())),
      textInput(paste0(id, "_mod_col_title"), "Rename column to:",
                placeholder = "Enter new column title here..."),
      if (failed)
        div(tags$b("Invalid column title.", style = "color: red;")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(paste0(id, "_mod_col_ok"), "OK")
      )
    )
  }

  observeEvent(input[[paste0(id, "_mod_col_btn")]], {
    showModal(modColModal())
  })

  observeEvent(input[[paste0(id, "_mod_col_ok")]], {
    mod_col <- input[[paste0(id, "_mod_col")]]
    mod_col_title <- input[[paste0(id, "_mod_col_title")]]
    if (!is.null(mod_col_title) && nzchar(trimws(mod_col_title))) {
      removeModal()
      param_values <- param_values()
      col <- which(names(param_values) == mod_col)
      names(param_values)[col] <- mod_col_title
      configs <- convertConfigsToExtendedAndSort(configurations())
      installModMorphField(input, output, id, param_values, value_descriptions(),
                           ccm(), configs, spec_columns(), styleFunc)
    } else {
      showModal(modColModal(failed = TRUE))
    }
  })

  output[[paste0(id, "_download_btn")]] <- downloadHandler(
    filename = function() {
      paste0('morphfield-data-', Sys.Date(), '.rds')
    },
    content = function(con) {
      object <- list(
        param_values = param_values(),
        value_descriptions = value_descriptions(),
        ccm = ccm(),
        configurations = convertConfigsToExtendedAndSort(configurations())
      )
      saveRDS(object, con)
    }
  )

  uploadModal <- function() {
    modalDialog(
      fileInput(paste0(id, "_file_input"), "Select file to upload"),
      footer = modalButton("Cancel")
    )
  }

  observeEvent(input[[paste0(id, "_upload_btn")]], {
    showModal(uploadModal())
  })

  observeEvent(input[[paste0(id, "_file_input")]], {
    removeModal()
    object <- readRDS(input[[paste0(id, "_file_input")]]$datapath)
    installModMorphField(input, output, id, object$param_values,
                         object$value_descriptions, object$ccm,
                         object$configurations, object$spec_columns, styleFunc,
                         edit_mode = input[[paste0(id, "_edit_mode")]],
                         edit_config_mode = input[[paste0(id, "_edit_config_mode")]])
  })

  specColumnModal <- function() {
    modalDialog(
      selectizeInput(paste0(id, "_spec_columns"), HTML(paste(
        "Which column(s) shall be marked as", tags$em("specifying"), "or",
        tags$em("special"), "(with dark gray)?")
      ), choices = names(param_values()), multiple = TRUE),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(paste0(id, "_set_spec_columns_ok"), "OK")
      )
    )
  }

  observeEvent(input[[paste0(id, "_set_spec_columns_btn")]], {
    showModal(specColumnModal())
  })

  observeEvent(input[[paste0(id, "_set_spec_columns_ok")]], {
    removeModal()
    spec_columns <- input[[paste0(id, "_spec_columns")]]
    configs <- convertConfigsToExtendedAndSort(configurations())
    installModMorphField(input, output, id, param_values(), value_descriptions(),
                         ccm(), configs, spec_columns, styleFunc,
                         edit_config_mode = input[[paste0(id, "_edit_config_mode")]])
  })

  observeEvent(input[[paste0(id, "_edit_config_mode")]], {
    configs <- convertConfigsToExtendedAndSort(configurations())
    placeMorphFieldUIWithoutToolbar(
      output, id, param_values(), value_descriptions(),
      configs, spec_columns(), styleFunc,
      edit_mode = TRUE, edit_config_mode = input[[paste0(id, "_edit_config_mode")]]
    )
    disableAllEditButtons(id)
  })

  observeEvent(input[[paste0(id, "_save_config_btn")]], {
    sel_cells <- input[[paste0(id, "_cells_selected")]]
    configs <- convertConfigsToExtendedAndSort(configurations())
    field_df <- getFieldDF(field_df, param_values)
    rows <- sel_cells[, 1]
    cols <- sel_cells[, 2] + 1
    params <- names(field_df)[cols]
    config <- lapply(1:length(cols), function(i) {
      list(
        param = params[i],
        value = field_df[rows[i], cols[i]]
      )
    })
    index <- which(
      sapply(1:length(configs), function(i) {
        identical(configs[[i]], config)
      })
    )
    if (length(index) == 0) {
      # config not present yet, insert at end
      index <- length(configs) + 1
      configs[[index]] <- config
    } else if (length(index) > 1) {
      # Delete duplicate configs
      for (i in 2:length(index)) {
        configs[[i]] <- NULL
      }
    }
    installModMorphField(input, output, id, param_values(), value_descriptions(),
                         ccm(), configs, spec_columns(), styleFunc,
                         edit_config_mode = input[[paste0(id, "_edit_config_mode")]])
  })

  remConfigModal <- function() {
    configs <- convertConfigsToExtendedAndSort(configurations())
    config_strings <- sapply(configs, function(config) {
      paste(sapply(config, function(item) {
        paste(item$param, paste0("(", paste(item$value, collapse = ", "), ")"),
              sep = ": ")
      }), collapse = " | ")
    })
    choices <- 1:length(configs)
    names(choices) <- config_strings
    modalDialog(
      selectizeInput(paste0(id, "_configs"), HTML(paste(
        "Select configurations(s) to remove.")
      ), choices = choices, multiple = TRUE),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(paste0(id, "_rem_config_ok"), "OK")
      )
    )
  }

  observeEvent(input[[paste0(id, "_rem_config_btn")]], {
    showModal(remConfigModal())
  })

  observeEvent(input[[paste0(id, "_rem_config_ok")]], {
    removeModal()
    indices <- as.integer(input[[paste0(id, "_configs")]])
    configs <- convertConfigsToExtendedAndSort(configurations())
    configs[indices] <- NULL
    installModMorphField(input, output, id, param_values(), value_descriptions(),
                         ccm(), configs, spec_columns(), styleFunc,
                         edit_config_mode = input[[paste0(id, "_edit_config_mode")]])
  })

  ccmModal <- function() {
    modalDialog(
      dataTableOutput(paste0(id, "_ccm")),
      footer = tagList(
        # modalButton("Cancel"),
        # actionButton(paste0(id, "_rem_config_ok"), "OK")
        modalButton("Close")
      )
    )
  }

  observeEvent(input[[paste0(id, "_show_ccm_btn")]], {
    showModal(ccmModal())
    output[[paste0(id, "_ccm")]] <- renderDataTable(
      dataFrameFromCCM(param_values(), ccm())
    )
  })
}


updateEditButtons <- function(input, id, sel_cells, field_df,
                              configurations) {
  if (req(input[[paste0(id, "_edit_mode")]])) {
    esm <- input[[paste0(id, "_edit_config_mode")]]
    if (is.null(esm) || !esm) {
      if (nrow(sel_cells) == 1) {
        shinyjs::enable(selector = paste0("#", id, "_rem_item_btn"))
        shinyjs::enable(selector = paste0("#", id, "_mod_item_btn"))
      } else {
        shinyjs::disable(selector = paste0("#", id, "_rem_item_btn"))
        shinyjs::disable(selector = paste0("#", id, "_mod_item_btn"))
      }
    } else {
      if (nrow(sel_cells) == 0) {
        cols <- 0
      } else {
        cols <- unique(sel_cells[, 2] + 1)
      }
      if (length(cols) > 1) {
        shinyjs::enable(selector = paste0("#", id, "_save_config_btn"))
      } else {
        shinyjs::disable(selector = paste0("#", id, "_save_config_btn"))
      }
    }
  }
}

disableAllEditButtons <- function(id) {
  shinyjs::disable(selector = paste0("#", id, "_rem_item_btn"))
  shinyjs::disable(selector = paste0("#", id, "_mod_item_btn"))
  shinyjs::disable(selector = paste0("#", id, "_save_config_btn"))
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
