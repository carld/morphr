#' Visualize a Morphological Field Using Shiny
#'
#' With this package you can create an interactive widget that represents a
#' morphological field. A morphological field is a collection of parameters that
#' can take on several possible values. Each parameter and its values are
#' written into one column of a table. If the CCM (cross-consistency matrix) is
#' specified for the field, it is possible to select any cell and thus constrain
#' the possible configurations (parameter value combinations). The cells that
#' are part of consistent configurations are marked with a color, updated at
#' each click. It is therefore easy to explore the field and the relations
#' between its parameters.
#'
#' The only two functions you're likely to need from \pkg{morphr} are
#' \code{\link{installMorphField}} to install the widget in a \pkg{shiny} server
#' function and \code{\link{morphFieldOutput}} to install it in a \pkg{shiny}
#' UI function.
#'
#' To see a live demo, run \code{shiny::runApp(system.file("examples", "morphr-simple", package="morphr"))}.
"_PACKAGE"

# This command in the "Tools" -> "Project Options" -> "Build Tools"
# configuration breaks new roxygen2 version 6. I have now checked to
# use devtools to build documentation ("Generate documentation with Roxygen" and
# "Configure" -> "Build & Reload") and it generates the files correctly,
# including comment at the top). See also
# https://stackoverflow.com/questions/29135971/namespace-not-generated-by-roxygen2-skipped-confusion-with-hadley-book.
# Removed command:
# && -v Rscript -e "Rd2roxygen::rab(install=T,build=F)"

################################################################################


#' Create an HTML morphological field widget using the DataTables library
#'
#' This function creates an HTML widget to display a morphological field (a kind
#' of table where each column represents the possible configurations of a
#' certain parameter) using the JavaScript library DataTables. This function is
#' called internally by \code{\link{installMorphField}()}.
#'
#' @inheritSection installMorphField Details
#' @inheritSection installMorphField Cross-consistency matrix (CCM)
#' @inheritSection installMorphField Specific configurations
#' @inheritParams installMorphField
#' @return List with items \code{field} (a \code{\link{datatable}} object) and
#'   \code{field_df} (the \code{data.frame} used to create the datatable object).
#' @export
morphfield <- function(param_values = NULL, value_descriptions = NULL,
                       specific_configurations = NULL, edit_mode = FALSE,
                       id = NULL, set_spec_mode = FALSE) {
  field_df <- paramValuesToDataFrame(param_values, value_descriptions)
  if (edit_mode) {
    last_non_empty_index <- lapply(field_df, function(col) {
      non_empty <- (1:length(col))[nchar(col) > 0]
      lne <- length(non_empty)
      if (lne == 0) 0 else non_empty[lne]
    })
    for (i in seq_along(last_non_empty_index)) {
      field_df[last_non_empty_index[[i]] + 1, i] <- as.character(
        actionButton(paste0(id, "_add_item_btn_", i), "Add Item",
                     class = paste0("add-item-btn-", id))
      )
    }
  }

  field <- datatable(
    field_df,
    options = list(
      # Disable search/filter box:
      searching = FALSE,
      # Show all rows on one page and no navigation:
      paging = FALSE,
      # Do not show info about data in footer:
      info = FALSE,
      # Disable sorting by columns for all columns:
      ordering = FALSE
      # enable addition of button(s) that are registered with shiny: see https://github.com/rstudio/DT/issues/178
      # preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      # drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
    ),
    # Disable display of rownames:
    rownames = FALSE,
    # Enable cell selection:
    selection = list(
      mode = if (edit_mode && !set_spec_mode) "single" else "multiple",
      target = "cell"
    ),
    # Do not escape HTML
    escape = FALSE
  )

  if (!is.null(specific_configurations)) {
    # Style the parameters that define specific configurations differently:
    field <- field %>%
      formatStyle(names(specific_configurations),
                  color = "white", backgroundColor = "gray")
  }

  return(list(
    field = field, field_df = field_df
  ))
}


#' Parse strings, replace invalid characters like '\\n'
parseMorphFieldString <- function(string) {
  gsub("\n", "</br>", string)
}


#' Create a fixed dimension data.frame from variable length lists of morphological field parameter values
#'
#' A morphological field can consist of parameters with variable number of
#' values. However, a DataTable accepts only a data.frame, which must
#' have the same number of rows in each column. Therefore, this function takes
#' a list of lists (each list representing one list of parameter configurations,
#' i.e. one column) and fills the empty cells with empty character strings to
#' create a fixed dimension data.frame.
#' @inheritParams installMorphField
#' @export
paramValuesToDataFrame <- function(param_values = NULL, value_descriptions = NULL) {
  # Need to set seed manually to ensure random unique IDs of the tipify/popify elements
  # (Not understood why, but every morphfield was using the same ID sequence as the previous,
  # only omitting the first 8 IDs of the previous field.)
  t <- as.character(as.numeric(Sys.time()))
  set.seed(as.integer(as.numeric(substr(t, 7, nchar(t))) * 1e5))

  ret_val <- param_values
  if (is.null(ret_val)) ret_val <- list(list())
  if (class(ret_val) == "list") {
    # Make sure that all list items have same length.
    # If not: fill with empty character strings
    max_length <- max(sapply(ret_val, length))
    cols <- length(ret_val)
    ret_val <- lapply(seq_along(ret_val), function(i) {
      param1 <- names(ret_val)[i]
      placement <- if (i/cols <= 0.5) "right" else "left"
      items <- sapply(ret_val[[i]], function(value1) {
        value1 <- parseMorphFieldString(value1)
        desc <- value_descriptions[[param1]][[value1]]
        if (!is.null(desc)) {
          desc <- parseMorphFieldString(desc)
          width <- if (nchar(desc) < 200) "300px" else "1000px"
          as.character(shinyBS::popify(
            htmltools::span(value1), value1, desc,
            placement = placement,
            options = list(
              container = "body",
              template = paste0('<div class="popover" role="tooltip">',
                                '  <div class="arrow"></div>',
                                '  <h3 class="popover-title"></h3>',
                                sprintf('  <div class="popover-content" style="width: %s;"></div>', width),
                                '</div>')
            )
          ))
        } else {
          value1
        }
      }, USE.NAMES = FALSE)
      if (length(items) == 0) items <- NULL
      c(items, rep("", max_length - length(items)))
    })
    names(ret_val) <- names(param_values)
    # Turn it into a data.frame, because that is what is expected by DT
    ret_val <- as.data.frame(ret_val, optional = TRUE, stringsAsFactors = FALSE) # optional keeps whitespace in column names
  }
  ret_val
}


#' Find cells that are consistent in at least one configuration
#'
#' Of all configurations spanned by combinations of the parameter values in
#' \code{param_values}, find the ones that are possible in at least one
#' combination, as it is told by the CCM (cross-consistency matrix). If some
#' cells are selected, then they are not marked as consistent (because that
#' would cause them to get the consistent color so that they do not appear to be
#' selected). However, selected cells restrict the consistent configurations to
#' only those including one of the selected cells (in each column where selected
#' cells are present).
#' @inheritParams installMorphField
#' @param selected_cells A two-column matrix with the row and column indices of
#'   selected cells, starting at 1 for rows (0 is header) and at 0 for columns.
#'   It is obtained from \code{input$tableId_cells_selected}.
#' @return A two-column matrix with the row and column indices of consistent
#'   cells, starting at 1 for rows (0 is header) and at 0 for columns. This can
#'   be plugged into \code{\link{setCellsConsistent}()}.
#' @export
findConsistentCells <- function(param_values, ccm, selected_cells = NULL) {
  sel_cols <- list()
  sel_rows <- list()
  try({
    sel_cols <- selected_cells[,2] + 1
    sel_rows <- selected_cells[,1]
  }, silent = TRUE)
  consistent_cells <- matrix(ncol = 2)[-1,] # empty two-column matrix
  lapply(seq_along(param_values), function(col) { # loop over all columns (index col)
    param1 <- names(param_values)[col]
    # print(param1)
    if (!col %in% sel_cols) { # don't mark cells in columns with selections
      lapply(seq_along(param_values[[col]]), function(row) { # loop over all values in column (rows)
        value1 <- param_values[[col]][[row]]
        # print(paste0("   ", value1))
        cell_consistent <- TRUE
        for (ocol in seq_along(param_values)[-col]) { # loop over all other columns (index ocol)
          # in each column (index ocol), at least one value (only within the selection
          # if there is a selection) must be consistent with value1
          param2 <- names(param_values)[ocol]
          # print(paste0("      ", param2))
          if (ocol %in% sel_cols) {
            # check only with selected values
            check_values <- param_values[[ocol]][sel_rows[sel_cols == ocol]]
          } else {
            check_values <- param_values[[ocol]]
          }
          consistent <- FALSE
          for (value2 in check_values) {
            # print(paste0("         ", value2))
            if (ccm[[buildHashValue(param1, value1, param2, value2)]]) {
              consistent <- TRUE
              # print(paste0("            CONSISTENT!"))
              break()
            }
          }
          if (!consistent) {
            cell_consistent <- FALSE
            break()
          }
        }
        if (cell_consistent) {
          # print(paste0("   => CELL CONSISTENT!"))
          consistent_cells <<- rbind(consistent_cells, c(row, col - 1))
        } else {
          # print(paste0("   => CELL NOT CONSISTENT!"))
        }
      })
    }
  })
  consistent_cells
}


#' Build a single hash value from two parameter/value combinations
#'
#' This function simply combines four strings (with two strings forming a pair
#' consisting of a parameter and a value) into a single one. It does so in a
#'way that the order of the parameter-value combinations does not matter.
#' Switching param1/value1 pair and the param2/value2 pair will result in the
#' same hash character string.
#'
#' The purpose of this function is to provide hash keys that can be used to
#' index a named list representing the CCM (cross-consistency matrix) of a
#' morphological field. See \code{\link{morphfield}()} for information on the
#' CCM and in what format it is expected by \code{\link{morphfield}()}.
#'
#' @param param1 One of the two morphological field parameters to look up the
#'   consistency for. It corresponds to one column in the morphological field.
#' @param value1 The value within \code{param1} to look up consistency for. It
#'   belongs to one cell in the morphological field.
#' @param param2 Like \code{param1}, but for the other side of the comparison.
#' @param value2 Like \code{value1}, but for the other side of the comparison.
#' @return A single character string, which is the sorted hash of the value
#'   combination for lookup in the CCM.
#' @export
buildHashValue <- function(param1, value1, param2, value2) {
  paste(sort(c(
    paste(param1, value1, sep = "->"),
    paste(param2, value2, sep = "->")
  )), sep = "->", collapse = "|")
}


#' Create a CCM from parameter values with a single default value
#'
#' The CCM (cross-consistency matrix) from this function will have the same
#' logical value \code{def_val} for all value combinations. Use this to create
#' either a completely unconstrained CCM (all \code{TRUE}), or a completely
#' impossible CCM (all \code{FALSE}).
#'
#' @inheritParams installMorphField
#' @param def_val Logical for the initialization of all CCM entries.
#' @export
initializeCCM <- function(param_values, def_val = TRUE) {
  ccm <- list()
  if (length(param_values) > 1) {
    lapply(1:(length(param_values) - 1), function(i) {
      param1 <- names(param_values)[i]
      lapply(param_values[[i]], function(value1) {
        lapply((i + 1):length(param_values), function(j) {
          param2 <- names(param_values)[j]
          lapply(param_values[[j]], function(value2) {
            ccm[[buildHashValue(param1, value1, param2, value2)]] <<- def_val
          })
        })
      })
    })
  }
  ccm
}

#' Create a completely unconstrained CCM from parameter values
#'
#' All entries in the CCM (cross-consistency matrix) will be \code{TRUE}.
#'
#' @inheritParams installMorphField
#' @export
buildUnconstrainedCCM <- function(param_values) {
  initializeCCM(param_values, def_val = TRUE)
}


#' Convert specific configurations to CCM
#'
#' If you specify your morphological field with \code{specific_configurations},
#' then they can be converted to a CCM (cross-consistency matrix) using this
#' function.
#'
#' @inheritParams installMorphField
#' @export
buildCCMFromSpecificConfigurations <- function(param_values,
                                               specific_configurations) {
  ccm <- initializeCCM(param_values, def_val = FALSE) # first set all combinations to inconsistent
  # Then, set only valid combinations consistent:
  lapply(names(specific_configurations), function(param1) {
    lapply(names(specific_configurations[[param1]]), function(value1) {
      configs <- specific_configurations[[param1]][[value1]]
      # add relations from higher to lower hierarchy
      lapply(names(configs), function(param2) {
        lapply(configs[[param2]], function(value2) {
          ccm[[buildHashValue(param1, value1, param2, value2)]] <<- TRUE # cross-correlation between specifying and specified parameter
        })
      })
      if (length(configs) > 1) { # for more than one specified parameter: need to cross-correlate the specified parameters among themselves as well
        lapply(1:(length(configs) - 1), function(i) {
          param2 <- names(configs)[i]
          lapply(configs[[param2]], function(value2) {
            lapply((i + 1):length(configs), function(j) {
              param3 <- names(configs)[j]
              lapply(configs[[param3]], function(value3) {
                ccm[[buildHashValue(param2, value2, param3, value3)]] <<- TRUE
              })
            })
          })
        })
      }
    })
  })
  ccm
}


#' Convert a long table format data.frame to a nested list
#'
#' Use this function if you have a data.frame in long table format (with
#' redundant information in some columns) and need a nested list expected as
#' input to \code{\link{installMorphField}}, e.g. arguments \code{param_values},
#' \code{value_descriptions}, \code{specific_configurations}.
#'
#' The redundant columns must be on the left, the non-redundant on the right
#' side of the data.frame.
#'
#' @param df A \code{data.frame} to be converted to a nested list.
#' @return The converted data as nested list.
#' @export
buildNestedListFromDataFrame <- function(df) {
  dff <- df
  if ("factor" %in% class(dff[[1]])) {
    # create new factor omitting the missing levels
    dff[[1]] <- factor(dff[[1]], levels = unique(as.character(dff[[1]])))
  }
  nested_list <- split(dff[-1], dff[1])
  if (length(nested_list) == 0) { # happens if last col's item is NA
    return()
  }
  if (ncol(nested_list[[1]]) == 0) { # happens normally on last col
    return(names(nested_list))
  }
  nested_list <- lapply(nested_list, buildNestedListFromDataFrame)
  return(nested_list)
}
