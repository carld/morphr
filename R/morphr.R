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
# configuration (behind -v) breaks new roxygen2 version 6. I have now checked to
# use devtools to build documentation ("Generate documentation with Roxygen" and
# "Configure" -> "Build & Reload") and it generates the files correctly,
# including comment at the top). See also
# https://stackoverflow.com/questions/29135971/namespace-not-generated-by-roxygen2-skipped-confusion-with-hadley-book.
# Removed command:
# && Rscript -e "Rd2roxygen::rab(install=T,build=F)"

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
#' @export
morphfield <- function(param_values, ccm = NULL, specific_configurations = NULL) {
  field_df <- paramValuesToDataFrame(param_values)
  if (is.null(ccm)) {
    if (!is.null(specific_configurations)) {
      ccm <- buildCCMFromSpecificConfigurations(param_values, specific_configurations)
    } else {
      ccm <- buildUnconstrainedCCM(param_values)
    }
  }

  field <- datatable(
    field_df,
    options = list(
      # Disable search/filter box:
      searching = FALSE,
      # Disable sorting by columns for all columns:
      columnDefs = list(
        list(orderable = FALSE, targets = 0:(ncol(field_df) - 1))
      ),
      # Show all rows on one page and no navigation:
      paging = FALSE,
      # Do not show info about data in footer:
      info = FALSE
    ),
    # Disable display of rownames:
    rownames = FALSE,
    # Enable cell selection:
    selection = list(
      mode = "multiple",
      target = "cell"
    )
  )

  if (!is.null(specific_configurations)) {
    # Style the parameters that define specific configurations differently:
    field <- field %>%
      formatStyle(names(specific_configurations),
                  color = "white", backgroundColor = "gray")
  }

  return(field)
}


#' Create a fixed dimension data.frame from variable length lists of morphological field parameter values
#'
#' A morphological field can consist of parameters with variable number of
#' values. However, a DataTable accepts only a data.frame, which must
#' have the same number of rows in each column. Therefore, this function takes
#' a list of lists (each list representing one list of parameter configurations,
#' i.e. one column) and fills the empty cells with empty character strings to
#' create a fixed dimension data.frame.
#' @inheritParams morphfield
#' @export
paramValuesToDataFrame <- function(param_values) {
  ret_val <- param_values
  if (class(ret_val) == "list") {
    # Make sure that all list items have same length.
    # If not: fill with empty character strings
    max_length <- max(sapply(ret_val, length))
    ret_val <- lapply(ret_val, function(li) {
      c(li, rep("", max_length - length(li)))
    })
    # Turn it into a data.frame, because that is what is expected by DT
    ret_val <- as.data.frame(ret_val, optional = TRUE) # optional keeps whitespace in column names
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
#' @inheritParams morphfield
#' @param selected_cells A two-column matrix with the row and column indices of
#'   selected cells, starting at 1 for rows (0 is header) and at 0 for columns.
#'   It is obtained from \code{input$tableId_cells_selected}.
#' @return A two-column matrix with the row and column indices of consistent
#'   cells, starting at 1 for rows (0 is header) and at 0 for columns. This can
#'   be plugged into \code{\link{setCellsConsistent}()}.
#' @export
findConsistentCells <- function(param_values, ccm, selected_cells = NULL) {
  sel_cols <- c()
  sel_rows <- c()
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
#' @inheritParams morphfield
#' @param def_val Logical for the initialization of all CCM entries.
#' @export
initializeCCM <- function(param_values, def_val = TRUE) {
  ccm <- list()
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
  ccm
}

#' Create a completely unconstrained CCM from parameter values
#'
#' All entries in the CCM (cross-consistency matrix) will be \code{TRUE}.
#'
#' @inheritParams morphfield
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
#' @inheritParams morphfield
#' @export
buildCCMFromSpecificConfigurations <- function(param_values,
                                               specific_configurations) {
  # Old code was wrong (does not include value combinations on same hierarchy):
  ccm <- initializeCCM(param_values, def_val = FALSE) # first set all combinations to inconsistent
  # Then, set only valid combinations consistent:
  lapply(names(specific_configurations), function(param1) {
    lapply(names(specific_configurations[[param1]]), function(value1) {
      configs <- specific_configurations[[param1]][[value1]]
      # add relations from higher to lower hierarchy
      lapply(names(configs), function(param2) {
        lapply(configs[[param2]], function(value2) {
          ccm[[buildHashValue(param1, value1, param2, value2)]] <<- TRUE
        })
      })
      # add relations within the same (lower) hierarchy, compare with all items of higher index
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
    })
  })
  ccm
}
