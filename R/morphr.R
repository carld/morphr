#' Create an HTML morphological field widget using the DataTables library
#'
#' This function creates an HTML widget to display a morphological field (a kind
#' of table where each column represents the possible configurations of a
#' certain parameter) using the JavaScript library DataTables.
#'
#' The \code{param_values} are the only required argument to this function. They
#' contain the column-wise elements of the morphological field in form of a
#' named list. The names are the column headers (names of the parameters of the
#' field). The values are vectors/lists with the character strings that
#' represent the possible values that the parameter can take on.
#'
#' There are two ways to constrain the possible configurations, i.e. parameter
#' value combinations, of the morphological field. Number one is to provide a
#' \emph{cross-consistency matrix (CCM)}, which lists the validity status of all
#' pair-wise parameter combinations (see below for details). Number two is to
#' provide a list of \emph{specific configurations}, which instead of listing
#' all value combinations lists only the valid configurations. Internally, the
#' specific configurations are converted to a CCM as well.
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
#' With \emph{specific configurations}, one can define one or more parameter
#' columns to \emph{specify} the field. This means that choosing a value in the
#' specifying column sets the values of some or all other columns. It constrains
#' the field. The \code{specific_configurations} are expected to be a named list
#' of named lists of named lists. The top-level hierarchy names represent the
#' parameter names that are specifying. The next level names represent the
#' parameter's values that are specifying. The deepest level names represent the
#' parameter names that are specified and the values (list elements) are the
#' parameter value(s) that are specified (i.e. considered possible) in this
#' configuration. Defaults to NULL, i.e. no specification and the field is
#' \emph{open}.
#'
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


determineCells <- function(field_df, specific_configurations, row, col) {
  col_names <- names(field_df)
  col_name <- col_names[col + 1] # + 1 needed because JavaScript starts counting at 0
  determined_cells <- NULL
  if (col_name %in% names(specific_configurations)) {
    row_name <- field_df[row, col + 1] # + 1 see above
    if (row_name %in% names(specific_configurations[[col_name]])) {
      determined_col_names <- names(specific_configurations[[col_name]][[row_name]])
      determined_cols <- match(determined_col_names, col_names)
      determined_row_names <- specific_configurations[[col_name]][[row_name]]
      determined_rows <- sapply(determined_col_names, function(c) {
        match(determined_row_names[[c]], field_df[[c]])
      })
      determined_cells <- matrix(
        c(determined_rows, determined_cols - 1), # - 1 needed because JavaScript starts counting at 0
        ncol = 2
      )
    }
  }
  determined_cells
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


#' @export
buildUnconstrainedCCM <- function(param_values) {
  initializeCCM(param_values, def_val = TRUE)
}


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
