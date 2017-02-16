#' Create an HTML morphological field widget using the DataTables library
#'
#' This function creates an HTML widget to display a morphological field (a kind
#' of table where each column represents the possible configurations of a
#' certain parameter) using the JavaScript library DataTables.
#' @export
morphfield <- function(param_values, specific_configurations = NULL) {
  param_values <- paramValuesToDataFrame(param_values)

  field <- datatable(
    param_values,
    options = list(
      # Disable search/filter box:
      searching = FALSE,
      # Disable sorting by columns for all columns:
      columnDefs = list(
        list(orderable = FALSE, targets = 0:(ncol(param_values) - 1))
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
                  backgroundColor = "gray")
  }

  return(field)
}


#' Create a Fixed Dimension data.frame from Variable Length Lists of Morphological Field Parameter Values
#'
#' A morphological field can consist of parameters with variable number of
#' configurations. However, a DataTable accepts only a data.frame, which must
#' have the same number of rows in each column. Therefore, this function takes
#' a list of lists (each list representing one list of parameter configurations,
#' i.e. one column) and fills the empty cells with empty character strings to
#' create a fixed dimension data.frame.
paramValuesToDataFrame <- function(param_values) {
  if (class(param_values) == "list") {
    # Make sure that all list items have same length.
    # If not: fill with empty character strings
    max_length <- max(sapply(param_values, length))
    param_values <- lapply(param_values, function(li) {
      c(li, rep("", max_length - length(li)))
    })
    # Turn it into a data.frame, because that is what is expected by DT
    param_values <- as.data.frame(param_values)
  }
  param_values
}


determineCells <- function(param_values, specific_configurations, row, col) {
  col_names <- names(param_values)
  col_name <- col_names[col + 1] # + 1 needed because JavaScript starts counting at 0
  determined_cells <- NULL
  if (col_name %in% names(specific_configurations)) {
    row_name <- param_values[row, col + 1] # + 1 see above
    if (row_name %in% names(specific_configurations[[col_name]])) {
      determined_col_names <- names(specific_configurations[[col_name]][[row_name]])
      determined_cols <- match(determined_col_names, col_names)
      determined_row_names <- specific_configurations[[col_name]][[row_name]]
      determined_rows <- sapply(determined_col_names, function(c) {
        match(determined_row_names[[c]], param_values[[c]])
      })
      determined_cells <- matrix(
        c(row, determined_rows, col, determined_cols - 1), # - 1 needed because JavaScript starts counting at 0
        ncol = 2
      )
    }
  }
  determined_cells
}
