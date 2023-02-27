#' Construct a `comparison` object
#'
#' @description Create a new `comparison` object.
#'
#' @param data A `data.table` holding the outcomes of epidemic model runs.
#' @param n_scenarios Number giving the number of scenario objects ingested to
#' create the `comparison`.
#'
#' @return A `comparison` object
#' @noRd
#' @keywords internal
new_comparison <- function(data,
                           n_scenarios) {
  # Input checking in `comparison()`

  # create and return comparison class
  structure(
    list(
      data = data,
      baseline = NA_character_,
      matching_variables = NA_character_,
      comparison_variables = NA_character_,
      n_scenarios = n_scenarios
    ),
    class = "comparison"
  )
}

#' Create a `comparison` object
#'
#' @description The `comparison` class is intended to store `scenario` data and
#' to quickly run comparisons against a user-specified baseline.
#'
#' @param ... Multiple `scenario`s or a list of `scenario` objects.
#'
#' @return A `comparison` object.
#' @export
#'
#' @examples
comparison <- function(...) {
  # check input
  data <- list(...)
  if ((length(data) == 1L) && (is.list(data[[1]])) &&
    (!is_scenario(data[[1]]))) {
    data <- data[[1]]
    stopifnot(
      "All list objects must be of the `scenario` class" =
        all(
          vapply(
            data, is_scenario,
            FUN.VALUE = TRUE
          )
        )
    )
  } else if (length(data) == 1 && is_scenario(data[[1]])) {
    stop(
      "`comparison` must be passed at least two `scenario`s"
    )
  }

  # count scenarios
  n_scenarios <- length(data)

  # collect parameters and extra information
  parameters <- lapply(data, sce_get_information)

  # add to scenario data.tables
  data <- Map(
    data, parameters,
    f = function(df, params) {
      df <- data.table::as.data.table(df)
      df[, names(params) := params]
    }
  )

  # prepare single data.table
  data <- data.table::rbindlist(data, fill = TRUE) # allow filling

  # call comparison constructor
  object <- new_comparison(data = data, n_scenarios = n_scenarios)

  # call comparison validator
  validate_comparison(object)

  # return comparison object
  object
}

#' Validator for the `comparison` class
#'
#' @param object A `comparison` object.
#' @keywords internal
#' @noRd
#' @return None. Errors when an invalid `comparison` object is provided.
validate_comparison <- function(object) {
  # check for class and class invariants
  stopifnot(
    "Object should be of class comparison" =
      (is_comparison(object)),
    "`comparison` object does not contain the correct attributes" =
      (all(
        c(
          "data", "baseline", "matching_variables", "comparison_variables"
        ) %in% attributes(object)$names
      )
      ),
    "Comparison data must inherit from `data.table`" =
      (data.table::is.data.table(object$data)),
    "Baseline must be a character vector" =
      (is.character(object$baseline)),
    "Matching variables must be a character vector" =
      (is.character(object$matching_variables)),
    "Comparison variables must be a character vector" =
      (is.character(object$comparison_variables))
  )
  invisible(object)
}

#' @export
print.comparison <- function(x, ...) {
  # prepare header and scenario count
  header <- cli::style_bold("Scenario comparison object")
  scenario_count <- glue::glue("Number of scenarios: {x$n_scenarios}")

  # prepare baseline
  baseline <- ifelse(
    is.na(x$baseline),
    glue::glue("Basline: None specified (NA)"),
    glue::glue("Basline: {glue::double_quote(x$baseline)}")
  )

  # the scenario matching variables
  matching_variables <- c(
    "Matching variables:",
    ifelse(
      all(is.na(x$matching_variables)),
      cli::col_magenta("  None specified"),
      cli::col_green(
        glue::glue(
          "
            {glue::glue_collapse(glue::double_quote(x$matching_variables), \\
          sep = ', ')}
          "
        )
      )
    )
  )
  # the output comparison variables
  comparison_variables <- c(
    "Comparison variables:",
    ifelse(
      all(is.na(x$matching_variables)),
      cli::col_magenta("  None specified"),
      cli::col_green(
        glue::glue(
          "
            {glue::glue_collapse(glue::double_quote(x$comparison_variables), \\
          sep = ', ')}
          "
        )
      )
    )
  )

  # output all
  writeLines(
    c(
      header, scenario_count, baseline,
      matching_variables, comparison_variables
    )
  )
}

#' Check whether an object is a `comparison`
#'
#' @param x An R object.
#'
#' @return A logical indicating whether the object inherits from the class
#' `comparison`.
#'
#' @export
#'
#' @examples
is_comparison <- function(x) {
  inherits(x, "comparison")
}

#' Filter `comparison` data by key-values
#'
#' @param x A `comparison` object.
#' @param matching_variables Named list of the key-value pairs by which to
#' filter the `comparison` data. The list-element names are used to key the data
#' and the list-element values are used to filter the data.
#'
#' @return A `comparison` object.
#' @export
#'
#' @examples
sce_filter_comparison <- function(x, matching_variables) {
  checkmate::assert_class(x, "comparison")
  checkmate::assert_list(
    matching_variables,
    min.len = 1,
    unique = TRUE, names = "unique"
  )

  # get key variable names
  keyvars <- names(matching_variables)

  # check that these exist in the data
  stopifnot(
    "All `matching_variables` must exist in the data" =
      all(keyvars %in% colnames(x$data))
  )

  # key the data by the matching variables and filter for key value
  # can be done in one step but make clearer
  data.table::setkeyv(x$data, keyvars)
  x$data <- x$data[matching_variables, ]

  # check if any data remain
  if (nrow(x$data) == 0) {
    warning(
      "No data remaining! Please check matching variables."
    )
  }

  # return the filtered scenario
  x
}
