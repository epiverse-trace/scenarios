#' Constructor for the `comparisons` class
#'
#' @description Create a comparisons object after input checks.
#'
#' @param data A list of `scenario` objects.
#' @param baseline A string or integer for the element of the list of `scenario`
#' objects which indicates which should be considered the 'baseline' outcome,
#' against which other outcomes are compared.
#' @param matching_vars The variables in the `scenario` outputs on which to
#' match the scenarios and check whether they are comparable.
#' @param comparison_vars The variables in the `scenario` outputs to compare
#' against the 'baseline' scenario.
#'
#' @return A `comparison` object
#' @keywords internal
new_comparison <- function(data,
                           baseline = 1,
                           matching_vars,
                           comparison_vars) {
  # Input checking in `comparison()`

  # create and return comparison class
  structure(
    list(
      "data" = data,
      "baseline" = baseline,
      "matching_vars" = matching_vars,
      "comparison_vars" = comparison_vars
    ),
    class = "comparison"
  )
}

#' Create a `comparison` object
#'
#' @description The `comparison` class is intended to store `scenario`s and to
#' compare among them. One `scenario` must be set as the 'baseline' for such
#' comparisons.
#'
#' @param ... Multiple `scenario`s or a list of `scenario` objects.
#' @param baseline A string or integer for the element of the list of `scenario`
#' objects which indicates which should be considered the 'baseline' outcome,
#' against which other outcomes are compared. Taken to be the first element from
#' among the `scenario` objects provided.
#' @param matching_vars The variables in the `scenario` outputs on which to
#' match the scenarios and check whether they are comparable.
#' @param comparison_vars The variables in the `scenario` outputs to compare
#' against the 'baseline' scenario.
#'
#' @return A `comparison` object
#' @export
#'
#' @examples
#' # prepare two scenarios of the final size of an epidemic
#' pandemic_flu <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(r0 = 1.5),
#'   replicates = 1L
#' )
#'
#' covid19 <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(r0 = 5.0),
#'   replicates = 1L
#' )
#'
#' # create a comparison object
#' comparison(
#'   pandemic_flu = pandemic_flu, covid19 = covid19,
#'   baseline = "pandemic_flu"
#' )
comparison <- function(...,
                       baseline = 1L,
                       matching_vars,
                       comparison_vars) {
  # check input
  data <- list(...)
  if ((length(data) == 1L) && (is.list(data[[1]]))) {
    data <- data[[1]]
  }

  stopifnot(
    "All objects must be of the `scenario` class" =
      all(
        vapply(
          data, function(x) inherits(x, "scenario"),
          FUN.VALUE = TRUE
        )
      ),
    "Baseline must be among scenario names, or a number >= 1" =
      ((is.character(baseline) && baseline %in% names(data)) ||
        (checkmate::check_integerish(baseline))),
    "Baseline specified by list position must be within list elements" =
      (checkmate::check_integerish(baseline) && baseline <= length(data))
  )

  # call comparison constructor
  object <- new_comparison(
    data, baseline, matching_vars, comparison_vars
  )

  # call comparison validator
  validate_comparison(comparison = object)

  # return comparison object
  object
}

#' Validator for the `comparison` class
#'
#' @param comparison A `comparison` object.
#'
#' @return None. Errors when an invalid `comparison` object is provided.
validate_comparison <- function(comparison) {
  # check for class and class invariants
  stopifnot(
    "Object should be of class comparison" =
      (inherits(comparison, "comparison")),
    "comparison object does not contain the correct attributes" =
      (all(
        c(
          "data", "baseline"
        ) %in% attributes(comparison)$names
      )
      ),
    "Comparison must be a list of `scenario` objects" =
      (is.list(comparison$data) && (
        all(
          vapply(
            comparison$data, function(x) inherits(x, "scenario"),
            FUN.VALUE = TRUE
          )
        )
      )),
    "Baseline must be among scenario names, or a number >= 1" =
      ((is.character(comparison$baseline) &&
        comparison$baseline %in% names(comparison$data)) ||
        (checkmate::check_integerish(comparison$baseline))),
    "Baseline specified by list position must be within list elements" =
      (checkmate::check_integerish(comparison$baseline) &&
        (comparison$baseline <= length(comparison$data)))
  )
  invisible(comparison)
}

#' @export
print.comparison <- function(x, ...) {
  writeLines(
    c(
      cli::style_bold(sprintf("Scenario comparison object")),
      sprintf(" # Scenarios: %i", length(x$data)),
      ifelse(
        !all(
          vapply(x$data, sce_has_data, FUN.VALUE = TRUE)
        ),
        cli::col_magenta(
          " Some scenarios have no data, use `run_scenario()` to prepare data"
        ),
        "All scenario data are prepared, use `sce_get_outcomes()` to get data"
      ),
      " Scenario matching variables:",
      cli::col_green(sprintf("  %s", x$matching_vars)),
      " Scenario comparison variables:",
      cli::col_blue(sprintf("  %s", x$comparison_vars)),
      " Model functions found:",
      cli::col_cyan(
        sprintf("  %s", unique(unlist(lapply(x$data, `[`, "model_function"))))
      )
    )
  )
  invisible(x)
}
