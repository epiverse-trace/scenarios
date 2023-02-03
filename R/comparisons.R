#' Constructor for the `comparison` class
#'
#' @description Create a `comparison` object after input checks.
#'
#' @param data A list of `scenario` objects.
#' @param baseline A string for the element of the list of `scenario`
#' objects which indicates which should be considered the 'baseline' outcome,
#' against which other outcomes are compared.
#' @param match_variables The variables in the `scenario` outputs on which to
#' match the scenarios and check whether they are comparable.
#' @param comparison_variables The variables in the `scenario` outputs to
#' compare against the 'baseline' scenario.
#'
#' @return A `comparison` object
#' @keywords internal
new_comparison <- function(data,
                           baseline,
                           match_variables,
                           comparison_variables) {
  # Input checking in `comparison()`

  # create and return comparison class
  structure(
    list(
      "data" = data,
      "baseline" = baseline,
      "match_variables" = match_variables,
      "comparison_variables" = comparison_variables
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
#' @param ... Multiple `scenario`s or a list of `scenario` objects. At least one
#' of these scenarios, the 'baseline' scenario, must be named for the comparison
#' to be correctly constructed. The baseline scenario name must be the same as
#' passed to `baseline`.
#' @param baseline A string for the element of the list of `scenario`
#' objects which indicates which should be considered the 'baseline' outcome,
#' against which other outcomes are compared.
#' @param match_variables The variables in the `scenario` outputs on which to
#' match the scenarios and check whether they are comparable.
#' @param comparison_variables The variables in the `scenario` outputs to
#' compare against the 'baseline' scenario.
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
#'
#' # pass scenario objects as a list
#' # create a comparison object
#' comparison(
#'   list(pandemic_flu = pandemic_flu, covid19 = covid19),
#'   baseline = "pandemic_flu"
#' )
comparison <- function(...,
                       baseline,
                       match_variables,
                       comparison_variables) {
  # check input
  data <- list(...)
  if ((length(data) == 1L) && (is.list(data[[1]])) &&
    (!is_scenario(data[[1]]))) {
    data <- data[[1]]
  }
  if (missing(match_variables)) {
    match_variables <- NA_character_
  }
  if (missing(comparison_variables)) {
    comparison_variables <- NA_character_
  }

  stopifnot(
    "All objects must be of the `scenario` class" =
      all(
        vapply(
          data, is_scenario,
          FUN.VALUE = TRUE
        )
      ),
    "Baseline must be among scenario names" =
      (is.character(baseline) && baseline %in% names(data)),
    "Matching variables must be a string" =
      (is.character(match_variables)),
    "Comparison variables must be a string" =
      (is.character(comparison_variables))
  )

  # call comparison constructor
  object <- new_comparison(
    data, baseline, match_variables, comparison_variables
  )

  # call comparison validator
  validate_comparison(object)

  # return comparison object
  object
}

#' Validator for the `comparison` class
#'
#' @param object A `comparison` object.
#'
#' @return None. Errors when an invalid `comparison` object is provided.
validate_comparison <- function(object) {
  # check for class and class invariants
  stopifnot(
    "Object should be of class comparison" =
      (is_comparison(object)),
    "`comparison` object does not contain the correct attributes" =
      (all(
        c(
          "data", "baseline"
        ) %in% attributes(object)$names
      )
      ),
    "Comparison must be a list of `scenario` objects" =
      (is.list(object$data) && (
        all(
          vapply(
            object$data, is_scenario,
            FUN.VALUE = TRUE
          )
        )
      )),
    "Baseline must be among scenario names" =
      (is.character(object$baseline) &&
        object$baseline %in% names(object$data))
  )
  invisible(object)
}

#' @export
print.comparison <- function(x, ...) {
  # prepare information
  header <- cli::style_bold("Scenario comparison object")
  scenario_count <- glue::glue(" Number of scenarios: {length(x$data)}")
  data_status <- ifelse(
    !all(
      vapply(x$data, sce_has_data, FUN.VALUE = TRUE)
    ),
    cli::col_magenta(
      " Some scenarios have no data, use `run_scenario()` to prepare data"
    ),
    " All scenario data are prepared, use `sce_get_outcomes()` to get data"
  )
  baseline <- glue::glue(" Baseline scenario: {cli::col_blue(x$baseline)}")
  # the scenario matching variables
  matching_variables <- c(
    " Scenario matching variables:",
    ifelse(is.na(x$match_variables),
      cli::col_magenta("  No matching variables specified!"),
      cli::col_green(glue::glue("  {x$match_variables}"))
    )
  )
  # the output comparison variables
  comparison_variables <- c(
    " Scenario comparison variables:",
    ifelse(is.na(x$comparison_variables),
      cli::col_magenta("  No comparison variables specified!"),
      cli::col_green(glue::glue("  {x$comparison_variables}"))
    )
  )
  # the model function(s)
  model_fun <- c(
    " Model functions found:",
    cli::col_cyan(
      glue::glue("  {unique(unlist(lapply(x$data, `[`, 'model_function')))}")
    )
  )
  # output all
  writeLines(
    c(
      header, scenario_count, baseline, data_status,
      matching_variables, comparison_variables, model_fun
    )
  )
  invisible(x)
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
#' x <- comparison(
#'   pandemic_flu = pandemic_flu, covid19 = covid19,
#'   baseline = "pandemic_flu"
#' )
#'
#' is_comparison(x)
is_comparison <- function(x) {
  inherits(x, "comparison")
}
