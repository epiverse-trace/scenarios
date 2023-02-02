#' Constructor for the `scenario` class
#'
#' @description Create a scenario object with input checks.
#'
#' @param model_function Function that is expected to run an epidemic scenario
#' model, such as [finalsize::final_size()], as a string e.g.
#' "finalsize::final_size". Explicit namespacing is preferred.
#' @param parameters Parameters to the `model_function`.
#' @param extra_info Extra model information that may be useful for matching
#' models.
#' @param replicates The number of scenario replicates. This is the number of
#' times the model_function is run.
#'
#' @return A `scenario` object
#' @keywords internal
new_scenario <- function(model_function,
                         parameters,
                         extra_info = list(),
                         replicates = integer(1)) {
  # Input checking in `scenario()`

  # create and return scenario class
  structure(
    list(
      model_function = model_function,
      parameters = parameters,
      extra_info = extra_info,
      replicates = replicates,
      data = vector("list", length = replicates)
    ),
    class = "scenario"
  )
}

#' Create a `scenario` object
#'
#' @description The `scenario` class is intended to store the outcomes of a
#' number of runs of an epidemic simulation. This is a work in progress, and is
#' initially targeted for compatibility with outputs from
#' [finalsize::final_size()].
#'
#' @param model_function Function that is expected to run an epidemic scenario
#' model, such as [finalsize::final_size()], as a string e.g.
#' "finalsize::final_size". Explicit namespacing is preferred.
#' @param parameters Parameters to the `model_function`.
#' @param extra_info Extra information that is useful when comparing scenarios,
#' such as details of the population structure or infection characteristics.
#' @param replicates The number of scenario replicates. This is the number of
#' times the `model_function` is run.
#'
#' @return A `scenario` object
#' @export
#'
#' @examples
#' # prepare arguments to `finalsize::final_size()`
#' # using the included convenience function
#' pandemic_flu_args <- make_parameters_finalsize_UK(r0 = 1.5)
#'
#' # prepare extra information on age group limits
#' age_groups <- rownames(pandemic_flu_args$contact_matrix)
#'
#' scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = pandemic_flu_args,
#'   extra_info = list(
#'     age_groups = age_groups
#'   ),
#'   replicates = 1L
#' )
scenario <- function(model_function,
                     parameters,
                     extra_info = list(),
                     replicates = 1L) {
  # check input
  checkmate::assert_string(model_function)
  checkmate::assert_list(parameters, any.missing = FALSE, names = "unique")
  checkmate::assert_list(extra_info, any.missing = FALSE, names = "unique")
  checkmate::assert_integerish(replicates, lower = 1, null.ok = FALSE)

  if (!grepl(pattern = "::", x = model_function, fixed = TRUE)) {
    warning(
      glue::glue(
        "'model_function' may not be explicitly namespaced.
        Explicit namespacing is preferred to avoid confusion.
        E.g. 'finalsize::final_size' rather than 'final_size'."
      )
    )
  }

  # call scenario constructor
  scenario <- new_scenario(
    model_function = model_function,
    parameters = parameters,
    extra_info = extra_info,
    replicates = replicates
  )

  # call scenario validator
  validate_scenario(object = scenario)

  # return scenario object
  scenario
}

#' Validator for the `scenario` class
#'
#' @param object A `scenario` object
#' @param data_ok A boolean of whether the scenario can have data. This is
#' useful when creating scenarios manually from existing objects, or when
#' reading in a `scenario` with data from a file.
#'
#' @return None. Errors when an invalid `scenario` object is provided.
validate_scenario <- function(object, data_ok = FALSE) {
  # check for class and class invariants
  stopifnot(
    "Object should be of class scenario" =
      (is_scenario(object)),
    "scenario object does not contain the correct attributes" =
      (c(
        "model_function", "parameters", "extra_info", "replicates", "data"
      ) %in% attributes(object)$names),
    "Model function must be a single function name" =
      (is.character(object$model_function)),
    "Model parameter list must be a list" =
      (is.list(object$parameters)),
    "Extra information must be a list" =
      (is.list(object$extra_info)),
    "Model replicates must be at least 1" =
      (checkmate::check_integerish(object$replicates) &&
        object$replicates >= 1),
    "Scenario data must be the same length as the number of replicates" =
      (nrow(object$data) == object$replicates),
    "Scenario data list should not be initialised" =
      (data_ok || all(
        vapply(object$data, is.null, FUN.VALUE = TRUE)
      )
      )
  )
  invisible(object)
}

#' @export
print.scenario <- function(x, ...) {

  # collect information
  extra_info <- glue::glue_collapse(
    glue::glue("'{names(x$extra_info)}'"),
    sep = ", "
  )
  extra_info <- cli::col_cyan(extra_info)

  # print to screen
  writeLines(
    c(
      cli::style_bold("Epidemic scenario object"),
      glue::glue(" Model function: {cli::col_cyan(x$model_function)}"),
      glue::glue(" Extra information on: {extra_info}"),
      glue::glue(" Scenario replicates: {x$replicates}"),
      glue::glue(
        "
         Scenario outcomes are \\
        {ifelse(sce_has_data(x), 'prepared', 'not prepared')}
        "
      )
    )
  )

  invisible(x)
}

#' Check whether an object is a `scenario`
#'
#' @param x An R object.
#'
#' @return A logical indicating whether the object inherits from the class
#' `scenario`.
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
#' is_scenario(pandemic_flu)
is_scenario <- function(x) inherits(x, "scenario")
