#' Construct a `scenario` object
#'
#' @description Create a scenario object with input checks.
#'
#' @param data Any object that can be sensibly coerced to a `data.table`,
#' representing the epidemiological model data.
#' @param name String of the senario name. Defaults to `NA` if not provided.
#' @param model String of the model name. Defaults to `NA` if not provided.
#' @param parameters Named list of the model parameters. Only atomic list
#' elements are allowed.
#' @param extra_info Extra model information that may be useful for matching
#' models. Only atomic list elements are supported.
#'
#' @return A `scenario` object.
#' @keywords internal
new_scenario <- function(data,
                         name = NA_character_,
                         model = NA_character_,
                         parameters = list(),
                         extra_info = list()) {
  # Input checking in `scenario()`

  # create and return scenario class
  structure(
    data.table::as.data.table(data),
    class = c("scenario", "data.table", "data.frame"),
    name = name,
    model = model,
    parameters = parameters,
    extra_info = extra_info
  )
}

#' Create a `scenario` object
#'
#' @description The `scenario` class is intended to store the outcomes of an
#' epidemic simulation.
#'
#' @param data Any object that can be sensibly coerced to a `data.table`,
#' representing the epidemiological model data.
#' @param name String of the senario name. Defaults to `NA` if not provided.
#' @param model String of the model name. Defaults to `NA` if not provided.
#' @param parameters Named list of the model parameters. Only atomic list
#' elements are allowed.
#' @param extra_info Extra model information that may be useful for matching
#' models. Only atomic list elements are supported.
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
#' data <- do.call(
#'   finalsize::final_size,
#'   pandemic_flu_args
#' )
#'
#' scenario(
#'   data = data,
#'   name = "final_size_UK",
#'   model = "final_size",
#'   parameters = list(
#'     R0 = 1.5
#'   ),
#'   extra_info = list(
#'     n_age_groups = 3
#'   )
#' )
scenario <- function(data = data.table::data.table(),
                     name = NA_character_,
                     model = NA_character_,
                     parameters = list(),
                     extra_info = list()) {
  # check input
  checkmate::assert_string(name, na.ok = TRUE)
  checkmate::assert_string(model)
  checkmate::assert_list(
    parameters,
    all.missing = FALSE, any.missing = FALSE,
    min.len = 1, names = "unique", null.ok = FALSE
  )
  checkmate::assert_list(extra_info, any.missing = FALSE, names = "unique")

  # call scenario constructor
  scenario <- new_scenario(
    data = data.table::as.data.table(data),
    name = name,
    model = model,
    parameters = parameters,
    extra_info = extra_info
  )

  # call scenario validator
  validate_scenario(object = scenario)

  # return scenario object
  scenario
}

#' Validate a `scenario` class object
#'
#' @param object A `scenario` object
#'
#' @return None. Checks the validity of the provided `scenario` object.
validate_scenario <- function(object) {
  # check for class and class invariants
  stopifnot(
    "Object should be of class scenario" =
      (is_scenario(object)),
    "`scenario` does not contain the correct attributes" =
      (c(
        "name", "model", "parameters",
        "extra_info"
      ) %in% names(attributes(object))),
    "Scenario name must be a single string" =
      (checkmate::test_string(attributes(object)$name, na.ok = TRUE)),
    "Model name must be a single function name" =
      (is.character(attributes(object)$model)),
    "Model parameter list must be a named, non-empty list with no NULLs" =
      (checkmate::test_list(
        attributes(object)$parameters,
        all.missing = FALSE, any.missing = FALSE,
        min.len = 0, names = "unique", null.ok = FALSE
      )
      ),
    "Extra information must be a list" =
      (is.list(attributes(object)$extra_info))
  )
  invisible(object)
}

#' Print a `scenario` object
#'
#' @param x A `scenario` object.
#' @param ... Other parameters passed to [print()].
#' @noRd
#' @export
print.scenario <- function(x, ...) {
  format(x, ...)
}

#' Format a `scenario` object
#'
#' @param x A `scenario` object.
#' @param ... Other arguments passed to [format()].
#'
#' @return None. Formats the `scenario` for printing.
#' @keywords internal
#' @noRd
format.scenario <- function(x, ...) {

  # header
  header <- cli::style_bold("Epidemic scenario object")

  # collect information on name
  name <- ifelse(
    is.na(attr(x, "sce_name")),
    "No name specified (NA)",
    glue::double_quote(attr(x, "sce_name"))
  )
  name <- glue::glue("Scenario name: {cli::col_cyan(name)}")

  # information on model
  model <- ifelse(
    is.na(attr(x, "model")),
    cli::col_grey("No model specified (NA)"),
    glue::glue(
      "Model: {cli::col_cyan(glue::double_quote(attr(x, 'model')))}"
    )
  )

  parameters <- names(attr(x, "parameters"))
  if (is.null(parameters)) {
    parameters <- "None specified"
  } else {
    parameters <- glue::glue_collapse(
      cli::col_green(
        glue::double_quote(parameters)
      ),
      sep = ", "
    )
    parameters <- glue::glue("{parameters}")
  }
  parameters <- glue::glue("Parameters: {parameters}")

  # extra information
  extra_info <- names(attr(x, "extra_info"))
  if (is.null(extra_info)) {
    extra_info <- cli::col_grey("None")
  } else {
    extra_info <- glue::glue_collapse(
      cli::col_green(
        glue::double_quote(extra_info)
      ),
      sep = ", "
    )
  }
  extra_info <- glue::glue("Extra information: {extra_info}")

  # print header to screen
  writeLines(
    c(
      header,
      name,
      model,
      parameters,
      extra_info,
      glue::glue(
        "

        <Head of scenario data>
        "
      )
    )
  )
  # print head of data
  print(utils::head(data.table::as.data.table(x)))

  # calculate remaining rows and cols and
  remaining_rows <- max(0, nrow(x) - 5)
  writeLines(
    glue::glue(
      "<{remaining_rows} more rows in data>"
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
