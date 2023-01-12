#' Constructor for the `scenario` class
#'
#' @description Create a scenario object check inputs.
#'
#' @param model_function Function that is expected to run an epidemic scenario
#' model, such as [finalsize::final_size()], as a string e.g.
#' "finalsize::final_size". Explicit namespacing is preferred.
#' @param parameters Parameters to the `model_function`.
#' @param replicates The number of scenario replicates. This is the number of
#' times the `model_function` is run.
#'
#' @return scenario object
#' @keywords internal
new_scenario <- function(model_function,
                         parameters = list(),
                         replicates = integer(1)) {
  # Input checking in `scenario()`

  # create and return epidist class
  structure(
    list(
      model_function = model_function,
      parameters = parameters,
      replicates = replicates,
      data = data.table::data.table(
        replicate = seq_len(replicates),
        output = vector("list", length = replicates)
      )
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
#' @param replicates The number of scenario replicates. This is the number of
#' times the `model_function` is run.
#'
#' @return A `scenario` object
#' @export
#'
#' @examples
#' # prepare arguments to `finalsize::final_size()`
#' pandemic_flu_args <- make_parameters_finalsize_UK(r0 = 1.5)
#'
#' scenarios::scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = pandemic_flu_args,
#'   replicates = 1L
#' )
scenario <- function(model_function,
                     parameters = list(),
                     replicates = integer(1)) {
  # check input
  checkmate::assert_string(model_function)
  checkmate::assert_list(parameters, any.missing = FALSE, names = "unique")
  checkmate::assert_integerish(replicates, lower = 1, null.ok = FALSE)

  if (!grepl(pattern = "::", x = model_function, fixed = TRUE)) {
    warning(
      "`model_function` may not be explicitly namespaced. ",
      "Explicit namespacing is preferred to avoid confusion. ",
      "E.g. 'finalsize::final_size' rather than 'final_size'."
    )
  }

  # call scenario constructor
  scenario <- new_scenario(
    model_function = model_function,
    parameters = parameters,
    replicates = replicates
  )

  # call scenario validator
  validate_scenario(scenario = scenario)

  # return epidist object
  scenario
}

#' Validator for the `scenario` class
#'
#' @param scenario A `scenario` object
#' @param data_ok A boolean of whether the scenario can have data. This is
#' useful when creating scenarios manually from existing objects, or when
#' reading in a `scenario` with data from a file.
#'
#' @return None. Errors when an invalid `scenario` object is provided.
validate_scenario <- function(scenario, data_ok = FALSE) {
  # check for class and class invariants
  stopifnot(
    "Object should be of class scenario" =
      (inherits(scenario, "scenario")),
    "scenario object does not contain the correct attributes" =
      (c(
        "model_function", "parameters", "replicates", "data"
      ) %in% attributes(scenario)$names),
    "Model function must be a single function name" =
      (is.character(scenario$model_function)),
    "Model parameter list must be a list" =
      (is.list(scenario$parameters)),
    "Model replicates must be at least 1" =
      (checkmate::check_integerish(scenario$replicates) &&
        scenario$replicates >= 1),
    "Scenario data must be the same length as the number of replicates" =
      (nrow(scenario$data) == scenario$replicates),
    "Scenario data list should not be initialised" =
      (data_ok || all(
        vapply(scenario$data$output, is.null, FUN.VALUE = TRUE)
      )
      )
  )
  invisible(scenario)
}

#' @export
print.scenario <- function(x, ...) {
  writeLines(
    c(
      sprintf("Epidemic scenario object"),
      sprintf(" Model function: %s", x$model_function),
      sprintf(" Scenario replicates: %s", x$replicates),
      sprintf(" Scenario outcomes are %s", ifelse(
        sce_has_data(x), "prepared", "not prepared"
      ))
    )
  )

  invisible(x)
}

#' Run a scenario
#'
#' @description Run an epidemic model scenario using the function stored in
#' `model_function` with the arguments in `parameters`. Runs as many replicates
#' of the function as specified in `replicates`. The simulation output data are
#' stored under `data`, and the `data_available` tag is updated to `TRUE`.
#'
#' @param x A `scenario` object.
#'
#' @return The original `scenario` object with the `data` field populated with
#' simulation output. This object must be assigned.
#' @export
#'
#' @examples
#' # prepare a scenario
#' scenario_pandemic_flu <- scenarios::scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(), # using helper function
#'   replicates = 1L
#' )
#'
#' # print to check that data are not prepared
#' scenario_pandemic_flu
#'
#' # generate scenario data
#' # NOTE that no assignment is required
#' run_scenario(scenario_pandemic_flu)
#'
#' # print to check that data are prepared
#' scenario_pandemic_flu
run_scenario <- function(x) {

  # input checking
  checkmate::assert_class(x, "scenario")

  # get model function whether explicitly namespaced or not
  if (grepl(pattern = "::", x = x$model_function, fixed = TRUE)) {
    pkg_name <- sub("::.*", "", x$model_function)
    fn_name <- sub(".*::", "", x$model_function)
    fn <- get(fn_name, asNamespace(pkg_name))
  } else {
    fn <- x$model_function
  }

  # populate the data list and mark data as prepared
  x$data[, "output" := lapply(
    seq_len(x$replicates),
    function(i) {
      do.call(
        what = fn,
        args = x$parameters
      )
    }
  )]
}
