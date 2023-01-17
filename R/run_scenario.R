#' Run a scenario
#'
#' @description Run an epidemic model scenario using the function stored in
#' `model_function` with the arguments in `parameters`. Runs as many replicates
#' of the function as specified in `replicates`. The simulation output data are
#' stored under `data`.
#'
#' @param x A `scenario` or `comparison` object.
#'
#' @return The original object with the `data` field populated with
#' simulation output. This object must be assigned.
#' Returns a `scenario` object when called on a `scenario` object as expected,
#' with the `data` field populated with epidemic simulation model outputs.
#' Returns a `comparison` object with the `data` fields of the `scenario`
#' objects contained in it populated.
#' @export
#'
#' @examples
run_scenario <- function(x) {
  UseMethod("run_scenario", x)
}

#' Run a scenario
#'
#' @description Run an epidemic model scenario using the function stored in
#' `model_function` with the arguments in `parameters`. Runs as many replicates
#' of the function as specified in `replicates`. The simulation output data are
#' stored under `data`.
#'
#' @param x A `scenario` object.
#'
#' @return The original `scenario` object with the `data` field populated with
#' simulation output. This object must be assigned.
#' @export
#'
#' @examples
#' # prepare a scenario
#' scenario_pandemic_flu <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(), # using helper function
#'   replicates = 1L
#' )
#'
#' # print to check that data are not prepared
#' scenario_pandemic_flu
#'
#' # generate scenario data
#' scenario_pandemic_flu <- run_scenario(scenario_pandemic_flu)
#'
#' # print to check that data are prepared
#' scenario_pandemic_flu
run_scenario.scenario <- function(x) {

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

  # populate the data list
  output <- lapply(
    seq_len(x$replicates),
    function(i) {
      data <- do.call(
        what = fn,
        args = x$parameters
      )
      data$replicate <- i
      data
    }
  )
  # assign output to data
  x$data <- output

  x
}

#' Run scenarios contained in a comparison
#'
#' @description Run the epidemic model scenarios specified as `scenario` obejcts
#' in a `comparison` object. Calls [run_scenario.scenario()] on each object in
#' the `data` list of the comparison object.
#'
#' @param x A `scenario` object.
#'
#' @return A `comparison` with the attributes of the original object passed to
#' the function, but with the `data` list containing `scenario` objects with
#' replicate outcomes.
#' @export
#'
#' @examples
run_scenario.comparison <- function(x) {

  # input checking
  checkmate::assert_class(x, "comparison")

  # assign output to data
  x$data <- lapply(
    X = x$data,
    FUN = run_scenario.scenario
  )

  x
}
