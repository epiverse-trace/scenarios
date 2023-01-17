#' Get scenario outcomes
#'
#' @param x A `scenario` or `comparison` object with the scenario outcome data
#' prepared. Check for whether data has been prepared using [sce_has_data()].
#'
#' @return A single `data.table` holding the output of all replicates of the
#' scenario. If `x` is a `comparison` object, the `data.table` holds the
#' outcomes of each replicate of each scenario specified therein. Scenarios in
#' this case are identified either by their names if any, or by a simple
#' synthetic identifier such as 'scenario_01'.
#' @export
#'
#' @examples
sce_get_outcomes <- function(x) {
  UseMethod("sce_get_outcomes", x)
}

#' Get scenario outcomes from a scenario object
#'
#' @param x A scenario object with data prepared. Check for whether data has
#' been prepared using [sce_has_data()].
#'
#' @return A single data.table holding the output of all replicates of the
#' scenario. Contains the `replicate` column to help differentiate data from
#' each replicate.
#' @export
#'
#' @examples
#' # create a scenario
#' scenario_pandemic_flu <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(),
#'   replicates = 3 # note extra replicates
#' )
#'
#' # run scenario
#' scenario_pandemic_flu <- run_scenario(scenario_pandemic_flu)
#'
#' # get outcomes
#' sce_get_outcomes(scenario_pandemic_flu)
sce_get_outcomes.scenario <- function(x) {
  # check input
  checkmate::assert_class(x, "scenario")

  stopifnot(
    "Scenario data are not prepared, run `run_scenario()` to prepare data." =
      sce_has_data(x)
  )
  if (!is.data.frame(data.table::first(x$data))) {
    stop(
      "Scenario model outputs are not `data.frames`."
    )
  }

  data.table::rbindlist(x$data)
}


#' Get scenario outcomes from a comparison object
#'
#' @param x A `comparison` object with data prepared. Check for whether data has
#' been prepared using [sce_has_data()].
#'
#' @return A single data.table holding the outcomes of each replicate of each
#' scenario specified in `x`. Scenarios are identified either by their names if
#' any are provided in the `data` list in `x`, or by a simple synthetic
#' identifier such as 'scenario_01'.
#' @export
#'
#' @examples
sce_get_outcomes.comparison <- function(x) {
  # check input
  checkmate::assert_class(x, "comparison")

  stopifnot(
    "Scenario data are not prepared, run `run_scenario()` to prepare data." =
      sce_has_data(x)
  )
  data <- lapply(x$data, sce_get_outcomes.scenario)
  # check whether all outputs inherit from data.frames
  if (!all(vapply(data, is.data.frame, FUN.VALUE = TRUE))) {
    stop(
      "Scenario model outputs must all be or inherit from `data.frame`s."
    )
  }

  # check for scenario names in x$data and fix names if needed
  scenario_names <- names(x$data)
  # check if all are NULL and assign synthetic names
  if (all(vapply(scenario_names, is.null, FUN.VALUE = TRUE))) {
    scenario_names <- sprintf("scenario_%i", length(x$data))
  } else if (any(scenario_names == "")) {
    scenario_names[scenario_names == ""] <- sprintf(
      "scenario_%i", which(scenario_names == "")
    )
  }

  # assign names to data list
  data <- Map(
    data, scenario_names,
    f = function(df, name) {
      df$scenario_name <- name
      # return the data frame after assignment
      df
    }
  )

  data.table::rbindlist(data)
}
