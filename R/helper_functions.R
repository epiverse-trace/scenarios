#' Print scenario parameters
#'
#' @param x A [scenarios::scenario()] object.
#' @param which Which parameters to print.
#'
#' @return Nothing. Prints a list of parameters to screen.
#' @export
#'
#' @examples
#' # create a scenario
#' scenario_pandemic_flu <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(),
#'   replicates = 1
#' )
#'
#' # get all parameters
#' sce_get_parameters(scenario_pandemic_flu)
#'
#' # get only some parameters
#' sce_get_parameters(scenario_pandemic_flu, which = c("r0", "solver"))
sce_get_parameters <- function(x, which = NULL) {
  # check input
  checkmate::assert_class(x, "scenario")

  # print/return chosen parameters as list
  if (is.null(which)) {
    x$parameters
  } else {
    x$parameters[which]
  }
}

#' Check for scenario data
#'
#' @param x A `scenario` object.
#'
#' @return A boolean, whether the simulation object has data.
#' @export
#' @examples
#' # create a scenario
#' scenario_pandemic_flu <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(),
#'   replicates = 1
#' )
#'
#' sce_has_data(scenario_pandemic_flu)
sce_has_data <- function(x) {
  # check input
  checkmate::assert_class(x, "scenario")

  !all(vapply(x$data$output, is.null, FUN.VALUE = TRUE))
}

#' Get scenario outcomes
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
#' run_scenario(scenario_pandemic_flu)
#'
#' # get outcomes
#' sce_get_outcomes(scenario_pandemic_flu)
sce_get_outcomes <- function(x) {
  # check input
  checkmate::assert_class(x, "scenario")

  stopifnot(
    "Scenario data are not prepared, run `run_scenario()` to prepare data." =
      sce_has_data(x)
  )
  if (!is.data.frame(data.table::first(x$data$output))) {
    warning(
      "Scenario model outputs are not `data.frames`."
    )
  }

  output <- "output"
  x$data[, unlist(output, recursive = FALSE), by = "replicate"]
}

#' Get scenario outcome names
#'
#' Function to quickly view the names and types of columns in the `scenario`
#' outcome data. Operates on the first replicate of each scenario run, and
#' assumes that the outcome data are data.frames.
#'
#' @param x A scenario object with data prepared. Check for whether data has
#' been prepared using [sce_has_data()].
#' @param view_rows Whether to return the first few rows of the first replicate
#' outcome, using [head()].
#'
#' @return Prints the scenario outcome data's column names and types to screen,
#' or the [head()] of the first outcome replicate if `view_rows = TRUE`.
#' @export
#'
#' @examples
#' # create a scenario
#' scenario_pandemic_flu <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(),
#'   replicates = 1
#' )
#' run_scenario(scenario_pandemic_flu)
#'
#' # for column names and types
#' sce_peek_outcomes(scenario_pandemic_flu)
#' # for data.frame head
#' sce_peek_outcomes(scenario_pandemic_flu, view_rows = FALSE)
sce_peek_outcomes <- function(x, view_rows = FALSE) {
  # check input
  checkmate::assert_class(x, "scenario")
  stopifnot(
    "Scenario data are not prepared, run `run_scenario()` to prepare data." =
      sce_has_data(x)
  )
  checkmate::assert_data_frame(data.table::first(x$data$output))

  if (view_rows) {
    utils::head(data.table::first(x$data$output))
  } else {
    vapply(data.table::first(x$data$output), class, FUN.VALUE = "numeric")
  }
}

#' Aggregate scenario outcomes across replicates
#'
#' @param x A scenario object with data prepared.
#' @param grouping_variables The variables that should be used to group the
#' outcomes of interest. Examples include demographic and susceptibility groups.
#' @param measure_variables The outcomes of interest which are summarised over
#' scenario replicates, and by all variables in the `grouping_variables`.
#' Examples include `p_infected` from the output of [finalsize::final_size()].
#' @param summary_functions The summary function names to apply to the measure
#' variables, passed as strings, i.e., "mean" rather than simply `mean`.
#'
#' @return A data.table with the outcomes of interest (measure variables)
#' summarised using the summary functions, by each grouping variable.
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
#' run_scenario(scenario_pandemic_flu)
#'
#' # peek at outcome to see column names
#' sce_peek_outcomes(scenario_pandemic_flu)
#'
#' # aggregate outcome by demographic group
#' sce_aggregate_outcomes(
#'   x = scenario_pandemic_flu,
#'   grouping_variables = c("demo_grp"),
#'   measure_variables = c("p_infected"),
#'   summary_functions = c("mean", "min", "max")
#' )
sce_aggregate_outcomes <- function(x, grouping_variables, measure_variables,
                                   summary_functions = c("mean", "sd")) {
  # check input
  checkmate::assert_class(x, "scenario")

  stopifnot(
    "Scenario data are not prepared, run `run_scenario()` to prepare data." =
      sce_has_data(x)
  )

  # prepare cast formula
  formula <- eval(
    paste(
      # grouping variables reduced to: var1 + var2 + ...
      Reduce(
        f = function(x1, x2) paste(x1, x2, sep = " + "),
        x = grouping_variables
      ),
      # a tilde
      " ~ ."
    )
  )

  # return aggregated data.table
  dt <- data.table::dcast(
    data = sce_get_outcomes(x),
    formula = formula,
    value.var = measure_variables,
    fun.aggregate = lapply(summary_functions, as.symbol)
  )

  # fix malformed names from dcast
  if (length(measure_variables) == 1 && length(summary_functions) == 1) {
    # fix names when only a single column is cast
    data.table::setnames(dt, ".", sprintf(
      "%s_%s", measure_variables, summary_functions
    ))
  }

  # return data.table
  dt
}
