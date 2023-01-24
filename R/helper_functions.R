#' Get scenario information
#'
#' @description Gets the value of one or more object in the `parameters` or the
#' extra information list `extra_info`.
#' @param x A 'scenario' object.
#' @param which Which parameters to print.
#'
#' @return A named list with two elements, 'parameters' and 'extra_info', which
#' are themselves lists. Each of these lists has named elements corresponding to
#' the names passed in `which`. These are separated into two lists to make it
#' easier to identify whether they are model function arguments or extra
#' information for the scenario.
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
#' sce_get_information(scenario_pandemic_flu)
#'
#' # get only some parameters
#' sce_get_information(scenario_pandemic_flu, which = c("r0", "solver"))
sce_get_information <- function(x, which) {
  # check input
  stopifnot(
    "Input 'x' must be a 'scenario' object" =
      is.scenario(x)
  )

  # print/return chosen parameters as list
  if (missing(which)) {
    list(
      model_parameters = x$parameters,
      scenario_information = x$extra_info
    )
  } else {
    info_list <- c(
      x$parameters[which],
      x$extra_info[which]
    )
    info_list <- Filter(function(x) !is.null(x), info_list)
    if (length(info_list) == 0L) {
      stop(
        glue::glue(
          "
          '{which}' not found among scenario model parameters or extra \\
          information
          "
        )
      )
    } else {
      info_list
    }
  }
}

#' Add extra information to a scenario
#'
#' @param x A 'scenario' object.
#' @param info A named list of information to be added to the `extra_info` list
#' of the scenario object `x`.
#'
#' @return The scenario `x` with the extra information added.
#' @export
#'
#' @examples
#' # get some parameters for a `finalsize` run
#' parameters <- make_parameters_finalsize_UK(r0 = 1.5)
#' extra_info <- list(
#'   age_groups = rownames(parameters$contact_matrix)
#' )
#' x <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = parameters
#' )
#'
#' sce_add_info(x, extra_info)
#'
sce_add_info <- function(x, info) {
  # check input
  stopifnot(
    "Input 'x' must be a 'scenario' object" =
      is.scenario(x),
    "Input 'info' must be a list with unique names" =
      checkmate::test_list(info, any.missing = FALSE, names = "unique"),
    # check for names already present in extra info
    "Some input list elements in 'info' are already present in this scenario" =
      (!any(names(info) %in% names(x$extra_info)))
  )

  # add data
  x$extra_info <- c(x$extra_info, info)

  # validate scenario and return
  validate_scenario(x)
  x
}

#' Check for scenario data
#'
#' @param x A 'scenario' or 'comparison' object.
#' @return Whether the 'scenario' has data, or whether all 'scenario' objects in
#' a 'comparison' object have data generated.
#' @export
#' @examples
#' # create a scenario
#' pandemic_flu <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(),
#'   replicates = 1
#' )
#' covid19 <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(r0 = 3.0),
#'   replicates = 1
#' )
#'
#' # for a 'scenario' object
#' sce_has_data(pandemic_flu)
#'
#' # for a 'comparison' object
#' comparison_flu_covid <- comparison(
#'   pandemic_flu = pandemic_flu, covid19 = covid19,
#'   baseline = "pandemic_flu"
#' )
#' sce_has_data(comparison_flu_covid)
sce_has_data <- function(x) {
  UseMethod("sce_has_data", x)
}

#' Check for scenario data
#'
#' @param x A 'scenario' object.
#'
#' @return A boolean, whether the simulation object has data.
#' @method sce_has_data scenario
#' @export
sce_has_data.scenario <- function(x) {
  # check input
  checkmate::assert_class(x, "scenario")

  !all(vapply(x$data, is.null, FUN.VALUE = TRUE))
}

#' Check for scenario data
#'
#' @param x A 'comparison' object.
#'
#' @method sce_has_data comparison
#' @export
#' @return A boolean, whether the simulation object has data.
sce_has_data.comparison <- function(x) {
  # check input
  checkmate::assert_class(x, "comparison")

  all(vapply(x$data, sce_has_data.scenario, FUN.VALUE = TRUE))
}

#' Get scenario outcome names
#'
#' Function to quickly view the names and types of columns in the 'scenario'
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
#' scenario_pandemic_flu <- run_scenario(scenario_pandemic_flu)
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
  checkmate::assert_data_frame(data.table::first(x$data))

  if (view_rows) {
    utils::head(data.table::first(x$data))
  } else {
    vapply(data.table::first(x$data), class, FUN.VALUE = "numeric")
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
#' scenario_pandemic_flu <- run_scenario(scenario_pandemic_flu)
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
    data.table::setnames(dt, ".", glue::glue(
      "{measure_variables}_{summary_functions}"
    ))
  }

  # return data.table
  dt
}
