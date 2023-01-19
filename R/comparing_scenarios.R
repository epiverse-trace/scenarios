#' Check for scenario matching variables
#'
#' @param x A 'scenario' object.
#' @param variables A character vector of 'scenario' parameter names for which
#' to search in `x`.
#'
#' @return A logical indicating whether the variables correspond to parameter
#' names in `x`.
sce_has_match_variables <- function(x, variables) {
  stopifnot(
    "Object must be of the 'scenario' class" =
      is.scenario(x)
  )
  # informative error message about which variable(s) is/are missing
  if (all(variables %in% names(x$parameters))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Check for variables in scenario outcomes
#'
#' @param x A 'scenario' object.
#' @param variables A character vector of variables that are expected to be
#' found in the `data` list of `x`.
#'
#' @return A logical indicating whether the variables correspond to column names
#' in `x$data`.
sce_has_comparison_variables <- function(x, variables) {
  stopifnot(
    "Object must be of the 'scenario' class" =
      is.scenario(x),
    "Scenario object must have data to check for comparison variables" =
      sce_has_data(x)
  )
  data_ <- sce_get_outcomes(x)

  # informative error message about which variable(s) is/are missing
  if (all(variables %in% colnames(data_))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Check whether a pair of scenarios is comparable
#'
#' @param baseline A 'scenario' object.
#' @param compare A second 'scenario' object.
#' @param match_variables A character string of scenario parameter names that is
#' used to check whether the two scenarios have identical parameters.
#' @param comparison_variables A character string of column names expected in
#' the scenarios' outcome data. This is used to check whether the scenarios both
#' have the required columns in their data.
#'
#' @return A logical value indicating whether a pair of scenarios is comparable.
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
#' # run scenarios to generate data
#' pandemic_flu <- run_scenario(pandemic_flu)
#' covid19 <- run_scenario(covid19)
#'
#' # check whether scenarios are comparable
#' sce_are_comparable(
#'   baseline = pandemic_flu,
#'   compare = covid19,
#'   match_variables = "demography_vector",
#'   comparison_variables = "p_infected"
#' )
sce_are_comparable <- function(baseline, compare, match_variables,
                               comparison_variables) {
  # check inputs
  stopifnot(
    "Baseline or comparator are not 'scenario' objects" =
      (all(is.scenario(baseline), is.scenario(compare))),
    "Matching variables are missing, pass a string of variables" =
      !missing(match_variables),
    "Comparison variables are missing, pass a string of variables" =
      !missing(comparison_variables),
    "Matching variables are not strings" =
      is.character(match_variables),
    "Comparison variables are not strings" =
      is.character(comparison_variables),
    # check whether scenarios have the same model function
    # no allowances made for differences in namespacing
    "Scenarios have different model functions and cannot be compared" =
      (baseline$model_function == compare$model_function)
  )

  # check whether both scenarios have matching and comparison variables at all
  have_match_variables <- sce_has_match_variables(baseline, match_variables) &&
    sce_has_match_variables(compare, match_variables)
  have_comparison_variables <- sce_has_comparison_variables(
    baseline,
    comparison_variables
  ) &&
    sce_has_comparison_variables(compare, comparison_variables)

  # if scenarios have variables, check whether matching variables are identical
  if (all(have_match_variables, have_comparison_variables)) {
    baseline_parameters <- sce_get_parameters(baseline, which = match_variables)
    compare_parameters <- sce_get_parameters(compare, which = match_variables)

    can_match <- mapply(
      baseline_parameters, compare_parameters,
      FUN = function(x, y) {
        stopifnot(
          "Matching variables must refer to atomic parameters" =
            all(is.atomic(x), is.atomic(y))
        )
        all(x == y)
      }
    )

    # if all matching variables are identical
    if (all(can_match)) {
      return(TRUE)
    } else {
      non_match_variables <- match_variables[!can_match]
      non_match_variables <- glue::glue_collapse(
        glue::glue("'{non_match_variables}'"),
        sep = ", ", last = " and "
      )
      message(
        glue::glue(
          "Scenario parameters do not match, scenarios are not comparable.
          These parameters do not match: {non_match_variables}"
        )
      )
      return(FALSE)
    }
  } else { # case where either matching or comparison variables are missing
    message(
      glue::glue(
        "Scenarios do not share matching or comparison variables, and cannot \\
        be compared."
      )
    )
    return(FALSE)
  }
}
