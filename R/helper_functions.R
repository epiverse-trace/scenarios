#' Get scenario information
#'
#' @description Gets the value of one or more object in the `parameters` or the
#' extra information list `extra_info`.
#' @param x A `scenario` object.
#' @param which_info Which parameters or extra information to return.
#'
#' @return A named list with named elements corresponding to the names passed in
#' `which_info`.
#' @export
#'
#' @examples
sce_get_information <- function(x, which_info) {
  # check input
  stopifnot(
    "Input 'x' must be a `scenario` object" =
      is_scenario(x)
  )

  # get all parameters and attributes that could identify a scenario
  name <- attr(x, "name")
  model <- attr(x, "model")
  model_parameters <- attr(x, "parameters")
  extra_info <- attr(x, "extra_info")

  info_list <- c(name = name, model = model, model_parameters, extra_info)

  # print/return chosen parameters as list
  # if which info is missing, return all information
  if (missing(which_info)) {
    return(info_list)
  } else {
    # else first subset info_list for chosen parameters

    # check whether all elements of `which_info` are in the list
    # and error if not

    # collect parameter names that are not found
    not_found <- which_info[which(!which_info %in% names(info_list))]

    # error if any are not found
    if (length(not_found) > 0L) {
      # glue into single string with quotes
      not_found <- glue::glue_collapse(
        glue::double_quote(not_found),
        sep = ", "
      )

      # print an informative message
      stop(
        glue::glue(
          "
          {not_found} not found among scenario model parameters or extra \\
          information
          "
        )
      )
    }

    # if all elements are present, subset them
    info_list <- lapply(info_list, `[`, which_info)

    # finally return the subsetted list
    return(info_list)
  }
}

#' Add extra information to a scenario
#'
#' @param x A `scenario` object.
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
    "Input 'x' must be a `scenario` object" =
      is_scenario(x),
    "Input 'info' must be a list with unique names" =
      checkmate::test_list(info, any.missing = FALSE, names = "unique"),
    # check for names already present in extra info
    "Some input list elements in 'info' are already present in this scenario" =
      (!any(names(info) %in% names(attr(x, "extra_info"))))
  )

  # add data
  attr(x, "extra_info") <- c(attr(x, "extra_info"), info)

  # validate scenario and return
  validate_scenario(x)
  x
}
