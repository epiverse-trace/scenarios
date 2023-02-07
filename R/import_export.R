
#' Write scenario to JSON
#'
#' @param x A `scenario` object.
#' @param file The file to which to write the JSON.
#'
#' @return Writes the `scenario` object to JSON.
#' @export
#'
#' @examples
#' scenario_pandemic_flu <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(),
#'   replicates = 1
#' )
#'
#' sce_to_json(scenario_pandemic_flu, file = tempfile())
sce_to_json <- function(x, file) {
  # input checking
  checkmate::assert_class(x, "scenario")
  validate_scenario(x, data_ok = TRUE)
  checkmate::assert_named(x)
  checkmate::assert_named(x$parameters)

  jsonlite::write_json(
    lapply(x, `[`),
    path = file,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )
}

#' Read scenario from JSON
#'
#' @param file A JSON file
#'
#' @return A `scenario` object.
#' @export
#'
#' @examples
#' scenario_pandemic_flu <- scenario(
#'   name = "pandemic_flu",
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(),
#'   replicates = 1
#' )
#' # create temporary filepath
#' tmpfile <- tempfile(fileext = ".json")
#' sce_to_json(scenario_pandemic_flu, file = tmpfile)
#' sce_from_json(tmpfile)
sce_from_json <- function(file) {
  stopifnot(
    "File may not be a JSON file" =
      (grepl(pattern = "(?i)json", file))
  )
  input <- jsonlite::read_json(
    path = file,
    simplifyVector = TRUE
  )

  # check input
  checkmate::assert_named(
    input
  )
  checkmate::assert_names(
    names(input),
    must.include = c(
      "name",
      "model_function", "parameters", "extra_info", "replicates", "data"
    )
  )
  # get scenario name
  name <- ifelse(is.null(input[["name"]]), NA_character_, input[["name"]])

  # get model function
  model_function <- input[["model_function"]]

  # get parameter list
  parameters <- input[["parameters"]]

  # get extra_information
  extra_info <- input[["extra_info"]]

  # get replicates
  replicates <- input[["replicates"]]

  # get scenario data
  data <- input[["data"]]
  # works for empty scenario objects
  if (all(vapply(data, is.null, FUN.VALUE = TRUE))) {
    data <- vector("list", length = replicates)
  }

  # create scenario object
  scenario_ <- structure(
    list(
      name = name,
      model_function = model_function,
      parameters = parameters,
      extra_info = extra_info,
      replicates = replicates,
      data = data
    ),
    class = "scenario"
  )
  validate_scenario(scenario_, data_ok = TRUE)

  # return scenario
  scenario_
}
