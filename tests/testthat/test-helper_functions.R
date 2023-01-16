
# Prepare a finalsize scenario
n_replicates <- 3L
scenario_pandemic_flu <- scenario(
  model_function = "finalsize::final_size",
  parameters = make_parameters_finalsize_UK(),
  replicates = n_replicates # note extra replicates
)

#### Tests for sce_get_parameters ####
test_that("Getting parameters from scenario", {
  # general case
  parameters <- sce_get_parameters(scenario_pandemic_flu)
  expect_vector(parameters, ptype = list())
  expect_named(
    parameters,
    names(scenario_pandemic_flu$parameters)
  )

  # subset parameters
  which_params <- "r0"
  parameters <- sce_get_parameters(scenario_pandemic_flu, which = which_params)
  expect_length(
    parameters,
    length(which_params)
  )
  expect_named(
    parameters, "r0"
  )
})

#### Tests for sce_get_parameters ####
test_that("Checking for scenario data", {
  expect_false(
    sce_has_data(scenario_pandemic_flu)
  )
})

#### Tests for sce_get_outcomes ####
# run the scenario
scenario_pandemic_flu <- run_scenario(scenario_pandemic_flu)
test_that("Getting scenario outcome data", {
  expect_true(
    sce_has_data(scenario_pandemic_flu) # extra test
  )

  data <- sce_get_outcomes(scenario_pandemic_flu)
  expect_s3_class(
    data, "data.frame"
  )
  # exepct equal rather than identical to prevent int-double comparison issues
  expect_identical(
    max(data$replicate),
    n_replicates
  )

  # set scenario outcomes to non-dataframe class
  # and expect warning
  scenario_pandemic_flu$data <- as.list(rep(
    matrix(1, 1), length(scenario_pandemic_flu$data)
  ))
  expect_error(
    sce_get_outcomes(scenario_pandemic_flu),
    regexp = "Scenario model outputs are not `data.frames`."
  )
})

#### Tests for sce_peek_outcomes ####
test_that("Peeking at scenario outcome data", {
  peek <- sce_peek_outcomes(scenario_pandemic_flu)
  expect_vector(
    peek, character()
  )
  # check for head of data
  peek <- sce_peek_outcomes(scenario_pandemic_flu, view_rows = TRUE)
  expect_s3_class(
    peek, "data.frame"
  )
})

#### Tests for sce_aggregate_outcomes ####
test_that("Aggregate scenario outcome data", {
  grouping_variable <- "demo_grp"
  measure_variable <- "p_infected"
  summary_funs <- c("mean", "min", "sd")

  agg <- sce_aggregate_outcomes(
    x = scenario_pandemic_flu,
    grouping_variables = grouping_variable,
    measure_variables = measure_variable,
    summary_functions = summary_funs
  )

  # get unique demographic groups
  demo_grp_names <- rownames(make_parameters_finalsize_UK()$contact_matrix)

  expect_s3_class(
    agg, "data.frame"
  )
  expect_identical(
    colnames(agg),
    c(
      grouping_variable,
      sprintf("%s_%s", measure_variable, summary_funs)
    )
  )
  # check for grouping variable names
  expect_true(
    all(unique(agg$demo_grp) %in% demo_grp_names)
  )

  # test case for when there is a single summary function
  summary_funs <- "mean"
  agg <- sce_aggregate_outcomes(
    x = scenario_pandemic_flu,
    grouping_variables = grouping_variable,
    measure_variables = measure_variable,
    summary_functions = summary_funs
  )
  expect_identical(
    colnames(agg),
    c(
      grouping_variable,
      sprintf("%s_%s", measure_variable, summary_funs)
    )
  )
})
