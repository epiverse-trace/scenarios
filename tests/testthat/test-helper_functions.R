
# Prepare a finalsize scenario
n_replicates <- 3L
age_groups <- rownames(make_parameters_finalsize_UK()$contact_matrix)
scenario_pandemic_flu <- scenario(
  model_function = "finalsize::final_size",
  parameters = make_parameters_finalsize_UK(),
  extra_info = list(
    age_groups = age_groups
  ),
  replicates = n_replicates # note extra replicates
)

#### Tests for sce_get_information ####
test_that("Getting information from scenario", {
  # general case
  parameters <- sce_get_information(scenario_pandemic_flu)
  expect_vector(parameters, ptype = list())
  expect_named(
    parameters,
    c("model_parameters", "scenario_information")
  )

  # subset parameters
  which_params <- c("r0", "age_groups")
  parameters <- sce_get_information(scenario_pandemic_flu, which = which_params)
  expect_length(
    parameters,
    length(which_params)
  )
  expect_named(
    parameters, which_params
  )
  expect_error(
    sce_get_information(scenario_pandemic_flu, which = "some param"),
    regexp = "('some param')*(not found among)"
  )
})

#### Tests for sce_has_data ####
test_that("Checking for scenario data", {
  expect_false(
    sce_has_data(scenario_pandemic_flu)
  )
})

#### Tests for sce_peek_outcomes ####
scenario_pandemic_flu <- run_scenario(scenario_pandemic_flu)
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
      glue::glue("{measure_variable}_{summary_funs}")
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
      glue::glue("{measure_variable}_{summary_funs}")
    )
  )
})

# test adding information
test_that("Adding extra information to a scenario", {
  parameters <- make_parameters_finalsize_UK()
  extra_info <- list(
    age_groups = rownames(parameters$contact_matrix)
  )
  x <- scenario(
    model_function = "finalsize::final_size",
    parameters = parameters
  )

  # expect success
  expect_silent(
    x <- sce_add_info(x, extra_info)
  )
  expect_named(
    x$extra_info,
    "age_groups"
  )

  # expect failure if x is not a scenario
  expect_error(
    sce_add_info("x", extra_info),
    regexp = "(Input)*(must be a `scenario` object)"
  )

  # expect failure if adding information again
  expect_error(
    x <- sce_add_info(x, extra_info),
    regexp = "(Some input list elements)*(are already present in this scenario)"
  )
})

#### Tests to drop scenario data ####
test_that("Dropping scenario data from scenarios", {
  # first generate data
  scenario_pandemic_flu <- run_scenario(scenario_pandemic_flu)
  expect_false(
    sce_has_data(sce_drop_data(scenario_pandemic_flu))
  )
})

test_that("Dropping scenario data from comparisons", {
  # first generate data
  scenario_pandemic_flu <- run_scenario(scenario_pandemic_flu)
  scenario_pandemic_flu$name <- "pandemic_flu"
  comparison_flu <- comparison(scenario_pandemic_flu, baseline = "pandemic_flu")
  expect_false(
    sce_has_data(sce_drop_data(comparison_flu))
  )
})
