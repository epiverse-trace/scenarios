#### Test functions for comparing scenarios ####

# prepare two scenarios
pandemic_flu <- scenario(
  model_function = "finalsize::final_size",
  parameters = make_parameters_finalsize_UK(r0 = 1.5),
  replicates = 1L
)
covid19 <- scenario(
  model_function = "finalsize::final_size",
  parameters = make_parameters_finalsize_UK(r0 = 5.0),
  replicates = 1L
)

test_that("Scenario matching works", {
  # expect error when scenario data are not prepared
  expect_error(
    sce_are_comparable(
      baseline = pandemic_flu,
      compare = covid19,
      match_variables = "demography_vector",
      comparison_variables = "p_infected"
    ),
    regexp = "Scenario object must have data to check for comparison variables"
  )

  # expect true once data are generated with 'run_scenario'
  pandemic_flu <- run_scenario(pandemic_flu)
  covid19 <- run_scenario(covid19)
  expect_true(
    sce_are_comparable(
      baseline = pandemic_flu,
      compare = covid19,
      match_variables = "demography_vector",
      comparison_variables = "p_infected"
    )
  )

  # expect false and also a message
  expect_false(
    sce_are_comparable(
      baseline = pandemic_flu,
      compare = covid19,
      match_variables = "r0",
      comparison_variables = "p_infected"
    )
  )
  expect_message(
    sce_are_comparable(
      baseline = pandemic_flu,
      compare = covid19,
      match_variables = "r0",
      comparison_variables = "p_infected"
    ),
    regexp = "(Scenario parameters do not match)*(do not match: 'r0')"
  )

  # expect error when variables are missing
  expect_error(
    sce_are_comparable(
      baseline = pandemic_flu,
      compare = covid19,
      match_variables = "demography_vector"
      # comparison variables missing
    ),
    regexp = "(Comparison variables are missing)"
  )
  expect_error(
    sce_are_comparable(
      baseline = pandemic_flu,
      compare = covid19
      # matching variables also missing, this error triggered first
    ),
    regexp = "(Matching variables are missing)"
  )

  # check for correct message when comparison variables are missing
  expect_message(
    sce_are_comparable(
      baseline = pandemic_flu,
      compare = covid19,
      match_variables = "demography_vector",
      comparison_variables = "test_outcome"
    ),
    regexp = "(Scenarios do not have)*(matching)*(comparison)"
  )

  # check for correct message when model functions differ
  pandemic_flu$model_function <- "some_function"
  expect_error(
    sce_are_comparable(
      baseline = pandemic_flu,
      compare = covid19,
      match_variables = "demography_vector",
      comparison_variables = "p_infected"
    ),
    regexp = "Scenarios have different model functions and cannot be compared"
  )
})
