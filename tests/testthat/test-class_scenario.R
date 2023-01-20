
# Prepare a finalsize scenario
scenario_pandemic_flu <- scenario(
  model_function = "finalsize::final_size",
  parameters = make_parameters_finalsize_UK(),
  replicates = 3 # note extra replicates
)

test_that("scenario class is initialised correctly", {
  expect_s3_class(scenario_pandemic_flu, class = "scenario")
  expect_length(scenario_pandemic_flu, 5L)
  expect_named(
    scenario_pandemic_flu,
    c("model_function", "parameters", "extra_info", "replicates", "data")
  )
  expect_type(scenario_pandemic_flu$model_function, "character")
  expect_type(scenario_pandemic_flu$parameters, "list")
  expect_type(scenario_pandemic_flu$extra_info, "list")
  expect_type(scenario_pandemic_flu$data, "list")
})

test_that("Correct printing of scenario class", {
  # save a snapshot
  expect_snapshot(
    scenario_pandemic_flu
  )
})

test_that("Model function namespacing warning", {
  expect_warning(
    scenario(
      model_function = "final_size",
      parameters = make_parameters_finalsize_UK(),
      replicates = 3 # note extra replicates
    ),
    regexp = "`model_function` may not be explicitly namespaced."
  )
})
