
# Prepare a finalsize scenario
scenario_pandemic_flu <- scenario(
  model_function = "finalsize::final_size",
  parameters = make_parameters_finalsize_UK(),
  replicates = 3 # note extra replicates
)

test_that("scenario class is initialised correctly", {
  expect_s3_class(scenario_pandemic_flu, class = "scenario")
  expect_length(scenario_pandemic_flu, 4)
  expect_named(
    scenario_pandemic_flu,
    c("model_function", "parameters", "replicates", "data")
  )
  expect_type(scenario_pandemic_flu$model_function, "character")
  expect_type(scenario_pandemic_flu$parameters, "list")
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

test_that("Running scenario works", {
  expect_false(
    sce_has_data(scenario_pandemic_flu)
  )
  expect_silent(
    scenario_pandemic_flu <- run_scenario(scenario_pandemic_flu)
  )
  expect_true(
    sce_has_data(scenario_pandemic_flu)
  )
  expect_s3_class(
    scenario_pandemic_flu$data[[1]],
    "data.frame"
  ) # expect data.frame

  # expect that running with no namespacing works
  # defining a dummy scenario that runs a linear model
  expect_warning(
    scenario_lm <- scenario(
      model_function = "lm",
      parameters = list(
        formula = as.formula("y ~ x"),
        data = data.frame(
          x = seq(10), y = seq(10)
        )
      ),
      replicates = 1
    )
  )
  expect_silent(
    run_scenario(scenario_lm)
  )
})
