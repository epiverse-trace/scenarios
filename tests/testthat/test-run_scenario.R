#### Tests for the generic function  'run_scenario' ####
# Prepare a comparison of two scenarios of the final size of an epidemic
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

# create a comparison object without matching or comparison variables
x <- comparison(
  pandemic_flu = pandemic_flu, covid19 = covid19,
  baseline = "pandemic_flu"
)

#### for the 'scenario' class ####
test_that("'run_scenario' works for 'scenario' class", {
  expect_false(
    sce_has_data(pandemic_flu)
  )
  expect_silent(
    pandemic_flu <- run_scenario(pandemic_flu)
  )
  expect_true(
    sce_has_data(pandemic_flu)
  )
  # add a snapshot for informative printing
  # must include that data are prepared
  expect_snapshot(
    pandemic_flu
  )
  expect_s3_class(
    pandemic_flu$data[[1]],
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
    ),
    regexp = "(model_function)*(may not be explicitly namespaced)"
  )
  expect_silent(
    run_scenario(scenario_lm)
  )
})

#### for the 'comparison' class ####
test_that("'run_scenario' works for 'comparison' class", {
  expect_false(
    sce_has_data(x)
  )
  expect_silent(
    x <- run_scenario(x)
  )
  expect_true(
    sce_has_data(x)
  )
  # add a snapshot for informative printing
  # must include that data are prepared
  expect_snapshot(
    print(x) # conflicts with method from 'testthat'
  )
  expect_s3_class(
    x$data[[1]],
    "scenario"
  ) # expect scenarios in data

  # check that data are prepared within scenarios
  expect_s3_class(
    x$data[[1]]$data[[1]],
    "data.frame"
  )
})
