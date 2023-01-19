
#### Tests for sce_get_outcomes ####
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

# run scenarios
pandemic_flu <- run_scenario(pandemic_flu)
covid19 <- run_scenario(covid19)

# create a comparison object without matching or comparison variables
x <- comparison(
  pandemic_flu = pandemic_flu, covid19 = covid19,
  baseline = "pandemic_flu"
)

test_that("'sce_get_outcome' works with 'scenario' object", {
  expect_true(
    sce_has_data(pandemic_flu) # extra test
  )

  data <- sce_get_outcomes(pandemic_flu)
  expect_s3_class(
    data, "data.frame"
  )

  # set scenario outcomes to non-dataframe class
  # and expect warning
  pandemic_flu$data <- as.list(rep(
    matrix(1, 1), length(pandemic_flu$data)
  ))
  expect_error(
    sce_get_outcomes(pandemic_flu),
    regexp = "Scenario model outputs are not `data.frames`."
  )
})

test_that("'sce_get_outcome' works with 'comparison' object", {
  expect_true(
    sce_has_data(x) # extra test
  )

  data <- sce_get_outcomes(x)
  expect_s3_class(
    data, "data.frame"
  )
})
