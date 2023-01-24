#### Tests for comparison helpers ####
# create some scenarios
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

# create comparison, passing prepared scenarios with some names
outbreak_comparison <- comparison(
  pandemic_flu = pandemic_flu, covid19 = covid19,
  baseline = "pandemic_flu"
)

test_that("Getting scenario names", {
  expect_identical(
    sce_get_scenario_names(outbreak_comparison),
    c("pandemic_flu", "covid19")
  )
  expect_error(
    sce_get_scenario_names(pandemic_flu),
    regexp = "(must be)|(comparison)"
  )
})

test_that("Setting scenario names", {
  expect_identical(
    sce_get_scenario_names(
      sce_set_scenario_names(
        outbreak_comparison,
        old_names = "pandemic_flu",
        new_names = "Influenza",
        new_baseline = "Influenza"
      )
    ),
    c("Influenza", "covid19")
  )
  # expect error when baseline changes and no new baseline is passed
  expect_error(
    sce_set_scenario_names(
      outbreak_comparison,
      old_names = "pandemic_flu",
      new_names = "Influenza"
    ),
    regexp = "(argument)*(new_baseline)*(is missing)"
  )
})

test_that("Getting and setting baseline scenario", {
  expect_identical(
    sce_get_baseline(outbreak_comparison),
    "pandemic_flu"
  )

  expect_identical(
    sce_get_baseline(
      sce_set_baseline(
        outbreak_comparison,
        "covid19"
      )
    ),
    "covid19"
  )

  # expect error when multiple scenario names match baseline
  expect_error(
    sce_set_scenario_names(
      outbreak_comparison,
      old_names = c("pandemic_flu", "covid19"),
      new_names = c("pandemic", "pandemic"),
      new_baseline = "pandemic"
    ),
    regexp = "Error: Multiple scenario names match new baseline!"
  )
})
