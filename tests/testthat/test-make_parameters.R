
test_that("Making finalsize parameters", {
  expect_vector(
    make_parameters_finalsize_UK(),
    ptype = list()
  )
})

test_that("Making epidemics parameters", {
  expect_vector(
    make_parameters_SIR_epidemic(),
    ptype = list()
  )
  expect_error(
    make_parameters_SIR_epidemic(beta = "1", gamma = "0.01")
  )
})
