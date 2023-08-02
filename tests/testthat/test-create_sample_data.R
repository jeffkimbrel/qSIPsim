test_that("isotope selection is allowed", {
  expect_error(create_sample_data(isotope = "C14"))
})

