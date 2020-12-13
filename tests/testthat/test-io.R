# msg ---------------------------------------------------------------------------------------------------------

test_that("msg sends message", {
  expect_equal(capture.output(msg("Hello"), type = "message"), "Hello")
  expect_equal(capture.output(msg(quiet = FALSE, "Hello", " world"), type = "message"), "Hello world")
  expect_equal(capture.output(msg("Hello", quiet = FALSE, " world"), type = "message"), "Hello world")
})

test_that("msg does not send message when quiet", {
  expect_equal(capture.output(msg("Hello", quiet = TRUE), type = "message"), character(0))
  expect_equal(capture.output(msg(quiet = TRUE, "Hello", " world"), type = "message"), character(0))
})


# read_csv_in_zip ---------------------------------------------------------------------------------------------

# Look into testing with files

# save_rds ----------------------------------------------------------------------------------------------------

# Look into testing with files
