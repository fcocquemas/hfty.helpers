# third_friday_of_month ---------------------------------------------------------------------------------------

test_that("third_friday_of_month finds third Friday of month", {
  expect_equal(third_friday_of_month("2020-12-14"), as.Date("2020-12-18"))
  expect_equal(third_friday_of_month(as.Date("2020-12-14")), as.Date("2020-12-18"))
  expect_equal(third_friday_of_month("2020-08-28"), as.Date("2020-08-21"))
})
