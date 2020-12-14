
# make_list_of_size -------------------------------------------------------------------------------------------
test_that("make_list_of_size pads list properly", {
  expect_equal(make_list_of_size(c(2, 1), 3, pad = NA_real_), list(2, 1, NA_real_))
  expect_equal(make_list_of_size(c(2, 1), 4, pad = NA_real_), list(2, 1, NA_real_, NA_real_))
  expect_equal(make_list_of_size(c(4, 5, 6), 3, pad = NA_real_), list(4, 5, 6))
  expect_equal(make_list_of_size(c("a", "b", "c"), 4, pad = NA_real_), list("a", "b", "c", NA_real_))
})
