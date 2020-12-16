
# make_list_of_size -------------------------------------------------------------------------------------------
test_that("make_list_of_size pads list properly", {
  expect_equal(make_list_of_size(c(2, 1), 3, pad = NA_real_), list(2, 1, NA_real_))
  expect_equal(make_list_of_size(c(2, 1), 4, pad = NA_real_), list(2, 1, NA_real_, NA_real_))
  expect_equal(make_list_of_size(c(4, 5, 6), 3, pad = NA_real_), list(4, 5, 6))
  expect_equal(make_list_of_size(c("a", "b", "c"), 4, pad = NA_real_), list("a", "b", "c", NA_real_))
})

# make_list_of_size -------------------------------------------------------------------------------------------
test_that("n_split splits in n parts", {
  expect_equal(length(n_split(1:10, 2)), 2)
  expect_equal(length(n_split(1:10, 20)), 10)
  expect_equal(length(n_split(1:10, 2)[[1]]), length(n_split(1:10, 2)[[2]]))
})
