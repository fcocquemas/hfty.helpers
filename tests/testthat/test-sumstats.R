
# count_by ------------------------------------------------------------------------------------------------
test_that("count_by returns counts", {
  require(data.table)
  DF = data.table(ID = c("b","b","b","a","a","c"), a = 1:6, b = 7:12, c = 13:18)
  expect_equal(count_by(DF, "ID"), 3)
  # dt <- data.table::as.data.table(iris)
  # expect_equal(as.vector(count_by(dt, c("*", "Species", "Species-Petal.Width"))), c(150, 3, 27))
  # expect_equal(as.vector(count_by(dt, c("*", "Species", "Species-Petal.Width"))), c(150, 3, 27))
})
