
# count_by ------------------------------------------------------------------------------------------------
test_that("count_by returns counts", {
  require(data.table)
  DF = data.table(ID = c("b","b","b","a","a","c"), a = 1:6, b = 7:12, c = 13:18)
  expect_equal(count_by(DF, "ID"), c(ID=3))
  expect_equal(count_by(DF, c("ID", "a")), c(ID=3, a=6))
  DF2 <- data.table::as.data.table(iris)
  expect_equal(as.vector(count_by(DF2, c("*", "Species", "Species-Petal.Width"))), c(150, 3, 27))
})
