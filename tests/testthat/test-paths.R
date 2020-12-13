# file_path ---------------------------------------------------------------------------------------------------
test_that("file_path removes trailing slash of path", {
  expect_equal(file_path("dir1/dir2/"), "dir1/dir2")
  expect_equal(file_path("dir1/dir2/", "file.ext"), "dir1/dir2/file.ext")
  expect_equal(file_path("dir1/dir2/", "dir3", "file.ext"), "dir1/dir2/dir3/file.ext")
})

test_that("file_path just runs file.path if no trailing slash", {
  expect_equal(file_path("dir1/dir2"), file.path("dir1/dir2"))
  expect_equal(file_path("dir1/dir2", "dir3"), file.path("dir1/dir2", "dir3"))
  expect_equal(file_path("dir1/dir2", "dir3", "file.ext"), file.path("dir1/dir2", "dir3", "file.ext"))
})

test_that("file_path does not remove trailing slash if :// pattern", {
  expect_equal(file_path("s3://", "dir1/dir2/"), "s3://dir1/dir2")
  expect_equal(file_path("s3://", "dir1/dir2/", "dir3"), "s3://dir1/dir2/dir3")
  expect_equal(file_path("s3://", "dir1/dir2/", "dir3", "file.ext"), "s3://dir1/dir2/dir3/file.ext")
})


# extension -----------------------------------------------------------------------------------------------
test_that("extension returns extension", {
  expect_equal(extension("file.ext"), ".ext")
  expect_equal(extension("dir1/file.ext"), ".ext")
  expect_equal(extension("dir1/dir2/file.ext"), ".ext")
  expect_equal(extension("dir1/dir2/file.name.ext"), ".ext")
})

test_that("extension returns extension without dot if option is set", {
  expect_equal(extension("file.ext", dot = FALSE), "ext")
  expect_equal(extension("dir1/file.ext", dot = FALSE), "ext")
  expect_equal(extension("dir1/dir2/file.ext", dot = FALSE), "ext")
  expect_equal(extension("dir1/dir2/file.name.ext", dot = FALSE), "ext")
})


test_that("extension returns empty string if no extension", {
  expect_equal(extension("file"), "")
  expect_equal(extension("dir1/file"), "")
  expect_equal(extension(""), "")
  expect_equal(extension("dir1/dir2/", dot = FALSE), "")
  expect_equal(extension("file", dot = FALSE), "")
  expect_equal(extension("dir1/file", dot = FALSE), "")
  expect_equal(extension("", dot = FALSE), "")
  expect_equal(extension("dir1/dir2/", dot = FALSE), "")
})


# add_extension -----------------------------------------------------------------------------------------------
test_that("add_extension adds extension", {
  expect_equal(add_extension("file", ".ext"), "file.ext")
  expect_equal(add_extension("file", "ext"), "file.ext")
  expect_equal(add_extension("dir1/file", ".ext"), "dir1/file.ext")
  expect_equal(add_extension("dir1/file", "ext"), "dir1/file.ext")
  expect_equal(add_extension("dir1/dir2/file", ".ext"), "dir1/dir2/file.ext")
  expect_equal(add_extension("dir1/dir2/file", "ext"), "dir1/dir2/file.ext")
  expect_equal(add_extension("dir1/dir2/file.name", ".ext"), "dir1/dir2/file.name.ext")
  expect_equal(add_extension("dir1/dir2/file.name", "ext"), "dir1/dir2/file.name.ext")
})


# remove_extension --------------------------------------------------------------------------------------------
test_that("remove_extension removes extension", {
  expect_equal(remove_extension("file.ext"), "file")
  expect_equal(remove_extension("dir1/file.ext"), "dir1/file")
  expect_equal(remove_extension("dir1/dir2/file.ext"), "dir1/dir2/file")
  expect_equal(remove_extension("dir1/dir2/file.name.ext"), "dir1/dir2/file.name")
})

# change_extension --------------------------------------------------------------------------------------------
test_that("change_extension changes extension", {
  expect_equal(change_extension("file.ext", ".new"), "file.new")
  expect_equal(change_extension("dir1/file.ext", ".new"), "dir1/file.new")
  expect_equal(change_extension("dir1/dir2/file.ext", ".new"), "dir1/dir2/file.new")
  expect_equal(change_extension("dir1/dir2/file.name.ext", ".new"), "dir1/dir2/file.name.new")
})

test_that("change_extension add extension if absent", {
  expect_equal(change_extension("file", ".new"), "file.new")
  expect_equal(change_extension("dir1/file", ".new"), "dir1/file.new")
  expect_equal(change_extension("dir1/dir2/file", ".new"), "dir1/dir2/file.new")
})

# basename ----------------------------------------------------------------------------------------------------
test_that("basename returns file name", {
  expect_equal(basename("file.ext"), "file.ext")
  expect_equal(basename("dir1/file.ext"), "file.ext")
  expect_equal(basename("dir1/dir2/file.ext"), "file.ext")
  expect_equal(basename("dir1/dir2/file.name.ext"), "file.name.ext")
})

test_that("basename returns file name without extension if option is set to TRUE", {
  expect_equal(basename("file.ext", remove.extension = TRUE), "file")
  expect_equal(basename("dir1/file.ext", remove.extension = TRUE), "file")
  expect_equal(basename("dir1/dir2/file.ext", remove.extension = TRUE), "file")
  expect_equal(basename("dir1/dir2/file.name.ext", remove.extension = TRUE), "file.name")
})


# dirname -----------------------------------------------------------------------------------------------------
test_that("dirname returns directory name", {
  expect_equal(dirname("file.ext"), ".")
  expect_equal(dirname("dir1/file.ext"), "dir1")
  expect_equal(dirname("./dir1/file.ext"), "./dir1")
  expect_equal(dirname("dir1/dir2/file.ext"), "dir1/dir2")
  expect_equal(dirname("/dir1/dir2/file.ext"), "/dir1/dir2")
  expect_equal(dirname("http://dir1/dir2/file.ext"), "http://dir1/dir2")
})

test_that("dirname returns directory name if ending in slash", {
  expect_equal(dirname("dir1/"), "dir1")
  expect_equal(dirname("dir1/dir2/"), "dir1/dir2")
})

test_that("dirname returns directory name without scheme if option is set to TRUE", {
  expect_equal(dirname("http://dir1/dir2/file.ext", remove.scheme = TRUE), "dir1/dir2")
  expect_equal(dirname("ftp://dir1/", remove.scheme = TRUE), "dir1")
})


# url_scheme --------------------------------------------------------------------------------------------------
test_that("url_scheme returns url scheme", {
  expect_equal(url_scheme("http://dir1/dir2/file.ext"), "http://")
  expect_equal(url_scheme("http://dir1/dir2/file.ext", colonslashes = FALSE), "http")
  expect_equal(url_scheme("s3://bucket_name/dir1/file.ext"), "s3://")
  expect_equal(url_scheme("s3://bucket_name/dir1/file.ext", colonslashes = FALSE), "s3")
  expect_equal(url_scheme("mailto:test@example.org"), "mailto:")
  expect_equal(url_scheme("mailto:test@example.org", colonslashes = FALSE), "mailto")
})

