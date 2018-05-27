context("epub")

file <- system.file("dracula.epub", package = "epubr")

test_that("epub unzipped as expected", {
  epub_unzip(file)
  expect_true(all(c("META-INF", "mimetype", "OEBPS") %in% list.files(tempdir())))
  expect_equal(length(list.files(file.path(tempdir(), "OEBPS"))), 22)
})

test_that("epub read as expected", {
  x <- epub(file)
  expect_equal(dim(x), c(1, 9))
  expect_equal(dim(x$data[[1]]), c(15, 4))

  x <- epub(file, fields = c("title", "creator"), drop_sections = "^cov")
  expect_equal(dim(x), c(1, 3))
  expect_equal(dim(x$data[[1]]), c(14, 4))
})
