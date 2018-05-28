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

  x <- epub(file, fields = c("title", "creator", "file"), drop_sections = "^cov", add_pattern = "xyz")
  expect_equal(dim(x), c(1, 4))
  expect_true("file" %in% names(x))
  expect_equal(dim(x$data[[1]]), c(14, 4))

  f <- function() list(pattern = "xyz", chapter_check = "xyz", chapter_doublecheck = "xyz")
  x <- epub(file, fields = c("title", "creator", "file"), drop_sections = "^cov", add_pattern = f)
  expect_equal(dim(x), c(1, 4))
  expect_true("file" %in% names(x))
  expect_equal(dim(x$data[[1]]), c(14, 4))

  x <- epub(file, fields = c("title", "creator", "file"), series = TRUE, parent_dir = strsplit(file, "/")[[1]][1])
  expect_equal(dim(x), c(1, 6))
  expect_true(all(c("series", "subseries") %in% names(x)))
  expect_equal(dim(x$data[[1]]), c(15, 4))


  x <- epub(file, fields = c("title", "creator", "file"), drop_sections = "^cov", chapter_pattern = "item\\d\\d")
  expect_equal(dim(x), c(1, 5))
  expect_true("nchap" %in% names(x))
  expect_equal(x$nchap, 10)
  expect_identical(x$data[[1]]$section, c(paste0("item", 6:9), paste0("ch0", 1:9), "ch10"))
  expect_identical(x$data[[1]]$is_chapter, rep(c(FALSE, TRUE), times = c(4, 10)))
  expect_equal(dim(x$data[[1]]), c(14, 5))

  x <- epub(file, fields = c("title", "creator", "file"), dedication = TRUE, hist_note = TRUE)
  expect_equal(dim(x), c(1, 4))
  expect_equal(dim(x$data[[1]]), c(15, 6))
  expect_true(all(c("dedication", "hist_note") %in% names(x$data[[1]])))
})
