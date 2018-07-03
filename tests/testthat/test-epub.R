context("epub")

file <- system.file("dracula.epub", package = "epubr")

test_that("epub unzipped as expected", {
  epub_unzip(file)
  expect_true(all(c("META-INF", "mimetype", "OEBPS") %in% list.files(tempdir())))
  expect_equal(length(list.files(file.path(tempdir(), "OEBPS"))), 22)
})

test_that("epub and epub_meta read as expected", {
  err1 <- "File not found."
  err2 <- "All files must end in `.epub`."
  f <- c(epub, epub_meta, epub_unzip)
  purrr::walk(f, ~expect_error(.x("X"), err1))
  purrr::walk(f, ~expect_error(.x(system.file("text.xml", package = "epubr")), err2))

  x <- epub_meta(file)
  expect_identical(names(x), c("rights", "identifier", "creator", "title", "language", "subject", "date", "source"))
  expect_equal(dim(x), c(1, 8))

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

  f <- function() list(pattern = "Dracula", chapter_check = "Dracula", chapter_doublecheck = "Dracula")
  x <- epub(file, fields = c("title", "creator", "file"), drop_sections = "^cov", add_pattern = f,
            chapter_pattern = "item\\d\\d")
  expect_equal(dim(x), c(1, 5))
  expect_true("file" %in% names(x))
  expect_equal(dim(x$data[[1]]), c(14, 5))

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

  x <- epub(file, fields = c("title", "creator", "file"), dedication = TRUE)
  expect_equal(dim(x), c(1, 4))
  expect_equal(dim(x$data[[1]]), c(15, 5))
  expect_true("dedication" %in% names(x$data[[1]]))

  x <- epub(file, fields = c("file", "creator", "title"), title = "creator")
  y <- c("file", "title", "data")
  expect_identical(names(x), y)
  expect_equal(x$title, "Bram Stoker")
  x <- epub(file, fields = "file", title = "X")
  expect_identical(names(x), y)
  expect_equal(x$title, x$file)
  expect_equal(x$title, "dracula.epub")
})

test_that("first_nchar returns as expected", {
  x <- first_nchar(file)
  expect_equal(names(x), c("section", "text"))
  expect_equal(dim(x), c(15, 2))
})
