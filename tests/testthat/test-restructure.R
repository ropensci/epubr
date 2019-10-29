context("restructure")

library(dplyr)

file <- system.file("dracula.epub", package = "epubr")
x <- epub(file)
x <- bind_rows(x, x)
pat <- "CHAPTER [IVX]+"

x1 <- epub_recombine(x, pat) %>% epub_sift(n = 1000) %>% epub_recombine(pat)
x2 <- epub_recombine(x, pat, list(n = 1000))

test_that("epub_recombine returns as expected", {
  expect_equal(lapply(epub_recombine(x, pat)$data, dim),
               list(c(55, 4), c(55, 4)))
  expect_identical(x1, x2)
  expect_equal(nrow(x1), 2)
  expect_identical(slice(x1, 1), slice(x1, 2))

  expect_equal(lapply(epub_sift(x, n = 3000)$data, dim),
               list(c(13, 4), c(13, 4)))
})

test_that("epub_reorder returns as expected", {
  set.seed(1)
  x1$data <- lapply(x1$data, sample_frac)
  f <- function(x, pattern) as.numeric(as.roman(gsub(pattern, "\\1", x)))
  y <- epub_reorder(x1, f, "^CHAPTER ([IVX]+).*")
  expect_identical(x2, y)

})
