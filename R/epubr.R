globalVariables(c(".", ".data"))

#' epubr: Read EPUB file metadata and text.
#'
#' epubr provides functions supporting the reading and parsing of internal e-book content from EPUB files. Book metadata and textual contents are parse separately.
#'
#' E-book formatting is non-standard enough across all literature that no function can curate parsed e-book content across an arbitrary collection of e-books, in completely general form, resulting in a singular, consistently formatted output containing all the same variables.
#'
#' EPUB file parsing functionality in this package is intended for relatively general application to arbitrary e-books. However, poorly formatted e-books or e-books with highly uncommon formatting may not work with this package.
#' Text is read 'as is'. Additional text cleaning should be performed by the user at their discretion, such as with functions from packages like \code{tm} or \code{qdap}.
#'
#' @docType package
#' @name epubr
NULL

#' @importFrom magrittr %>%
NULL
