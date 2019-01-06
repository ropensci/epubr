globalVariables(c(".", ".data"))

#' epubr: Read EPUB file metadata and text.
#'
#' \code{epubr} provides functions supporting the reading and parsing of internal e-book content from EPUB files.
#' E-book metadata and textual contents are parsed separately.
#'
#' E-book formatting is not completely standardized across all literature.
#' It can be challenging to curate parsed e-book content across an arbitrary collection of e-books perfectly and in completely general form, to yield a singular, consistently formatted output.
#' Many EPUB files do not even contain all the same pieces of information in their respective metadata.
#'
#' EPUB file parsing functionality in this package is intended for relatively general application to arbitrary EPUB e-books.
#' However, poorly formatted e-books or e-books with highly uncommon formatting may not work with this package.
#' There may even be cases where an EPUB file has DRM or some other property that makes it impossible to read with \code{epubr}.
#'
#' Text is read 'as is' for the most part. The only nominal changes are minor substitutions, for example curly quotes changed to straight quotes.
#' Substantive changes are expected to be performed subsequently by the user as part of their text analysis.
#' Additional text cleaning can be performed at the user's discretion, such as with functions from packages like \code{tm} or \code{qdap}.
#'
#' @docType package
#' @name epubr
NULL

#' @importFrom magrittr %>%
NULL
