#' Recombine text sections
#'
#' Split and recombine EPUB text sections based on regular expression pattern matching.
#'
#' This function takes a regular expression and uses it to determine new break points for the full e-book text.
#' This is particularly useful when sections pulled from EPUB metadata have arbitrary breaks and the text contains meaningful breaks at random locations in various sections.
#' \code{epub_recombine} collapses the text and then creates a new nested data frame containing new chapter/section labels, word counts and character counts,
#' associated with the text based on the new break points.
#'
#' Usefulness depends on the quality of the e-book. While this function exists to improve the data structure of e-book content parsed from e-books with poor metadata formatting,
#' it still requires original formatting that will at least allow such an operation to be successful, specifically a consistent, non-ambiguous regular expression pattern.
#' See examples below using the built in e-book dataset.
#'
#' When used in conjunction with \code{epub_sift} via the \code{sift} argument, recombining and resifting is done recursively.
#' This is because it is possible that sifting can create a need to rerun the recombine step in order to regenerate proper chapter indexing for the section column.
#' However, recombining a second time does not lead to a need to resift, so recursion ends after one round regardless.
#'
#' This is a convenient way to avoid the syntax:
#'
#' \code{epub_recombine([args]) \%>\% epub_sift([args]) \%>\% epub_recombine([args])}.
#'
#' @param data a data frame created by \code{epub}.
#' @param pattern character, a regular expression.
#' @param sift \code{NULL} or a named list of parameters passed to \code{\link{epub_sift}}. See details.
#'
#' @return a data frame
#' @export
#' @seealso \code{\link{epub_sift}}
#'
#' @examples
#' file <- system.file("dracula.epub", package = "epubr")
#' x <- epub(file) # parse entire e-book
#' x$data[[1]] # note arbitrary section breaks (not between chapters)
#'
#' pat <- "CHAPTER [IVX]+" # but a reliable pattern exists for new breaks
#' epub_recombine(x, pat) # not quite as expected; pattern also appears in table of contents!
#'
#' epub_recombine(x, pat, sift = list(n = 1000)) # also sift low word-count sections
epub_recombine <- function(data, pattern, sift = NULL){
  data$data <- lapply(data$data, .epub_recombine, pattern)
  data <- dplyr::mutate(data, nchap = sapply(.data[["data"]], function(x) sum(grepl("^ch\\d+$", x$section))))
  if(which(names(data) == "data") == ncol(data) - 1){
    idx <- 1:ncol(data)
    n <- length(idx)
    idx[(n - 1):n] <- idx[n:(n - 1)]
    data <- dplyr::select(data, idx)
  }
  if(!is.list(sift)) return(data)
  if(!"n" %in% names(sift)) stop("`sift` argument is a list but does not contain `n`.")
  if(!"type" %in% names(sift)) sift$type <- "word"
  resift <- any(sapply(data$data, function(x) any(x[[paste0("n", sift$type)]] < sift$n)))
  if(resift){
    data <- do.call(epub_sift, c(list(data), sift))
    Recall(data, pattern, sift)
  } else {
    data
  }
}

.epub_recombine <- function(data, pattern){
  p <- paste0("(", pattern, ")")
  text <- paste0(data$text, collapse = "\n")
  text <- strsplit(gsub(p, "__BREAK__\\1", text), "__BREAK__")[[1]]
  section <- formatC(1:(length(text) - 1), width = max(2, nchar(length(text))), flag = "0")
  section <- c("prior", paste0("ch", section))
  dplyr::data_frame(section = section, text = text) %>%
    dplyr::mutate(nword = count_words(.data[["text"]]), nchar = nchar(.data[["text"]])) %>%
    dplyr::filter(!(.data[["section"]] == "prior" & .data[["text"]] == ""))
}
