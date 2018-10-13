#' Preview the first n characters
#'
#' Preview the first n characters of each EPUB e-book section.
#'
#' This function returns a simplified data frame of only the unnested \code{section} and \code{text} columns of a data frame returned by \code{\link{epub}}, with the text included only up to the first \code{n} characters.
#' This is useful for previewing the opening text of each e-book section to inspect for possible useful regular expression patterns to use for text-based section identification.
#' For example, an e-book may not have meaningful section IDs that distinguish one type of book section from another, such as chapters from non-chapter sections,
#' but the text itself may contain this information at or near the start of a section.
#'
#' @param x a data frame returned by \code{\link{epub}} or a character string giving the EPUB filename(s).
#' @param n integer, first n characters to retain from each e-book section.
#'
#' @return a data frame.
#' @export
#' @name epub_head
#' @seealso \code{\link{epub_cat}}
#'
#' @examples
#' file <- system.file("dracula.epub", package = "epubr")
#' epub_head(file)
epub_head <- function(x, n = 50){
  if(inherits(x, "character")) x <- epub(x)
  tidyr::unnest(x) %>% dplyr::select(!! c("section", "text")) %>%
    dplyr::mutate(text = substr(.data[["text"]], 1, n))
}

#' Pretty printing of EPUB text
#'
#' Print EPUB text to the console in a more readable format.
#'
#' This function prints text from EPUB files to the console using \code{cat}.
#' This is useful for quickly obtaining an overview of the book text parsed by \code{\link{epub}} that is easier to read that looking at strings in the table.
#' \code{max_paragraphs} is set low by default to prevent accidentally printing entire books to the console.
#' To print everything in \code{x}, set \code{max_paragraphs = NULL}.
#'
#' @param x a data frame returned by \code{\link{epub}} or a character string giving the EPUB filename(s).
#' @param max_paragraphs integer, maximum number of paragraphs (non-empty lines) to \code{cat} to console.
#' @param skip integer, number of paragraphs to skip.
#' @param paragraph_spacing integer, number of empty lines between paragraphs.
#' @param paragraph_indent integer, number of spaces to indent paragraphs.
#' @param section_sep character, a string to indicate section breaks.
#' @param book_sep character, separator shown between books when \code{x} has multiple rows (books).
#'
#' @return nothing is returned but a more readable format of the text content for books in \code{x} is printed to the console.
#' @export
#' @seealso \code{\link{epub_head}}
#'
#' @examples
#' file <- system.file("dracula.epub", package = "epubr")
#' d <- epub(file)
#' epub_cat(d, max_paragraphs = 3)
epub_cat <- function(x, max_paragraphs = 10, skip = 0, paragraph_spacing = 1, paragraph_indent = 2,
                     section_sep = "====", book_sep = "====\n===="){
  if(inherits(x, "character")) x <- epub(x)
  paragraph_spacing <- max(0, round(paragraph_spacing))
  f <- function(x){
    x <- dplyr::rowwise(x) %>%
      dplyr::do(x = .pretty_text(.data[["text"]], paragraph_spacing, paragraph_indent))
    x <- x$x
    x <- x[x != ""]
    if(length(x) > 1){
      idx <- 1:(length(x) - 1)
      x[idx] <- paste0(x[idx], "\n\n", section_sep, "\n\n")
    }
    paste0(paste0(unlist(x), collapse = ""), "\n")
  }
  x <- (dplyr::rowwise(x) %>% dplyr::do(x = f(.[["data"]])))$x
  if(length(x) > 1){
    idx <- 1:(length(x) - 1)
    x[idx] <- paste0(x[idx], "\n\n\n\n", book_sep, "\n\n\n\n")
  }
  x <- paste0(unlist(x), collapse = "")
  x <- strsplit(x, "\n") %>% unlist()
  first_line <- skip + 1
  idx <- which(!grepl("^$|^\\s+$", x))
  if(length(idx)){
    if(length(idx) < skip + 1){
      message("`skip` is too large. All text skipped.")
      return(invisible())
    }
    first_line <- idx[skip + 1]
  }
  if(first_line > 1 & length(idx)) idx <- idx[(skip + 1):length(idx)]
  if(is.null(max_paragraphs) || max_paragraphs > length(idx)){
    max_lines <- length(x)
  } else {
    max_lines <- if(length(idx)) min(idx[max_paragraphs], length(x)) else max_paragraphs
  }
  cat(paste0(paste0(x[first_line:max_lines], collapse = "\n"), "\n"))
  invisible()
}

.pretty_text <- function(x, paragraph_spacing = 1, paragraph_indent = 2){
  p1 <- if(paragraph_spacing < 1) "" else paste0(rep("\n", paragraph_spacing), collapse = "")
  p2 <- if(paragraph_indent < 1) "" else paste0(rep(" ", paragraph_indent), collapse = "")
  x <- gsub("\n", paste0(p1, "\n", p2), x)
  x
}

.epub_metakeep <- function(id, href, pattern = NULL){
  x <- !is.na(id) & grepl("html$|htm$", href)
  if(inherits(pattern, "character")) x <- x & !grepl(pattern, id)
  x
}

.chapter_recovery <- function(d, x, override = FALSE){
  if(!"nchap" %in% names(d) || (d$nchap != 0 && !override)) return(list(d, x))
  nc <- nchar(x$text)
  opening <- substr(x$text, 1, 30)
  r <- utils::as.roman(1:30)
  rom <- paste0("^(", paste0(r, collapse = "|"), ")(\\s\\.\\s|\\.\\s|\\n\\n|\\.|[A-Z])")
  one_to_nine <- c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine")
  f <- function(x) c(x, paste0(x, one_to_nine), paste0(paste0(x, "-"), tolower(one_to_nine)))
  numnames0 <- c(one_to_nine, "Ten", "Eleven", "Twelve",
                 "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen",
                 as.character(sapply(
                   c("Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"),
                   f)))
  numnames1 <- toupper(numnames0)
  pat <- paste0(rom, paste0("|^(", paste0(numnames1, collapse = "|"), ")"),
                "|^\\d+\\s+\\n|(C|c)(HAPTER|hapter)|^SECTION|^\\d+[a-zA-Z]")
  idx <- grepl(pat, opening)
  if(!any(idx) & override){
    idx <- nc > 5000
  }
  if(any(idx)){
    d$nchap <- sum(idx)
    x$section[idx] <- paste0("ch", formatC(1:sum(idx), width = 2, flag = "0"))
    dup <- duplicated(rev(x$section))
    if(any(dup)){
      fix_idx <- rev(nrow(x) - which(duplicated(rev(x$section))) + 1)
      x$section[fix_idx] <- paste0("x", seq_along(fix_idx))
    }
    double_check <- unlist(purrr::map(paste0("Chapter ", numnames0, "(\\.|\\s|\\n|)"), ~{
      a <- grep(.x, opening)
      if(!length(a)) return()
      if(length(a) == 1) a else a[which.min(nchar(a))]
    }))
    if(length(double_check)){
      idx0 <- which(grepl("ch\\d\\d", x$section) & grepl("chapter", tolower(opening)) == TRUE)
      if(length(idx0) != length(double_check)) idx0 <- rep(idx0, length = length(double_check))
      x[idx0, ] <- x[double_check, ]
      x$section[idx] <- paste0("ch", formatC(1:sum(idx), width = 2, flag = "0"))
    }
  }
  list(d, x)
}

.clean <- function(x, template = NULL) {
  if (!inherits(x, "html_document")) x <- xml2::read_html(x)
  if(is.null(template)) template <- system.file("text.xml", package = "epubr")
  if(!inherits(template, "character") || !file.exists(template)) stop("`clean` template not found.")
  template <- xml2::read_xml(template)
  x2 <- xslt::xml_xslt(x, template) %>% xml2::xml_text()
  if(nchar(x2) == 0) x <- trimws(xml2::xml_text(x)) else x <- trimws(x2)
  x <- .clean_sub(x)
  x
}

.clean_sub <- function(x){
  x <- gsub("\u2010|\u2011|\u2012|\u2013|\u2014|\u2015", "-", x)
  x <- gsub("\u2018|\u2019", "'", x)
  x <- gsub("\u201c|\u201d", "\"", x)
  x <- gsub("\u2026", "...", x)
  x
}

.get_series <- function(x, subseries = FALSE, parent_dir = "novels"){
  x <- strsplit(dirname(x), "/")
  idx <- purrr::map_dbl(x, ~(which(.x == parent_dir) + 1))
  if(subseries) idx <- idx + 1
  x <- purrr::map_chr(seq_along(x), ~x[[.x]][idx[.x]])
}
