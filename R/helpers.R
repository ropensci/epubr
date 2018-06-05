#' Preview the first n characters
#'
#' Preview the first n characters of each EPUB e-book section.
#'
#' This function is a wrapper around \code{epub} that returns a simplified data frame of only the \code{section} and \code{text} columns, with the text included only up to the first \code{n} character.
#' This is useful for previewing the opening text of each e-book section to inspect for possible useful regular expression patterns to use for text-based section identification.
#' For example, an e-book may not have meaningful section IDs that distinguish one type of book section from another, such as chapters from non-chapter sections,
#' but the text itself may contain this information at or near the start of a section.
#'
#' @param file character, input EPUB filename. May be a vector.
#' @param n integer, first n characters to retain from each e-book section.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' file <- system.file("dracula.epub", package = "epubr")
#' first_nchar(file)
first_nchar <- function(file, n = 50){
  epub(file) %>% tidyr::unnest() %>% dplyr::select(!! c("section", "text")) %>%
    dplyr::mutate(text = substr(.data[["text"]], 1, n))
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
  numnames0 <- c(one_to_nine, "Ten", "Eleven", "Twelve",
                 "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen", "Twenty",
                 paste0("Twenty", one_to_nine), "Thirty", paste0("Thirty", one_to_nine),
                 paste0("Twenty-", tolower(one_to_nine)), paste0("Thirty-", tolower(one_to_nine)))
  numnames1 <- toupper(numnames0)
  numnames2 <- numnames0#[1:39]
  pat <- paste0(rom, paste0("|^(", paste0(numnames1, collapse = "|"), ")"),
                "|^\\d+\\s+\\n|chapter|CHAPTER|Chapter|^SECTION|^\\d+[a-zA-Z]")
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
    double_check <- unlist(purrr::map(paste0("Chapter ", numnames2, "(\\.|\\s|\\n|)"), ~{
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
  if(nchar(x2) == 0) trimws(xml2::xml_text(x)) else trimws(x2)
}

.get_series <- function(x, subseries = FALSE, parent_dir = "novels"){
  x <- strsplit(dirname(x), "/")
  idx <- purrr::map_dbl(x, ~(which(.x == parent_dir) + 1))
  if(subseries) idx <- idx + 1
  x <- purrr::map_chr(seq_along(x), ~x[[.x]][idx[.x]])
}
