.st_epub_metakeep <- function(id, href){
  pat <- paste0("signup|isbn|font|css|^cop$|copy|style|image|logo|contents")
  !is.na(id) & grepl("html$|htm$", href) & !grepl(pat, id)
}

.st_chappat_overrides <- c("INVINCIBLE", "Have Tech, Will Travel", "SPARTACUS", "WAR DRUMS", "THE ROMULAN STRATAGEM",
                           "ROGUE SAUCER", "I,Q", "GEMWORLD: BOOK ONE OF TWO", "The Genesis Wave Book Three",
                           "Star Trek: The Next Generation: The Stuff of Dreams", "Memory Prime")

.st_chapcheck_overrides <- c("MARTYR", "No Limits", "Spectre", "SPARTACUS", "WAR DRUMS", "THE ROMULAN STRATAGEM",
                             "ROGUE SAUCER", "I,Q", "GEMWORLD: BOOK ONE OF TWO",
                             "Star Trek: The Next Generation: The Stuff of Dreams", "Memory Prime")

#' Star Trek novel overrides
#'
#' Pattern helper function specific to Star Trek novels. A function that takes no arguments and returns a named list of three elements: \code{pattern} is a regular expression pattern string,
#' \code{chapter_check} is a character vector giving exact (in-metadata) book titles as a filter to only apply the supplemental \code{pattern} to specific books,
#' and \code{chapter_doublecheck} is a vector of titles that applies additional checks that may only be necessary and helpful (and non-harmful) for specific titles.
#'
#' @return a named list.
#' @export
#'
#' @examples
#' pat_startrek()
pat_startrek <- function(){
  list(pattern = "toc_ref\\d|^con$|^i2000\\d|^ref(\\d$|\\d\\d$)|^dreams-\\d",
       chapter_check = .st_chappat_overrides,
       chapter_doublecheck = .st_chapcheck_overrides)
}

#' Star Trek novel section filter
#'
#' Regular expression pattern for dropping commonly named non-chapter sections that typically apply to popular Star Trek novels.
#'
#' @return a character string.
#' @export
#'
#' @examples
#' sec_drop_startrek()
sec_drop_startrek <- function(){
  "^(C|c)o(v|n|p)|^(T|t)it|^(A|a)ck|^htl|reg(front|back)"
}

.chapter_recovery <- function(d, x, override = FALSE){
  if(d$nchap != 0 & !override) return(list(d, x))
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
  numnames2 <- numnames0#[1:20]
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

#' Star Trek EPUB tester
#'
#' Example testing function for parsing Star Trek novels.
#'
#' @param file character, input EPUB file.
#' @param details logical, print more details to console.
#' @param add_tail logical, also print tail of data frame.
#'
#' @return nothing is returned but information is logged at the console.
#' @export
#'
#' @examples
#' # not run
st_epub_test <- function(file, details = FALSE, add_tail = FALSE){
  x <- epub_read(file, add_pattern = pat_startrek(), hgr_clean = TRUE)
  if(!all(c("title", "creator") %in% names(x))) warning("`title` and/or `author` missing.")
  if(x$nchap == 0) warning("`nchap` is zero.")
  if(nrow(x$data[[1]]) < 5) warning("Content data frame has fewer than five rows.")
  if(details){
    print(x)
    print(x$data[[1]])
    if(add_tail) print(utils::tail(x$data[[1]]))
  }
  cat("Checks completed. ---- ", x$title, "\n")
  invisible()
}

.clean_text <- function(x){
  tm::stripWhitespace(x) %>% qdap::replace_ordinal() %>% qdap::replace_symbol()
}

.get_series <- function(x, subseries = FALSE, parent_dir = "novels"){
  x <- strsplit(dirname(x), "/")
  idx <- purrr::map_dbl(x, ~(which(.x == parent_dir) + 1))
  if(subseries) idx <- idx + 1
  x <- purrr::map_chr(seq_along(x), ~x[[.x]][idx[.x]])
}

#' Fix Star Trek date column
#'
#' Improve \code{date} column in EPUB data frame output for Star Trek novels by replacing their year dates with \code{yyyy-mm-dd} format date strings taken from any available (most) \code{source} column input filenames.
#'
#' @param x a data frame returned by a function such as \code{epub_read} or \code{epub_combine}.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' # not run
st_fix_date <- function(x){
  y <- stringr::str_extract(basename(x$source), "\\d{8}")
  dplyr::mutate(x, date = ifelse(is.na(y), x, paste(substr(y, 1, 4), substr(y, 5, 6), substr(y, 7, 8), sep = "-")))
}
