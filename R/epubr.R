globalVariables(c(".data"))

#' epubr: Read EPUB file metadata and text.
#'
#' epubr provides functions supporting the reading and parsing of internal e-book content from EPUB files. Book metadata and textual contents are parse separately.
#'
#' E-book formatting is non-standard enough across all literature that no function can curate parsed e-book content across an arbitrary collection of e-books, in completely general form, resulting in a singular, consistently formatted output containing all the same variables.
#'
#' EPUB file parsing functionality in this package is intended for relatively general application to arbitrary e-books. However, poorly formatted e-books or e-books with highly uncommon formatting may not work at all with this package.
#' Text is read 'as is'. Additional text cleaning should be performed by the user at their discretion, such as with functions from packages like \code{tm} or \code{qdap}.
#'
#' @docType package
#' @name epubr
NULL

#' @importFrom magrittr %>%
NULL

#' Extract and read EPUB e-books
#'
#' Read EPUB format e-books into a data frame using \code{epub} or extract epub archive files for direct use with \code{epub_unzip}.
#'
#' The primary function here is \code{epub}. It parses EPUB file metadata and textual content into a data frame.
#' The output data frame has one row for each file in \code{file}.
#' It has metadata in all columns except the \code{data} column, which is a column of nested data frames containing e-book text by book section (e.g., chapters).
#' Both the primary and nested data frames are tibbles and safe to print to the console "as is".
#'
#' Be careful if \code{file} is a long vector of many EPUB files.
#' This could take a long time to process as well as could potentially use up all of your system RAM if you have far too many large books in one call to \code{epub}.
#'
#' The \code{fields} argument can be used to limit the columns returned. E.g., \code{fields = c("title", "creator", "date", "identifier", "publisher", "file")}. Some fields will be returned even if not in \code{fields}.
#' You should already know what metadata fields are in the EPUB file. \code{file} in \code{fields} will include the original file name, in case this is different from the content of a \code{source} field that may be present in the metadata.
#'
#' The \code{chapter_pattern} argument may be helpful for bulk processing of similarly formatted EPUB files. This should be ignored for poorly formatted EPUB files or where there is inconsistent naming across an e-book collection.
#' Like with \code{fields}, you should explore file metadata in advance or this argument will not be useful. If provided, a column \code{nchap} is added to the output data frame giving the guessed number of chapters.
#' The \code{section} column will also be updated to reflect guessed chapters with new, consistent chapter IDs.
#'
#' If using \code{epub_unzip} directly on individual EPUB files, this gives you control over where to extract archive files to and what to do with them subsequently.
#' \code{epub} uses \code{epub_unzip} internally to extract EPUB archive files to the R session temp directory (with \code{tempdir()}).
#'
#' @param file character, input EPUB filename. May be a vector for \code{epub}. Always a single file for \code{epub_unzip}.
#' @param fields character, vector of metadata fields (data frame columns) to parse from metadata, if they exist. See details.
#' @param drop_sections character, a regular expression pattern string to identify text sections (rows of nested text data frame) to drop.
#' @param chapter_pattern character, a regular expression pattern string to attempt distinguishing nested data frame rows of chapter text entries from other types of entries.
#' @param exdir character, extraction directory to place archive contents (files).
#' @param ... additional arguments. Currently unsupported.
#'
#' @return \code{epub} returns a data frame. \code{epub_unzip} returns nothing but extracts files from an epub file archive.
#' @export
#'
#' @examples
#' file <- system.file("dracula.epub", package = "epubr")
#' epub_unzip(file) # unzip to directly inspect archive files
#' list.files(file.path(tempdir(), "OEBPS"))
#'
#' x <- epub(file)
#' x
#' x$data[[1]]
#'
#' x <- epub(file, fields = c("title", "creator"), drop_sections = "^cov")
#' x
#' x$data[[1]]
epub <- function(file, fields = NULL, drop_sections = NULL, chapter_pattern = NULL, ...){
  dots <- list(...)
  add_pattern <- dots$add_pattern # nolint
  series <- if(is.logical(dots$series)) dots$series else FALSE
  dedication <- if(is.logical(dots$dedication)) dots$dedication else FALSE
  hist_note <- if(is.logical(dots$hist_note)) dots$hist_note else FALSE
  parent_dir <- if(is.null(dots$parent_dir)) "novels" else dots$parent_dir
  hist_note <- if(is.logical(dots$hist_note)) dots$hist_note else FALSE
  d <- purrr::map(file, ~.epub_read(.x, fields = fields, drop_sections = drop_sections,
                                    chapter_pattern = chapter_pattern, add_pattern = add_pattern,
                                    clean = dots$clean)) %>% dplyr::bind_rows()
  path <- file
  if(!"file" %in% names(d) & "file" %in% fields) d <- dplyr::mutate(d, file = basename(path))
  if(series) d <- dplyr::mutate(d, series = .get_series(path, FALSE, parent_dir),
                                subseries = .get_series(path, TRUE, parent_dir))
  d <- tidyr::unnest(d)
  if("nchap" %in% names(d)) d <- dplyr::mutate(d, is_chapter = grepl("^ch\\d\\d$", .data[["section"]]))
  d <- dplyr::mutate(d, nchar = nchar(.data[["text"]]),
                     nword = purrr::map_int(strsplit(.data[["text"]], " "), length))
  if(dedication) d <- dplyr::mutate(d,
    dedication = ifelse(grepl("^ded", tolower(substr(.data[["section"]], 1, 3))), .data[["text"]], NA))
  if(hist_note) d <- dplyr::mutate(d,
    hist_note = ifelse(grepl("^hist", tolower(substr(.data[["section"]], 1, 3))), .data[["text"]], NA))
  if(inherits(drop_sections, "character")) d <- dplyr::filter(d, !grepl(drop_sections, .data[["section"]]))
  nested <- c("section", "text")
  if("is_chapter" %in% names(d)) nested <- c(nested, "is_chapter")
  if(dedication) nested <- c(nested, "dedication")
  if(hist_note) nested <- c(nested, "hist_note")
  nested <- c(nested, "nchar", "nword")
  d <- tidyr::nest(d, !! nested)
  cols <- unique(c(fields, names(d)))
  cols <- cols[cols != "data"]
  if(series) cols <- c(cols, c("series", "subseries"))
  if("nchap" %in% names(d)) cols <- c(cols, "nchap")
  cols <- unique(c(cols, "data"))
  if(!"date" %in% names(d)) d <- dplyr::mutate(d, date = NA)
  if(!"publisher" %in% names(d)) d <- dplyr::mutate(d, publisher = NA)
  d <- dplyr::mutate_if(d, is.character, ~ifelse(.x == "", NA, .x))
  dplyr::select(d, !! cols)
}

#' @export
#' @rdname epub
epub_unzip <- function(file, exdir = tempdir()){
  utils::unzip(file, exdir = exdir)
}

.epub_meta <- function(files, fields = NULL, drop_sections = NULL, chapter_pattern = NULL, ...){
  suppressWarnings(opf <- files[grep("opf$", files)] %>% xml2::read_xml())
  meta <- xml2::xml_find_all(opf, ".//dc:*")
  x <- as.list(gsub("\"", "", xml2::xml_text(meta)))
  names(x) <- xml2::xml_name(meta)
  x <- x[!duplicated(names(x))]
  if(!is.null(fields)) x <- x[names(x) %in% fields]
  d <- dplyr::as_data_frame(x)
  meta2 <- xml2::xml_children(opf) %>% xml2::xml_children()
  meta2_id <- xml2::xml_attr(meta2, "id")
  meta2_href <- basename(xml2::xml_attr(meta2, "href"))
  idx <- .epub_metakeep(meta2_id, meta2_href, drop_sections)
  meta2_id <- meta2_id[idx]
  meta2_href <- meta2_href[idx]
  if(inherits(chapter_pattern, "character")){
    pat <- chapter_pattern
    dots <- list(...)
    add_pattern <- dots$add_pattern
    if(inherits(add_pattern, "character")){
      pat <- paste0(pat, "|", add_pattern)
    } else if(inherits(add_pattern, "function")){
      check <- add_pattern()$chapter_check
      pat2 <- paste0(pat, "|", add_pattern()$pattern)
      if(!is.null(check) && d$title %in% check) pat <- pat2 else if(is.null(check)) pat <- pat2
    }
    chap_idx <- grep(pat, meta2_id)
    meta2_id[chap_idx] <- paste0("ch", formatC(seq_along(chap_idx), width = 2, flag = "0"))
    d <- dplyr::mutate(d, nchap = length(chap_idx))
  }
  attr(d, "section order") <- meta2_id
  attr(d, "section href") <- meta2_href
  d
}

.epub_read <- function(file, fields = NULL, drop_sections = NULL, chapter_pattern = NULL, ...){
  read <- if(requireNamespace("readr", quietly = TRUE)) readr::read_lines else readLines # nolint
  files <- epub_unzip(file)
  d <- .epub_meta(files, fields = fields, drop_sections = drop_sections,
                  chapter_pattern = chapter_pattern, ...)
  files <- files[grep("html$|htm$", files)]
  files <- files[match(attr(d, "section href"), basename(files))]
  clean <- list(...)$clean # nolint
  x <- dplyr::data_frame(
    title = d$title, section = attr(d, "section order"), text = purrr::map_chr(files, ~{
      read(.x) %>% paste0(collapse = "\n") %>% .clean(clean)
    }))
  unlink(file.path(tempdir(), basename(file)), recursive = TRUE, force = TRUE)
  attr(d, "section order") <- NULL
  attr(d, "section href") <- NULL
  override <- FALSE
  add_pattern <- list(...)$add_pattern
  if(!is.null(add_pattern)){
    if(inherits(add_pattern, "function")) check <- add_pattern()$chapter_doublecheck else
      check <- add_pattern
    if(inherits(check, "character") && d$title %in% check) override <- TRUE
  }
  out <- .chapter_recovery(d, x, override = override)
  dplyr::left_join(out[[1]], out[[2]], by = "title") %>%
    tidyr::nest(.data[["section"]], .data[["text"]])
}
