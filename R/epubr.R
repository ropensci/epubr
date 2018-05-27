globalVariables(c(".data"))

#' epubr: Read EPUB file metadata and text.
#'
#' epubr provides functions supporting the reading and parsing of internal e-book content from EPUB files. Book metadata and textual contents are parse separately.
#'
#' E-book formatting is non-standard enough across all literature that no function can curate parsed e-book content for an across an arbitrary collection of e-books, in completely general form, resulting in a singular, consistently formatted output. Functions in this package that are intended for relatively general application to arbitrary e-books with *a priori* unknown formatting and metadata structure, do minimal to no processing of the output. They essentially only read the content "as is" and attempt to curate them in a data frame. Some poorly formatted e-books may not be readable by these functions at all.
#'
#' @docType package
#' @name epubr
NULL

#' @importFrom magrittr %>%
NULL

#' Open EPUB file archive
#'
#' Unzip an EPUB file.
#'
#' @param file character, input EPUB file.
#' @param exdir character, extraction directory to place archive contents (files).
#'
#' @return nothing returned by files are extracted from archive.
#' @export
#' @seealso \code{\link{epub_meta}}, \code{\link{epub_read}}
#'
#' @examples
#' # not run
epub_unzip <- function(file, exdir = file.path(tempdir(), basename(file))){
  utils::unzip(file, exdir = exdir)
}


#' Read EPUB file metadata
#'
#' Parse EPUB file metadata structure into a data frame. Assumptions are made, liberties taken, some fields may be dropped. You can always inspect the complete archive contents directly after using \code{epub_unzip}.
#'
#' This function is not typically called directly. If called directly, it must be done with \code{files} being a vector of the files created by the extraction performed by \code{epub_unzip}.
#' Typically this function is called internally as part of \code{epub_read}, which combines the e-book metadata with the e-book text in a single data frame.
#'
#' @param files character vector of extracted files from EPUB file archive. See details.
#' @param add_pattern supplemental regular expression pattern string to use for identifying chapters among all document sections. May be a function like, e.g. \code{\link{pat_startrek}} with additional filtering information for specific book titles.
#'
#' @return a data frame.
#' @export
#' @seealso \code{\link{epub_unzip}}, \code{\link{epub_read}}
#'
#' @examples
#' # not run
epub_meta <- function(files, add_pattern = NULL){
  suppressWarnings(opf <- files[grep("opf$", files)] %>% xml2::read_xml())
  meta <- xml2::xml_find_all(opf, ".//dc:*")
  x <- as.list(gsub("\"", "", xml2::xml_text(meta)))
  names(x) <- xml2::xml_name(meta)
  x <- x[!duplicated(names(x))]
  x <- x[names(x) %in% c("title", "creator", "date", "identifier", "publisher")]
  d <- dplyr::as_data_frame(x)
  meta2 <- xml2::xml_children(opf) %>% xml2::xml_children()
  meta2_id <- xml2::xml_attr(meta2, "id")
  meta2_href <- basename(xml2::xml_attr(meta2, "href"))
  idx <- .st_epub_metakeep(meta2_id, meta2_href)
  meta2_id <- meta2_id[idx]
  meta2_href <- meta2_href[idx]
  pat <- paste0("(C|c)(H|h)(_|\\d)|^i0\\d\\d|000000|c0\\d\\d|^c(\\d$|\\d\\d$)|",
                "tocref\\d|Chapter|chapter_\\d\\d|C\\d-|p\\d_c\\d|^c_\\d|",
                "^bk\\d|^rule\\d|^prt$|_RWTOC|\\d+text-(\\d|\\d\\d)$", collapse = "")
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
  cov_idx <- grep("cov", meta2_id)
  if(length(cov_idx)){
    cov_idx <- cov_idx[1]
    if(cov_idx > 1){
      idx <- c(1:(cov_idx - 1))
      idx <- idx[!idx %in% chap_idx]
      if(length(idx)){
        meta2_id <- meta2_id[-idx]
        meta2_href <- meta2_href[-idx]
      }
      cov_idx <- grep("cov", meta2_id)[1]
      if(cov_idx > 1){
        meta2_id <- c(meta2_id[cov_idx], meta2_id[-cov_idx])
        meta2_href <- c(meta2_href[cov_idx], meta2_href[-cov_idx])
      }
    }
  }
  cov_idx <- grep("backcov", meta2_id)
  if(length(cov_idx)){
    cov_idx <- cov_idx[1]
    if(cov_idx < length(meta2_id)){
      meta2_id <- c(meta2_id[-cov_idx], meta2_id[cov_idx])
      meta2_href <- c(meta2_href[-cov_idx], meta2_href[cov_idx])
    }
  }
  attr(d, "section order") <- meta2_id
  attr(d, "section href") <- meta2_href
  d
}

#' Read EPUB file
#'
#' Parse EPUB file metadata and textual content into a data frame.
#'
#' The data frame has metadata in all columns except the \code{data} column, which is a nested column containing a data frame of the e-book text. Both data frames are tibbles and safe to print to the console "as is".
#'
#' @param file character, input EPUB filename.
#' @param add_pattern supplemental regular expression pattern string to use for identifying chapters among all document sections. May be a function like, e.g. \code{\link{pat_startrek}} with additional filtering information for specific book titles.
#' @param hgr_clean logical, if \code{TRUE} and \code{hgr} package is installed from GitHub, an extra cleaning step is performed on the e-book text.
#'
#' @return a data frame
#' @export
#' @seealso \code{\link{epub_unzip}}, \code{\link{epub_meta}}, \code{\link{epub_combine}}
#'
#' @examples
#' # not run
epub_read <- function(file, add_pattern = NULL, hgr_clean = FALSE){
  read <- if(requireNamespace("readr", quietly = TRUE)) readr::read_lines else readLines # nolint start
  if(requireNamespace("hgr", quietly = TRUE)){
    clean <- hgr::clean_text
  } else {
    clean <- function(x) x # nolint end
    warning("`hgr_clean = TRUE` is ignored unless the `hgr` package is installed from GitHub.")
  }
  files <- epub_unzip(file)
  d <- epub_meta(files)
  files <- files[grep("html$|htm$", files)]
  files <- files[match(attr(d, "section href"), basename(files))]
  x <- dplyr::data_frame(
    title = d$title, section = attr(d, "section order"), text = purrr::map_chr(files, ~{
      read(.x) %>% paste0(collapse = "\n") %>% clean()
    }))
  unlink(file.path(tempdir(), basename(file)), recursive = TRUE, force = TRUE)
  attr(d, "section order") <- NULL
  attr(d, "section href") <- NULL
  override <- FALSE
  if(!is.null(add_pattern)){
    if(inherits(add_pattern, "function")) check <- add_pattern()$chapter_doublecheck else
      check <- add_pattern
    if(inherits(check, "character") && d$title %in% check) override <- TRUE
  }
  out <- .chapter_recovery(d, x, override = override)
  dplyr::left_join(out[[1]], out[[2]], by = "title") %>%
    tidyr::nest(.data[["section"]], .data[["text"]])
}

#' Read and combine EPUB e-books
#'
#' Read and combine in a data frame multiple EPUB format e-books.
#'
#' This function will attempt some additional processing of the e-books' metadata and text content. For example, it can attempt to guess series and subseries based on the lowest two directories if files are stored in a nested directory tree.
#' If this does not have any meaning for the \code{files}, leave \code{series = FALSE}.
#'
#' @param files character, input EPUB filename, may be a vector.
#' @param add_pattern supplemental regular expression pattern string to use for identifying chapters among all document sections. May be a function like, e.g. \code{\link{pat_startrek}} with additional filtering information for specific book titles.
#' @param hgr_clean logical, if \code{TRUE} and \code{hgr} package is installed from GitHub, an extra cleaning step is performed on the e-book text.
#' @param section_drop supplemental regular expression pattern string to use for dropping rows, typically non-chapter book sections. May be a function that returns a string like, e.g. \code{\link{sec_drop_startrek}}.
#' @param series logical, attempt to guess series and subseries from directory structure. See details.
#' @param parent_dir character, parent directory under which any series and subseries, if applicable, are nested. Set this whenever \code{series = TRUE}.
#'
#' @return a data frame
#' @export
#' @seealso \code{\link{epub_read}}
#'
#' @examples
#' # not run
epub_combine <- function(files, add_pattern = NULL, hgr_clean = FALSE, section_drop = NULL,
                         series = FALSE, parent_dir = "novels"){
  d <- purrr::map(files, ~epub_read(.x, add_pattern = add_pattern, hgr_clean = hgr_clean)) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(source = basename(files),
                  series = .get_series(files),
                  subseries = .get_series(files, subseries = TRUE)) %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      is_chapter = grepl("^ch\\d\\d$", .data[["section"]]),
      text = .clean_text(.data[["text"]]),
      dedication = ifelse(grepl("^ded", tolower(substr(.data[["section"]], 1, 3))), .data[["text"]], NA),
      hist_note = ifelse(grepl("^hist", tolower(substr(.data[["section"]], 1, 3))), .data[["text"]], NA),
      nchar = nchar(.data[["text"]]),
      nword = purrr::map_int(strsplit(.data[["text"]], " "), length))
  if(inherits(section_drop, "character")) d <- dplyr::filter(d, !grepl(section_drop, .data[["section"]]))
  nested <- c("section", "text", "is_chapter", "dedication", "hist_note", "nchar", "nword")
  d <- tidyr::nest(d, !! nested)
  cols <- c("title", "creator", "date", "publisher", "identifier", "source")
  if(series) cols <- c(cols, c("series", "subseries"))
  cols <- c(cols, "nchap", "data")
  if(!"date" %in% names(d)) d <- dplyr::mutate(d, date = NA)
  if(!"publisher" %in% names(d)) d <- dplyr::mutate(d, publisher = NA)
  d <- dplyr::mutate_if(d, is.character, ~ifelse(.x == "", NA, .x))
  f <- function(x) tools::toTitleCase(tolower(x))
  d <- dplyr::mutate(d, title = f(.data[["title"]]), creator = f(tolower(.data[["creator"]])),
                     publisher = f(tolower(.data[["publisher"]])))
  dplyr::select(d, !! cols)
}
