#' Extract and read EPUB e-books
#'
#' Read EPUB format e-books into a data frame using \code{epub} or extract EPUB archive files for direct use with \code{epub_unzip}.
#'
#' @details
#' The primary function here is \code{epub}. It parses EPUB file metadata and textual content into a data frame.
#' The output data frame has one row for each file in \code{file}.
#' It has metadata in all columns except the \code{data} column, which is a column of nested data frames containing e-book text by book section (e.g., chapters).
#' Both the primary and nested data frames are tibbles and safe to print to the console "as is".
#'
#' Be careful if \code{file} is a long vector of many EPUB files.
#' This could take a long time to process as well as could potentially use up all of your system RAM if you have far too many large books in one call to \code{epub}.
#'
#' On a case by case basis, you can always select columns and filter rows of a resulting data frame for a single e-book subsequent to visual inspection.
#' However, the optional arguments \code{fields}, \code{drop_sections} and \code{chapter_pattern} allow you to do some of this as part of the EPUB file reading process.
#' You can ignore these arguments and do all your own post-processing of the resulting data frame, but if using these arguments,
#' they are most likely to be useful for bulk e-book processing where \code{file} is a vector of like-formatted files.
#'
#' \subsection{Main columns}{
#' The \code{fields} argument can be used to limit the columns returned in the primary data frame.
#' E.g., \code{fields = c("title", "creator", "date", "identifier", "publisher", "file")}. Some fields will be returned even if not in \code{fields}, such as \code{data} and \code{title}.
#' \cr\cr
#' Ideally, you should already know what metadata fields are in the EPUB file. This is not possible for large collections with possibly different formatting.
#' Note that when \code{"file"} is included in \code{fields}, the output will include a column of the original file names, in case this is different from the content of a \code{source} field that may be present in the metadata.
#' So this field is always available even if not part of the file metadata.
#' \cr\cr
#' Additionally, if there is no \code{title} field in the metadata, the output data frame will include a \code{title} column filled in with the same file names,
#' unless you pass the additional optional title argument, e.g. \code{title = "TitleFieldID"} so that another field can me mapped to \code{title}.
#' If supplying a \code{title} argument that also does not match an existing field in the e-book, the output \code{title} column will again default to file names.
#' File names are the fallback option because unlike e-book metadata fields, file names always exist and should also always be unique when performing vectorized reads over multiple books,
#' ensuring that \code{title} can be a column in the output data frame that uniquely identifies different e-books even if the books did not have a \code{title} field in their metadata.
#' \cr\cr
#' Columns of the nested data frames in \code{data} are fixed. Select from these in subsequent data frame manipulations.
#' }
#' \subsection{Nested rows}{
#' The \code{chapter_pattern} argument may be helpful for bulk processing of similarly formatted EPUB files. This should be ignored for poorly formatted EPUB files or where there is inconsistent naming across an e-book collection.
#' Like with \code{fields}, you should explore file metadata in advance or this argument will not be useful. If provided, a column \code{nchap} is added to the output data frame giving the guessed number of chapters.
#' In the \code{data} column, the \code{section} column of the nested data frames will also be updated to reflect guessed chapters with new, consistent chapter IDs, always beginning with \code{ch} and ending with digits.
#' \cr\cr
#' The \code{drop_sections} argument also uses regular expression pattern matching like \code{chapter_pattern} and operates on the same \code{section} column. It simply filters out any matched rows.
#' This is useful for dropping rows that may pertain to book cover, copyright and acknowledgements pages, and other similar, clearly non-chapter text, e-book sections.
#' An example that might work for many books could be \code{drop_sections = "^co(v|p)|^ack"}
#' \cr\cr
#' Rows of the primary data frame are fixed. Filter or otherwise manipulate these in subsequent data frame manipulations. There is one row per file so filtering does not make sense to do as part of the initial file reading.
#' }
#' \subsection{EPUB metadata}{
#' Use \code{epub_meta} to return a data frame of only the metadata for each file in \code{file}. This skips the reading of each file's text contents, strictly parsing the metadata.
#' It returns a data frame with one row for each file and \code{n} columns where \code{n} is equal to the union of all fields identified across all files in \code{file}.
#' Fields available for at least one e-book in \code{file} will return \code{NA} in that column for any row pertaining to an e-book that does not have that field in its metadata.
#' If the metadata contains multiple entries for a field, such as multiple subjects or publication dates, they are collapsed using the pipe character.
#' }
#' \subsection{Unzipping EPUB files}{
#' If using \code{epub_unzip} directly on individual EPUB files, this gives you control over where to extract archive files to and what to do with them subsequently.
#' \code{epub} and \code{epub_meta} use \code{epub_unzip} internally to extract EPUB archive files to the R session temp directory (with \code{tempdir()}).
#' You do not need to use \code{epub_unzip} directly prior to using these other functions. It is only needed if you want the internal files for some other purpose in or out of R.
#' }
#'
#' @param file character, input EPUB filename. May be a vector for \code{epub} and \code{epub_meta}. Always a single file for \code{epub_unzip}.
#' @param fields character, vector of metadata fields (data frame columns) to parse from metadata, if they exist. See details.
#' @param drop_sections character, a regular expression pattern string to identify text sections (rows of nested text data frame) to drop.
#' @param chapter_pattern character, a regular expression pattern string to attempt distinguishing nested data frame rows of chapter text entries from other types of entries.
#' @param encoding character, defaults to \code{"UTF-8"}.
#' @param ... additional arguments. With the exception of passing \code{title} (see details), currently developmental/unsupported.
#' @param exdir for \code{epub_unzip}, extraction directory to place archive contents (files). It will be created if necessary.
#'
#' @return \code{epub} returns a data frame. \code{epub_unzip} returns nothing but extracts files from an EPUB file archive.
#' @export
#'
#' @examples
#' # Use a local example EPUB file included in the package
#' file <- system.file("dracula.epub", package = "epubr")
#' bookdir <- file.path(tempdir(), "dracula")
#' epub_unzip(file, exdir = bookdir) # unzip to directly inspect archive files
#' list.files(bookdir, recursive = TRUE)
#'
#' \donttest{
#' epub_meta(file) # parse EPUB file metadata only
#'
#' x <- epub(file) # parse entire e-book
#' x
#' x$data[[1]]
#'
#' epub(file, fields = c("title", "creator"), drop_sections = "^cov")
#' }
epub <- function(file, fields = NULL, drop_sections = NULL,
                 chapter_pattern = NULL, encoding = "UTF-8", ...){
  .check_file(file)
  dots <- list(...)
  if(is.null(dots$title)){
    title <- "title"
  } else {
    title <- dots$title
    fields <- fields[fields != title]
  }
  add_pattern <- dots$add_pattern # nolint
  d <- dplyr::bind_rows(
    lapply(file, .epub_read, fields = fields, drop_sections = drop_sections,
           chapter_pattern = chapter_pattern, add_pattern = add_pattern,
           clean = dots$clean, title = title, encoding = encoding)
  )
  path <- file
  if(!"file" %in% names(d) & "file" %in% fields)
    d <- dplyr::mutate(d, file = basename(path))
  d <- tidyr::unnest(d, .data[["data"]])
  if("nchap" %in% names(d))
    d <- dplyr::mutate(d, is_chapter = grepl("^ch\\d\\d$", .data[["section"]]))
  d <- dplyr::mutate(d, nchar = nchar(.data[["text"]]),
                     nword = count_words(.data[["text"]]))
  if(inherits(drop_sections, "character"))
    d <- dplyr::filter(d, !grepl(drop_sections, .data[["section"]]))
  nested <- c("section", "text")
  if("is_chapter" %in% names(d)) nested <- c(nested, "is_chapter")
  nested <- c(nested, "nword", "nchar")
  d <- tidyr::nest(d, data = !! nested) %>%
    dplyr::mutate_at("data", ~as.list(.))
  cols <- unique(c(fields, names(d)))
  cols <- cols[cols != "data"]
  if("nchap" %in% names(d)) cols <- c(cols, "nchap")
  cols <- unique(c(cols, "data"))
  if(!"date" %in% names(d)) d <- dplyr::mutate(d, date = NA)
  if(!"publisher" %in% names(d)) d <- dplyr::mutate(d, publisher = NA)
  d <- dplyr::mutate_if(d, is.character, ~ifelse(.x == "", NA, .x))
  dplyr::select(d, !! cols)
}

#' @export
#' @rdname epub
epub_meta <- function(file){
  .check_file(file)
  dplyr::bind_rows(lapply(file, function(x){
    exdir <- file.path(tempdir(), gsub("[^A-Za-z0-9]", "", gsub("\\.epub", "",
                                                                basename(x))))
    x <- .epub_meta(epub_unzip(x, exdir), file)
    unlink(exdir, recursive = TRUE, force = TRUE)
    x
  }))
}

#' @export
#' @rdname epub
epub_unzip <- function(file, exdir = tempdir()){
  .check_file(file)
  utils::unzip(file, exdir = exdir)
}

.epub_meta <- function(files, epubfile, fields = NULL, drop_sections = NULL,
                       chapter_pattern = NULL, ...){
  dots <- list(...)
  suppressWarnings(opf <- files[grep("opf$", files)] %>% xml2::read_xml())
  meta <- xml2::xml_find_all(opf, ".//dc:*")
  x <- as.list(gsub("\"", "", xml2::xml_text(meta)))
  y <- xml2::xml_name(meta)
  y <- factor(y, levels = unique(y))
  x <- lapply(split(x, y), function(i) paste(i, collapse = "|"))
  epubtitle <- if(is.null(dots$title)) "title" else dots$title
  d <- tibble::as_tibble(x)
  if(!epubtitle %in% names(d)){
    d <- dplyr::mutate(d, title = epubfile)
  } else if(epubtitle != "title"){
    if("title" %in% names(d)) d <- dplyr::mutate(d, title = NULL)
    d <- dplyr::rename(d, title = .data[[epubtitle]])
  }
  if(!is.null(fields)){
    idx <- which(!names(d) %in% unique(c(fields, "title")))
    d <- dplyr::select(d, -idx)
  }
  meta2 <- xml2::xml_children(opf) %>% xml2::xml_children()
  meta2_id <- xml2::xml_attr(meta2, "id")
  meta2_href <- basename(xml2::xml_attr(meta2, "href"))
  idx <- .epub_metakeep(meta2_id, meta2_href, drop_sections)
  meta2_id <- meta2_id[idx]
  meta2_href <- meta2_href[idx]
  if(inherits(chapter_pattern, "character")){
    pat <- chapter_pattern
    add_pattern <- dots$add_pattern
    if(inherits(add_pattern, "character")){
      pat <- paste0(pat, "|", add_pattern)
    } else if(inherits(add_pattern, "function")){
      check <- add_pattern()$chapter_check
      pat2 <- paste0(pat, "|", add_pattern()$pattern)
      if(!is.null(check) && d$title %in% check) pat <- pat2 else
        if(is.null(check)) pat <- pat2
    }
    chap_idx <- grep(pat, meta2_id)
    meta2_id[chap_idx] <- paste0("ch", formatC(seq_along(chap_idx), width = 2,
                                               flag = "0"))
    d <- dplyr::mutate(d, nchap = length(chap_idx))
  }
  attr(d, "section order") <- meta2_id
  attr(d, "section href") <- meta2_href
  d
}

.epub_read <- function(file, epubfile = basename(file), fields = NULL,
                       drop_sections = NULL, chapter_pattern = NULL,
                       encoding = getOption("encoding"), ...){
  use_readr <- requireNamespace("readr", quietly = TRUE) # nolint
  exdir <- file.path(tempdir(), gsub("[^A-Za-z0-9]", "", gsub("\\.epub", "",
                                                              basename(file))))
  files <- epub_unzip(file, exdir)
  d <- .epub_meta(files, epubfile, fields = fields,
                  drop_sections = drop_sections,
                  chapter_pattern = chapter_pattern, ...)
  files <- files[grep("html$|htm$", files)]
  files <- files[match(attr(d, "section href"), basename(files))]
  clean <- list(...)$clean
  x <- tibble::tibble(
    title = d$title, section = attr(d, "section order"),
    text = sapply(files, function(i){
      if(use_readr){
        con <- file(i, "rb", encoding = encoding)
        x <- readr::read_lines(con)
      } else {
        con <- file(i, "r", encoding = encoding)
        x <- readLines(con, warn = FALSE)
      }
      x <- paste0(x, collapse = "\n") %>% .clean(clean)
      close(con)
      x
    }))
  unlink(exdir, recursive = TRUE, force = TRUE)
  attr(d, "section order") <- NULL
  attr(d, "section href") <- NULL
  override <- FALSE
  add_pattern <- list(...)$add_pattern
  if(!is.null(add_pattern)){
    if(inherits(add_pattern, "function"))
      check <- add_pattern()$chapter_doublecheck else check <- add_pattern
    if(inherits(check, "character") && d$title %in% check) override <- TRUE
  }
  out <- .chapter_recovery(d, x, override = override)
  dplyr::left_join(out[[1]], out[[2]], by = "title") %>%
    tidyr::nest_legacy(.data[["section"]], .data[["text"]])
}

.check_file <- function(file){
  if(!all(file.exists(file))) stop("File not found.")
  if(any(sapply(strsplit(file, "\\."), utils::tail, 1) != "epub"))
    stop("All files must end in `.epub`.")
}
