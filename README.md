
<!-- README.md is generated from README.Rmd. Please edit that file -->
epubr
=====

[![Travis-CI Build Status](https://travis-ci.org/leonawicz/epubr.svg?branch=master)](https://travis-ci.org/leonawicz/epubr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/epubr?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/epubr) [![Coverage Status](https://img.shields.io/codecov/c/github/leonawicz/epubr/master.svg)](https://codecov.io/github/leonawicz/epubr?branch=master)

Read metadata and textual content of epub files.

*Package under development.*

`epubr` provides functions supporting the reading and parsing of internal e-book content from EPUB files. E-book metadata and text content are parsed separately and joined together in a tidy, nested tibble data frame.

E-book formatting is non-standard enough across all literature that no function can curate parsed e-book content across an arbitrary collection of e-books, in completely general form, resulting in a singular, consistently formatted output containing all the same variables.

EPUB file parsing functionality in this package is intended for relatively general application to arbitrary EPUB e-books. However, poorly formatted e-books or e-books with highly uncommon formatting may not work at all with this package. Text is largely read 'as is', along with some nominal cleaning performed during the parsing process primarily to strip xml content, leaving only the original text.

Additional text cleaning should be performed by the user at their discretion, such as with functions from packages like or .

Installation
------------

Install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("leonawicz/epubr")
```
