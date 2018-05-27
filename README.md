
<!-- README.md is generated from README.Rmd. Please edit that file -->
epubr
=====

[![Travis-CI Build Status](https://travis-ci.org/leonawicz/epubr.svg?branch=master)](https://travis-ci.org/leonawicz/epubr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/epubr?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/epubr) [![Coverage Status](https://img.shields.io/codecov/c/github/leonawicz/epubr/master.svg)](https://codecov.io/github/leonawicz/epubr?branch=master)

Read metadata and textual content of epub files. *Under development.*

`epubr` provides functions supporting the reading and parsing of internal e-book content from EPUB files. E-book metadata and textual contents are parse separately.

E-book formatting is non-standard enough across all literature that no function can curate parsed e-book content for an across an arbitrary collection of e-books, in completely general form, resulting in a singular, consistently formatted output. Functions in this package that are intended for relatively general application to arbitrary e-books with *a priori* unknown formatting and metadata structure, do minimal to no processing of the output. They essentially only read the content "as is" and attempt to curate them in a data frame. Some poorly formatted e-books may not be readable by these functions at all.

Installation
------------

Install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("leonawicz/epubr")
```
