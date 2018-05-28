
<!-- README.md is generated from README.Rmd. Please edit that file -->
epubr
=====

[![CRAN status](http://www.r-pkg.org/badges/version/epubr)](https://cran.r-project.org/package=epubr) [![CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/epubr)](https://cran.r-project.org/package=epubr) [![Rdoc](http://www.rdocumentation.org/badges/version/epubr)](http://www.rdocumentation.org/packages/epubr) [![Travis-CI Build Status](https://travis-ci.org/leonawicz/epubr.svg?branch=master)](https://travis-ci.org/leonawicz/epubr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/epubr?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/epubr) [![Coverage Status](https://img.shields.io/codecov/c/github/leonawicz/epubr/master.svg)](https://codecov.io/github/leonawicz/epubr?branch=master) [![gitter](https://img.shields.io/badge/GITTER-join%20chat-green.svg)](https://gitter.im/leonawicz/epubr)

Read EPUB files in R
--------------------

Read metadata and textual content of epub files.

`epubr` provides functions supporting the reading and parsing of internal e-book content from EPUB files. E-book metadata and text content are parsed separately and joined together in a tidy, nested tibble data frame.

E-book formatting is non-standard enough across all literature that no function can curate parsed e-book content across an arbitrary collection of e-books, in completely general form, resulting in a singular, consistently formatted output containing all the same variables.

EPUB file parsing functionality in this package is intended for relatively general application to arbitrary EPUB e-books. However, poorly formatted e-books or e-books with highly uncommon formatting may not work at all with this package. Text is read 'as is'. Additional text cleaning should be performed by the user at their discretion, such as with functions from packages like `tm` or `qdap`.

This package provides sufficient core functionality at this time. Subsequent versions will include a more robust set of features.

Installation
------------

Install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("leonawicz/epubr")
```

Example
-------

``` r
file <- system.file("dracula.epub", package = "epubr")
(x <- epub(file))
#> # A tibble: 1 x 9
#>   rights  identifier   creator  title language subject date  source  data 
#>   <chr>   <chr>        <chr>    <chr> <chr>    <chr>   <chr> <chr>   <lis>
#> 1 Public~ http://www.~ Bram St~ Drac~ en       Horror~ 1995~ http:/~ <tib~

x$data[[1]]
#> # A tibble: 15 x 4
#>    section           text                                      nchar nword
#>    <chr>             <chr>                                     <int> <int>
#>  1 item6             "The Project Gutenberg EBook of Dracula,~ 60972 11252
#>  2 item7             "But I am not in heart to describe beaut~ 71798 13740
#>  3 item8             "“ ‘Lucy, you are an honest-hearted girl~ 65522 12356
#>  4 item9             "CHAPTER VIIIMINA MURRAY’S JOURNAL\nSame~ 62724 12042
#>  5 item10            "CHAPTER X\nLetter, Dr. Seward to Hon. A~ 66678 12599
#>  6 item11            "Once again we went through that ghastly~ 62949 11919
#>  7 item12            "CHAPTER XIVMINA HARKER’S JOURNAL\n23 Se~ 62234 12003
#>  8 item13            "CHAPTER XVIDR. SEWARD’S DIARY—continued~ 72903 13812
#>  9 item14            "“Thus when we find the habitation of th~ 69779 13201
#> 10 item15            "“I see,” I said. “You want big things t~ 66921 12706
#> 11 item16            "CHAPTER XXIIIDR. SEWARD’S DIARY\n3 Octo~ 61550 11818
#> 12 item17            "CHAPTER XXVDR. SEWARD’S DIARY\n11 Octob~ 68564 12989
#> 13 item18            " \nLater.—Dr. Van Helsing has returned.~ 43464  8356
#> 14 item19            "End of the Project Gutenberg EBook of D~ 18541  2669
#> 15 coverpage-wrapper ""                                            0     0
```

Reference
---------

[Complete package reference and function documentation](https://leonawicz.github.io/epubr/)
