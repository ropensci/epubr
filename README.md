
<!-- README.md is generated from README.Rmd. Please edit that file -->
epubr <img src="https://github.com/leonawicz/epubr/blob/master/data-raw/epubr.png?raw=true" style="margin-left:10px;margin-bottom:5px;" width="120" align="right">
==================================================================================================================================================================

<br/> **Author:** [Matthew Leonawicz](https://leonawicz.github.io/blog/)<br/> **License:** [MIT](https://opensource.org/licenses/MIT)<br/>

[![CRAN status](http://www.r-pkg.org/badges/version/epubr)](https://cran.r-project.org/package=epubr) [![CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/epubr)](https://cran.r-project.org/package=epubr) [![Rdoc](http://www.rdocumentation.org/badges/version/epubr)](http://www.rdocumentation.org/packages/epubr) [![Travis-CI Build Status](https://travis-ci.org/leonawicz/epubr.svg?branch=master)](https://travis-ci.org/leonawicz/epubr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/epubr?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/epubr) [![Coverage Status](https://img.shields.io/codecov/c/github/leonawicz/epubr/master.svg)](https://codecov.io/github/leonawicz/epubr?branch=master) [![gitter](https://img.shields.io/badge/GITTER-join%20chat-green.svg)](https://gitter.im/leonawicz/epubr)
<p>
</p>


Read EPUB files in R
--------------------

Read metadata and textual content of epub files.

`epubr` provides functions supporting the reading and parsing of internal e-book content from EPUB files. E-book metadata and text content are parsed separately and joined together in a tidy, nested tibble data frame.

E-book formatting is non-standard enough across all literature that no function can curate parsed e-book content across an arbitrary collection of e-books, in completely general form, resulting in a singular, consistently formatted output containing all the same variables.

EPUB file parsing functionality in this package is intended for relatively general application to arbitrary EPUB e-books. However, poorly formatted e-books or e-books with highly uncommon formatting may not work with this package. Text is read 'as is'. Additional text cleaning should be performed by the user at their discretion, such as with functions from packages like `tm` or `qdap`.

Installation
------------

Install `epubr` from CRAN with:

``` r
install.packages("epubr")
```

Install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("leonawicz/epubr")
```

*Note: Prior versions required R &gt;= 3.5.0 due to package dependencies. This is no longer the case, but if using R &lt; 3.5.0, install from GitHub until the next CRAN release.*

Example
-------

Bram Stoker's Dracula novel sourced from Project Gutenberg is a good example of an EPUB file with unfortunate formatting. The first thing that stands out is the naming convention using `item` followed by some ordered digits does not differentiate sections like the book preamble from the chapters. The numbering also starts in a weird place. But it is actually worse than this. Notice that sections are not broken into chapters; they can begin and end in the middle of chapters!

These annoyances aside, the metadata and contents can still be read into a convenient table. Text mining analyses can still be performed on the overall book, if not so easily on individual chapters.

``` r
file <- system.file("dracula.epub", package = "epubr")
(x <- epub(file))
#> # A tibble: 1 x 9
#>   rights  identifier   creator  title language subject date  source  data 
#>   <chr>   <chr>        <chr>    <chr> <chr>    <chr>   <chr> <chr>   <lis>
#> 1 Public~ http://www.~ Bram St~ Drac~ en       Horror~ 1995~ http:/~ <tib~

x$data[[1]]
#> # A tibble: 15 x 4
#>    section           text                                      nword nchar
#>    <chr>             <chr>                                     <int> <int>
#>  1 item6             "The Project Gutenberg EBook of Dracula,~ 11252 60972
#>  2 item7             "But I am not in heart to describe beaut~ 13740 71798
#>  3 item8             "“ ‘Lucy, you are an honest-hearted girl~ 12356 65522
#>  4 item9             "CHAPTER VIIIMINA MURRAY’S JOURNAL\nSame~ 12042 62724
#>  5 item10            "CHAPTER X\nLetter, Dr. Seward to Hon. A~ 12599 66678
#>  6 item11            "Once again we went through that ghastly~ 11919 62949
#>  7 item12            "CHAPTER XIVMINA HARKER’S JOURNAL\n23 Se~ 12003 62234
#>  8 item13            "CHAPTER XVIDR. SEWARD’S DIARY—continued~ 13812 72903
#>  9 item14            "“Thus when we find the habitation of th~ 13201 69779
#> 10 item15            "“I see,” I said. “You want big things t~ 12706 66921
#> 11 item16            "CHAPTER XXIIIDR. SEWARD’S DIARY\n3 Octo~ 11818 61550
#> 12 item17            "CHAPTER XXVDR. SEWARD’S DIARY\n11 Octob~ 12989 68564
#> 13 item18            " \nLater.—Dr. Van Helsing has returned.~  8356 43464
#> 14 item19            "End of the Project Gutenberg EBook of D~  2669 18541
#> 15 coverpage-wrapper ""                                            0     0
```

Reference
---------

#### Documentation

[Complete package reference and function documentation](https://leonawicz.github.io/epubr/)

#### Related packages

Users may also be interested in the related package, [gutenbergr](https://github.com/ropenscilabs/gutenbergr), for searching and downloading public domain texts from Project Gutenberg.

------------------------------------------------------------------------

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
