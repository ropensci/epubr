
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epubr <img src="man/figures/logo.png" style="margin-left:10px;margin-bottom:5px;" width="120" align="right">

**Author:** [Matthew Leonawicz](https://github.com/leonawicz)
<a href="https://orcid.org/0000-0001-9452-2771" target="orcid.widget">
<img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16" /></a>
<br/> **License:** [MIT](https://opensource.org/licenses/MIT)<br/>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/ropensci/epubr?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/epubr)
[![codecov](https://codecov.io/gh/ropensci/epubr/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/epubr)
[![](https://badges.ropensci.org/222_status.svg)](https://github.com/ropensci/software-review/issues/222)

[![CRAN
status](http://www.r-pkg.org/badges/version/epubr)](https://cran.r-project.org/package=epubr)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/epubr)](https://cran.r-project.org/package=epubr)
[![Github
Stars](https://img.shields.io/github/stars/ropensci/epubr.svg?style=social&label=Github)](https://github.com/ropensci/epubr)

## Read EPUB files in R

Read EPUB text and metadata.

The `epubr` package provides functions supporting the reading and
parsing of internal e-book content from EPUB files. E-book metadata and
text content are parsed separately and joined together in a tidy, nested
tibble data frame.

E-book formatting is not completely standardized across all literature.
It can be challenging to curate parsed e-book content across an
arbitrary collection of e-books perfectly and in completely general
form, to yield a singular, consistently formatted output. Many EPUB
files do not even contain all the same pieces of information in their
respective metadata.

EPUB file parsing functionality in this package is intended for
relatively general application to arbitrary EPUB e-books. However,
poorly formatted e-books or e-books with highly uncommon formatting may
not work with this package. There may even be cases where an EPUB file
has DRM or some other property that makes it impossible to read with
`epubr`.

Text is read ‘as is’ for the most part. The only nominal changes are
minor substitutions, for example curly quotes changed to straight
quotes. Substantive changes are expected to be performed subsequently by
the user as part of their text analysis. Additional text cleaning can be
performed at the user’s discretion, such as with functions from packages
like `tm` or `qdap`.

## Installation

Install `epubr` from CRAN with:

``` r
install.packages("epubr")
```

Install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("ropensci/epubr")
```

## Example

Bram Stoker’s Dracula novel sourced from Project Gutenberg is a good
example of an EPUB file with unfortunate formatting. The first thing
that stands out is the naming convention using `item` followed by some
ordered digits does not differentiate sections like the book preamble
from the chapters. The numbering also starts in a weird place. But it is
actually worse than this. Notice that sections are not broken into
chapters; they can begin and end in the middle of chapters\!

These annoyances aside, the metadata and contents can still be read into
a convenient table. Text mining analyses can still be performed on the
overall book, if not so easily on individual chapters. See the [package
vignette](https://docs.ropensci.org/epubr/articles/epubr.html) for
examples on how to further improve the structure of an e-book with
formatting like this.

``` r
file <- system.file("dracula.epub", package = "epubr")
(x <- epub(file))
#> # A tibble: 1 x 9
#>   rights         identifier           creator   title  language subject                                                                       date                source                 data       
#>   <chr>          <chr>                <chr>     <chr>  <chr>    <chr>                                                                         <chr>               <chr>                  <list>     
#> 1 Public domain~ http://www.gutenber~ Bram Sto~ Dracu~ en       Horror tales|Epistolary fiction|Gothic fiction (Literary genre)|Vampires -- ~ 1995-10-01|2017-10~ http://www.gutenberg.~ <tibble[,4~

x$data[[1]]
#> # A tibble: 15 x 4
#>    section          text                                                                                                                                                                 nword nchar
#>    <chr>            <chr>                                                                                                                                                                <int> <int>
#>  1 item6            "The Project Gutenberg EBook of Dracula, by Bram StokerThis eBook is for the use of anyone anywhere at no cost and withalmost no restrictions whatsoever.  You may ~ 11446 60972
#>  2 item7            "But I am not in heart to describe beauty, for when I had seen the view I explored further; doors, doors, doors everywhere, and all locked and bolted. In no place ~ 13879 71798
#>  3 item8            "\" 'Lucy, you are an honest-hearted girl, I know. I should not be here speaking to you as I am now if I did not believe you clean grit, right through to the very ~ 12474 65522
#>  4 item9            "CHAPTER VIIIMINA MURRAY'S JOURNAL\nSame day, 11 o'clock p. m.-Oh, but I am tired! If it were not that I had made my diary a duty I should not open it to-night. We~ 12177 62724
#>  5 item10           "CHAPTER X\nLetter, Dr. Seward to Hon. Arthur Holmwood.\n\"6 September.\n\"My dear Art,-\n\"My news to-day is not so good. Lucy this morning had gone back a bit. T~ 12806 66678
#>  6 item11           "Once again we went through that ghastly operation. I have not the heart to go through with the details. Lucy had got a terrible shock and it told on her more than~ 12103 62949
#>  7 item12           "CHAPTER XIVMINA HARKER'S JOURNAL\n23 September.-Jonathan is better after a bad night. I am so glad that he has plenty of work to do, for that keeps his mind off t~ 12214 62234
#>  8 item13           "CHAPTER XVIDR. SEWARD'S DIARY-continued\nIT was just a quarter before twelve o'clock when we got into the churchyard over the low wall. The night was dark with oc~ 13990 72903
#>  9 item14           "\"Thus when we find the habitation of this man-that-was, we can confine him to his coffin and destroy him, if we obey what we know. But he is clever. I have asked~ 13356 69779
#> 10 item15           "\"I see,\" I said. \"You want big things that you can make your teeth meet in? How would you like to breakfast on elephant?\"\n\"What ridiculous nonsense you are ~ 12866 66921
#> 11 item16           "CHAPTER XXIIIDR. SEWARD'S DIARY\n3 October.-The time seemed terrible long whilst we were waiting for the coming of Godalming and Quincey Morris. The Professor tri~ 11928 61550
#> 12 item17           "CHAPTER XXVDR. SEWARD'S DIARY\n11 October, Evening.-Jonathan Harker has asked me to note this, as he says he is hardly equal to the task, and he wants an exact re~ 13119 68564
#> 13 item18           " \nLater.-Dr. Van Helsing has returned. He has got the carriage and horses; we are to have some dinner, and to start in an hour. The landlady is putting us up a h~  8435 43464
#> 14 item19           "End of the Project Gutenberg EBook of Dracula, by Bram Stoker*** END OF THIS PROJECT GUTENBERG EBOOK DRACULA ******** This file should be named 345-h.htm or 345-h~  2665 18541
#> 15 coverpage-wrapp~ ""                                                                                                                                                                       0     0
```

## Related packages

[tesseract](https://github.com/ropensci/tesseract) by @jeroen for more
direct control of the OCR process.

[pdftools](https://github.com/ropensci/pdftools) for extracting metadata
and text from PDF files (therefore more specific to PDF, and without a
Java dependency)

[tabulizer](https://github.com/ropensci/tabulizer) by @leeper and
@tpaskhalis, Bindings for Tabula PDF Table Extractor Library, to extract
tables, therefore not text, from PDF files.

[rtika](https://github.com/ropensci/rtika) by @goodmansasha for more
general text parsing.

[gutenbergr](https://github.com/ropensci/gutenbergr) by @dgrtwo for
searching and downloading public domain texts from Project Gutenberg.

-----

Please note that the `epubr` project is released with a [Contributor
Code of
Conduct](https://github.com/ropensci/epubr/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
