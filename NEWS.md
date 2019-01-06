# epubr 0.6.0

* Added `count_words` helper function.
* Improved word count accuracy in `epub`. Now also splitting words on new line characters rather than only on spaces. Now also ignoring vector elements in the split result that are most likely to not be words, such as stranded pieces of punctuation.
* Added `epub_recombine` for breaking apart and recombining text sections into new data frame rows using alternative breaks based on a regular expression pattern.
* Added `epub_sift` function for filtering out small text sections based on low word or character count. This function can also be used directly inside calls to `epub_recombine` through an argument list.
* Added `epub_reorder` for reordering a specified (by index) subset of text section data frame rows according to a text parsing function (several template functions are available for convenient use to address common cases).
* Refactored code to remove `purrr` dependency.
* Added unit tests.
* Updated function documentation, readme and vignette.

# epubr 0.5.0

* Added `epub_cat` function for pretty printing to console as a helpful way to quickly inspect the parsed text in a more easily readable format than looking at the quoted strings in the table entries. `epub_cat` can take an EPUB filename string (may be a vector) as its first argument or a data frame already returned by `epub`.
* Like `epub_cat`, `epub_head` accepts EPUB character filenames or now also a data frame already returned by `epub` based on those files. Because of this change, the first argument has been renamed from `file` to `x`.
* Added `encoding` argument to `epub` function, defaulting to UTF-8.
    * This helps significantly with reading EPUB archive files properly, e.g., providing ability to parse and substitute all the curly single and double quotes, apostrophes, various forms of hyphens and ellipses.
    * Previously, these were not substituted (e.g., replacing curly quotes with straight quotes), but attempting to do so would have failed anyway because they were not initially read correctly due to the lack of encoding specification.
    * Now non-standard characters are more likely to be read correctly, and those mentioned above are substituted with standard versions. If necessary, the encoding can be changed from UTF-8 via the new argument.
    * It appears that the EPUB format *requires* UTF encoding. Currently the only permissible option other than UTF-8 is UTF-16. This keeps things very simple and straightforward. Users should not encounter EPUB files in other encodings.
* Added unit tests and updated documentation.

# epubr 0.4.1

* Improved handling of errors and better messages.
* More robust handling of `title` field when missing, redundant or requiring remapping/renaming. All outputs of `epub` now include a `title` as well as `data` field, even if the e-book does not have a metadata field named `title`.
* Minor improvements to e-book section handling.
* Added `epub_head` function for previewing the opening text of each e-book section.
* Removed R version from Depends field of DESCRIPTION. Package Imports that necessitated a higher R version were previously removed.
* Minor fixes.
* Updated documentation, vignette, unit tests.

# epubr 0.4.0

* Enhanced function documentation details.
* Added `epub_meta` for strictly parsing EPUB metadata without reading the full file contents.
* When working with a vector of EPUB files, functions now clean up each unzipped archive temp directory with `unlink` immediately after use, rather than after all files are read into memory or by overwriting files in a single temp directory.
* Added initial introduction vignette content.
* Minor function refactors.
* Minor bug fixes.
* Added unit tests.

# epubr 0.3.0

* Refactor functions.
* Further reduce package dependencies.
* Update unit tests and documentation.

# epubr 0.2.0

* Refactor functions.
* Move contextual and e-book collection-specific functionality to other packages.
* Make any other remaining edge-case related options hidden arguments so that general usage of `epubr` functions is not too inflexible.
* Reduce package dependencies.
* Add basic unit tests.
* Add example public domain EPUB book for examples and testing.
* Update readme and documentation.

# epubr 0.1.0

* Added initial package scaffolding and function.
