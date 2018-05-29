## Test environments
* local Windows 10 install, R 3.5.0
* ubuntu 14.04 (on travis-ci), R 3.5.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release resubmission.

This resubmission fixes a broken url in the documentation.

This resubmission also incorporates additional functionality and documentation. All package builds, tests, and checks have been redone.

## Downstream dependencies

I have also run R CMD check on downstream dependencies of epubr 
(https://github.com/leonawicz/epubr/blob/master/revdep/checks.rds). 
All packages passed.
