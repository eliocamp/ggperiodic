# ggperiodic 1.0.2

* Updates documentation to CRAN HTML5 format

# ggperiodic 1.0.1

* Removes vdiffr from dependencies.

# ggperiodic 1.0.0 - Jörmungandr

## New Features

* `setperiodic()` and `setunperiodic()` modify objects
by reference. `periodic()` and `unperiodic()` can modify `data.table`s by
reference also by setting `options(ggperiodic.data.table.copy = TRUE)` 

* New `qwrap()` function for quickly wraping to any range.

## Bug Fixes

* Fixed a nasty bug in the implementation of `wrap()`.

# ggperiodic 0.1.0 - Möbius strip

* Implemented `dplyr` methods. 
* Published on github
* Added a `NEWS.md` file to track changes to the package.

