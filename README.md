# VarKit

R toolkit for working with variant data.

## Description

The goal of VarKit is to simplify the downstream filtering and analysis
of VCF files in a way that is reasonably memory efficient and compatible
with and comparable to the [tidyverse](https://www.tidyverse.org/) suite
of packages.

It provides methods to easily index, read (in chunks), and write VCF
files from R, access desired fields, dynamically mask specific sites
from the downstream analysis, and identify linkage between sites.

## Installation

You can install the latest release of VarKit using the `pak` package via
this custom function:

``` r
# install.packages("pak")
pak::pkg_install("DennieTeMolder/VarKit@stable", upgrade = FALSE)
```

And load the package using:

``` r
library(varkit)
```

## Examples

For examples please run the following after installation:

``` r
browseVignettes("varkit")
```

Or download the [introduction vignette](https://github.com/DennieTeMolder/VarKit/raw/refs/heads/stable/doc/varkit.html)
and open it in your browser.

## Development version

To install the latest development version of varkit run:

``` r
pak::pkg_install("DennieTeMolder/VarKit@main", upgrade = TRUE, dependencies = TRUE)
```

## Authors and acknowledgements

Dennie te Molder: Creator and main developer.

## License

GPL-3

## Project status

Beta, under active development.

Nearly all functionality is implemented, but documentation is still
sorely lacking.
