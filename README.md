
<!-- README.md is generated from README.Rmd. Please edit that file -->

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

You can install the latest development version of VarKit using the
`remotes` package:

``` r
install.packages("remotes")
remotes::install_gitlab("molde006/varkit", host = "git.wur.nl", dependencies = NA, upgrade = TRUE)
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

Or download the [introduction
vignette](https://git.wur.nl/molde006/varkit/-/raw/main/doc/varkit.html?inline=false)
and open it in your browser.

## Authors and acknowledgements

Dennie te Molder: Creator and main developer.

## License

GPL-3

## Project status

Beta, under active development.

Nearly all functionality is implemented, but documentation is still
sorely lacking.

## Alternative installation method

Download `varkit_0.8.1.tar.gz` from the releases tab
([link](https://git.wur.nl/molde006/varkit/uploads/452778708f0a05a1e1831f347e1d6daf/varkit_0.8.1.tar.gz)).

Run the following commands and select the downloaded file in the window
that pops up:

``` r
install.packages("remotes")
install_varkit <- function(pkg_path, upgrade_dependencies = TRUE) {
  remotes::install_deps(pkg_path, upgrade = if (upgrade_dependencies) "always" else "never")
  install.packages(pkg_path, repos = NULL, type = "source")
}

# If desired you can set 'upgrade_dependencies' to FALSE
install_varkit(file.choose(), upgrade_dependencies = TRUE)
```
