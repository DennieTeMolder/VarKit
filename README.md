
<!-- README.md is generated from README.Rmd. Please edit that file -->

# VarKit

R toolkit for working with variant data.

## Description

The goal of VarKit is to simplify the downstream filtering and analysis
of VCF files, in a way that is reasonably memory efficient and
compatible with and comparable to the
[tidyverse](https://www.tidyverse.org/) suite of packages.

It provides methods to easily read/write VCF files into/from R, access
desired fields, dynamically mask specific sites from the downstream
analysis, and identify linkage between sites.

### Notes

Due to the limitations of R this package does not aim to be a
comprehensive tool for manipulating VCF files. For many of the missing
operations the user is better off using the
[Bcftools](https://samtools.github.io/bcftools/) CLI (e.g. the “query”
subcommand for subsetting sites/samples or selecting specific fields).

For large VCF files (\> 1 Gb) it is often beneficial (in terms of speed
and memory usage) to subset the VCF by chromosome/scaffold before
loading it into R using the aforementioned Bcftools.

> Reading a VCF file uses **\~5.5x** the **uncompressed** VCF file size
> of memory (RAM) and the resulting varkit object occupies **\~2.5x**
> the uncompressed VCF file size of memory. However, specifying
> `skip_cols` will reduce this footprint.

## Installation

Download `varkit_0.4.1.tar.gz` from the releases tab
([link](https://git.wur.nl/molde006/varkit/uploads/c4bc2ea2cf3ed258022c2daccd202969/varkit_0.4.1.tar.gz)).

Run the following commands and select the downloaded file in the window
that pops up:

``` r
install.packages("remotes")
pkg_path <- file.choose()
remotes::install_deps(pkg_path, upgrade = "never")
install.packages(pkg_path, repos = NULL, type = "source")
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
