# VarKit

R toolkit for working with variant data.

## Description
The goal of VarKit is to simplify the downstream filtering and analysis of VCF files, in a way that is reasonably memory efficient.

Due to the limitations of R it does not aim to be a comprehensive tool for manipulating VCF files.
For many of such operations the user is better off using [Bcftools](https://samtools.github.io/bcftools/) CLI (e.g. the "query" subcommand for subsetting or selecting specific fields).

## Installation
Download `varkit_0.1.0.tar.gz` from the releases tab ([link](https://git.wur.nl/molde006/varkit/uploads/ec2b654c670285f91ae25af7202bd01b/varkit_0.1.0.tar.gz)).

Run the following command (adjust the path first!):

``` R
install.packages("/path/to/downloads/varkit_0.1.0.tar.gz")
```

And load the package using:

``` R
library(varkit)
```

## Examples
Coming soon…

## Authors and acknowledgements
Dennie te Molder: Creator and main developer.

## License
GPL-3

## Project status
Beta, under active development. 

Nearly all functionality is implemented, but documentation is still sorely lacking.
