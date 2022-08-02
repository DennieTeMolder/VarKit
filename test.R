# Load package
devtools::load_all()

# Reload
devtools::unload()
devtools::load_all()

# Document
devtools::document()
devtools::unload()
devtools::load_all()

# Setup
path <- "./inst/extdata/celegans.vcf.bgz"

## Geno tests ------
header <- read_vcf_header(path)
data <- readr::read_tsv(path,
  col_names = header$col_names,
  col_types = readr::cols(),
  comment = "#",
  na = "."
)
data[, .fixed_columns()] <- NULL

geno <- varkit_geno(data)

test <- data |>
  dplyr::mutate(rowname = paste0("V", 1:nrow(data))) |>
  tidyr::pivot_longer(-rowname) |>
  tidyr::pivot_wider(names_from = rowname) |>
  dplyr::select(-name)

isTRUE(all.equal(geno, test, check.attributes = F))

## Examples ------
read_vcf_header(path)
vcf <- read_vcf(path, skip_cols = c("info", "filter", "N2"))
print(vcf)

varkit_help(x = vcf)

meta(vcf)
sites(vcf)
fixed(vcf)
geno(vcf)
samples(vcf)

parse_meta_type(vcf, "INFO")
parse_meta_type(vcf, "contig")

hide_site <- sites(vcf)$chrom == "I"
vcf <- mask(vcf, site = hide_site)
summary(vcf)
sites(vcf)

my_mask <- export_mask(vcf)
vcf <- unmask_all(vcf)
summary(vcf)

vcf <- import_mask(vcf, my_mask)
summary(vcf)

summary(remove_masked(vcf))
