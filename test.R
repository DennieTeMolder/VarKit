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

# Check if geno is a transposed version of data
all(sapply(seq_along(data), \(i) all(geno[i,] == data[,i])))
all(colnames(data) == rownames(geno))


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

write_vcf(vcf, "/tmp/test.vcf.bz2", replace_filter = TRUE)
read_vcf("/tmp/test.vcf.bz2")
