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
fixed(vcf)
geno(vcf)
samples(vcf)

parse_meta_type(vcf, "INFO")
parse_meta_type(vcf, "contig")

hide_site <- fixed(vcf)$chrom == "I"
vcf <- mask(vcf, site = hide_site)
summary(vcf)
fixed(vcf)

my_mask <- export_mask(vcf)
vcf <- unmask_all(vcf)
summary(vcf)

vcf <- import_mask(vcf, my_mask)
summary(vcf)

summary(remove_masked(vcf))

temp_file <- tempfile(fileext = ".vcf.bz2")
write_vcf(vcf, temp_file, replace_filter = TRUE)
read_vcf(temp_file)

## Link list ------
names <- LETTERS[1:10]
ll <- link_list(names)
ll <- link_entries(ll, 3, c(2, 7))
print(ll, n = NULL)
lookup(ll , c(3, 6))

ll <- link_entries(ll, 6, 3)
lookup(ll, 6)
which_remove(ll)

temp_file <- tempfile(fileext = ".json")
write_link_list(ll, temp_file)
read_link_list(temp_file)

## Linkage ------
vcf$geno <- .bi_gt2dosage(vcf$geno)
vcf <- unmask_all(vcf)
hide_site <- apply(geno(vcf), 2, .not_variable)
vcf <- mask(vcf, site = hide_site)

devtools::unload()
devtools::load_all()

x <- geno(vcf)[,1]
y <- geno(vcf)[,2]
