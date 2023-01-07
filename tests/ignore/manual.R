# Setup
Sys.setenv("VROOM_CONNECTION_SIZE" = 1e6)
library(devtools)
load_all()
example <- "./inst/extdata/celegans.vcf.bgz"


### Examples ------
## Varkit object ------
read_vcf_header(example)

vcf <- read_vcf(example, skip_cols = c("info", "filter", "N2"))
print(vcf)

varkit_help(vcf)

meta(vcf)
site(vcf)
geno(vcf)
samples(vcf)

parse_meta_type(vcf, "INFO")
parse_meta_type(vcf, "contig")

hide_site <- site(vcf)$chrom == "I"
vcf <- mask(vcf, site = hide_site)
summary(vcf)
site(vcf)

my_mask <- export_mask(vcf)
vcf <- unmask_all(vcf)
summary(vcf)

vcf <- import_mask(vcf, my_mask)
summary(vcf)

temp_file <- tempfile(fileext = ".vcf.bz2")
write_vcf(vcf, temp_file, replace_filter = TRUE)
read_vcf(temp_file)

summary(remove_masked(vcf))


## Linked sites ------
vcf <- read_vcf(example, skip_cols = c("info", "filter"))
vcf$geno <- .bi_gt2dosage(vcf$geno)

llist <- find_identical_sites(vcf, cores = 1)
summary(llist)

vcf <- mask(vcf, llist)
summary(vcf)

llist <- find_linked_sites(vcf, cutoff = 0.7)
summary(llist)

mask(vcf, llist)

temp_file <- tempfile(fileext = ".tsv.gz")
write_link_list(llist, temp_file)
summary(read_link_list(temp_file))


## Link list ------
names <- LETTERS[1:10]
llist <- link_list(names)

# Link entries by their index
llist <- link_entries(llist, 3, c(2, 7))
print(llist, n = NULL)
llist <- link_entries(llist, 6, 3)
print(llist, n = NULL)

# Lookup by name
lookup(llist, "F")
# Lookup by name to index
lookup(llist, "F", translate = "in")
# Lookup by index to name
lookup(llist, 6, translate = "out")
# Lookup by index to index
lookup(llist, 6, translate = "none")

# Request entries to be removed to make data unlinked
which_remove(llist, translate_out = TRUE)


### Tests ------
## Geno ------
header <- read_vcf_header(example)
data <- readr::read_tsv(example,
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
