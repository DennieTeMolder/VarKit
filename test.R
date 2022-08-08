# Setup
devtools::load_all()
example <- "./inst/extdata/celegans.vcf.bgz"

# Reload
devtools::unload()
devtools::load_all()

# Document
devtools::document()
devtools::unload()
devtools::load_all()


### Examples ------
## Varkit object ------
read_vcf_header(example)
vcf <- read_vcf(example, skip_cols = c("info", "filter", "N2"))
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


## Linked sites ------
vcf <- read_vcf(example, skip_cols = c("info", "filter"))
vcf$geno <- .bi_gt2dosage(vcf$geno)

llist <- find_identical_sites(vcf, cores = 1)
summary(llist)

vcf <- mask(vcf, llist)
summary(vcf)

llist <- find_linked_sites(vcf, cutoff = 0.7)
summary(llist)


## Link list ------
names <- LETTERS[1:10]
llist <- link_list(names)
llist <- link_entries(llist, 3, c(2, 7))
print(llist, n = NULL)
lookup(llist , c(3, 6))

llist <- link_entries(llist, 6, 3)
lookup(llist, 6)
which_remove(llist)

temp_file <- tempfile(fileext = ".json")
write_link_list(llist, temp_file)
read_link_list(temp_file)


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
