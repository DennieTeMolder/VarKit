# Setup
Sys.setenv("VROOM_CONNECTION_SIZE" = 1e6)
library(devtools)
load_all()
example <- "./inst/extdata/celegans.vcf.bgz"


### Examples ------
## Varkit object ------
read_vcf_header(example)

vcf <- read_vcf(example, skip_cols = c("id", "filter", "N2"))
print(vcf)

varkit_help(vcf)

meta(vcf)
site(vcf)
geno(vcf)
samples(vcf)

parse_meta(vcf)
parse_meta(vcf, type = "INFO")
parse_meta(vcf, type = "contig")

parse_geno(vcf)
parse_geno(vcf, type = "GT")
parse_geno(vcf, type = "DP")

parse_info(vcf)
parse_info(vcf, type = "DP")

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
vcf <- unmask_all(read_vcf(example))
vcf$geno <- parse_format_type(vcf, type = "GT")
vcf$geno <- .bi_gt2dosage(vcf$geno)

links <- find_linked_sites(vcf, n_window = -1, cores = 1)
lgraph <- resolve_links(links, cores = 1)
summary(lgraph, vcf = vcf)

temp_file <- tempfile(fileext = ".xml")
write_graph(lgraph, temp_file)
read_graph(temp_file)

summary(mask(vcf, lgraph))

groups <- igraph::groups(lgraph)
length(groups)
length(unlist(groups))


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
