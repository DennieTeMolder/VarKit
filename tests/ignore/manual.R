# Setup
devtools::load_all()
example <- "./inst/extdata/celegans.vcf.bgz"

# REVIEW allow all sites to masked in a VCF
# REVIEW check use of anyNA (slow on mtrx)

header <- read_vcf_header(example)
body <- read_tbl(example, col.names = header$col_names)
x <- as_varkit(body, meta = meta(header))

x <- read_vcf(example)


### Examples ------
## Varkit object ------
read_vcf_header(example)

vcf <- read_vcf(example, col_select = !c(id, filter, N2))
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
parse_geno(vcf, type = "FMT/GT")
parse_geno(vcf, type = "FORMAT/DP")

parse_info(vcf)
parse_info(vcf, type = "INFO/DP")
parse_info_all(vcf, use_meta = T)
parse_info_all(vcf, use_meta = F)

vcf <- mask(vcf, chrom == "I")
summary(vcf)
site(vcf)

my_mask <- export_mask(vcf)
vcf <- unmask_all(vcf)
summary(vcf)

vcf <- import_mask(vcf, my_mask)
summary(vcf)

temp_file <- tempfile(fileext = ".vcf.gz")
write_vcf(vcf, temp_file, replace_filter = TRUE)
read_vcf(temp_file)

summary(delete_masked_sites(vcf))

## Apply ------
apply(geno(vcf), "samples", length)
apply(geno(vcf), "sites", length)

## Linked sites ------
vcf <- unmask_all(read_vcf(example))
geno(vcf) <- as_integer(round(ad2aaf(vcf)$aaf) * 2L)

links <- find_linked_sites(vcf, windows = -1, return_r2 = TRUE, cores = 1)
lgraph <- resolve_links(links, cores = 1)
summary(lgraph, vcf = vcf)

summary(mask(vcf, is_linked(lgraph, vcf = .)))
summary(is_linked(lgraph, vcf))

temp_file <- tempfile(fileext = ".xml")
write_linkage_graph(lgraph, temp_file)
read_linkage_graph(temp_file)

groups <- igraph::groups(lgraph)
length(groups)
length(unlist(groups))

## Index ------
# TODO test N_MAX
index <- index_vcf(example, col_select = -info, n_max = 10)
print(index)

seek(index, "II")
seek(index)
seek(index, 1)
seek(index)

next_chunk(index)
next_chunk(index, col_select = -id)
next_chunk(index, col_select = NULL)

## Looping over indexes
# Using a complete index
res <- apply_index(index, function(vcf) {
  Sys.sleep(2)
  mask(vcf, qual < 1e5)
})

# Combining results
concat_vcf(res)
concat_vcf(res[[1]])
concat_vcf(res[[1]], res[2:3])

# Using a partial index
apply_index(index[c(1, 3), ], function(vcf) table(site(vcf, chrom)))

# Using a header only index while reading raw lines (also works with next_chunk)
h_index <- index_vcf_header(example, chunk_size  = 25)
seek(h_index, 1)
while (!is.null(vcf <- next_lines(h_index))) {
  str(vcf)
}

# The index can also contain 0 line entries which are skipped
test <- rbind(
  index[1:2, ],
  data.frame(id = "test", linum = index$linum[3], n_lines = 0L),
  index[3, ]
)

seek(test, 1)
seq_index(test)

## Related data ------
obj <- related_data()
obj <- rd_register(obj, meta[, ], site[sites, ], geno[samples, sites])

obj$meta <- vcf$meta

obj <- obj |>
  rd_set("site", vcf$site, sites = c(chrom, pos)) |>
  rd_set("geno", vcf$geno, samples = c(sample = rownames(.)))

# This should remain sorted
rd_set(
  obj,
  slot = "site",
  data = dplyr::slice_sample(vcf$site, prop = 1),
  sites = c(chrom, pos)
)$site

obj <- rd_filter(obj, samples = rep_len(c(T, F), n_samples(vcf)))

rd_get(obj, "geno", , 1)
rd_get(obj, "geno", samples = 1:5)

obj[]
str(obj)
summary(obj)

rd_reset(obj, "samples")

obj2 <- obj
obj2$geno <- NULL
rd_trim(obj2)
