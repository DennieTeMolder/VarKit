# Setup
library(devtools)
load_all()
example <- "./inst/extdata/celegans.vcf.bgz"

header <- read_vcf_header(example)
body <- read_tbl(example, col.names = header$col_names)
site <- body[.fixed_columns()]
geno <- varkit_geno(body[setdiff(colnames(body), colnames(site))])
rm(body)

vcf <- varkit(meta = header$meta, site, geno)
drm_get(vcf)

drm_preselect(drm_get(vcf$site), sites = vcf$site$chrom == "I")

drm_n_total(vcf$site, "sites")
drm_n_selected(vcf$site, "sites")

n_sites(vcf)

site(vcf, chrom)

# REVIEW make site() never drop?
# REVIEW allow all sites to masked in a VCF

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
parse_geno(vcf, type = "GT")
parse_geno(vcf, type = "DP")

parse_info(vcf)
str(parse_info(vcf, type = "DP"))
parse_info_all(vcf)

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

summary(delete_masked(vcf))

## Varkit_geno object ------
apply(geno(vcf), "samples", length)
apply(geno(vcf), "sites", length)

## Linked sites ------
vcf <- unmask_all(read_vcf(example))
geno(vcf) <- as_integer(round(ad2aaf(vcf)$aaf) * 2L)

links <- find_linked_sites(vcf, windows = -1, return_r2 = TRUE, cores = 1)
lgraph <- resolve_links(links, cores = 1)
summary(lgraph, vcf = vcf)

summary(mask(vcf, is_linked(lgraph, vcf = .)))
summary(is_linked(lgraph, unmask_all(vcf)))

temp_file <- tempfile(fileext = ".xml")
write_graph(lgraph, temp_file)
read_graph(temp_file)

groups <- igraph::groups(lgraph)
length(groups)
length(unlist(groups))

## Index ------
# TODO test N_MAX
index <- index_vcf(example, col_select = -info)
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

### Data relation model ------
my_drm <- drm_create()

x <- geno(vcf)
has_drm(x)
drm_get(x)

x <- drm_do_attach(x, my_drm, c("samples", "sites"))
has_drm(x)
drm_get(x)

y <- site(vcf)
y <- drm_do_attach(y, my_drm, c("sites", NA))

drm_preselect(my_drm, sites = y$qual > 2000)
drm_preselect(my_drm, sites = drm_do_subset(y, , "ref") == "C")

drm_do_subset(x)
drm_do_subset(y)
drm_do_subset(y, , "ref", .drop = TRUE)

z <- samples(vcf)
z <- drm_do_attach(z, my_drm, "samples")
drm_preselect(my_drm, samples = z %in% z[1:5])
drm_do_subset(z)
drm_do_subset(x)

y_site_keys <- dplyr::select(dplyr::arrange(y, qual), chrom, pos)
drm_do_match(y, sites = c(!!!y_site_keys), .override = T)
drm_get(y)

check_cols <- names(my_drm$nodes$sites$match_on)
all.equal(
  drm_do_match(y, sites = c(chrom, pos))[check_cols],
  dplyr::inner_join(my_drm$nodes$sites$match_on, y)[check_cols],
  check.attributes = FALSE
)

drm_do_match(y, sites = c(pos), .override = T)
drm_get(y)

drm_deselect(my_drm, sites)

drm_forget(my_drm)

drm_trim(my_drm, y)
drm_contains(my_drm, x)
drm_copy(my_drm)

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
