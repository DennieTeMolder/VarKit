devtools::document()
devtools::unload()
devtools::load_all()

MemSize <- function(x) {
  format(object.size(x), units = "auto")
}

path <- "./inst/extdata/celegans.vcf.bgz"

header <- read_vcf_header(path)
data <- readr::read_tsv(path,
  col_names = header$col_names,
  col_types = readr::cols(),
  comment = "#",
  na = "."
)
data[, .fixed_columns()] <- NULL

geno <- varkit_geno(data)
isTRUE(all.equal(data, to_tibble(geno), check.attributes = F))

vcf <- read_vcf(path)
sites(vcf)
samples(vcf)
info(vcf)
geno(vcf)

mask(vcf, site = 1:5, sample = "N2")
