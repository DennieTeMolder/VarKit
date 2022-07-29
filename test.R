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

test <- data |>
  dplyr::mutate(rowname = paste0("V", 1:nrow(data))) |>
  tidyr::pivot_longer(-rowname) |>
  tidyr::pivot_wider(names_from = rowname) |>
  dplyr::select(-name)

isTRUE(all.equal(geno, test, check.attributes = F))

vcf <- read_vcf(path)
sites(vcf)
samples(vcf)
fixed(vcf)
geno(vcf)

mask(vcf, site = 1:5, sample = "N2")

# TODO
## export_mask(vcf)
## remove_all_masks(vcf)
## import_mask(vcf, my_mask)
