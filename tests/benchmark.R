devtools::load_all()

path <- "./inst/extdata/celegans.vcf.bgz"
header <- read_vcf_header(path)
data <- readr::read_tsv(path,
  col_names = header$col_names,
  col_types = readr::cols(),
  comment = "#",
  na = "."
)
data[, .fixed_columns()] <- NULL

# Create test dataset
test <- NULL
for (i in 1:100) {
  test <- dplyr::bind_rows(test, data)
}

mat <- as.matrix(test)
tmat <- t(mat)
geno <- tibble::as_tibble(tmat, rownames = NULL)

# Memory usage
pryr::object_size(test)
## 92.89 MB
pryr::object_size(geno)
## 179.95 MB
pryr::object_size(mat)
## 92.88 MB
pryr::object_size(tmat)
## 92.88 MB

# Selecting sites
sample_sites <- purrr::map(1:1e3, ~sample(1:nrow(test), nrow(data), replace = F))
system.time(for (i in sample_sites) test[i, ])
##  user  system elapsed
## 3.075   0.009   3.087
system.time(for (i in sample_sites) geno[, i])
##  user  system elapsed
## 0.918   0.000   0.919
system.time(for (i in sample_sites) mat[i, ])
##  user  system elapsed
## 3.483   0.000   3.486
system.time(for (i in sample_sites) tmat[, i])
##  user  system elapsed
## 1.187   0.004   1.192
system.time(for (i in sample_sites) dplyr::slice(test, i))
##  user  system elapsed
## 4.445   0.000   4.450

# Selecting samples
sample_samples <- purrr::map(1:1e3, ~sample(1:ncol(test), 10, replace = F))
system.time(for (i in sample_samples) test[, i])
##  user  system elapsed
## 0.034   0.000   0.033
system.time(geno[i, ]) * length(sample_samples)
## user  system elapsed
## 2398       0    2401
system.time(for (i in sample_samples) mat[, i])
##   user  system elapsed
## 43.906   2.793  46.760
system.time(for (i in sample_samples) tmat[i, ])
##   user  system elapsed
## 45.167   2.717  47.936
system.time(dplyr::slice(geno, i)) * length(sample_samples)
##  user  system elapsed
## 10518       0   10528

# Sample both
system.time(for (i in 1:1e3) test[sample_sites[[i]], sample_samples[[i]]])
##  user  system elapsed
## 1.947   0.000   1.949
system.time(for (i in 1:1e3) geno[sample_samples[[i]], sample_sites[[i]]])
##   user  system elapsed
## 25.660   0.000  25.741
system.time(for (i in 1:1e3) mat[sample_sites[[i]], sample_samples[[i]]])
##  user  system elapsed
## 2.246   0.000   2.248
system.time(for (i in 1:1e3) tmat[sample_samples[[i]], sample_sites[[i]]])
##  user  system elapsed
## 1.030   0.000   1.031

# Boolean all TRUE for cols
use_cols <- rep(T, ncol(test))
system.time(for (i in sample_sites) test[i, use_cols])
##  user  system elapsed
## 3.203   0.000   3.206
system.time(for (i in sample_sites) geno[use_cols, i])
##   user  system elapsed
## 25.615   0.006  25.654
system.time(for (i in sample_sites) mat[i, use_cols])
##  user  system elapsed
## 3.681   0.000   3.687
system.time(for (i in sample_sites) tmat[use_cols, i])
##  user  system elapsed
## 1.324   0.000   1.325
