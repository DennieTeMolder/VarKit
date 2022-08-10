##' @export
fixed <- function(x,
                  add_index = FALSE,
                  by_chrom = FALSE,
                  only_positions = FALSE) {
  UseMethod("fixed", x)
}

##' @export
fixed.varkit <- function(x,
                         add_index = FALSE,
                         by_chrom = FALSE,
                         only_positions = FALSE) {
  if (all(x$use_site)) {
    res <- x$fixed
  } else {
    res <- x$fixed[x$use_site, ]
  }
  if (only_positions)
    res[, !colnames(res) %in% c("chrom", "pos")] <- NULL
  if (add_index)
    res <- dplyr::bind_cols(res, idx = which(x$use_site))
  if (by_chrom) {
    chrom <- unique(res$chrom)
    my_fun <- function(chr)
      res[res$chrom == chr, ]
    res <- sapply(chrom, my_fun, simplify = FALSE)
  }
  res
}
