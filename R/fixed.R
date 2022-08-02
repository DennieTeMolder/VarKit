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
  res <- x$fixed[x$use_site, ]
  if (only_positions)
    res <- dplyr::select(res, chrom, pos)
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
