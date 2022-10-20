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
  if (only_positions) {
    res <- x$fixed[, c("chrom", "pos")]
  } else {
    res <- x$fixed
  }
  if (!all(x$use_site))
    res <- res[x$use_site,]
  if (add_index)
    res$idx <- which(x$use_site)
  if (by_chrom)
    return(split(res, ~chrom))
  res
}
