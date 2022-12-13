##' @export
fixed <- function(x,
                  add_index = FALSE,
                  by_chrom = FALSE,
                  cols = NULL) {
  UseMethod("fixed", x)
}

##' @export
fixed.varkit <- function(x,
                         add_index = FALSE,
                         by_chrom = FALSE,
                         cols = NULL) {
  if (!is.null(cols)) {
    # Checks
    stopifnot(is.character(cols))
    if (!all(cols %in% colnames(x$fixed)))
      stop("Not all 'cols' are in fixed(", substitute(x), ")!")
    if (by_chrom)
      cols <- unique(c("chrom", cols))

    # Subset cols
    res <- x$fixed[, cols]
  } else {
    res <- x$fixed
  }

  if (!all(x$use_site))
    res <- res[x$use_site,]
  if (add_index)
    res$idx <- which(x$use_site)

  if (by_chrom) {
    chroms <- unique(res$chrom)
    return(sapply(chroms, simplify = FALSE, FUN = function(chr) {
      res[res$chrom == chr,]
    }))
  }

  if (!is.null(cols) && length(cols) == 1)
    return(unlist(res, use.names = FALSE))
  res
}
