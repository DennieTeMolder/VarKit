##' @export
site <- function(x,
                 cols = NULL,
                 add_index = FALSE,
                 by_chrom = FALSE) {
  UseMethod("site", x)
}

##' @export
site.varkit <- function(x,
                        cols = NULL,
                        add_index = FALSE,
                        by_chrom = FALSE) {
  if (!is.null(cols)) {
    # Checks
    stopifnot(is.character(cols))
    if (!all(cols %in% colnames(x$site)))
      stop("Not all 'cols' are in site(", substitute(x), ")!")
    if (by_chrom)
      cols <- unique(c("chrom", cols))

    # Subset cols
    res <- x$site[, cols]
  } else {
    res <- x$site
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
