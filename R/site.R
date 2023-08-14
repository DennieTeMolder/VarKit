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
    col_in_site <- cols %in% colnames(x$site)
    if (!all(col_in_site))
      stop("The following 'cols' are not present in site(", substitute(x), "): ",
           .collapse(cols[!col_in_site]))
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
    return(lapply(chroms, function(chr) res[res$chrom == chr, ]))
  }

  if (!is.null(cols) && length(cols) == 1)
    return(unlist(res, use.names = FALSE))
  res
}
