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

##' @export
`site<-` <- function(x, value) {
  if (!is_varkit(x))
    stop("`site() <-` is only supported on 'varkit' objects!")
  if (!is.data.frame(value))
    stop("The new value must be a data frame!")
  if (!all(c("chrom", "pos") %in% colnames(value)))
    stop("The new value must include columns 'chrom' & 'pos'")
  if (nrow(value) != nrow(x$site)) {
    if (!all(x$use_site))
      stop("Remove current mask before modifying! See `unmask_all()`.")
    stop("The new value should have an equal number of rows!\n",
         "Use `mask()/retain()` (+ `remove_masked()`) to remove rows/sites.")
  }
  value$chrom <- as.character(value$chrom)
  value$pos <- as.numeric(value$pos)

  x$meta <- value
  x
}
