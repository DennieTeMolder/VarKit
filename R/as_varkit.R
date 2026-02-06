##' @export
as_varkit <- function(x, ...) {
  UseMethod("as_varkit", x)
}

##' @export
as_varkit.data.frame <- function(x, ..., meta = NULL, use_filter = TRUE) {
  stopifnot(is.data.frame(x))
  .stopifnotsingle(use_filter, what = "logical")

  if (nrow(x) == 0L)
    return(NULL)

  site <- x[tolower(colnames(x)) %in% .fixed_columns()]
  geno <- x[!colnames(x) %in% colnames(site)]
  rm(x)

  colnames(site) <- tolower(colnames(site))
  geno <- t(geno)

  varkit(site = site, geno = geno, meta = meta, use_filter = use_filter)
}
