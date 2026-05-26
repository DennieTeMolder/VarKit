##' @export
as_varkit <- function(x, ...) {
  UseMethod("as_varkit", x)
}

##' @export
as_varkit.data.frame <- function(x, ...) {
  stopifnot(is.data.frame(x))

  if (nrow(x) == 0L)
    return(NULL)

  site <- x[tolower(colnames(x)) %in% .fixed_columns()]
  geno <- x[!colnames(x) %in% colnames(site)]
  rm(x)

  colnames(site) <- tolower(colnames(site))
  geno <- do.call(rbind, geno)

  varkit(site = site, geno = geno, ...)
}
