##' @export
samples <- function(x) {
  UseMethod("samples", x)
}

##' @export
samples.varkit_geno <- function(x) {
  rownames(x)
}

##' @export
samples.varkit <- function(x) {
  samples.varkit_geno(drm_do_subset(x$geno, , integer()))
}

##' @export
samples.varkit_header <- function(x) {
  x$col_names[!tolower(x$col_names) %in% .fixed_columns()]
}

##' @export
`samples<-` <- function(x, value) {
  if (!(is_varkit_geno(x) || is_varkit(x)))
    stop("`samples() <-` is only supported for 'varkit' & 'varkit_geno' objects!")

  gt <- if (is_varkit(x)) x$geno else x
  if (length(value) != nrow(gt))
    stop("The length of the new value should match `samples()`!")
  rownames(gt) <- value

  if (anyDuplicated(rownames(gt)) != 0L)
    stop("The new value contains duplicates!")

  if (is_varkit_geno(x))
    return(gt)
  x$geno <- gt
  x
}
