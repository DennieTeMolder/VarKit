##' @export
geno <- function(x, ..., .drop = FALSE) {
  UseMethod("geno", x)
}

##' @export
geno.varkit <- function(x, ..., .drop = FALSE) {
  if (...length() > 0L) {
    .data <- setNames(nm = samples.varkit(x))
    loc <- tidyselect::eval_select(rlang::expr(c(...)), data = .data)
    loc <- setNames(which(.indices(x)$samples)[loc], nm = names(loc))

    res <- rd_get(x, i = "geno", samples = loc, , .drop = .drop)
    if (!is.vector(res))
      rownames(res) <- names(loc)
  } else {
    res <- rd_get(x, i = "geno", .drop = .drop)
  }

  res
}

##' @export
`geno<-` <- function(x, value) {
  if (!is_varkit(x))
    stop("`geno() <-` is only supported for 'varkit' objects!")
  `$<-.varkit`(x, "geno", value)
}
