##' @export
site <- function(x, ..., .drop = NULL, .add_index = FALSE) {
  UseMethod("site", x)
}

##' @export
site.varkit <- function(x, ..., .drop = NULL, .add_index = FALSE) {
  .stopifnotsingle(.add_index, what = "logical")
  if (is.null(.drop)) {
    # REVIEW should this always default to FALSE like geno()?
    .drop <- ...length() > 0L && is.null(...names())
  } else {
    .stopifnotsingle(.drop, what = "logical")
  }

  if (...length() > 0L) {
    col_select <- tidyselect::eval_select(
      rlang::expr(c(...)),
      data = rd_unclass(x)$site
    )
    res <- rd_get(x, i = "site", , col_select, .drop = .drop)
    if (!is.vector(res))
      colnames(res) <- names(col_select)
  } else {
    res <- rd_get(x, i = "site", .drop = .drop)
  }

  if (.add_index)
    res$idx <- which(attr(x, "indices")$sites)

  res
}

##' @export
`site<-` <- function(x, value) {
  if (!is_varkit(x))
    stop("`site()<-` is only supported on 'varkit' objects!")
  `$<-.varkit`(x, "site", value)
}
