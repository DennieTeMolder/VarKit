##' @export
site <- function(x, ..., .add_index = FALSE) {
  UseMethod("site", x)
}

##' @export
site.varkit <- function(x, ..., .add_index = FALSE) {
  .stopifnotsingle(.add_index, what = "logical")

  if (...length() > 0L) {
    col_select <- tidyselect::eval_select(
      rlang::expr(c(...)),
      data = linked_data_unclass(x)$site
    )
    res <- linked_data_get(x, i = "site", , col_select, .drop = is.null(...names()))
    if (!is.vector(res))
      colnames(res) <- names(col_select)
  } else {
    res <- linked_data_get(x, i = "site")
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
