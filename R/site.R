##' @export
site <- function(x, ..., .add_index = FALSE) {
  UseMethod("site", x)
}

##' @export
site.varkit <- function(x, ..., .add_index = FALSE) {
  .stopifnotsingle(.add_index, what = "logical")

  if (...length() > 0L) {
    col_select <- tidyselect::eval_select(rlang::expr(c(...)), data = unclass(x)$site)
    res <- linked_data_get(x, i = "site", , col_select, .drop = is.null(...names()))
    if (!is.vector(res))
      colnames(res) <- names(col_select)
  } else {
    res <- x$site
  }

  if (.add_index)
    res$idx <- which(attr(x, "indices")$sites)

  res
}

##' @export
`site<-` <- function(x, value) {
  if (!is_varkit(x))
    stop("`site()<-` is only supported on 'varkit' objects!")
  if (!is.data.frame(value))
    stop("The new value must be a data frame!")
  if (nrow(value) != nrow(unclass(x)$site)) {
    if (!all(.indices(x)$sites))
      stop("Remove current mask before modifying! See `unmask_all()`.")
    stop("The new value should have an equal number of rows!\n",
         "Use `mask()/retain()` (+ `delete_masked_sites()`) to remove rows/sites.")
  }

  x$site <- value
  validate_varkit(x)
}
