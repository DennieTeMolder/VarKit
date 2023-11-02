##' @export
site <- function(x, ..., .add_index = FALSE) {
  UseMethod("site", x)
}

##' @export
site.varkit <- function(x, ..., .add_index = FALSE) {
  res <- x$site
  if (...length() > 0L)
    res <- dplyr::select(res, ...)
  if (.add_index)
    res <- dplyr::bind_cols(res, idx = seq_len(nrow(res)))
  if (!all(x$use_site))
    res <- res[x$use_site, ]

  if (ncol(res) == 1L && ...length() > 0L)
    return(res[[1L]])
  res
}

##' @export
`site<-` <- function(x, value) {
  if (!is_varkit(x))
    stop("`site() <-` is only supported on 'varkit' objects!")
  if (!is.data.frame(value))
    stop("The new value must be a data frame!")
  if (nrow(value) != nrow(x$site)) {
    if (!all(x$use_site))
      stop("Remove current mask before modifying! See `unmask_all()`.")
    stop("The new value should have an equal number of rows!\n",
         "Use `mask()/retain()` (+ `remove_masked()`) to remove rows/sites.")
  }

  x$site <- value
  validate_varkit(x)
}
