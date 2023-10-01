# Example: lapply_rowwise(df, fucntion(...) x <- list(...) x$col_name)
##' @export
lapply_rowwise <- function(x, FUN, ..., envir = parent.frame()) {
  .mapply(dots = x, MoreArgs = list(...), FUN = function(...) {
    do.call(FUN, args = list(...), envir = envir)
  })
}
