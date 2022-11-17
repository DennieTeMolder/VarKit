##' @export
get_meta_types <- function(x) {
  UseMethod("get_meta_types", x)
}

##' @export
get_meta_types.default <- function(x) {
  df <- data.frame(table(x$name))
  colnames(df) <- c("type", "lines")
  tibble::as_tibble(df[order(df$type), ])
}

##' @export
get_meta_types.varkit_header <- function(x) {
  get_meta_types(meta(x))
}
