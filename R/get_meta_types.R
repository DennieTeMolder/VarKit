##' @export
get_meta_types <- function(x) {
  UseMethod("get_meta_types", x)
}

##' @export
get_meta_types.data.frame <- function(x) {
  stopifnot("name" %in% colnames(x))
  df <- as.data.frame(table(x$name), stringsAsFactors = FALSE)
  colnames(df) <- c("type", "n")
  tibble::as_tibble(df[order(df$type), ])
}

##' @export
get_meta_types.varkit_header <- function(x) {
  get_meta_types(meta(x))
}
