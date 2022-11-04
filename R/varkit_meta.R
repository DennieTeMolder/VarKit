##' @export
varkit_meta <- function(x) {
  stopifnot(is.character(x))
  x <- stringr::str_remove(x, "##")
  x <- stringr::str_split_fixed(x, "=", n = 2)
  colnames(x) <- c("name", "value")
  x <- tibble::as_tibble(x)
  class(x) <- c("varkit_meta", "tbl_df", "tbl", "data.frame")
  x
}

##' @export
is_varkit_meta <- function(x) {
  inherits(x, "varkit_meta")
}

##' @export
print.varkit_meta <- function(x, ..., n = NULL) {
  pillar:::print.tbl(x, ..., n = n)
  cat("# Use `summary(...)` to get an overview of available types/names.\n")
  invisible(x)
}

##' @export
##' @importFrom rlang .data
summary.varkit_meta <- function(object, ...) {
  object <- dplyr::summarise(dplyr::group_by(object, .data$name), lines = dplyr::n())
  colnames(object)[1] <- "type"
  object
}
