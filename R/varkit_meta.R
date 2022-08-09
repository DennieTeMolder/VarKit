##' @export
varkit_meta <- function(x) {
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
print.varkit_meta <- function(x, ..., n = 10, verbose = TRUE) {
  print(tibble::tibble(x), ..., n = n)
  if (verbose) {
    cat("\nMeta summary (summary(x)):\n")
    print(summary(x), ..., n = n)
  }
  invisible(x)
}

##' @export
##' @importFrom rlang .data
summary.varkit_meta <- function(object, ...) {
  object <- dplyr::summarise(dplyr::group_by(object, .data$name), lines = dplyr::n())
  colnames(object)[1] <- "type"
  object
}
