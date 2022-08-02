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
print.varkit_meta <- function(x, n = 10, verbose = TRUE) {
  cat("Meta header data (meta(x)):\n")
  print(tibble::tibble(x), n = n)
  if (verbose) {
    cat("\nMeta summary (summary(meta(x))):\n")
    print(summary(x), n = n)
  }
  invisible(x)
}

##' @export
summary.varkit_meta <- function(x) {
  x <- dplyr::summarise(dplyr::group_by(x, name), lines = dplyr::n())
  colnames(x)[1] <- "type"
  x
}
