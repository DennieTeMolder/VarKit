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
print.varkit_meta <- function(x, n = 10) {
  cat("Meta header data (meta(x)):\n")
  print(tibble::tibble(x), n = n)
  cat("\nMeta summary (summary(meta(x))):\n")
  print(summary(x), n = n)
}

##' @export
summary.varkit_meta <- function(x) {
  x <- dplyr::summarise(dplyr::group_by(x, name), lines = dplyr::n())
  colnames(x)[1] <- "type"
  x
}
