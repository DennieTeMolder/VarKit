varkit_geno <- function(x) {
  UseMethod("varkit_geno", x)
}

##' @export
varkit_geno.data.frame <- function(x) {
  x <- t(x)
  # Tibbles require colnames, supply arbitrary ones
  colnames(x) <- paste0("V", 1:ncol(x))
  x <- tibble::as_tibble(x, rownames = NULL, .name_repair = "minimal")
  class(x) <- c("varkit_geno", "tbl_df", "tbl", "data.frame")
  x
}
