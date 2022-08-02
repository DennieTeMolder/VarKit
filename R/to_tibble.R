##' @export
to_tibble <- function(x) {
  UseMethod("to_tibble", x)
}

##' @export
to_tibble.varkit_geno <- function(x) {
  tibble::as_tibble(as.data.frame(x))
}
